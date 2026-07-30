;;;; src/pike-vm.lisp
(in-package #:cl-regex-kit)

(defstruct match-result start
  end
  groups
  group-names)

(defstruct vm-thread pc
  slots)

(defun slot-count-for-program (program)
  (loop for instruction across program
        when (eq (inst-op instruction) :save)
          maximize (1+ (inst-a instruction)) into maximum
        finally (return (or maximum 0))))

(defun make-match-result-from-slots (slots slot-count)
  (let ((groups (make-array (/ slot-count 2) :initial-element nil)))
    (dotimes (index (length groups))
      (let ((from (aref slots (* 2 index)))
            (to (aref slots (1+ (* 2 index)))))
        (when (and from to)
          (setf (aref groups index) (cons from to)))))
    (make-match-result :start (aref slots 0) :end (aref slots 1) :groups groups)))

(defun instruction-matches-p (instruction element)
  "Return true when consuming INSTRUCTION accepts CHARACTER or an octet."
  (case (inst-op instruction)
    (:char
      (if (characterp element) (if (literal-node-case-insensitive-p (inst-a instruction)) (funcall
            (if (literal-node-unicode-p (inst-a instruction)) #'unicode-case-insensitive-char=
              #'ascii-case-insensitive-char=)
            element
            (literal-node-char (inst-a instruction)))
          (char= element (literal-node-char (inst-a instruction))))
        (let ((literal (char-code (literal-node-char (inst-a instruction)))))
          (if (literal-node-case-insensitive-p (inst-a instruction)) (= (ascii-fold-octet element) (ascii-fold-octet literal))
            (= element literal)))))
    (:class
      (if (characterp element) (class-matches-p (inst-a instruction) element)
        (class-matches-octet-p (inst-a instruction) element)))
    (:any
      (let ((node (inst-a instruction)))
        (or
          (any-char-node-dotall-p node)
          (if (characterp element) (if (any-char-node-crlf-p node) (and (not (char= element #\Return)) (not (char= element #\Newline)))
              (not (char= element (any-char-node-line-terminator node))))
            (if (any-char-node-crlf-p node) (and (/= element #x0d) (/= element #x0a))
              (/= element (char-code (any-char-node-line-terminator node))))))))))

(defun instruction-unicode-p (instruction)
  "Return whether a consuming INSTRUCTION uses Unicode scalar semantics."
  (case (inst-op instruction)
    (:char (literal-node-unicode-p (inst-a instruction)))
    (:class (char-class-node-unicode-p (inst-a instruction)))
    (:any (any-char-node-unicode-p (inst-a instruction)))))

(defun newline-element-p (element)
  "Return true when ELEMENT is a character or octet newline."
  (if (characterp element)
      (char= element #\Newline)
      (= element #x0a)))

(defun instruction-match-end (instruction text position limit byte-mode-p never-newline-p)
  "Return the exclusive end index and success flag for a consuming instruction."
  (cond
    ((>= position limit) (values nil nil))
    ((and byte-mode-p (instruction-unicode-p instruction))
      (multiple-value-bind (character end valid-p) (utf8-character-at text position)
        (if (and valid-p (<= end limit)
                 (not (and never-newline-p (newline-element-p character)))
                 (instruction-matches-p instruction character))
            (values end t)
          (values nil nil))))
    (t
      (let ((element (elt text position)))
        (if (and (not (and never-newline-p (newline-element-p element)))
                 (instruction-matches-p instruction element))
            (values (1+ position) t)
          (values nil nil))))))

(defun vm-word-position-p (kind text position byte-mode-p unicode-p)
  "Evaluate a word-boundary KIND at POSITION for string or byte input."
  (if byte-mode-p (if unicode-p (ecase kind
        (:boundary (byte-unicode-word-boundary-p text position))
        (:start (byte-unicode-word-start-p text position))
        (:end (byte-unicode-word-end-p text position))
        (:start-half (byte-unicode-word-start-half-p text position))
        (:end-half (byte-unicode-word-end-half-p text position)))
      (ecase kind
        (:boundary (byte-word-boundary-p text position))
        (:start (byte-word-start-p text position))
        (:end (byte-word-end-p text position))
        (:start-half (byte-word-start-half-p text position))
        (:end-half (byte-word-end-half-p text position))))
    (ecase kind
      (:boundary (word-boundary-p text position unicode-p))
      (:start (word-start-p text position unicode-p))
      (:end (word-end-p text position unicode-p))
      (:start-half (word-start-half-p text position unicode-p))
      (:end-half (word-end-half-p text position unicode-p)))))

(defun vm-non-boundary-position-p (text position byte-mode-p unicode-p)
  "Return true when a non-boundary assertion may evaluate at POSITION."
  (or (not byte-mode-p)
      (not unicode-p)
      (byte-unicode-non-boundary-position-p text position)))

(defun zero-width-instruction-matches-p (instruction text position length byte-mode-p)
  "Return whether zero-width INSTRUCTION succeeds at POSITION."
  (case (inst-op instruction)
    (:bol
      (or (= position 0)
          (and (logbitp 0 (or (inst-b instruction) 0))
               (funcall (if byte-mode-p #'byte-line-start-p #'line-start-p)
                        text position
                        (logbitp 2 (or (inst-b instruction) 0))
                        (or (inst-c instruction) #\Newline)))))
    (:eol
      (or (= position length)
          (and (logbitp 0 (or (inst-b instruction) 0))
               (funcall (if byte-mode-p #'byte-line-end-p #'line-end-p)
                        text position
                        (logbitp 2 (or (inst-b instruction) 0))
                        (or (inst-c instruction) #\Newline)))))
    (:bos (= position 0))
    (:eos (= position length))
    (:boundary
      (vm-word-position-p :boundary text position byte-mode-p
                          (logbitp 1 (or (inst-b instruction) 0))))
    (:non-boundary
      (and (vm-non-boundary-position-p text position byte-mode-p
                                       (logbitp 1 (or (inst-b instruction) 0)))
           (not (vm-word-position-p :boundary text position byte-mode-p
                                    (logbitp 1 (or (inst-b instruction) 0))))))
    (:word-start
      (vm-word-position-p :start text position byte-mode-p
                          (logbitp 1 (or (inst-b instruction) 0))))
    (:word-end
      (vm-word-position-p :end text position byte-mode-p
                          (logbitp 1 (or (inst-b instruction) 0))))
    (:word-start-half
      (vm-word-position-p :start-half text position byte-mode-p
                          (logbitp 1 (or (inst-b instruction) 0))))
    (:word-end-half
      (vm-word-position-p :end-half text position byte-mode-p
                          (logbitp 1 (or (inst-b instruction) 0))))))

(defun pike-vm-closure (program text position length byte-mode-p seeds &key on-save)
  "Compute the epsilon-closure of SEEDS (a list of VM-THREADs) at POSITION:
expand :SPLIT/:JMP/:SAVE and every zero-width assertion, returning the
consuming-or-:MATCH/:SET-MATCH threads reached, each still paired with the
SLOTS its path accumulated.

ON-SAVE, when supplied, is called as (FUNCALL ON-SAVE INSTRUCTION SLOTS) to
compute a :SAVE instruction's continuation slots -- RUN-PIKE-VM's capturing
walk records POSITION into the target slot. Omitting it, as RUN-PIKE-VM-SET
does, threads SLOTS through unchanged, since set matching tracks no
captures; every seed there carries SLOTS NIL and it is never read."
  (let ((seen (make-array (length program) :initial-element nil))
        (head (list nil))
        (tail nil))
    (setf tail head)
    (labels ((enqueue (thread)
               (setf (cdr tail) (list thread)
                     tail (cdr tail)))
             (visit (pc slots)
               (unless (aref seen pc)
                 (setf (aref seen pc) t)
                 (let ((instruction (aref program pc)))
                   (case (inst-op instruction)
                     (:split
                      (visit (inst-a instruction) (copy-seq slots))
                      (visit (inst-b instruction) (copy-seq slots)))
                     (:jmp (visit (inst-a instruction) slots))
                     (:save
                      (visit (inst-b instruction) (if on-save (funcall on-save instruction slots) slots)))
                     ((:bol :eol :bos :eos :boundary :non-boundary
                       :word-start :word-end :word-start-half :word-end-half)
                      (when (zero-width-instruction-matches-p instruction text position length byte-mode-p)
                        (visit (inst-a instruction) slots)))
                     (otherwise (enqueue (make-vm-thread :pc pc :slots slots))))))))
      (dolist (seed seeds)
        (visit (vm-thread-pc seed) (vm-thread-slots seed)))
      (cdr head))))

(defun run-pike-vm (program text &key (start 0) end shortest-p longest-p never-newline-p)
  "Run PROGRAM against TEXT and return its leftmost-first match, if any.

When SHORTEST-P is true, select the earliest ending match at the leftmost
start position instead of applying the usual greedy/lazy branch priority.
When LONGEST-P is true, select the longest match at the leftmost start
position, retaining the usual branch priority to resolve equal-length paths."
  (when (and shortest-p longest-p)
    (error "SHORTEST-P and LONGEST-P cannot both be true"))
  (check-type never-newline-p boolean)
  (let* ((slot-count (slot-count-for-program program))
         (length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text))))
    (unless (and (integerp start) (integerp limit) (<= 0 start limit length))
      (error "START and END must define a range within TEXT"))
    (flet ((closure (seeds position)
             (pike-vm-closure
              program text position length byte-mode-p seeds
              :on-save (lambda (instruction slots)
                         (let ((next (copy-seq slots)))
                           (setf (aref next (inst-a instruction)) position)
                           next)))))
      (let ((pending (make-array (1+ limit) :initial-element nil))
            (best-result nil))
        (loop for position from start to limit
              do (let* ((current
                (closure
                  (append
                    (nreverse (aref pending position))
                    (unless (and longest-p best-result)
                      (list
                        (make-vm-thread :pc 0 :slots (make-array slot-count :initial-element nil)))))
                  position))
                (blocking-p nil)
                (earliest-start
                (and
                  shortest-p
                  (loop for thread in current
                        minimize (aref (vm-thread-slots thread) 0)))))
            (dolist (thread current)
              (let ((instruction (aref program (vm-thread-pc thread))))
                (case (inst-op instruction)
                  (:match
                    (let ((result
                            (make-match-result-from-slots
                             (vm-thread-slots thread) slot-count)))
                      (cond
                        (longest-p
                         (when (or (null best-result)
                                   (< (match-result-start result)
                                      (match-result-start best-result))
                                   (and (= (match-result-start result)
                                           (match-result-start best-result))
                                        (> (match-result-end result)
                                           (match-result-end best-result))))
                           (setf best-result result)))
                        ((if shortest-p (= (aref (vm-thread-slots thread) 0) earliest-start)
                             (not blocking-p))
                         (return-from run-pike-vm result)))))
                  ((:char :class :any)
                    (multiple-value-bind (next-position matched-p)
                        (instruction-match-end instruction text position limit byte-mode-p never-newline-p)
                      (when matched-p
                        (setf blocking-p t)
                        (push
                          (make-vm-thread :pc (inst-b instruction) :slots (vm-thread-slots thread))
                          (aref pending next-position)))))))))
            finally
            (return best-result))))))

(defun run-pike-vm-set (program pattern-count text
                        &key (start 0) end stop-at-first-match-p never-newline-p
                          (matches nil matches-supplied-p))
  "Run merged PROGRAM once and return matching pattern indexes.\n\nWhen STOP-AT-FIRST-MATCH-P is true, return true as soon as any member matches.\nOtherwise return all matching indexes in source order. PROGRAM is produced by\nMERGE-NFA-PROGRAMS. Set matching has no capture results, so :SAVE transitions\nare followed without allocating slot vectors."
  (check-type never-newline-p boolean)
  (when (and matches-supplied-p
             (or (not (typep matches 'bit-vector))
                 (/= (length matches) pattern-count)))
    (error 'type-error :datum matches
                       :expected-type `(bit-vector ,pattern-count)))
  (let* ((length (length text))
        (limit (or end length))
        (byte-mode-p (not (stringp text)))
        (match-bits (cond (stop-at-first-match-p nil)
                          (matches-supplied-p matches)
                          (t (make-array pattern-count
                                         :element-type 'bit
                                         :initial-element 0)))))
    (unless (and (integerp start) (integerp limit) (<= 0 start limit length))
      (error "START and END must define a range within TEXT"))
    (when (and matches-supplied-p (not stop-at-first-match-p))
      (fill match-bits 0))
    (flet ((closure (seeds position)
             (pike-vm-closure program text position length byte-mode-p seeds)))
      (when (plusp pattern-count)
        (let ((pending (make-array (1+ limit) :initial-element nil)))
          (loop for position from start to limit
                do (let ((current
                           (closure
                            (cons (make-vm-thread :pc 0 :slots nil) (nreverse (aref pending position)))
                            position)))
              (dolist (thread current)
                (let* ((pc (vm-thread-pc thread)) (instruction (aref program pc)))
                  (case (inst-op instruction)
                    (:set-match (if stop-at-first-match-p (return-from run-pike-vm-set t) (setf (aref match-bits (inst-a instruction)) 1)))
                    ((:char :class :any)
                      (multiple-value-bind (next-position matched-p)
                          (instruction-match-end instruction text position limit byte-mode-p never-newline-p)
                        (when matched-p
                          (push (make-vm-thread :pc (inst-b instruction) :slots nil)
                                (aref pending next-position))))))))))))
      (cond (stop-at-first-match-p nil)
            (matches-supplied-p match-bits)
            (t (loop for index below pattern-count
                     when (= 1 (aref match-bits index))
                       collect index))))))
