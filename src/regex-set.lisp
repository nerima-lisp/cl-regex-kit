;;;; src/regex-set.lisp
;;;;
;;;; Multi-pattern matching built directly from compiled REGEX instances.
(in-package #:cl-regex-kit)

(defclass regex-set ()
  ((program :initarg :program :reader regex-set-program)
   (patterns :initarg :patterns :reader regex-set-source-patterns)
   (members :initarg :members :reader regex-set-members)
   (ordinary-indices
    :initarg :ordinary-indices
    :reader regex-set-ordinary-indices)
   (advanced-indices
    :initarg :advanced-indices
    :reader regex-set-advanced-indices)
   (never-newline-p
    :initarg :never-newline-p
    :reader regex-set-never-newline-p
    :initform nil)
   (byte-mode-p
    :initarg :byte-mode-p
    :reader regex-set-byte-mode-p
    :initform nil))
  (:documentation
   "An immutable collection of patterns with merged-NFA and single-regex members."))

(defun regex-set-p (object)
  "Return true when OBJECT is a compiled REGEX-SET."
  (typep object 'regex-set))

(defun byte-regex-set-p (object)
  "Return true when OBJECT is a byte-oriented compiled REGEX-SET."
  (and (typep object 'regex-set) (regex-set-byte-mode-p object)))

(defconstant +regex-set-max-parallelism+ 8
  "Upper bound on concurrent member compilations for REGEX-SET construction.")

(defconstant +regex-set-min-parallel-count+ 256
  "Minimum member count before REGEX-SET compilation parallelizes by default.")

(defconstant +regex-set-min-parallel-bytes+ 8192
  "Minimum aggregate pattern bytes before REGEX-SET compilation parallelizes by default.")

(defvar *regex-set-compile-parallelism-override* nil
  "When non-NIL, force REGEX-SET member compilation to use this parallelism.")

(defun regex-set-compile-parallelism (count total-pattern-bytes)
  "Choose a bounded parallelism level for COUNT member compilations."
  (check-type count (integer 0 *))
  (check-type total-pattern-bytes (integer 0 *))
  (let ((override
         (validate-parallelism-override
          *regex-set-compile-parallelism-override*
          +regex-set-max-parallelism+)))
    (cond
      ((< count 2) 1)
      (override (min count override))
      ((or
        (< count +regex-set-min-parallel-count+)
        (< total-pattern-bytes +regex-set-min-parallel-bytes+))
       1)
      (t (min count +regex-set-max-parallelism+)))))

(defun compile-regex-set-with (patterns options compiler byte-mode-p)
  "Compile PATTERNS with COMPILER into a REGEX-SET.

PATTERNS must be a list or vector of pattern strings.  Its order defines the
indexes returned by REGEX-SET-MATCHES.  OPTIONS are keyword arguments
accepted by COMPILER and apply consistently to every pattern.

Each member SIZE-LIMIT bounds its individual compilation (enforced by
COMPILER), but the aggregate merged program -- one root instruction per
member, plus every member program -- can still exceed SIZE-LIMIT even
though no single member does. Checking that only after every member has
compiled would let a pattern list compile up to (length PATTERNS) times
SIZE-LIMIT worth of instructions before rejecting the set, so this submits
at most one executor-width window of members, preserves source order, and
re-checks the running total after each compiled member before submitting the
next one. That bounds the worst-case unchecked work to the executor width
rather than the full pattern list."
  (unless (or (listp patterns)
              (and (vectorp patterns) (not (stringp patterns))))
    (error (quote type-error)
           :datum patterns
           :expected-type (quote (or list (and vector (not string))))))
  (apply (function validate-regex-compile-options) byte-mode-p options)
  (let ((size-limit (getf options :size-limit +maximum-instruction-count+)))
    (check-type size-limit (integer 1 *))
    (let* ((pattern-vector (map (quote vector) (function copy-seq) patterns))
           (count (length pattern-vector)))
      (when (> count size-limit)
        (error (quote regex-syntax-error)
               :pattern patterns
               :reason "Regular expression set exceeds the configured NFA instruction limit"))
      (let* ((total-pattern-bytes
               (loop for pattern across pattern-vector
                     sum (length pattern)))
             (parallelism (regex-set-compile-parallelism count total-pattern-bytes))
             ;; Merging adds one root instruction per source program.
             (running-instruction-count count)
             (regexes (make-array count))
             (ordinary-indices
               (make-array count
                           :element-type (quote fixnum)
                           :adjustable t
                           :fill-pointer 0))
             (advanced-indices
               (make-array count
                           :element-type (quote fixnum)
                           :adjustable t
                           :fill-pointer 0)))
        (flet ((store-compiled (regex index)
                 (setf (aref regexes index) regex)
                 (if (or (regex-advanced-p regex) (null (regex-program regex)))
                     (vector-push-extend index advanced-indices)
                   (progn
                     (vector-push-extend index ordinary-indices)
                     (incf running-instruction-count
                           (length (regex-program regex)))
                     (when (> running-instruction-count size-limit)
                       (error (quote regex-syntax-error)
                              :pattern patterns
                              :reason "Regular expression set exceeds the configured NFA instruction limit")))))
               (make-regex-set (executor)
                 (make-instance (quote regex-set)
                                :patterns pattern-vector
                                :members regexes
                                :ordinary-indices ordinary-indices
                                :advanced-indices advanced-indices
                                :byte-mode-p byte-mode-p
                                :never-newline-p (and (plusp count)
                                                      (regex-never-newline-p
                                                       (aref regexes 0)))
                                :program (merge-nfa-programs
                                          (map (quote vector)
                                               (lambda (index)
                                                 (regex-program
                                                  (aref regexes index)))
                                               ordinary-indices)
                                          executor))))
          (if (= parallelism 1)
              (progn
                (dotimes (index count)
                  (store-compiled
                   (apply compiler
                          (aref pattern-vector index)
                          options)
                   index))
                (make-regex-set nil))
            (cl-concurrent-kit:with-executor (executor :size parallelism)
              (let* ((window-size (min count parallelism))
                     (promises (make-array window-size))
                     (next-index 0))
                (labels ((submit-next (slot)
                           (when (< next-index count)
                             (let ((index next-index))
                               (incf next-index)
                               (setf (aref promises slot)
                                     (cl-concurrent-kit:submit
                                      executor
                                      (lambda ()
                                        (apply compiler
                                               (aref pattern-vector index)
                                               options))))))))
                  (dotimes (slot window-size)
                    (submit-next slot))
                  (dotimes (index count)
                    (let ((slot (mod index window-size)))
                      (store-compiled
                       (cl-concurrent-kit:await (aref promises slot))
                       index)
                      (submit-next slot)))))
              (make-regex-set executor))))))))

(defun compile-regex-set (patterns &rest options)
  "Compile PATTERNS into a character-oriented REGEX-SET.

PATTERNS must be a list or vector of pattern strings.  Its order defines the
indexes returned by REGEX-SET-MATCHES.  OPTIONS are keyword arguments
accepted by COMPILE-REGEX and apply consistently to every pattern."
  (compile-regex-set-with patterns options #'compile-regex nil))

(defun compile-byte-regex-set (patterns &rest options)
  "Compile PATTERNS into a byte-oriented REGEX-SET.

The pattern syntax and OPTIONS are those accepted by COMPILE-BYTE-REGEX.
The resulting set matches octet vectors. Unicode-aware constructs consume valid
UTF-8 scalars by default; use (?-u:...) or \\C for raw octet matching."
  (compile-regex-set-with patterns options #'compile-byte-regex t))

(defun regex-set-patterns (regex-set)
  "Return a fresh vector of fresh copies of REGEX-SET's source patterns."
  (check-type regex-set regex-set)
  (map 'vector #'copy-seq (regex-set-source-patterns regex-set)))

(defun regex-set-count (regex-set)
  "Return the number of source patterns in REGEX-SET."
  (check-type regex-set regex-set)
  (length (regex-set-source-patterns regex-set)))

(defun regex-set-empty-p (regex-set)
  "Return true when REGEX-SET contains no source patterns."
  (zerop (regex-set-count regex-set)))

(defun collect-literal-set-patterns (arguments)
  "Split literal macro ARGUMENTS into pattern strings and trailing options.

Returns the leading run of string-literal patterns and the remaining keyword
options as two values."
  (let ((patterns '()))
    (loop while (and arguments (stringp (first arguments)))
          do (push (pop arguments) patterns))
    (values (nreverse patterns) arguments)))

(defmacro regex-set (&rest arguments)
  "Compile literal patterns and literal builder options once when the file loads."
  (multiple-value-bind (patterns options) (collect-literal-set-patterns
                                           arguments)
    (validate-literal-compiler-options "REGEX-SET" options)
    `(load-time-value (compile-regex-set ',patterns ,@options) t)))

(defmacro byte-regex-set (&rest arguments)
  "Compile literal byte patterns and literal options once when the file loads."
  (multiple-value-bind (patterns options) (collect-literal-set-patterns
                                           arguments)
    (validate-literal-compiler-options "BYTE-REGEX-SET" options)
    `(load-time-value (compile-byte-regex-set ',patterns ,@options) t)))

(defun validate-regex-set-text-range (regex-set text start end)
  "Validate TEXT's half-open range against REGEX-SET's input representation."
  (if (byte-regex-set-p regex-set) (progn
                                     (check-type text octet-vector)
                                     (validate-input-range text start end))
    (validate-string-range text start end)))

(defun call-with-validated-regex-set-match (regex-set
                                            text
                                            start
                                            end
                                            timeout
                                            thunk)
  "Validate REGEX-SET/TEXT/START/END, then invoke THUNK with the validated
LIMIT under TIMEOUT, continuation-passing style: THUNK performs the actual
VM run and returns its result, which this function returns unchanged. Shared
by every REGEX-SET matching entry point the same way CALL-WITH-VALIDATED-MATCH
is shared by the single-pattern ones in api-match.lisp."
  (check-type regex-set regex-set)
  (let ((limit (validate-regex-set-text-range regex-set text start end)))
    (call-with-timeout
     timeout
     (lambda ()
       (funcall thunk limit)))))

(defun regex-set-matches-into (regex-set
                               matches
                               text
                               &key
                               (start 0)
                               end
                               timeout)
  "Record REGEX-SET matches in MATCHES and return MATCHES.

MATCHES must be a bit vector whose length exactly matches REGEX-SET pattern
count. It is cleared before matching, then each matching source-order pattern
index is marked with a one."
  (check-type regex-set regex-set)
  (unless (and
           (typep matches (quote bit-vector))
           (= (length matches) (regex-set-count regex-set)))
    (error
     (quote type-error)
     :datum
     matches
     :expected-type
     (list (quote bit-vector) (regex-set-count regex-set))))
  (call-with-validated-regex-set-match
   regex-set
   text
   start
   end
   timeout
   (lambda (limit)
     (fill matches 0)
     (let* ((ordinary-indices (regex-set-ordinary-indices regex-set))
            (ordinary-count (length ordinary-indices))
            (direct-p
              (and (= ordinary-count (regex-set-count regex-set))
                   (loop for index below ordinary-count
                         always (= (aref ordinary-indices index) index))))
            (ordinary-matches
              (if direct-p
                  matches
                (make-array ordinary-count
                            :element-type (quote bit)
                            :initial-element 0))))
       (when (plusp ordinary-count)
         (run-pike-vm-set
          (regex-set-program regex-set)
          ordinary-count
          text
          :start
          start
          :end
          limit
          :matches
          ordinary-matches
          :never-newline-p
          (regex-set-never-newline-p regex-set)))
       (unless direct-p
         (loop for merged-index below ordinary-count
               when (= 1 (aref ordinary-matches merged-index))
                 do (setf (aref matches
                                (aref ordinary-indices merged-index))
                          1)))
       (loop for index across (regex-set-advanced-indices regex-set)
             when (scan
                    (aref (regex-set-members regex-set) index)
                    text
                    :start start
                    :end limit)
               do (setf (aref matches index) 1))
       matches))))

(defun regex-set-matches (regex-set text &key (start 0) end timeout)
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

Indexes are returned in source order, including duplicate source patterns."
  (check-type regex-set regex-set)
  (let ((matches
         (make-array
          (regex-set-count regex-set)
          :element-type
          'bit
          :initial-element
          0)))
    (regex-set-matches-into
     regex-set
     matches
     text
     :start
     start
     :end
     end
     :timeout
     timeout)
    (loop for index below (length matches)
          when (= 1 (aref matches index))
            collect index)))

(defun regex-set-matches-at (regex-set text start &key end timeout)
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

This is the Rust RegexSet::matches_at equivalent. END, when supplied, remains
the exclusive upper bound of the searched range."
  (regex-set-matches regex-set text :start start :end end :timeout timeout))

(defun regex-set-search (regex-set text &key (start 0) end timeout)
  "Find the earliest REGEX-SET member match in TEXT.

Returns two values: the source-pattern index and that member's MATCH-RESULT.
When several members begin at the same position, the lowest source-pattern
index wins. Returns NIL, NIL when no member matches."
  (check-type regex-set regex-set)
  (call-with-validated-regex-set-match
   regex-set
   text
   start
   end
   timeout
   (lambda (limit)
     (let ((best-index nil)
           (best-result nil))
       (loop for index below (regex-set-count regex-set)
             for member = (aref (regex-set-members regex-set) index)
             for candidate = (scan member text :start start :end limit)
             when candidate
               do (when (or (null best-result)
                            (< (match-start candidate)
                               (match-start best-result))
                            (and (= (match-start candidate)
                                    (match-start best-result))
                                 (< index best-index)))
                    (setf best-index index
                          best-result candidate)))
       (values best-index best-result)))))

(defun regex-set-search-at (regex-set text start &key end timeout)
  "Find the earliest REGEX-SET member match in TEXT at or after START.

Returns the same two values as REGEX-SET-SEARCH."
  (regex-set-search regex-set text :start start :end end :timeout timeout))

(defun regex-set-match-p (regex-set text &key (start 0) end timeout)
  "Return true when any pattern in REGEX-SET matches within [START, END)."
  (call-with-validated-regex-set-match
   regex-set
   text
   start
   end
   timeout
   (lambda (limit)
     (let* ((ordinary-indices (regex-set-ordinary-indices regex-set))
            (ordinary-count (length ordinary-indices)))
       (or
        (and (plusp ordinary-count)
             (run-pike-vm-set
              (regex-set-program regex-set)
              ordinary-count
              text
              :start
              start
              :end
              limit
              :stop-at-first-match-p
              t
              :never-newline-p
              (regex-set-never-newline-p regex-set)))
        (loop for index across (regex-set-advanced-indices regex-set)
              thereis
              (scan
               (aref (regex-set-members regex-set) index)
               text
               :start start
               :end limit)))))))

(defun regex-set-match-at-p (regex-set text start &key end timeout)
  "Return true when any pattern in REGEX-SET matches TEXT at or after START.

This is the Rust RegexSet::is_match_at equivalent. END, when supplied,
remains the exclusive upper bound of the searched range."
  (regex-set-match-p regex-set text :start start :end end :timeout timeout))
