(in-package #:cl-regex-kit)

(export '(run-advanced-regex advanced-regex-limit-error))

(defstruct (advanced-state
            (:constructor
             %make-advanced-state
             (position slots &optional control skip-to mark reported-start
                       recursion-depth recursion-target committed-p))) position
  slots
  control
  skip-to
  mark
  reported-start
  recursion-depth
  recursion-target
  committed-p)

(defstruct advanced-context text
  search-start
  limit
  text-length
  byte-mode-p
  never-newline-p
  root
  group-count
  group-names
  step-limit
  steps
  nest-limit)

(defstruct advanced-grapheme-unit character
  start
  end
  class
  extended-pictographic-p)

(declaim (ftype function %advanced-node-evaluate))

(define-condition advanced-regex-limit-error (cl-regex-kit-error)
  ((kind :initarg :kind :reader advanced-regex-limit-kind)
   (limit :initarg :limit :reader advanced-regex-limit)
   (used :initarg :used :reader advanced-regex-limit-used))
  (:report
   (lambda (condition stream)
     (format
      stream
      "Advanced regex ~A limit ~D exceeded after ~D units."
      (advanced-regex-limit-kind condition)
      (advanced-regex-limit condition)
      (advanced-regex-limit-used condition)))))

(defun %advanced-state-copy (state)
  (%make-advanced-state
   (advanced-state-position state)
   (copy-seq (advanced-state-slots state))
   (advanced-state-control state)
   (advanced-state-skip-to state)
   (advanced-state-mark state)
   (advanced-state-reported-start state)
   (advanced-state-recursion-depth state)
   (advanced-state-recursion-target state)
   (advanced-state-committed-p state)))

(defun %advanced-state-terminal-p (state)
  (member
   (advanced-state-control state)
   (list :accept :commit :commit-failure :prune :skip :then)
   :test
   (function eq)))

(defun %advanced-state-normal-p (state)
  (null (advanced-state-control state)))

(defun %advanced-step (context depth)
  (incf (advanced-context-steps context))
  (when (> depth (advanced-context-nest-limit context))
    (error
     'advanced-regex-limit-error
     :kind
     :nest-depth
     :limit
     (advanced-context-nest-limit context)
     :used
     depth))
  (when (>
         (advanced-context-steps context)
         (advanced-context-step-limit context))
    (error
     'advanced-regex-limit-error
     :kind
     :steps
     :limit
     (advanced-context-step-limit context)
     :used
     (advanced-context-steps context))))

(defun %advanced-name= (left right)
  (and left right (string-equal (string left) (string right))))

(defun %advanced-capture-index-by-name (name context)
  (let ((entry
         (find-if
          (lambda (candidate)
            (%advanced-name= name (car candidate)))
          (advanced-context-group-names context))))
    (when entry
      (cdr entry))))

(defun %advanced-capture-index-for-group (group)
  (and group (group-node-capture-index group)))

(defun %advanced-group-matches-p (group name index)
  (and
   (typep group 'group-node)
   (or
    (and index (= index (or (group-node-capture-index group) -1)))
    (and name (%advanced-name= name (group-node-name group))))))

(defun %advanced-find-group (node &key name index)
  (when (typep node 'regex-node)
    (typecase node
      (group-node
       (or
        (when (%advanced-group-matches-p node name index)
          node)
        (%advanced-find-group (group-node-child node) :name name :index index)))
      (concat-node
       (loop for child in (concat-node-children node)
             thereis (%advanced-find-group child :name name :index index)))
      (alternation-node
       (loop for branch in (alternation-node-branches node)
             thereis (%advanced-find-group branch :name name :index index)))
      (repetition-node
       (%advanced-find-group
        (repetition-node-child node)
        :name
        name
        :index
        index))
      (possessive-repetition-node
       (%advanced-find-group
        (possessive-repetition-node-child node)
        :name
        name
        :index
        index))
      (assertion-node
       (%advanced-find-group
        (assertion-node-child node)
        :name
        name
        :index
        index))
      (atomic-node
       (%advanced-find-group (atomic-node-child node) :name name :index index))
      (conditional-node
       (or
        (%advanced-find-group
         (conditional-node-yes-branch node)
         :name
         name
         :index
         index)
        (%advanced-find-group
         (conditional-node-no-branch node)
         :name
         name
         :index
         index)))
      (otherwise nil))))

(defun %advanced-capture-index (node context)
  (or
   (and
    (backreference-node-capture-index node)
    (backreference-node-capture-index node))
   (and
    (backreference-node-name node)
    (or
     (%advanced-capture-index-by-name (backreference-node-name node) context)
     (let ((group
            (%advanced-find-group
             (advanced-context-root context)
             :name
             (backreference-node-name node))))
       (%advanced-capture-index-for-group group))))))

(defun %advanced-read-element (context position unicode-p)
  (let ((text (advanced-context-text context))
        (limit (advanced-context-limit context)))
    (cond
      ((>= position limit) (values nil nil nil))
      ((stringp text) (values (aref text position) (1+ position) t))
      (unicode-p
       (multiple-value-bind (character end valid-p) (utf8-character-at
                                                     text
                                                     position)
         (if (and valid-p (<= end limit)) (values character end t)
           (values nil nil nil))))
      (t (values (aref text position) (1+ position) t)))))

(defun %advanced-element-equal-p (left right case-insensitive-p unicode-p)
  (cond
    ((and (characterp left) (characterp right))
     (if case-insensitive-p (if unicode-p (unicode-case-insensitive-char=
                                           left
                                           right)
                              (ascii-case-insensitive-char= left right))
       (char= left right)))
    ((and (integerp left) (integerp right))
     (if case-insensitive-p (= (ascii-fold-octet left) (ascii-fold-octet right))
       (= left right)))
    (t nil)))

(defun %advanced-backreference-result (node state context &optional (depth 0))
  (let* ((index (%advanced-capture-index node context))
         (slots (advanced-state-slots state))
         (from
          (and index (< (* 2 index) (length slots)) (aref slots (* 2 index))))
         (to
          (and
           index
           (< (1+ (* 2 index)) (length slots))
           (aref slots (1+ (* 2 index))))))
    (when (and from to)
      (let ((source-position from)
            (target-position (advanced-state-position state))
            (case-insensitive-p (backreference-node-case-insensitive-p node))
            (unicode-p (backreference-node-unicode-p node)))
        (block matched
          (loop while (< source-position to)
                do (progn
                     (%advanced-step context depth)
                     (multiple-value-bind (source-element
                                           source-end
                                           source-valid-p) (%advanced-read-element
                                                            context
                                                            source-position
                                                            unicode-p)
                       (multiple-value-bind (target-element
                                             target-end
                                             target-valid-p) (%advanced-read-element
                                                              context
                                                              target-position
                                                              unicode-p)
                         (unless (and
                                  source-valid-p
                                  target-valid-p
                                  (%advanced-element-equal-p
                                   source-element
                                   target-element
                                   case-insensitive-p
                                   unicode-p))
                           (return-from matched nil))
                         (setf source-position source-end
                               target-position target-end)))))
          (when (= source-position to)
            (list
             (%make-advanced-state
              target-position
              (advanced-state-slots state)
              nil
              nil
              (advanced-state-mark state)
              (advanced-state-reported-start state)
              (advanced-state-recursion-depth state)
              (advanced-state-recursion-target state)
              (advanced-state-committed-p state)))))))))

(defun %advanced-consuming-result (node state context operation)
  (let ((instruction (make-inst :op operation :a node)))
    (multiple-value-bind (end success-p) (instruction-match-end
                                          instruction
                                          (advanced-context-text context)
                                          (advanced-state-position state)
                                          (advanced-context-limit context)
                                          (advanced-context-byte-mode-p context)
                                          (advanced-context-never-newline-p
                                           context))
      (when success-p
        (list
         (%make-advanced-state
          end
          (advanced-state-slots state)
          nil
          nil
          (advanced-state-mark state)
          (advanced-state-reported-start state)
          (advanced-state-recursion-depth state)
          (advanced-state-recursion-target state)
          (advanced-state-committed-p state)))))))

(defun %advanced-anchor-result (node state context)
  (let ((kind (anchor-node-kind node))
        (position (advanced-state-position state)))
    (cond
      ((eq kind :match-start)
       (when (= position (advanced-context-search-start context))
         (list state)))
      ((eq kind :end-before-final-newline)
       (let* ((text (advanced-context-text context))
              (limit (advanced-context-limit context))
              (byte-mode-p (advanced-context-byte-mode-p context)))
         (labels ((newline-p (value)
                    (if byte-mode-p
                        (= value #x0a)
                        (char= value #\Newline)))
                  (carriage-return-p (value)
                    (if byte-mode-p
                        (= value #x0d)
                        (char= value #\Return))))
           (when (= position
                    (cond
                      ((zerop limit) limit)
                      ((not (newline-p (aref text (1- limit)))) limit)
                      ((and (> limit 1)
                            (carriage-return-p (aref text (- limit 2))))
                       (- limit 2))
                      (t (1- limit))))
             (list state)))))
      (t
       (let* ((bits
               (logior
                (if (anchor-node-multiline-p node) 1
                  0)
                (if (anchor-node-unicode-p node) 2
                  0)
                (if (anchor-node-crlf-p node) 4
                  0)))
              (instruction
               (make-inst
                :op
                (anchor-kind-op kind)
                :b
                bits
                :c
                (anchor-node-line-terminator node))))
         (when (zero-width-instruction-matches-p
                instruction
                (advanced-context-text context)
                position
                (advanced-context-limit context)
                (advanced-context-byte-mode-p context))
           (list state)))))))

(defun %advanced-control-name (verb)
  (string-upcase (string verb)))

(defun %advanced-control-result (node state)
  (let* ((name (%advanced-control-name (control-verb-node-verb node)))
         (argument (control-verb-node-argument node))
         (control
          (cond
            ((member name (list "FAIL" "F") :test (function string=)) :fail)
            ((member name (list "ACCEPT" "A") :test (function string=)) :accept)
            ((member name (list "COMMIT" "C") :test (function string=)) :commit)
            ((member name (list "PRUNE" "P") :test (function string=)) :prune)
            ((member name (list "SKIP" "S") :test (function string=)) :skip)
            ((string= name "THEN") :then)
            ((string= name "MARK") :mark)
            (t (error "Unknown advanced regex control verb ~S" name)))))
    (case control
      (:fail nil)
      (:mark
       (list
        (%make-advanced-state
         (advanced-state-position state)
         (advanced-state-slots state)
         nil
         nil
         argument
         (advanced-state-reported-start state)
         (advanced-state-recursion-depth state)
         (advanced-state-recursion-target state)
         (advanced-state-committed-p state))))
      (otherwise
       (list
        (%make-advanced-state
         (advanced-state-position state)
         (advanced-state-slots state)
         control
         (and (integerp argument) argument)
         (or argument (advanced-state-mark state))
         (advanced-state-reported-start state)
         (advanced-state-recursion-depth state)
         (advanced-state-recursion-target state)
         (or (advanced-state-committed-p state) (eq control :commit))))))))

(defun %advanced-evaluate-concat (children state context depth)
  (labels ((commit-continuation (candidate)
             (let ((result (%advanced-state-copy candidate)))
               (setf (advanced-state-control result) nil
                     (advanced-state-committed-p result) t)
               result))
           (commit-failure (candidate)
             (let ((result (%advanced-state-copy candidate)))
               (setf (advanced-state-control result) :commit-failure)
               result))
           (walk (remaining current)
             (if (null remaining)
                 (if (eq (advanced-state-control current) :commit)
                     (list (commit-continuation current))
                     (list current))
                 (let ((results
                         (%advanced-node-evaluate
                          (car remaining)
                          current
                          context
                          (1+ depth))))
                   (if (null results)
                       (if (advanced-state-committed-p current)
                           (list (commit-failure current))
                           nil)
                       (let ((output nil))
                         (dolist (candidate results output)
                           (cond
                             ((eq (advanced-state-control candidate) :commit)
                              (let ((continued
                                      (walk
                                       (cdr remaining)
                                       (commit-continuation candidate))))
                                (cond
                                  ((null continued)
                                   (return-from walk
                                     (list (commit-failure candidate))))
                                  ((some
                                    (lambda (item)
                                      (eq
                                       (advanced-state-control item)
                                       :commit-failure))
                                    continued)
                                   (return-from walk continued))
                                  (t
                                   (setf output
                                         (nconc output continued))))))
                             ((eq
                               (advanced-state-control candidate)
                               :commit-failure)
                              (return-from walk (list candidate)))
                             ((%advanced-state-terminal-p candidate)
                              (setf output (nconc output (list candidate))))
                             (t
                              (let ((continued
                                      (walk (cdr remaining) candidate)))
                                (when
                                    (some
                                     (lambda (item)
                                       (eq
                                        (advanced-state-control item)
                                        :commit-failure))
                                     continued)
                                  (return-from walk continued))
                                (setf output
                                      (nconc output continued)))))))))))
    )
    (walk children state)))

(defun %advanced-evaluate-alternation (branches state context depth) (labels ((try-branches (remaining) (if (null remaining) nil (let ((results (%advanced-node-evaluate (car remaining) state context (1+ depth)))) (cond ((some (lambda (candidate) (eq (advanced-state-control candidate) :then)) results) (append (remove-if (lambda (candidate) (eq (advanced-state-control candidate) :then)) results) (try-branches (cdr remaining)))) ((some (function %advanced-state-terminal-p) results) results) (t (append results (try-branches (cdr remaining))))))))) (try-branches branches)))

(defun %advanced-progressing-results (results position)
  (remove-if-not
   (lambda (candidate)
     (and
      (%advanced-state-normal-p candidate)
      (> (advanced-state-position candidate) position)))
   results))

(defun %advanced-repeat-results (node state context depth possessive-p)
  (let ((child
         (if possessive-p (possessive-repetition-node-child node)
           (repetition-node-child node)))
        (minimum
         (if possessive-p (possessive-repetition-node-min node)
           (repetition-node-min node)))
        (maximum
         (if possessive-p (possessive-repetition-node-max node)
           (repetition-node-max node)))
        (greedy-p
         (if possessive-p (possessive-repetition-node-greedy-p node)
           (repetition-node-greedy-p node))))
    (if possessive-p (labels ((consume (current count)
                                (%advanced-step context (1+ depth))
                                (if (and maximum (>= count maximum)) (and
                                                                      (>=
                                                                       count
                                                                       minimum)
                                                                      (list
                                                                       current))
                                  (let* ((results
                                          (%advanced-node-evaluate
                                           child
                                           current
                                           context
                                           (1+ depth)))
                                         (control
                                          (find-if
                                           (lambda (candidate)
                                             (not
                                              (%advanced-state-normal-p
                                               candidate)))
                                           results))
                                         (progress
                                          (find-if
                                           (lambda (candidate)
                                             (and
                                              (%advanced-state-normal-p
                                               candidate)
                                              (>
                                               (advanced-state-position
                                                candidate)
                                               (advanced-state-position current))))
                                           results)))
                                    (cond
                                      (control (list control))
                                      (progress (consume progress (1+ count)))
                                      ((>= count minimum) (list current))
                                      (t nil))))))
                       (consume state 0))
      (labels ((expand (current count)
                 (%advanced-step context (1+ depth))
                 (let ((stop
                        (when (>= count minimum)
                          (list current))))
                   (if (and maximum (>= count maximum)) stop
                     (let* ((results
                             (%advanced-node-evaluate
                              child
                              current
                              context
                              (1+ depth)))
                            (controls
                             (remove-if #'%advanced-state-normal-p results))
                            (progress
                             (%advanced-progressing-results
                              results
                              (advanced-state-position current)))
                            (deeper
                             (mapcan
                              (lambda (candidate)
                                (expand candidate (1+ count)))
                              progress)))
                       (if greedy-p (append deeper controls stop)
                         (append stop deeper controls)))))))
        (expand state 0)))))

(defun %advanced-group-result (node state context depth)
  (let ((index (group-node-capture-index node)))
    (if (null index) (%advanced-node-evaluate
                      (group-node-child node)
                      state
                      context
                      (1+ depth))
      (let* ((entered (%advanced-state-copy state))
             (slots (advanced-state-slots entered)))
        (setf (aref slots (* 2 index)) (advanced-state-position state)
              (aref slots (1+ (* 2 index))) nil)
        (mapcar
         (lambda (candidate)
           (let ((result (%advanced-state-copy candidate)))
             (setf (aref (advanced-state-slots result) (1+ (* 2 index))) (advanced-state-position
                                                                          candidate))
             result))
         (%advanced-node-evaluate
          (group-node-child node)
          entered
          context
          (1+ depth)))))))

(defun %advanced-assertion-success (state outer-position)
  (let ((result (%advanced-state-copy state)))
    (setf (advanced-state-position result) outer-position
          (advanced-state-control result) nil
          (advanced-state-skip-to result) nil)
    result))

(defun %advanced-forward-assertion (node state context depth)
  (let* ((outer-position (advanced-state-position state))
         (results
          (%advanced-node-evaluate
           (assertion-node-child node)
           (%advanced-state-copy state)
           context
           (1+ depth)))
         (success
          (find-if
           (lambda (candidate)
             (or
              (%advanced-state-normal-p candidate)
              (eq (advanced-state-control candidate) :accept)))
           results)))
    (if (assertion-node-negative-p node) (unless success
                                           (list state))
      (when success
        (list (%advanced-assertion-success success outer-position))))))

(defun %advanced-backward-assertion (node state context depth)
  (let* ((outer-position (advanced-state-position state))
         (fixed-length (assertion-node-fixed-length node))
         (starts
          (if (and (integerp fixed-length) (>= fixed-length 0)) (list
                                                                 (max
                                                                  0
                                                                  (-
                                                                   outer-position
                                                                   fixed-length)))
            (loop for position from 0 below (1+ outer-position)
                  collect position)))
         (success nil))
    (dolist (candidate-start starts)
      (unless success
        (let* ((candidate-state (%advanced-state-copy state))
               (results
                (%advanced-node-evaluate
                 (assertion-node-child node)
                 (progn (setf (advanced-state-position candidate-state) candidate-start) candidate-state)
                 context
                 (1+ depth))))
          (when (some
                 (lambda (candidate)
                   (= (advanced-state-position candidate) outer-position))
                 results)
            (setf success (find-if
                           (lambda (candidate)
                             (=
                              (advanced-state-position candidate)
                              outer-position))
                           results))))))
    (if (assertion-node-negative-p node) (unless success
                                           (list state))
      (when success
        (list (%advanced-assertion-success success outer-position))))))

(defun %advanced-assertion-result (node state context depth)
  (if (member
       (assertion-node-direction node)
       '(:backward :backwards)
       :test
       #'eq) (%advanced-backward-assertion node state context depth)
    (%advanced-forward-assertion node state context depth)))

(defun %advanced-grapheme-class (character)
  (if (characterp character)
      (let ((name
              (string-upcase
               (string
                (sb-unicode:grapheme-break-class character)))))
        (setf name
              (coerce
               (remove-if
                (lambda (item)
                  (find item "-_ " :test (function char=)))
                name)
               (quote string)))
        (if (string= name "REGIONALINDICATOR") "RI" name))
      "OTHER"))

(defun %advanced-extended-pictographic-p (character)
  (and
   (characterp character)
   (range-matches-p +extended-pictographic-ranges+ character)))

(defun %advanced-grapheme-unit-at (node position context)
  (multiple-value-bind (character end valid-p) (%advanced-read-element
                                                context
                                                position
                                                (grapheme-node-unicode-p node))
    (when valid-p
      (make-advanced-grapheme-unit
       :character
       character
       :start
       position
       :end
       end
       :class
       (%advanced-grapheme-class character)
       :extended-pictographic-p
       (%advanced-extended-pictographic-p character)))))

(defun %advanced-grapheme-break-p (units next extended-p)
  (let* ((previous (car units))
         (previous-class (advanced-grapheme-unit-class previous))
         (next-class (advanced-grapheme-unit-class next)))
    (cond
      ((and (string= previous-class "CR") (string= next-class "LF")) nil)
      ((or
        (member previous-class '("CR" "LF" "CONTROL") :test #'string=)
        (member next-class '("CR" "LF" "CONTROL") :test #'string=))
       t)
      ((and
        (string= previous-class "L")
        (member next-class '("L" "V" "LV" "LVT") :test #'string=))
       nil)
      ((and
        (member previous-class '("LV" "V") :test #'string=)
        (member next-class '("V" "T") :test #'string=))
       nil)
      ((and
        (member previous-class '("LVT" "T") :test #'string=)
        (string= next-class "T"))
       nil)
      ((member next-class '("EXTEND" "ZWJ") :test #'string=) nil)
      ((string= next-class "SPACINGMARK") nil)
      ((string= previous-class "PREPEND") nil)
      ((and
        extended-p
        (advanced-grapheme-unit-extended-pictographic-p next)
        (string= previous-class "ZWJ")
        (loop for unit in (cdr units)
              while (string= (advanced-grapheme-unit-class unit) "EXTEND")
              finally (return
                       (and
                        unit
                        (advanced-grapheme-unit-extended-pictographic-p unit)))))
       nil)
      ((and
        (string= previous-class "RI")
        (string= next-class "RI")
        (oddp
         (loop for unit in units
               while (string= (advanced-grapheme-unit-class unit) "RI")
               count 1)))
       nil)
      ((not extended-p) (not (string= next-class "EXTEND")))
      (t t))))

(defun %advanced-grapheme-end (node state context)
  (let ((first
         (%advanced-grapheme-unit-at
          node
          (advanced-state-position state)
          context)))
    (when first
      (let ((units (list first))
            (position (advanced-grapheme-unit-end first)))
        (loop while (< position (advanced-context-limit context))
              for next = (%advanced-grapheme-unit-at node position context)
              while next
              do (if (%advanced-grapheme-break-p
                      units
                      next
                      (grapheme-node-extended-p node)) (return position)
                   (progn
                     (push next units)
                     (setf position (advanced-grapheme-unit-end next))))
              finally (return position))))))

(defun %advanced-grapheme-result (node state context)
  (let ((end (%advanced-grapheme-end node state context)))
    (when end
      (list
       (%make-advanced-state
        end
        (advanced-state-slots state)
        nil
        nil
        (advanced-state-mark state)
        (advanced-state-reported-start state)
        (advanced-state-recursion-depth state)
        (advanced-state-recursion-target state)
        (advanced-state-committed-p state))))))

(defun %advanced-condition-capture-index (condition context)
  (case (first condition)
    (:capture-index (second condition))
    (:name
     (or
      (%advanced-capture-index-by-name (second condition) context)
      (let ((group
             (%advanced-find-group
              (advanced-context-root context)
              :name
              (second condition))))
        (%advanced-capture-index-for-group group))))))

(defun %advanced-condition-true-p (condition state context depth)
  (cond
    ((eq condition :define)
     nil)
    ((eq condition :recursion)
     (plusp (or (advanced-state-recursion-depth state) 0)))
    ((and
      (consp condition)
      (member (first condition)
              (quote (:recursion-index :recursion-name))
              :test (function eq)))
     (let ((target (advanced-state-recursion-target state)))
       (and
        (plusp (or (advanced-state-recursion-depth state) 0))
        (typep target (quote group-node))
        (case (first condition)
          (:recursion-index
           (and
            (integerp (group-node-capture-index target))
            (= (group-node-capture-index target) (second condition))))
          (:recursion-name
           (and
            (group-node-name target)
            (string= (group-node-name target) (second condition))))))))
    ((and
      (consp condition)
      (member (first condition)
              (quote (:capture-index :name))
              :test (function eq)))
     (let* ((index (%advanced-condition-capture-index condition context))
            (slots (advanced-state-slots state)))
       (and
        index
        (< (1+ (* 2 index)) (length slots))
        (aref slots (* 2 index))
        (aref slots (1+ (* 2 index))))))
    ((typep condition (quote regex-node))
     (some
      (function %advanced-state-normal-p)
      (%advanced-node-evaluate condition state context (1+ depth))))
    (t nil)))

(defun %advanced-optional-node-evaluate (node state context depth)
  (if node (%advanced-node-evaluate node state context (1+ depth))
    (list state)))

(defun %advanced-conditional-result (node state context depth)
  (if (%advanced-condition-true-p
       (conditional-node-condition node)
       state
       context
       depth) (%advanced-optional-node-evaluate
               (conditional-node-yes-branch node)
               state
               context
               depth)
    (%advanced-optional-node-evaluate
     (conditional-node-no-branch node)
     state
     context
     depth)))

(defun %advanced-subroutine-target (node context)
  (let ((target (subroutine-node-target node)))
    (cond
      ((typep target 'regex-node) target)
      ((and (integerp target) (zerop target)) (advanced-context-root context))
      ((integerp target)
       (%advanced-find-group (advanced-context-root context) :index target))
      ((and target (or (stringp target) (symbolp target)))
       (%advanced-find-group (advanced-context-root context) :name target))
      ((typep node 'recursion-node) (advanced-context-root context))
      (t nil))))

(defun %advanced-subroutine-result (node state context depth)
  (let ((target (%advanced-subroutine-target node context)))
    (unless target
      (error
       "Unresolved advanced regex subroutine target ~S"
       (subroutine-node-target node)))
    (let* ((entry-depth (or (advanced-state-recursion-depth state) 0))
           (entry-target (advanced-state-recursion-target state))
           (entry-slots (copy-seq (advanced-state-slots state)))
           (target-index
             (and
              (typep target (quote group-node))
              (group-node-capture-index target)))
           (recursive-state (%advanced-state-copy state)))
      (setf (advanced-state-recursion-depth recursive-state) (1+ entry-depth)
            (advanced-state-recursion-target recursive-state) target)
      (mapcar
       (lambda (candidate)
         (let ((result (%advanced-state-copy candidate)))
           (setf (advanced-state-recursion-depth result) entry-depth
                 (advanced-state-recursion-target result) entry-target)
           (when
               (and
                (integerp target-index)
                (< (1+ (* 2 target-index)) (length entry-slots))
                (aref entry-slots (* 2 target-index)))
             (setf
              (aref
               (advanced-state-slots result)
               (* 2 target-index))
              (aref entry-slots (* 2 target-index))
              (aref
               (advanced-state-slots result)
               (1+ (* 2 target-index)))
              (aref entry-slots (1+ (* 2 target-index)))))
           result))
       (%advanced-node-evaluate target recursive-state context (1+ depth))))))

(defun %advanced-atomic-result (node state context depth)
  (let ((results
         (%advanced-node-evaluate
          (atomic-node-child node)
          state
          context
          (1+ depth))))
    (when results
      (list (first results)))))

(defun %advanced-node-evaluate (node state context depth)
  (%advanced-step context depth)
  (typecase node
    (literal-node (%advanced-consuming-result node state context :char))
    (char-class-node (%advanced-consuming-result node state context :class))
    (any-char-node (%advanced-consuming-result node state context :any))
    (line-break-node
     (%advanced-consuming-result node state context :line-break))
    (anchor-node (%advanced-anchor-result node state context))
    (reset-match-start-node
     (let ((result (%advanced-state-copy state)))
       (setf (advanced-state-reported-start result)
             (advanced-state-position state))
       (list result)))
    (concat-node
     (%advanced-evaluate-concat (concat-node-children node) state context depth))
    (alternation-node
     (%advanced-evaluate-alternation
      (alternation-node-branches node)
      state
      context
      depth))
    (possessive-repetition-node
     (%advanced-repeat-results node state context depth t))
    (repetition-node (%advanced-repeat-results node state context depth nil))
    (group-node (%advanced-group-result node state context depth))
    (assertion-node (%advanced-assertion-result node state context depth))
    (atomic-node (%advanced-atomic-result node state context depth))
    (backreference-node (%advanced-backreference-result node state context depth))
    (grapheme-node (%advanced-grapheme-result node state context))
    (conditional-node (%advanced-conditional-result node state context depth))
    (subroutine-node (%advanced-subroutine-result node state context depth))
    (control-verb-node (%advanced-control-result node state))
    (otherwise (error "Unknown advanced regex AST node: ~S" node))))

(defun %advanced-result-from-state (state start group-count group-names)
  (let* ((slots (copy-seq (advanced-state-slots state)))
         (slot-count (* 2 (1+ group-count)))
         (reported-start (or (advanced-state-reported-start state) start)))
    (setf (aref slots 0) reported-start
          (aref slots 1) (advanced-state-position state))
    (let ((result
           (make-match-result-from-slots
            slots
            slot-count
            (advanced-state-mark state))))
      (setf (match-result-group-names result) (copy-tree group-names))
      result)))

(defun %advanced-result-end (state)
  (advanced-state-position state))

(defun %advanced-select-result (states shortest-p longest-p)
  (if (or shortest-p longest-p) (reduce
                                 (lambda (best candidate)
                                   (if (or
                                        (null best)
                                        (if shortest-p (<
                                                        (%advanced-result-end
                                                         candidate)
                                                        (%advanced-result-end
                                                         best))
                                          (>
                                           (%advanced-result-end candidate)
                                           (%advanced-result-end best)))) candidate
                                     best))
                                 states
                                 :initial-value
                                 nil)
    (first states)))

(defun %advanced-input-p (text)
  (or
   (stringp text)
   (and
    (vectorp text)
    (every
     (lambda (element)
       (and (integerp element) (<= 0 element 255)))
     text))))

(defun run-advanced-regex (regex
                           text
                           &key
                           (start 0)
                           end
                           shortest-p
                           longest-p
                           never-newline-p)
  "Run compiled REGEX with ordered backtracking and return a MATCH-RESULT.

REGEX supplies the AST, capture metadata, resource limits, and byte mode.
AST is evaluated leftmost-first. TEXT may be a string or an octet vector;
byte offsets are preserved for octet input. The configured step and nest
limits are hard limits and signal ADVANCED-REGEX-LIMIT-ERROR when exceeded.
The result uses the same group-zero and (START . END) capture locations as
RUN-PIKE-VM."
  (check-type regex regex)
  (let* ((ast (regex-ast regex))
         (group-count (regex-group-count regex))
         (group-names (regex-group-names regex))
         (step-limit (regex-advanced-step-limit regex))
         (nest-limit (regex-advanced-nest-limit regex))
         (byte-mode-p (byte-regex-p regex)))
    (check-type ast regex-node)
    (check-type never-newline-p boolean)
    (check-type byte-mode-p boolean)
    (when (and shortest-p longest-p)
      (error "SHORTEST-P and LONGEST-P cannot both be true"))
    (validate-text-range regex text start end)
    (unless (and (integerp step-limit) (> step-limit 0))
      (error "STEP-LIMIT must be a positive integer"))
    (unless (and (integerp nest-limit) (>= nest-limit 0))
      (error "NEST-LIMIT must be a non-negative integer"))
    (let* ((text-length (length text))
           (limit (if (null end) text-length end))
           (highest-capture (max 0 (ast-group-count ast) group-count))
           (context
            (make-advanced-context
             :text
             text
             :search-start
             start
             :limit
             limit
             :text-length
             text-length
             :byte-mode-p
             byte-mode-p
             :never-newline-p
             never-newline-p
             :root
             ast
             :group-count
             highest-capture
             :group-names
             group-names
             :step-limit
             step-limit
             :steps
             0
             :nest-limit
             nest-limit)))
      (unless (and
               (integerp start)
               (integerp limit)
               (<= 0 start limit text-length))
        (error "START and END must define a range within TEXT"))
      (loop with candidate = start
            while (<= candidate limit)
            do (let* ((slots
                       (make-array
                        (* 2 (1+ highest-capture))
                        :initial-element
                        nil))
                      (seed
                       (%make-advanced-state
                        candidate
                        slots
                        nil
                        nil
                        nil
                        candidate
                        0
                        nil))
                      (states (%advanced-node-evaluate ast seed context 0))
                      (matches nil)
                      (skip-to nil)
                      (stop-candidate-p nil))
                 (dolist (state states)
                   (case (advanced-state-control state) ((nil :accept) (push state matches)) (:skip (unless skip-to (setf skip-to (advanced-state-skip-to state)))) (:commit (let ((committed (%advanced-state-copy state))) (setf (advanced-state-control committed) nil (advanced-state-committed-p committed) t) (push committed matches))) (:commit-failure (return-from run-advanced-regex nil)) ((:prune :then) (setf stop-candidate-p t))))
                 (when matches
                   (return-from
                    run-advanced-regex
                    (%advanced-result-from-state
                     (%advanced-select-result
                      (nreverse matches)
                      shortest-p
                      longest-p)
                     candidate
                     highest-capture
                     group-names)))
                 (setf candidate (if stop-candidate-p (1+ candidate)
                                     (max
                                      (1+ candidate)
                                      (or skip-to (1+ candidate))))))))))
