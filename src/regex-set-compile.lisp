;;;; src/regex-set-compile.lisp
;;;;
;;;; Multi-pattern compilation built directly from compiled REGEX instances.
(in-package #:cl-regex-kit)

(defclass regex-set ()
  ((program :initarg :program :reader regex-set-program)
   (patterns :initarg :patterns :reader regex-set-source-patterns)
   (members :initarg :members :reader regex-set-members)
   (ordinary-indices
    :initarg :ordinary-indices
    :reader regex-set-ordinary-indices)
   (direct-ordinary-indices-p
    :initarg :direct-ordinary-indices-p
    :reader regex-set-direct-ordinary-indices-p
    :initform nil)
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

(defun direct-ordinary-indices-p (ordinary-indices pattern-count)
  "Return true when ORDINARY-INDICES already map one-to-one to source order."
  (let ((ordinary-count (length ordinary-indices)))
    (and (= ordinary-count pattern-count)
         (loop for index below ordinary-count
               always (= (aref ordinary-indices index) index)))))

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

(defun normalize-regex-set-patterns (patterns)
  "Validate PATTERNS and return them as a fresh vector of fresh strings."
  (unless (or (listp patterns)
              (and (vectorp patterns) (not (stringp patterns))))
    (error 'type-error
           :datum patterns
           :expected-type '(or list (and vector (not string)))))
  (map 'vector #'copy-seq patterns))

(defun regex-set-total-pattern-bytes (pattern-vector)
  "Return the aggregate byte length of PATTERN-VECTOR."
  (loop for pattern across pattern-vector
        sum (length pattern)))

(defun make-regex-set-index-buffer (count)
  "Allocate an adjustable fill-pointer vector for source indexes."
  (make-array count
              :element-type 'fixnum
              :adjustable t
              :fill-pointer 0))

(defun record-compiled-regex-set-member (regexes
                                         ordinary-indices
                                         advanced-indices
                                         index
                                         regex
                                         running-instruction-count
                                         size-limit
                                         patterns)
  "Store REGEX and return the updated RUNNING-INSTRUCTION-COUNT."
  (setf (aref regexes index) regex)
  (if (or (regex-advanced-p regex) (null (regex-program regex)))
      (progn
        (vector-push-extend index advanced-indices)
        running-instruction-count)
    (progn
      (vector-push-extend index ordinary-indices)
      (incf running-instruction-count
            (length (regex-program regex)))
      (when (> running-instruction-count size-limit)
        (error 'regex-syntax-error
               :pattern patterns
               :reason
               "Regular expression set exceeds the configured NFA instruction limit"))
      running-instruction-count)))

(defun finalize-compiled-regex-set (pattern-vector
                                    regexes
                                    ordinary-indices
                                    advanced-indices
                                    byte-mode-p
                                    executor)
  "Assemble a REGEX-SET instance from compiled member state."
  (let ((count (length pattern-vector)))
    (make-instance 'regex-set
                   :patterns pattern-vector
                   :members regexes
                   :ordinary-indices ordinary-indices
                   :direct-ordinary-indices-p
                   (direct-ordinary-indices-p ordinary-indices count)
                   :advanced-indices advanced-indices
                   :byte-mode-p byte-mode-p
                   :never-newline-p
                   (and (plusp count)
                        (regex-never-newline-p
                         (aref regexes 0)))
                   :program
                   (merge-nfa-programs
                    (map 'vector
                         (lambda (index)
                           (regex-program (aref regexes index)))
                         ordinary-indices)
                    executor))))

(defun compile-regex-set-member-sequentially (pattern-vector
                                              options
                                              compiler
                                              regexes
                                              ordinary-indices
                                              advanced-indices
                                              running-instruction-count
                                              size-limit
                                              patterns)
  "Compile PATTERN-VECTOR sequentially and return the final instruction count."
  (let ((count (length pattern-vector)))
    (dotimes (index count running-instruction-count)
      (setf running-instruction-count
            (record-compiled-regex-set-member
             regexes
             ordinary-indices
             advanced-indices
             index
             (apply compiler
                    (aref pattern-vector index)
                    options)
             running-instruction-count
             size-limit
             patterns)))))

(defun submit-regex-set-compilation-job (executor promises slot pattern-vector options compiler index)
  "Submit INDEX in PATTERN-VECTOR to EXECUTOR and store the promise in SLOT."
  (setf (aref promises slot)
        (cl-concurrent-kit:submit
         executor
         (lambda ()
           (apply compiler
                  (aref pattern-vector index)
                  options)))))

(defun refill-regex-set-compilation-window (executor
                                            promises
                                            slot
                                            next-index
                                            count
                                            pattern-vector
                                            options
                                            compiler)
  "Submit the next pending member into SLOT and return the advanced NEXT-INDEX."
  (if (< next-index count)
      (progn
        (submit-regex-set-compilation-job
         executor
         promises
         slot
         pattern-vector
         options
         compiler
         next-index)
        (1+ next-index))
    next-index))

(defun compile-regex-set-members (pattern-vector
                                  options
                                  compiler
                                  parallelism
                                  regexes
                                  ordinary-indices
                                  advanced-indices
                                  running-instruction-count
                                  byte-mode-p
                                  size-limit
                                  patterns)
  "Compile PATTERN-VECTOR either sequentially or in parallel, then finalize it."
  (if (= parallelism 1)
      (progn
        (compile-regex-set-member-sequentially
         pattern-vector
         options
         compiler
         regexes
         ordinary-indices
         advanced-indices
         running-instruction-count
         size-limit
         patterns)
        (finalize-compiled-regex-set
         pattern-vector
         regexes
         ordinary-indices
         advanced-indices
         byte-mode-p
         nil))
    (compile-regex-set-members-in-parallel
     pattern-vector
     options
     compiler
     parallelism
     regexes
     ordinary-indices
     advanced-indices
     running-instruction-count
     byte-mode-p
     size-limit
     patterns)))

(defun compile-regex-set-members-in-parallel (pattern-vector
                                              options
                                              compiler
                                              parallelism
                                              regexes
                                              ordinary-indices
                                              advanced-indices
                                              running-instruction-count
                                              byte-mode-p
                                              size-limit
                                              patterns)
  "Compile PATTERN-VECTOR in executor-sized windows and assemble the REGEX-SET
before the executor shuts down."
  (let ((count (length pattern-vector)))
    (cl-concurrent-kit:with-executor (executor :size parallelism)
      (let* ((window-size (min count parallelism))
             (promises (make-array window-size))
             (next-index 0))
        (dotimes (slot window-size)
          (setf next-index
                (refill-regex-set-compilation-window
                 executor
                 promises
                 slot
                 next-index
                 count
                 pattern-vector
                 options
                 compiler)))
        (dotimes (index count)
          (let ((slot (mod index window-size)))
            (setf running-instruction-count
                  (record-compiled-regex-set-member
                   regexes
                   ordinary-indices
                   advanced-indices
                   index
                   (cl-concurrent-kit:await (aref promises slot))
                   running-instruction-count
                   size-limit
                   patterns))
            (setf next-index
                  (refill-regex-set-compilation-window
                   executor
                   promises
                   slot
                   next-index
                   count
                   pattern-vector
                   options
                   compiler))))
        (finalize-compiled-regex-set
         pattern-vector
         regexes
         ordinary-indices
         advanced-indices
         byte-mode-p
         executor)))))

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
  (apply (function validate-regex-compile-options) byte-mode-p options)
  (let ((size-limit (getf options :size-limit +maximum-instruction-count+)))
    (check-type size-limit (integer 1 *))
    (let* ((pattern-vector (normalize-regex-set-patterns patterns))
           (count (length pattern-vector)))
      (when (> count size-limit)
        (error 'regex-syntax-error
               :pattern patterns
               :reason "Regular expression set exceeds the configured NFA instruction limit"))
      (let* ((total-pattern-bytes
               (regex-set-total-pattern-bytes pattern-vector))
             (parallelism (regex-set-compile-parallelism count total-pattern-bytes))
             ;; Merging adds one root instruction per source program.
             (running-instruction-count count)
             (regexes (make-array count))
             (ordinary-indices (make-regex-set-index-buffer count))
             (advanced-indices (make-regex-set-index-buffer count)))
        (compile-regex-set-members
         pattern-vector
         options
         compiler
         parallelism
         regexes
         ordinary-indices
         advanced-indices
         running-instruction-count
         byte-mode-p
         size-limit
         patterns)))))

(defmacro define-regex-set-compiler (name compiler byte-mode-p docstring)
  "Define a public REGEX-SET compiler wrapper over COMPILE-REGEX-SET-WITH."
  `(defun ,name (patterns &rest options)
     ,docstring
     (compile-regex-set-with patterns options #',compiler ,byte-mode-p)))

(define-regex-set-compiler
  compile-regex-set
  compile-regex
  nil
  "Compile PATTERNS into a character-oriented REGEX-SET.

PATTERNS must be a list or vector of pattern strings.  Its order defines the
indexes returned by REGEX-SET-MATCHES.  OPTIONS are keyword arguments
accepted by COMPILE-REGEX and apply consistently to every pattern.")

(define-regex-set-compiler
  compile-byte-regex-set
  compile-byte-regex
  t
  "Compile PATTERNS into a byte-oriented REGEX-SET.

The pattern syntax and OPTIONS are those accepted by COMPILE-BYTE-REGEX.
The resulting set matches octet vectors. Unicode-aware constructs consume valid
UTF-8 scalars by default; use (?-u:...) or \\C for raw octet matching.")

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

(defmacro define-literal-regex-set-macro (name compiler literal-name docstring)
  "Define a load-time literal REGEX-SET builder macro."
  `(defmacro ,name (&rest arguments)
     ,docstring
     (multiple-value-bind (patterns options) (collect-literal-set-patterns
                                              arguments)
       (validate-literal-compiler-options ,literal-name options)
       `(load-time-value (,',compiler ',patterns ,@options) t))))

(define-literal-regex-set-macro
  regex-set
  compile-regex-set
  "REGEX-SET"
  "Compile literal patterns and literal builder options once when the file loads.")

(define-literal-regex-set-macro
  byte-regex-set
  compile-byte-regex-set
  "BYTE-REGEX-SET"
  "Compile literal byte patterns and literal options once when the file loads.")
