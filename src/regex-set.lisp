;;;; src/regex-set.lisp
;;;;
;;;; Multi-pattern matching built directly from compiled REGEX instances.
(in-package #:cl-regex-kit)

(defclass regex-set ()
  ((program :initarg :program :reader regex-set-program)
   (patterns :initarg :patterns :reader regex-set-source-patterns)
   (never-newline-p :initarg :never-newline-p :reader regex-set-never-newline-p
                    :initform nil)
   (byte-mode-p :initarg :byte-mode-p :reader regex-set-byte-mode-p :initform nil))
  (:documentation
   "An immutable collection of patterns compiled to one set-matching NFA."))

(defun regex-set-p (object)
  "Return true when OBJECT is a compiled REGEX-SET."
  (typep object 'regex-set))

(defun byte-regex-set-p (object)
  "Return true when OBJECT is a byte-oriented compiled REGEX-SET."
  (and (typep object 'regex-set)
       (regex-set-byte-mode-p object)))

(defun compile-regex-set-with (patterns options compiler byte-mode-p)
  "Compile PATTERNS with COMPILER into a REGEX-SET.

PATTERNS must be a list or vector of pattern strings.  Its order defines the
indexes returned by REGEX-SET-MATCHES.  OPTIONS are keyword arguments
accepted by COMPILER and apply consistently to every pattern.

Each member's own SIZE-LIMIT bounds its individual compilation (enforced by
COMPILER), but the aggregate merged program -- one root instruction per
member, plus every member's program -- can still exceed SIZE-LIMIT even
though no single member does. Checking that only after every member has
compiled would let a pattern list compile up to (length PATTERNS) times
SIZE-LIMIT worth of instructions before rejecting the set, so this compiles
members one at a time and re-checks the running total after each one,
bounding the worst-case unchecked work to one member's SIZE-LIMIT."
  (unless (or (listp patterns)
              (and (vectorp patterns) (not (stringp patterns))))
    (error 'type-error
           :datum patterns
           :expected-type '(or list (and vector (not string)))))
  (apply #'validate-regex-compile-options byte-mode-p options)
  (let ((size-limit (getf options :size-limit +maximum-instruction-count+)))
    (check-type size-limit (integer 1 *))
    (let* ((pattern-vector (map 'vector #'copy-seq patterns))
           (count (length pattern-vector))
           ;; Merging adds one root instruction per source program.
           (running-instruction-count count)
           (regexes (make-array count)))
      (dotimes (index count)
        (let ((regex (apply compiler (aref pattern-vector index) options)))
          (incf running-instruction-count (length (regex-program regex)))
          (when (> running-instruction-count size-limit)
            (error 'regex-syntax-error
                   :pattern patterns
                   :reason "Regular expression set exceeds the configured NFA instruction limit"))
          (setf (aref regexes index) regex)))
      (make-instance 'regex-set
                     :patterns pattern-vector
                     :byte-mode-p byte-mode-p
                     :never-newline-p (and (plusp count)
                                           (regex-never-newline-p (aref regexes 0)))
                     :program (merge-nfa-programs (map 'vector #'regex-program regexes))))))

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
    (loop while (and arguments (stringp (first arguments))) do
      (push (pop arguments) patterns))
    (values (nreverse patterns) arguments)))

(defmacro regex-set (&rest arguments)
  "Compile literal patterns and literal builder options once when the file loads."
  (multiple-value-bind (patterns options) (collect-literal-set-patterns arguments)
    (validate-literal-compiler-options "REGEX-SET" options)
    `(load-time-value (compile-regex-set ',patterns ,@options) t)))

(defmacro byte-regex-set (&rest arguments)
  "Compile literal byte patterns and literal options once when the file loads."
  (multiple-value-bind (patterns options) (collect-literal-set-patterns arguments)
    (validate-literal-compiler-options "BYTE-REGEX-SET" options)
    `(load-time-value (compile-byte-regex-set ',patterns ,@options) t)))

(defun validate-regex-set-text-range (regex-set text start end)
  "Validate TEXT's half-open range against REGEX-SET's input representation."
  (if (byte-regex-set-p regex-set)
      (progn
        (check-type text octet-vector)
        (validate-input-range text start end))
      (validate-string-range text start end)))

(defun call-with-validated-regex-set-match (regex-set text start end timeout thunk)
  "Validate REGEX-SET/TEXT/START/END, then invoke THUNK with the validated
LIMIT under TIMEOUT, continuation-passing style: THUNK performs the actual
VM run and returns its result, which this function returns unchanged. Shared
by every REGEX-SET matching entry point the same way CALL-WITH-VALIDATED-MATCH
is shared by the single-pattern ones in api-match.lisp."
  (check-type regex-set regex-set)
  (let ((limit (validate-regex-set-text-range regex-set text start end)))
    (call-with-timeout timeout (lambda () (funcall thunk limit)))))

(defun regex-set-matches-into (regex-set matches text &key (start 0) end timeout)
  "Record REGEX-SET matches in MATCHES and return MATCHES.

MATCHES must be a bit vector whose length exactly matches REGEX-SET's pattern
count. It is cleared before matching, then each matching source-order pattern
index is marked with a one."
  (check-type regex-set regex-set)
  (unless (and (typep matches 'bit-vector)
               (= (length matches) (regex-set-count regex-set)))
    (error 'type-error :datum matches
                       :expected-type `(bit-vector ,(regex-set-count regex-set))))
  (call-with-validated-regex-set-match
   regex-set text start end timeout
   (lambda (limit)
     (run-pike-vm-set (regex-set-program regex-set)
                      (length (regex-set-source-patterns regex-set))
                      text
                      :start start
                      :end limit
                      :matches matches
                      :never-newline-p (regex-set-never-newline-p regex-set)))))

(defun regex-set-matches (regex-set text &key (start 0) end timeout)
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

Indexes are returned in source order, including duplicate source patterns."
  (check-type regex-set regex-set)
  (let ((matches (make-array (regex-set-count regex-set)
                             :element-type 'bit
                             :initial-element 0)))
    (regex-set-matches-into regex-set matches text
                            :start start :end end :timeout timeout)
    (loop for index below (length matches)
          when (= 1 (aref matches index))
            collect index)))

(defun regex-set-matches-at (regex-set text start &key end timeout)
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

This is the Rust RegexSet::matches_at equivalent. END, when supplied, remains
the exclusive upper bound of the searched range."
  (regex-set-matches regex-set text :start start :end end :timeout timeout))

(defun regex-set-match-p (regex-set text &key (start 0) end timeout)
  "Return true when any pattern in REGEX-SET matches within [START, END)."
  (call-with-validated-regex-set-match
   regex-set text start end timeout
   (lambda (limit)
     (run-pike-vm-set (regex-set-program regex-set)
                      (length (regex-set-source-patterns regex-set))
                      text
                      :start start
                      :end limit
                      :stop-at-first-match-p t
                      :never-newline-p (regex-set-never-newline-p regex-set)))))

(defun regex-set-match-at-p (regex-set text start &key end timeout)
  "Return true when any pattern in REGEX-SET matches TEXT at or after START.

This is the Rust RegexSet::is_match_at equivalent. END, when supplied,
remains the exclusive upper bound of the searched range."
  (regex-set-match-p regex-set text :start start :end end :timeout timeout))
