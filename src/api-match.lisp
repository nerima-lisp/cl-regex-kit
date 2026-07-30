(in-package #:cl-regex-kit)

(defun validate-string-and-start (text start)
  "Validate a string text range and return TEXT."
  (check-type text string)
  (unless (and (integerp start) (<= 0 start (length text)))
    (error 'type-error
           :datum start
           :expected-type `(integer 0 ,(length text))))
  text)

(defun validate-text-and-start (regex text start)
  "Validate a public text range for REGEX and return TEXT."
  (check-type regex regex)
  (if (byte-regex-p regex)
      (progn
        (check-type text octet-vector)
        (unless (and (integerp start) (<= 0 start (length text)))
          (error 'type-error
                 :datum start
                 :expected-type `(integer 0 ,(length text))))
        text)
      (validate-string-and-start text start)))

(defun validate-timeout (timeout)
  "Validate TIMEOUT and return it."
  (unless (or (null timeout) (and (realp timeout) (plusp timeout)))
    (error 'type-error :datum timeout :expected-type '(or null (real (0) *))))
  timeout)

(defun call-with-timeout (timeout thunk)
  "Call THUNK, converting SBCL's timeout condition to REGEX-TIMEOUT."
  (validate-timeout timeout)
  (if timeout
      (handler-case
          (sb-ext:with-timeout timeout
            (funcall thunk))
        (sb-ext:timeout ()
          (error 'regex-timeout :seconds timeout)))
      (funcall thunk)))

(defun scan (regex text &key (start 0) timeout)
  "Find the leftmost-first match of REGEX in TEXT at or after START.
Returns a MATCH-RESULT, or NIL if there is no match. TIMEOUT is a positive
number of seconds, or NIL to impose no deadline."
  (check-type regex regex)
  (validate-text-and-start regex text start)
  (let ((result (call-with-timeout
                 timeout
                 (lambda () (run-pike-vm (regex-program regex) text :start start)))))
    (when result
      (setf (match-result-group-names result) (regex-group-names regex)))
    result))

(defun shortest-match (regex text &key (start 0) timeout)
  "Return the end index of REGEX's shortest leftmost match in TEXT.

The selected match begins at the earliest position at or after START and has
the earliest possible end position there. Return NIL when REGEX does not
match. TIMEOUT is a positive number of seconds, or NIL to impose no deadline."
  (check-type regex regex)
  (validate-text-and-start regex text start)
  (let ((result (call-with-timeout
                 timeout
                 (lambda ()
                   (run-pike-vm (regex-program regex) text
                                :start start
                                :shortest-p t)))))
    (and result (match-result-end result))))

(defun is-match-p (regex text &key (start 0) timeout)
  "Return true when REGEX matches TEXT at or after START."
  (not (null (scan regex text :start start :timeout timeout))))

(defun match (pattern text &key (start 0) timeout)
  "Compile PATTERN and SCAN TEXT once. Prefer COMPILE-REGEX + SCAN when
matching the same pattern repeatedly."
  (scan (compile-regex pattern) text :start start :timeout timeout))

(defun byte-match (pattern text &key (start 0) timeout)
  "Compile byte PATTERN and scan octet-vector TEXT once."
  (scan (compile-byte-regex pattern) text :start start :timeout timeout))

(defun match-string (match-result text)
  "The substring of TEXT the whole match covers."
  (check-type match-result match-result)
  (check-type text (or string octet-vector))
  (subseq text (match-result-start match-result) (match-result-end match-result)))

(defun match-captures (match-result text)
  "Return a fresh vector of strings captured by MATCH-RESULT in TEXT.

Index zero is the whole match.  Each subsequent index corresponds to its
numeric capture group; a nonparticipating optional group is represented by
NIL.  The returned vector never exposes the VM's capture slots."
  (check-type match-result match-result)
  (check-type text (or string octet-vector))
  (map 'vector
       (lambda (range)
         (when range
           (subseq text (car range) (cdr range))))
       (match-result-groups match-result)))

(defun match-start (match-result)
  "The start offset of the whole match."
  (check-type match-result match-result)
  (match-result-start match-result))

(defun match-end (match-result)
  "The end offset of the whole match."
  (check-type match-result match-result)
  (match-result-end match-result))

(defun resolve-group-index (match-result index)
  (check-type match-result match-result)
  (let ((resolved
          (if (stringp index)
              (or (cdr (assoc index (match-result-group-names match-result) :test #'string=))
                  (error 'type-error
                         :datum index
                         :expected-type '(or integer known-capture-name)))
              index)))
    (unless (and (integerp resolved)
                 (<= 0 resolved)
                 (< resolved (length (match-result-groups match-result))))
      (error 'type-error
             :datum index
             :expected-type `(integer 0 ,(1- (length (match-result-groups match-result))))))
    resolved))

(defun match-group-start (match-result index)
  "The start offset of capture group INDEX, or NIL if it did not participate."
  (car (aref (match-result-groups match-result)
             (resolve-group-index match-result index))))

(defun match-group-end (match-result index)
  "The end offset of capture group INDEX, or NIL if it did not participate."
  (cdr (aref (match-result-groups match-result)
             (resolve-group-index match-result index))))

(defun match-group-string (match-result index text)
  "The substring TEXT captured by group INDEX, or NIL if it did not participate."
  (check-type text (or string octet-vector))
  (let ((start (match-group-start match-result index))
        (end (match-group-end match-result index)))
    (when (and start end)
      (subseq text start end))))

(defun full-match-p (regex text &key timeout)
  "Return true when REGEX matches all of TEXT."
  (let ((result (scan regex text :timeout timeout)))
    (and result
         (zerop (match-result-start result))
         (= (match-result-end result) (length text)))))
