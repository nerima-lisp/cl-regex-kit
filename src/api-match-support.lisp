(in-package #:cl-regex-kit)

(defun validate-input-range (text start end)
  "Validate a half-open input range and return its exclusive end."
  (unless (and (integerp start) (<= 0 start (length text)))
    (error 'type-error :datum start :expected-type `(integer 0 ,(length text))))
  (let ((limit (if (null end) (length text) end)))
    (unless (and (integerp limit) (<= start limit (length text)))
      (error 'type-error :datum end :expected-type `(or null (integer ,start ,(length text)))))
    limit))

(defun validate-string-range (text start end)
  "Validate a string text range and return its exclusive end."
  (check-type text string)
  (let ((limit (validate-input-range text start end)))
    (let ((invalid-position
            (first-non-unicode-scalar-position text start limit)))
      (when invalid-position
        (error 'type-error
               :datum (char text invalid-position)
               :expected-type '(satisfies unicode-scalar-character-p))))
    limit))

(defun validate-text-range (regex text start end)
  "Validate a public half-open text range for REGEX and return its end."
  (check-type regex regex)
  (if (byte-regex-p regex)
      (progn
        (check-type text octet-vector)
        (validate-input-range text start end))
      (validate-string-range text start end)))

(defun validate-timeout (timeout)
  "Validate TIMEOUT and return it."
  (unless (or (null timeout) (and (realp timeout) (plusp timeout)))
    (error 'type-error :datum timeout :expected-type '(or null (real (0) *))))
  timeout)

(defstruct (capture-locations
            (:constructor %make-capture-locations (starts ends)))
  "Reusable capture offset storage created by REGEX-CAPTURE-LOCATIONS."
  (starts #() :type simple-vector :read-only t)
  (ends #() :type simple-vector :read-only t))

(defun regex-capture-locations (regex)
  "Create reusable capture offset storage compatible with REGEX.

Use the result with SCAN-CAPTURES-INTO. A location buffer records offsets,
not substrings, so it can be reused safely across different input texts."
  (check-type regex regex)
  (let ((count (regex-capture-count regex)))
    (%make-capture-locations (make-array count :initial-element nil)
                             (make-array count :initial-element nil))))

(defun capture-locations-count (locations)
  "Return the number of capture slots in LOCATIONS, including group zero."
  (check-type locations capture-locations)
  (length (capture-locations-starts locations)))

(defun validate-capture-location-index (locations index)
  (check-type locations capture-locations)
  (unless (and (integerp index)
               (<= 0 index)
               (< index (capture-locations-count locations)))
    (error 'type-error
           :datum index
           :expected-type `(integer 0 ,(1- (capture-locations-count locations)))))
  index)

(defun capture-location-start (locations index)
  "Return LOCATIONS's start offset for capture INDEX, or NIL when unmatched."
  (aref (capture-locations-starts locations)
        (validate-capture-location-index locations index)))

(defun capture-location-end (locations index)
  "Return LOCATIONS's exclusive end offset for capture INDEX, or NIL when unmatched."
  (aref (capture-locations-ends locations)
        (validate-capture-location-index locations index)))

(defun clear-capture-locations (locations)
  (fill (capture-locations-starts locations) nil)
  (fill (capture-locations-ends locations) nil)
  locations)

(defun copy-match-result-to-capture-locations (result locations)
  (clear-capture-locations locations)
  (let ((groups (match-result-groups result)))
    (dotimes (index (length groups) locations)
      (let ((range (aref groups index)))
        (when range
          (setf (aref (capture-locations-starts locations) index) (car range)
                (aref (capture-locations-ends locations) index) (cdr range)))))))

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

(defun call-with-validated-match (regex text start end timeout thunk)
  "Validate REGEX/TEXT/START/END, then invoke THUNK with the validated LIMIT
under TIMEOUT, continuation-passing style: THUNK performs the actual VM run
and returns its MATCH-RESULT (or NIL), which this function returns unchanged.
Shared by every SCAN-shaped entry point so each one states only how it wants
RUN-PIKE-VM invoked, not how to get there."
  (check-type regex regex)
  (let ((limit (validate-text-range regex text start end)))
    (call-with-timeout timeout (lambda () (funcall thunk limit)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-pike-vm-match ((result regex text start end timeout &rest vm-keys) &body body)
    "Bind RESULT to the MATCH-RESULT (or NIL) in TEXT within the validated
[START, END) range, then evaluate BODY. RESULT is intentional anaphora: BODY
names it to read the outcome, exactly like the LET it replaces.

Expands to a CALL-WITH-VALIDATED-MATCH invocation whose continuation dispatches
advanced regexes directly to RUN-ADVANCED-REGEX, while ordinary regexes use
RUN-PIKE-VM with the :start/:end/:never-newline-p arguments every SCAN-shaped
entry point supplies, plus VM-KEYS for the one flag (:shortest-p or
:longest-p) that distinguishes it from a plain leftmost-first SCAN. The main
ASDF system loads the advanced backend before this file, so the direct call is
always available. This is the one place that assembles both execution calls,
so SCAN, SHORTEST-MATCH, and LONGEST-MATCH differ only in VM-KEYS and what BODY
does with RESULT.

REGEX/TEXT/START/END/TIMEOUT are each evaluated exactly once, in the order
written, regardless of how many times the expansion below references them."
    (let ((regex-var (gensym "REGEX-"))
          (text-var (gensym "TEXT-"))
          (start-var (gensym "START-"))
          (end-var (gensym "END-"))
          (timeout-var (gensym "TIMEOUT-")))
      `(let* ((,regex-var ,regex)
              (,text-var ,text)
              (,start-var ,start)
              (,end-var ,end)
              (,timeout-var ,timeout)
              (,result
                (call-with-validated-match
                 ,regex-var ,text-var ,start-var ,end-var ,timeout-var
                 (lambda (limit)
                   (if (regex-advanced-p ,regex-var)
                       (run-advanced-regex
                        ,regex-var ,text-var
                        :start ,start-var
                        :end limit
                        :shortest-p ,(getf vm-keys :shortest-p)
                        :longest-p ,(getf vm-keys :longest-p)
                        :never-newline-p (regex-never-newline-p ,regex-var))
                       (run-pike-vm (regex-program ,regex-var) ,text-var
                                    :start ,start-var
                                    :end limit
                                    :slot-count (regex-slot-count ,regex-var)
                                    :never-newline-p (regex-never-newline-p ,regex-var)
                                    ,@vm-keys))))))
         ,@body)))

  (defmacro define-forwarding-wrapper (name lambda-list docstring target arguments)
    "Define a trivial public wrapper that forwards to TARGET.

LAMBDA-LIST is the wrapper's ordinary lambda list. ARGUMENTS is the exact
argument form passed to TARGET, so wrappers can preserve keyword names and
one-off positional parameters without repeating the whole DEFUN body."
    `(defun ,name ,lambda-list
       ,docstring
       (,target ,@arguments))))
