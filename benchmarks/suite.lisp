(in-package #:cl-regex-kit/benchmarks)

(defstruct workload id
  name
  pattern
  subjects
  expected)

(defun environment-integer (name default)
  (let ((value (uiop:getenv name)))
    (if value (let ((parsed
            (handler-case (parse-integer value)
              (parse-error ()
                (error "~A must be a positive integer, got ~S" name value)))))
        (unless (plusp parsed)
          (error "~A must be a positive integer, got ~S" name value))
        parsed) default)))

(defun lcg-next (state)
  (mod (+ (* state 1664525) 1013904223) 4294967296))

(defun seeded-letters (seed length)
  (let ((state seed)
        (text (make-string length)))
    (dotimes (index length)
      (setf state (lcg-next state)
            (aref text index) (code-char (+ (char-code #\a) (mod state 26)))))
    (values text state)))

(defun make-workloads (seed)
  (multiple-value-bind (random-text next-seed) (seeded-letters seed 48)
    (declare (ignore next-seed))
    (list
      (make-workload
        :id
        :literal-hit-v1
        :name
        "literal-hit"
        :pattern
        "needle"
        :subjects
        #("haystack" "needle" "a needle here" "needless")
        :expected
        #(nil t t t))
      (make-workload
        :id
        :anchored-identifier-v1
        :name
        "anchored-identifier"
        :pattern
        "^[A-Za-z_][A-Za-z0-9_]*$"
        :subjects
        #("alpha" "_alpha42" "42alpha" "alpha-beta")
        :expected
        #(t t nil nil))
      (make-workload
        :id
        :alternation-and-repeat-v1
        :name
        "alternation-and-repeat"
        :pattern
        "^(cat|dog|fox)+$"
        :subjects
        #("catdogfox" "foxfox" "catbird" "")
        :expected
        #(t t nil nil))
      (make-workload
        :id
        :seeded-search-v1
        :name
        "seeded-search"
        :pattern
        "[0-9]{3}[a-z]{5}"
        :subjects
        (vector
          random-text
          (concatenate 'string random-text "123abcde")
          "12abcde"
          "123abcd!")
        :expected
        #(nil t nil nil)))))

(defun bytes-consed ()
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl nil)

(defun elapsed-seconds (start end)
  (/ (- end start) (float internal-time-units-per-second 1d0)))

(defun measure (thunk)
  (let ((start-time (get-internal-real-time))
        (start-bytes (bytes-consed)))
    (let ((checksum (funcall thunk))
          (end-bytes (bytes-consed)))
      (list
        :elapsed-seconds
        (elapsed-seconds start-time (get-internal-real-time))
        :bytes-consed
        (and start-bytes end-bytes (- end-bytes start-bytes))
        :checksum
        checksum))))

(defun median (values)
  (let* ((sorted (sort (copy-list values) #'<))
         (count (length sorted))
         (middle (floor count 2)))
    (if (oddp count) (nth middle sorted)
      (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2))))

(defun measurement-summary (measurements key)
  (let ((values
        (loop for measurement in measurements
              for value = (getf measurement key)
              when value
                collect value)))
    (when values
      (list
        :median
        (median values)
        :min
        (reduce #'min values)
        :max
        (reduce #'max values)))))

(defun measure-samples (sample-count thunk)
  (let ((samples
        (loop repeat sample-count
              collect (measure thunk))))
    (let ((checksums
          (mapcar
            (lambda (sample)
              (getf sample :checksum))
            samples)))
      (unless (every
          (lambda (checksum)
            (eql checksum (first checksums)))
          (rest checksums))
        (error "Benchmark checksums differ across samples: ~S" checksums)))
    (list
      :samples
      samples
      :elapsed-seconds
      (measurement-summary samples :elapsed-seconds)
      :bytes-consed
      (measurement-summary samples :bytes-consed)
      :checksum
      (getf (first samples) :checksum))))

(defun workload-description (workload)
  (list
    :workload-id
    (workload-id workload)
    :name
    (workload-name workload)
    :pattern
    (workload-pattern workload)
    :subject-count
    (length (workload-subjects workload))))

(defun verify-workload (workload timeout-seconds)
  (let ((subjects (workload-subjects workload))
        (expected-values (workload-expected workload)))
    (unless (= (length subjects) (length expected-values))
      (error
        "Workload ~A has ~D subjects but ~D expected values"
        (workload-name workload)
        (length subjects)
        (length expected-values)))
    (let ((regex
          (cl-regex-kit::call-with-timeout
            timeout-seconds
            (lambda ()
              (compile-regex (workload-pattern workload))))))
      (loop for subject across subjects
            for expected across expected-values
            for actual = (not (null (is-match-p regex subject :timeout timeout-seconds)))
            unless (eql actual expected)
              do (error
          "Correctness mismatch in ~A for ~S: expected ~S, got ~S"
          (workload-name workload)
          subject
          expected
          actual))))
  t)

(defun benchmark-compilation (workload iterations sample-count timeout-seconds)
  (append
    (workload-description workload)
    (list :phase :compile :operations iterations)
    (measure-samples
      sample-count
      (lambda ()
        (let ((checksum 0))
          (dotimes (index iterations checksum)
            (declare (ignore index))
            (when (cl-regex-kit::call-with-timeout
                timeout-seconds
                (lambda ()
                  (compile-regex (workload-pattern workload))))
              (incf checksum))))))))

(defun benchmark-hot-matching (workload iterations warmup sample-count timeout-seconds)
  (let* ((regex
        (cl-regex-kit::call-with-timeout
          timeout-seconds
          (lambda ()
            (compile-regex (workload-pattern workload)))))
         (subjects (workload-subjects workload))
         (subject-count (length subjects)))
    (dotimes (index warmup)
      (is-match-p
        regex
        (aref subjects (mod index subject-count))
        :timeout
        timeout-seconds))
    (append
      (workload-description workload)
      (list :phase :hot-match :operations iterations)
      (measure-samples
        sample-count
        (lambda ()
          (let ((matches 0))
            (dotimes (index iterations matches)
              (when (is-match-p
                  regex
                  (aref subjects (mod index subject-count))
                  :timeout
                  timeout-seconds)
                (incf matches)))))))))

(defun benchmark-metadata (iterations
    compile-iterations
    warmup
    sample-count
    seed
    timeout-seconds
    revision)
  (list
    :format-version
    3
    :suite-id
    :cl-regex-kit-core-v1
    :revision
    revision
    :implementation
    (lisp-implementation-type)
    :implementation-version
    (lisp-implementation-version)
    :sbcl-version
    #+sbcl (lisp-implementation-version)
    #-sbcl nil
    :machine-type
    (machine-type)
    :software-type
    (software-type)
    :software-version
    (software-version)
    :iterations
    iterations
    :compile-iterations
    compile-iterations
    :warmup
    warmup
    :seed
    seed
    :bytes-supported-p
    #+sbcl t
    #-sbcl nil
    :gc-policy
    :natural
    :sample-count
    sample-count
    :timeout-scope
    :per-compile-and-match-operation
    :per-operation-timeout-seconds
    timeout-seconds))

(defun require-positive-integer (name value)
  (unless (and (integerp value) (plusp value))
    (error "~A must be a positive integer, got ~S" name value)))

(defun run-benchmarks (&key
    (iterations (environment-integer "CL_REGEX_KIT_BENCH_ITERATIONS" 10000))
    (compile-iterations
      (environment-integer "CL_REGEX_KIT_BENCH_COMPILE_ITERATIONS" 100))
    (warmup (environment-integer "CL_REGEX_KIT_BENCH_WARMUP" 1000))
    (sample-count (environment-integer "CL_REGEX_KIT_BENCH_SAMPLES" 5))
    (seed (environment-integer "CL_REGEX_KIT_BENCH_SEED" 1729))
    (revision (or (uiop:getenv "CL_REGEX_KIT_BENCH_REVISION") "unspecified"))
    (timeout-seconds 1d0)
    (stream *standard-output*))
  (require-positive-integer "iterations" iterations)
  (require-positive-integer "compile-iterations" compile-iterations)
  (require-positive-integer "warmup" warmup)
  (require-positive-integer "sample-count" sample-count)
  (unless (and (stringp revision) (plusp (length revision)))
    (error "revision must be a non-empty string, got ~S" revision))
  (unless (and (realp timeout-seconds) (plusp timeout-seconds))
    (error "timeout-seconds must be positive, got ~S" timeout-seconds))
  (let ((workloads (make-workloads seed)))
    (dolist (workload workloads)
      (verify-workload workload timeout-seconds))
    (let ((report
          (list
            :metadata
            (benchmark-metadata
              iterations
              compile-iterations
              warmup
              sample-count
              seed
              timeout-seconds
              revision)
            :correctness
            :verified
            :results
            (loop for workload in workloads
                  append (list
                (benchmark-compilation workload compile-iterations sample-count timeout-seconds)
                (benchmark-hot-matching workload iterations warmup sample-count timeout-seconds))))))
      (write report :stream stream :pretty t)
      (terpri stream)
      (finish-output stream)
      report)))
