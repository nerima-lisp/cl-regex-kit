(in-package #:cl-regex-kit/test)

(defun run-benchmarks-for-test (seed revision output-format stream)
  (cl-regex-kit/benchmarks:run-benchmarks
   :iterations 1
   :compile-iterations 1
   :warmup 1
   :sample-count 1
   :seed seed
   :revision revision
   :stream stream
   :output-format output-format))

(describe "benchmarks JSON output")

(it-each ((1729 "test-revision" :json))
    "writes JSON to the stream while preserving the benchmark report return value"
    (seed revision output-format)
  (let* ((stream (make-string-output-stream))
         (report
           (run-benchmarks-for-test seed revision output-format stream)))
    (expect report :to-be-truthy)
    (expect (getf report :metadata) :to-be-truthy)
    (expect (getf report :results) :to-be-truthy)
    (expect-benchmark-json-report
     (get-output-stream-string stream)
     :revision
     revision)))

(it-each ((1729 "test-revision" :lisp))
    "rejects non-JSON benchmark output formats"
    (seed revision output-format)
  (signals error
    (run-benchmarks-for-test
     seed
     revision
     output-format
     (make-string-output-stream))))
