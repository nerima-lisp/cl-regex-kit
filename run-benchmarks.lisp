(require :asdf)

(let ((root
      (uiop:pathname-directory-pathname
        (or *load-truename* *default-pathname-defaults*))))
  (asdf:initialize-source-registry
    `(:source-registry (:directory ,root) :inherit-configuration))
  (asdf:load-system "cl-regex-kit/benchmark")
  (funcall
    (symbol-function (find-symbol "RUN-BENCHMARKS" "CL-REGEX-KIT/BENCHMARK"))))
