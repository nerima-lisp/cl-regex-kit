(require :asdf)

(let ((root
      (uiop:pathname-directory-pathname
        (or *load-truename* *default-pathname-defaults*))))
  (asdf:initialize-source-registry
    `(:source-registry (:directory ,root) :inherit-configuration))
  ;; Redirect compiled fasls to a writable temporary cache instead of
  ;; ASDF's default of writing them beside each source file: this script
  ;; also runs from a read-only checkout (a Nix store path, under
  ;; `mkTestApp`'s own build-time smoke check), where writing beside the
  ;; source signals SB-INT:SIMPLE-FILE-ERROR "Permission denied".
  (asdf:initialize-output-translations
    `(:output-translations
      (t (,(merge-pathnames "cl-regex-kit-benchmark-fasl-cache/"
                            (uiop:temporary-directory))
          :implementation))
      :ignore-inherited-configuration))
  (asdf:load-system "cl-regex-kit/benchmark")
  (funcall
    (symbol-function (find-symbol "RUN-BENCHMARKS" "CL-REGEX-KIT/BENCHMARK"))))
