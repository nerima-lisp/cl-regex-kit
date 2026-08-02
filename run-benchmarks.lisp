(require :asdf)

(let ((root
      (uiop:pathname-directory-pathname
        (or *load-truename* *default-pathname-defaults*))))
  (asdf:initialize-source-registry
    `(:source-registry (:directory ,root) :inherit-configuration))
  ;; Redirect this checkout's own compiled fasls to a writable temporary
  ;; cache instead of ASDF's default of writing them beside each source
  ;; file: this script also runs from a read-only checkout (a Nix store
  ;; path, under `mkTestApp`'s own build-time smoke check), where writing
  ;; beside the source signals SB-INT:SIMPLE-FILE-ERROR "Permission
  ;; denied". Scoped to ROOT specifically, with :INHERIT-CONFIGURATION
  ;; for everything else, so dependencies keep whatever translation
  ;; already resolves them correctly.
  (asdf:initialize-output-translations
    `(:output-translations
      (,(merge-pathnames "**/*.*" root)
        (,(merge-pathnames "cl-regex-kit-benchmark-fasl-cache/"
                           (uiop:temporary-directory))
         :implementation))
      :inherit-configuration))
  (asdf:load-system "cl-regex-kit/benchmark")
  (funcall
    ;; The package is CL-REGEX-KIT/BENCHMARKS (plural, matching the
    ;; `benchmarks/` pathname) even though the ASDF system component is
    ;; named "cl-regex-kit/benchmark" (singular, matching cl-regex-kit's
    ;; own naming convention) -- these are two different namespaces.
    (symbol-function (find-symbol "RUN-BENCHMARKS" "CL-REGEX-KIT/BENCHMARKS"))))
