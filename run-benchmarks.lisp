(require :asdf)

(defun benchmark-fasl-cache-name (root)
  (with-output-to-string (out)
    (loop with root-name = (namestring (truename root))
          for char across root-name
          do (write-char
              (if (or (alphanumericp char) (char= char #\-)) char
                #\_)
              out))))

(defun benchmark-home-root (root)
  (merge-pathnames
   (format
    nil
    "cl-regex-kit-benchmark-home/~A/"
    (benchmark-fasl-cache-name root))
   (uiop:temporary-directory)))

(let* ((root
         (uiop:pathname-directory-pathname
           (or *load-truename* *default-pathname-defaults*)))
       (home-root
         (benchmark-home-root root))
       (cache-root
         (merge-pathnames
           (format nil "cl-regex-kit-benchmark-fasl-cache/~A/"
                   (benchmark-fasl-cache-name root))
           (uiop:temporary-directory))))
  ;; The Nix benchmark wrapper keeps the caller's HOME when it is set,
  ;; which lets ASDF reuse stale benchmark fasls from ~/.cache/common-lisp.
  ;; Force an isolated temporary HOME before ASDF initializes so the
  ;; benchmark always compiles against the current checkout.
  (ensure-directories-exist home-root)
  (require :sb-posix)
  (funcall
    (symbol-function (find-symbol "SETENV" "SB-POSIX"))
    "HOME"
    (namestring home-root)
    1)
  (asdf:initialize-source-registry
    `(:source-registry (:directory ,root) :inherit-configuration))
  ;; Redirect this checkout's own compiled fasls to a writable temporary
  ;; cache instead of ASDF's default of writing them beside each source
  ;; file: this script also runs from a read-only checkout (a Nix store
  ;; path, under `mkTestApp`'s own build-time smoke check), where writing
  ;; beside the source signals SB-INT:SIMPLE-FILE-ERROR "Permission
  ;; denied". Scoped to ROOT specifically, with :INHERIT-CONFIGURATION
  ;; for everything else, so dependencies keep whatever translation
  ;; already resolves them correctly. Include the checkout path in the
  ;; cache directory name so a workspace run and a Nix-store run cannot
  ;; reuse each other's stale fasls.
  (asdf:initialize-output-translations
    `(:output-translations
      (,(merge-pathnames "**/*.*" root)
        (,cache-root :implementation))
      :inherit-configuration))
  (asdf:load-system "cl-regex-kit/benchmark")
  (funcall
    ;; The package is CL-REGEX-KIT/BENCHMARKS (plural, matching the
    ;; `benchmarks/` pathname) even though the ASDF system component is
    ;; named "cl-regex-kit/benchmark" (singular, matching cl-regex-kit's
    ;; own naming convention) -- these are two different namespaces.
    (symbol-function (find-symbol "RUN-BENCHMARKS" "CL-REGEX-KIT/BENCHMARKS"))))
