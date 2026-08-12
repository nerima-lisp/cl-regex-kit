;;;; t/api-regex-set-execution-test.lisp
;;;;
;;;; Regex-set execution, ordering, and compilation-parallelism behavior.
(in-package #:cl-regex-kit/test)

(it
 "compiles immutable regex sets and reports every matching pattern index"
 (expect-regex-set-cases
  ((compile-regex-set '("cat" "dog" "\\d+")) "dog42" 0 '(1 2))
  ((compile-regex-set '("cat" "dog" "\\d+")) "bird" 0 nil)
  ((compile-regex-set '("a" "a" "^z")) "a" 0 '(0 1))
  ((compile-regex-set '("a")) "za" 2 nil)
  ((compile-regex-set '()) "anything" 0 nil))
 (let* ((source (vector "cat" "dog"))
        (set (compile-regex-set source)))
   (setf (aref source 0) "bird")
   (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy)
   (setf (aref (regex-set-patterns set) 0) "bird")
   (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy))
 (dolist (compiler (list #'compile-regex #'compile-byte-regex))
   (let* ((source (copy-seq "cat"))
          (compiled (funcall compiler source)))
     (setf (char source 0) #\b)
     (expect (cl-regex-kit:regex-source compiled) :to-equal "cat")
     (let ((exposed (cl-regex-kit:regex-source compiled)))
       (setf (char exposed 0) #\r)
       (expect (cl-regex-kit:regex-source compiled) :to-equal "cat"))))
 (dolist (compiler (list #'compile-regex-set #'compile-byte-regex-set))
   (let* ((source (vector (copy-seq "cat") (copy-seq "dog")))
          (set (funcall compiler source)))
     (setf (char (aref source 0) 0) #\b)
     (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy)
     (let ((exposed (regex-set-patterns set)))
       (setf (char (aref exposed 0) 0) #\r)
       (expect (equalp (regex-set-patterns set) #("cat" "dog")) :to-be-truthy))))
 (let ((set (regex-set "cat" "dog")))
   (expect (regex-set-matches set "a dog") :to-equal '(1)))
 (let ((set (regex-set "cat" :case-insensitive t)))
   (expect (regex-set-matches set "CAT") :to-equal '(0)))
 (signals error (macroexpand-1 '(regex-set "cat" :case-insensitive enabled)))
 (let ((set
        (compile-regex-set
         '("^right$")
         :multi-line
         t
         :crlf
         t
         :line-terminator
         #\;))
       (crlf-text
        (format
         nil
         "left~C~Cright~C~Ctail"
         #\Return
         #\Newline
         #\Return
         #\Newline)))
   (expect (regex-set-matches set crlf-text) :to-equal '(0))))

(it
 "keeps merged regex-set execution equivalent to member regex scans"
 (expect-regex-set-equivalent-cases
  ('("(?<letter>a)" "b") "a" 0)
  ('("a*" "^$" "\\A\\z") "" 0)
  ('("(?i)cat" "\\p{Greek}+") "Cat α" 0)
  ('("(?i)k" "\\w+" "(?s:.)") "K\r\n" 0)
  ('("\\Afoo\\z" "o+") "foo" 1)
  ('("(?m)^foo$" "bar") "x\nfoo\ny" 0)
  ('("\\b{start-half}\\w+" "\\b{end-half}\\w+") "α" 0)
  ('("a" "a") "a" 0)))

(it
 "keeps larger merged regex sets equivalent to member regex scans"
 (let ((patterns
        (loop for index below 32
              collect (format nil "(?:item~D|code~D)(?:-x)?" index index))))
   (expect-regex-set-equivalent-cases
    (patterns "prefix code17-x middle item3 suffix" 0))))

(it
 "validates regex-set parallel compilation boundaries"
 (expect-signals-for-values
  (value '(0 9))
  type-error
  (let ((cl-regex-kit::*nfa-merge-parallelism-override* value))
    (cl-regex-kit::nfa-merge-parallelism 2 65536))
  (let ((cl-regex-kit::*regex-set-compile-parallelism-override* value))
    (cl-regex-kit::regex-set-compile-parallelism 2 65536)))
 (let ((cl-regex-kit::*nfa-merge-parallelism-override* 2)
       (cl-regex-kit::*regex-set-compile-parallelism-override* 2))
   (let ((set (compile-regex-set '("a" "b"))))
     (expect (regex-set-matches set "b") :to-equal '(1))))
 (let ((calls 0))
   (signals
    regex-syntax-error
    (cl-regex-kit::compile-regex-set-with
     '("a" "b")
     '(:size-limit 1)
     (lambda (pattern &rest options)
       (declare (ignore options))
       (incf calls)
       (compile-regex pattern))
     nil))
   (unless (zerop calls)
     (error "Size-limit rejection compiled ~D member patterns" calls))))

(it
 "selects default regex-set compilation parallelism from work size"
 (let ((cl-regex-kit::*regex-set-compile-parallelism-override* nil))
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 255 8192)
    :to-equal
    1)
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 256 8192)
    :to-equal
    cl-regex-kit::+regex-set-max-parallelism+)
   (expect
    (cl-regex-kit::regex-set-compile-parallelism 256 8191)
    :to-equal
    1)))

(it
 "preserves regex-set order across parallel compile batches and NFA relocation"
 (let* ((patterns
         (append
          (loop for index below 15
                collect (format nil "(?:item~D|code~D)(?:-x)?" index index))
          (list
           "(?:item3|code3)(?:-x)?"
           "(?:item3|code3)(?:-x)?")))
        (text "prefix code7-x middle item3 suffix"))
   (let ((cl-regex-kit::*nfa-merge-parallelism-override* 2)
         (cl-regex-kit::*regex-set-compile-parallelism-override* 2))
     (let ((set (compile-regex-set patterns)))
       (expect (regex-set-count set) :to-equal 17)
       (expect (regex-set-matches set text) :to-equal '(3 7 15 16))))))

(it
 "matches CRLF as one line break in regex-set execution"
 (let* ((text (format nil "~C~C" #\Return #\Newline))
        (set (compile-regex-set (list "\\R" "a")))
        (byte-text
         (make-array 2
                     :element-type '(unsigned-byte 8)
                     :initial-contents (list #x0d #x0a)))
        (byte-set (compile-byte-regex-set (list "\\R" "a"))))
   (expect (regex-set-matches set text) :to-equal '(0))
   (expect (regex-set-matches byte-set byte-text) :to-equal '(0))))
