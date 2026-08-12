;;;; t/api-fuzzy-test.lisp
;;;;
;;;; Fuzzy matching public APIs and control-flow boundaries.
(in-package #:cl-regex-kit/test)

(it
 "supports bounded fuzzy matching for regular and byte regexes"
 (let ((regex (compile-regex "cat")))
   (expect-match-metadata-cases
    ((fuzzy-scan regex "cot") 0 3 1)
    ((fuzzy-scan regex "ct") 0 2 1)
    ((fuzzy-scan regex "cxt") 0 3 1)
    ((fuzzy-scan (compile-regex "[ab]") "c") 0 0 1))
   (expect-match-string-cases
    ((fuzzy-scan regex "cot") "cot" "cot"))
   (expect (fuzzy-scan regex "cot" :max-edits 0) :to-be nil)
   (expect-match-metadata-cases
    ((scan regex "cat") 0 3 0))
   (let ((captured (fuzzy-scan (compile-regex "(c)(at)") "cot")))
     (expect (match-group-string captured 1 "cot") :to-equal "c")
     (expect (match-group-string captured 2 "cot") :to-equal "ot"))
   (expect-match-span-cases
    ((fuzzy-scan regex "xxcot") 2 5)
    ((fuzzy-scan-at regex "xxcot" 2) 2 5)
    ((fuzzy-match "cat" "xxcot") 2 5)))
 (let* ((text (octets 99 111 116))
        (result (byte-fuzzy-match "cat" text)))
   (expect-match-metadata-cases
    (result 0 3 1))
   (expect (coerce (match-string result text) 'list)
           :to-equal
           '(99 111 116))))

(it
 "covers fuzzy NFA control flow and input-unit boundaries"
 (let ((crlf (format nil "~C~C" #\Return #\Newline)))
   (expect-match-metadata-cases
    ((fuzzy-scan (compile-regex "\\R") crlf) 0 2 0)
    ((fuzzy-scan (compile-regex "\\R" :never-newline t) crlf) 0 0 1)))
 (expect-match-metadata-cases
  ((fuzzy-scan (compile-regex "^a") "ba") 0 0 1)
  ((fuzzy-scan (compile-regex "a|b") "c") 0 0 1)
  ((fuzzy-scan (compile-regex "a*") "bbb") 0 0 0)
  ((fuzzy-match (compile-regex "cat") "cot") 0 3 1)
  ((fuzzy-scan-at (compile-regex "cat") "xxcot" 2) 2 5 1))
 (let* ((unicode-regex (compile-byte-regex "." :unicode t))
        (valid (octets 195 169))
        (invalid (octets 255))
        (compiled (compile-byte-regex "cat"))
        (compiled-text (octets 99 111 116)))
   (expect-match-metadata-cases
    ((fuzzy-scan unicode-regex valid) 0 2 0)
    ((fuzzy-scan unicode-regex valid :end 1) 0 0 1)
    ((fuzzy-scan unicode-regex invalid) 0 0 1)
    ((byte-fuzzy-match compiled compiled-text) 0 3 1))))

(it-fuzzy-distance-cases
 "fuzzy public entry points preserve default bounds"
 (("cat" "cat" 0)
  ("cat" "cot" 1)))

(it
 "byte-fuzzy-match accepts a compiled byte regex with default bounds"
 (let ((text (octets 99 111 116)))
   (expect
    (match-edit-distance
     (byte-fuzzy-match (compile-byte-regex "cat") text))
    :to-be
    1)))

(it
 "rejects unsupported fuzzy dialects and enforces its state budget"
 (signals
  fuzzy-match-unsupported
  (fuzzy-scan (compile-regex "(?=a)a") "a"))
 (signals
  fuzzy-match-limit-error
  (fuzzy-scan (compile-regex "abc") "xyz" :state-limit 1))
 (signals type-error (fuzzy-scan (compile-regex "a") "a" :max-edits -1))
 (signals type-error (fuzzy-scan (compile-regex "a") "a" :state-limit 0)))

(it
 "signals when fuzzy matching encounters an unsupported internal instruction"
 (flet ((bogus-regex ()
          (make-instance
           'cl-regex-kit::regex
           :program
           (vector (cl-regex-kit::make-inst :op :bogus))
           :ast
           nil
           :group-count
           0
           :static-capture-count
           1
           :group-names
           nil
           :source
           "<bogus>")))
   (signals
    error
    (cl-regex-kit::fuzzy-match-at-position
     (bogus-regex)
     "a"
     0
     1
     1
     10))))
