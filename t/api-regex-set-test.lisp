;;;; t/api-regex-set-test.lisp
;;;;
;;;; Regex-set APIs, merged execution, and byte/set-specific behavior.
(in-package #:cl-regex-kit/test)

(it
 "reports regex-set cardinality without copying its source patterns"
 (let ((empty (compile-regex-set '() :size-limit 1))
       (set (compile-regex-set '("cat" "dog")))
       (byte-set (compile-byte-regex-set '("A"))))
   (expect (regex-set-count empty) :to-equal 0)
   (expect (regex-set-empty-p empty) :to-be-truthy)
   (expect (regex-set-count set) :to-equal 2)
   (expect (regex-set-empty-p set) :to-be nil)
   (expect (regex-set-count byte-set) :to-equal 1)
   (expect (regex-set-empty-p byte-set) :to-be nil)
   (signals type-error (regex-set-count nil))
   (signals type-error (regex-set-empty-p nil))))

(it
 "validates compiler options for empty regex sets"
 (expect-signals-for-functions
  error
  (list #'compile-regex-set #'compile-byte-regex-set)
  (funcall fn '() :unknown-option t))
 (expect-signals-for-functions
  type-error
  (list #'compile-regex-set #'compile-byte-regex-set)
  (funcall fn '() :case-insensitive :enabled)
  (funcall fn '() :literal :enabled)
  (funcall fn '() :nest-limit -1)
  (funcall fn '() :size-limit 0)
  (funcall fn '() :line-terminator "newline")
  (funcall fn '() :line-terminator (code-char #x80))))

(it
 "rejects malformed regex-set declarations before compilation"
 (expect-signals-cases
  type-error
 (compile-regex-set 42)
 (compile-regex-set "a")
 (compile-byte-regex-set "a"))
 (expect-macroexpand-signals-cases
  error
  (regex-set "a" :case-insensitive)
  (regex-set "a" case-insensitive t)
  (byte-regex-set "a" :case-insensitive)
  (byte-regex-set "a" case-insensitive t)
  (byte-regex-set "a" :case-insensitive enabled)))

(it
 "handles empty regex sets as empty match collections"
 (it-each (((compile-regex-set '()) "text")
           ((compile-regex-set #()) "text")
           ((compile-byte-regex-set '())
            (make-array 0 :element-type '(unsigned-byte 8))))
     "treats empty set ~S as a stable no-match collection"
     (set input)
   (expect (regex-set-matches set input) :to-equal '())
   (expect (regex-set-match-p set input) :to-be nil)))

(it
 "reuses regex-set result buffers without retaining stale matches"
 (let* ((set (compile-regex-set '("cat" "dog" "cat")))
        (matches (make-array 3 :element-type 'bit :initial-element 1)))
   (expect (regex-set-matches-into set matches "a cat") :to-be matches)
   (expect (coerce matches 'list) :to-equal '(1 0 1))
   (regex-set-matches-into set matches "a bird")
   (expect (coerce matches 'list) :to-equal '(0 0 0))
   (regex-set-matches-into set matches "cat dog" :start 4 :end 7)
   (expect (coerce matches 'list) :to-equal '(0 1 0))
   (expect (regex-set-matches-at set "cat dog" 4 :end 7) :to-equal '(1))
   (expect (regex-set-match-at-p set "cat dog" 4 :end 7) :to-be-truthy)
   (expect (regex-set-match-at-p set "cat dog" 7) :to-be nil)
   (signals type-error (regex-set-matches-at set "cat" -1))
   (signals type-error (regex-set-match-at-p set "cat" -1))
   (signals type-error (regex-set-matches-into set #*00 "cat"))
   (signals type-error (regex-set-matches-into set #(0 1 2) "cat"))
   (signals type-error (regex-set-matches-into set matches nil))
   (signals type-error (regex-set-matches-into set matches "cat" :start -1))
   (signals type-error (regex-set-matches-into set matches "cat" :end 4))
   (signals type-error (regex-set-matches-into set matches "cat" :timeout 0))))

(it
 "finds the first regex-set member with deterministic tie-breaking"
 (let ((set (compile-regex-set '("dog" "cat" "c(at)"))))
   (expect-regex-set-search-cases
    ((regex-set-search set "xxcat dog") "xxcat dog" 1 2 "cat")
    ((regex-set-search-at set "xxcat dog" 6) "xxcat dog" 0 6 "dog")
    ((regex-set-search set "bird") "bird" nil nil nil)))
 (let ((regex (compile-regex "cat")))
   (expect (scan regex "xxcat") :to-be-truthy)
   (expect (match-start (scan-at regex "xxcat" 2)) :to-be 2)
   (expect (scan regex "bird") :to-be nil)))

(it
 "routes advanced members through public regex-set APIs"
 (let* ((advanced (compile-regex "(?=a)a"))
        (set (compile-regex-set '("a" "(?=a)a"))))
   (expect (regex-advanced-p advanced) :to-be-truthy)
   (expect (regex-set-matches set "a") :to-equal '(0 1))
   (expect (regex-set-matches-at set "xa" 1 :end 2) :to-equal '(0 1))
   (expect (regex-set-match-at-p set "xa" 1 :end 2) :to-be-truthy)))

(it
 "routes advanced members through byte regex-set APIs"
 (let ((set (compile-byte-regex-set '("A" "(?=A)A"))))
   (expect (regex-set-matches set (octets 65)) :to-equal '(0 1))
   (expect (regex-set-match-p set (octets 65)) :to-be-truthy)))

(it
 "executes advanced-only regex sets through match and scan paths"
 (let ((set (compile-regex-set '("(?=a)a"))))
   (expect (regex-set-matches set "a") :to-equal '(0))
   (expect (regex-set-match-p set "a") :to-be-truthy)
   (expect (regex-set-matches set "b") :to-equal nil)
   (expect (regex-set-match-p set "b") :to-be nil)))

(it-regex-set-match-cases
 "preserves every word-boundary form in merged regex-set execution"
 #'compile-regex-set
 (("\\bcat\\b") " cat " (0))
 (("\\Bcat\\B") "scatx" (0))
 (("\\b{start}cat") " cat" (0))
 (("cat\\b{end}") "cat " (0))
 (("\\b{start-half}cat") "!!cat" (0))
 (("cat\\b{end-half}") "cat!!" (0)))

(it
 "matches octet vectors through the byte regex-set API"
 (let ((set (compile-byte-regex-set '("A" "\\C" "Z"))))
   (expect-truthy-cases
    (regex-set-p set)
    (byte-regex-set-p set)
    (regex-set-match-p set (octets #xff)))
   (expect-falsy-cases
    (byte-regex-set-p nil))
   (expect-equal-cases
    ((regex-set-matches set (octets #xff 65 #x80)) '(0 1)))
   (expect-signals-cases
    type-error
    (regex-set-matches set "A")
    (regex-set-matches set (octets 65) :start 2)
    (compile-byte-regex-set 42)))
   (expect-equal-cases
    ((regex-set-matches
      (compile-byte-regex-set '("\\bA" "^A$") :multi-line t :crlf t)
      (octets 65))
     '(0 1))
    ((regex-set-matches
      (compile-byte-regex-set '("^B$") :multi-line t :crlf t)
      (octets 65 13 10 66 13 10))
     '(0))
    ((regex-set-matches
      (compile-byte-regex-set '("\\p{L}" "(?-u:\\xFF)"))
      (octets #xff #xc3 #xa9))
     '(0 1)))
 (let ((set (byte-regex-set "A" "\\C")))
   (expect-equal-cases
    ((regex-set-matches
      set
      (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(65)))
     '(0 1)))))

(it-property
 "merged regex sets agree with their member scans for generated input"
 ((text (gen-string :min-length 0 :max-length 48 :alphabet "ab\n"))
  (requested-start (gen-integer :min 0 :max 48)))
 (let* ((patterns '("a+" "b+" "ab" "ba"))
        (start (min requested-start (length text)))
        (expected
         (loop for pattern in patterns
               for index from 0
               when (scan (compile-regex pattern) text :start start)
                 collect index)))
   (expect
    (regex-set-matches (compile-regex-set patterns) text :start start)
    :to-equal
    expected)))
