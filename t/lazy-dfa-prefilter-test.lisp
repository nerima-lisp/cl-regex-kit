;;;; t/lazy-dfa-prefilter-test.lisp
;;;;
;;;; REGEX-REQUIRED-LITERALS, the literal existence prefilter, and the lazy
;;;; DFA IS-MATCH-P consults. Every differential case compares the public
;;;; API against RUN-PIKE-VM-BOOLEAN, the pre-existing ground-truth boolean
;;;; matcher neither optimization touches, so a regression in either new
;;;; code path shows up as a public-API mismatch rather than as an
;;;; assertion against the optimization's own internals.
(in-package #:cl-regex-kit/test)

(defun pike-vm-boolean-oracle (regex text &key (start 0) end)
  "Return whether REGEX matches TEXT via RUN-PIKE-VM-BOOLEAN directly,
bypassing the literal prefilter and the lazy DFA entirely."
  (if (cl-regex-kit::regex-advanced-p regex)
      (not (null (scan regex text :start start :end end)))
      (cl-regex-kit::run-pike-vm-boolean
       (cl-regex-kit::regex-program regex) text
       :start start :end end
       :never-newline-p (cl-regex-kit::regex-never-newline-p regex))))

;;; -- REGEX-REQUIRED-LITERALS ------------------------------------------

(it
  "extracts a plain literal pattern whole"
  (expect (regex-required-literals (compile-regex "needle"))
          :to-equal
          (list "needle")))

(it
  "merges consecutive literal characters across a non-literal split"
  (let ((literals (regex-required-literals (compile-regex "abc[0-9]def"))))
    (expect (member "abc" literals :test #'string=) :to-be-truthy)
    (expect (member "def" literals :test #'string=) :to-be-truthy)))

(it
  "returns NIL for alternation, since no literal is common to every branch"
  (expect (regex-required-literals (compile-regex "cat|dog")) :to-be-null))

(it
  "returns NIL for an optional (MIN 0) literal run, but keeps a mandatory prefix"
  (expect (regex-required-literals (compile-regex "(?:needle)*")) :to-be-null)
  (expect (regex-required-literals (compile-regex "(?:ab)?")) :to-be-null)
  (expect (regex-required-literals (compile-regex "ab?"))
          :to-equal
          (list "a")))

(it
  "requires a repetition's literal body when MIN is at least 1"
  (expect (regex-required-literals (compile-regex "(?:needle)+"))
          :to-equal
          (list "needle")))

(it
  "excludes a case-insensitive literal run"
  (expect (regex-required-literals (compile-regex "(?i)needle")) :to-be-null))

(it
  "includes a non-optional group's literal content"
  (let ((literals (regex-required-literals (compile-regex "x(?:needle)y"))))
    (expect (member "needle" literals :test #'string=) :to-be-truthy)
    (expect (member "x" literals :test #'string=) :to-be-truthy)
    (expect (member "y" literals :test #'string=) :to-be-truthy)
    (expect (length literals) :to-equal 3)))

(it
  "includes an atomic group's literal content"
  (expect (regex-required-literals (compile-regex "(?>needle)"))
          :to-equal
          (list "needle")))

(it
  "includes a positive lookahead's literal content but not a negative one"
  (expect (regex-required-literals (compile-regex "(?=needle)"))
          :to-equal
          (list "needle"))
  (expect (regex-required-literals (compile-regex "(?!needle)")) :to-be-null))

(it
  "reports byte-mode literals as their required octet values"
  (let ((literals (regex-required-literals (compile-byte-regex "cat"))))
    (expect literals :to-equal (list "cat"))))

(it
  "returns NIL when no plain literal appears anywhere"
  (expect (regex-required-literals (compile-regex "[a-z]+")) :to-be-null)
  (expect (regex-required-literals (compile-regex "^$")) :to-be-null))

;;; -- Literal prefilter, functional equivalence ------------------------

(it
  "keeps scan/is-match-p/all-matches correct when the required literal is absent"
  (let ((regex (compile-regex "needle")))
    (expect (is-match-p regex "haystack") :to-be nil)
    (expect (scan regex "haystack") :to-be-null)
    (expect (all-matches regex "haystack") :to-be-null))
  (let ((regex (compile-regex "needle")))
    (expect (is-match-p regex "a needle here") :to-be-truthy)
    (expect (match-string (scan regex "a needle here") "a needle here") :to-equal "needle")))

(it
  "keeps a required-literal-derived pattern correct across a start offset"
  (let ((regex (compile-regex "abcneedle")))
    (expect (is-match-p regex "abcneedle" :start 1) :to-be nil)
    (expect (is-match-p regex "xxabcneedle" :start 2) :to-be-truthy)))

;;; -- Lazy DFA eligibility ----------------------------------------------

(it
  "builds a lazy DFA for an ordinary literal/class/repetition program"
  (expect (cl-regex-kit::regex-lazy-dfa (compile-regex "a[0-9]+b*")) :to-be-truthy))

(it
  "declines the lazy DFA for anchors, boundaries, and advanced constructs"
  (expect (cl-regex-kit::regex-lazy-dfa (compile-regex "^ab$")) :to-be-null)
  (expect (cl-regex-kit::regex-lazy-dfa (compile-regex "\\bab\\b")) :to-be-null)
  (expect (cl-regex-kit::regex-lazy-dfa (compile-regex "(a)\\1")) :to-be-null)
  (expect (cl-regex-kit::regex-lazy-dfa (compile-regex "(?<=a)b")) :to-be-null))

(it
  "declines the lazy DFA for a Unicode-aware byte-mode program but stays correct"
  (let ((regex (compile-byte-regex "a.b")))
    (expect (cl-regex-kit::regex-lazy-dfa regex) :to-be-null)
    (expect (is-match-p regex (ascii-octets "axb")) :to-be-truthy)))

(it
  "builds a lazy DFA for a raw-byte program compiled with :unicode nil"
  (let ((regex (compile-byte-regex "a.b" :unicode nil)))
    (expect (cl-regex-kit::regex-lazy-dfa regex) :to-be-truthy)
    (expect (is-match-p regex (ascii-octets "axb")) :to-be-truthy)))

;;; -- Differential property tests ----------------------------------------

(it-property
  "is-match-p agrees with SCAN's match existence across a mixed corpus"
  ((pattern
     (gen-member (list "needle" "cat|dog" "^[A-Za-z_][A-Za-z0-9_]*$" "[0-9]{2,4}[a-z]+"
                       "(?i)hello" "colou?r" "(?:ab)+c" "a*b*c*" "\\bfoo\\b"
                       "(a)\\1" "(?=needle)" "(?<=a)b" "x(?:needle){2,3}y")))
    (text (gen-string :min-length 0 :max-length 24 :alphabet "abcdefoxyz01234 _")))
  (let ((regex (compile-regex pattern)))
    (expect (is-match-p regex text)
            :to-equal
            (not (null (scan regex text))))))

(it-property
  "is-match-p agrees with the raw Pike VM boolean oracle for regular patterns"
  ((pattern
     (gen-member (list "needle" "cat|dog" "^[A-Za-z_][A-Za-z0-9_]*$" "[0-9]{2,4}[a-z]+"
                       "(?i)hello" "colou?r" "(?:ab)+c" "a*b*c*" "\\bfoo\\b")))
    (text (gen-string :min-length 0 :max-length 24 :alphabet "abcdefoxyz01234 _"))
    (requested-start (gen-integer :min 0 :max 24)))
  (let* ((regex (compile-regex pattern))
         (start (min requested-start (length text))))
    (expect (is-match-p regex text :start start)
            :to-equal
            (pike-vm-boolean-oracle regex text :start start))))

(it-property
  "is-match-p agrees with the Pike VM boolean oracle over Unicode text"
  ((text (gen-string :min-length 0 :max-length 16 :alphabet "abcé中文")))
  (let ((regex (compile-regex "(?:é|中)+")))
    (expect (is-match-p regex text) :to-equal (pike-vm-boolean-oracle regex text))))

(it-property
  "is-match-p agrees with SCAN for byte regexes, unicode on and off"
  ((text (gen-string :min-length 0 :max-length 20 :alphabet "abcneedlexy")))
  (let ((octets (ascii-octets text)))
    (dolist (regex (list (compile-byte-regex "needle")
                        (compile-byte-regex "needle" :unicode nil)))
      (expect (is-match-p regex octets) :to-equal (not (null (scan regex octets)))))))
