(in-package #:cl-regex-kit/test)

(it-match-cases
 "matches ordered alternatives from one compiled advanced pattern"
 "(?:(?<word>cat)|dog)"
 :matches ("cat" "dog" "a dog in a sentence")
 :non-matches ("cow" "bird"))

(it-match-cases
 "keeps a backreference tied to the participating capture"
 "(?<word>[a-z]+)-\\k<word>"
 :matches ("echo-echo" "test-test")
 :non-matches ("echo-test" "test-echo"))

(it-runner-availability-cases
 "loads the advanced runner as a separate ASDF component"
 ((cl-regex-kit:run-advanced-regex "(?=a)a" "a")))

(it-pattern-scan-cases
 "evaluates any-character and line-break nodes on the advanced path"
 (("(?=.)a" "a" 0 1)
  ("(?=\\R)\\R" #.(string (code-char 10)) 0 1)))

(it-advanced-element-equal-cases
 "compares advanced character and byte elements with their mode rules"
 ((#\A #\a t nil t)
  (65 97 t nil t)
  (#\A #\a nil nil nil)
  (#\A 65 t nil nil)))

(it-evaluated-scan-range-cases
 "matches Unicode grapheme clusters across UAX boundary rules"
 "\\X"
 (list (list (format nil "~C~C" (code-char 13) (code-char 10)) 0 2)
       (list (coerce (list (code-char #x1f1e6)
                           (code-char #x1f1e7)
                           (code-char #x1f1e8))
                     'string)
             0 2)
       (list (coerce (list (code-char #x1f469)
                           (code-char #x200d)
                           (code-char #x1f4bb))
                     'string)
             0 3)
       (list (coerce (list (code-char #x1100)
                           (code-char #x1161)
                           (code-char #x11a8))
                     'string)
             0 3)
       (list (coerce (list (code-char #x0915)
                           (code-char #x093e))
                     'string)
             0 2)
       (list (coerce (list (code-char #x0600) #\A) 'string)
             0 2)))

(it-scan-range-cases
 "recognizes sentence boundaries around terminal punctuation and separators"
 "\\b{sb}B"
 (("A. B" 3 4)
  ("A. )B" nil nil)
  ("A. !B" 4 5)
  ("A.   B" 5 6)
  ("A. 1B" nil nil)
  ("A. b" nil nil)))

(it
 "honors commit control across ordered alternatives"
 (let ((committed (compile-regex "a(*COMMIT)b|ac"))
       (commit-failure (compile-regex "a(*COMMIT)(*FAIL)|b"))
       (standalone-commit (compile-regex "(*COMMIT)")))
   (expect "ab" :to-match-regex committed)
   (expect (scan committed "ac") :to-be-null)
   (expect (scan commit-failure "ab") :to-be-null)
   (expect "b" :to-match-regex commit-failure)
   ;; Exercise the public advanced-runner boundary, not only SCAN's dispatch.
   ;; COMMIT at the root is normalized into an ordinary successful result.
   (let ((result (cl-regex-kit:run-advanced-regex standalone-commit "a")))
     (expect (list (match-start result) (match-end result))
             :to-equal
             '(0 0)))
   (signals error
    (cl-regex-kit:run-advanced-regex
      (compile-regex "(?=a)a")
      "a"
      :shortest-p t
      :longest-p t))))

(it-advanced-limit-cases
 "reports the public execution limits when advanced execution is bounded"
 (("(?=a)a" :size-limit 1 :steps 1 2)
  ("(?=a)a" :nest-limit 1 :nest-depth 1 2)))

(it-advanced-runner-cases
 "applies explicit ranges and result selection in the advanced runner"
 "(?=a)a+"
 "baaa"
 (((:start 1 :end 4 :shortest-p t) 1 2)
  ((:start 1 :end 4 :longest-p t) 1 4)
  ((:start 2 :end 3) 2 3)
  ((:start 3 :end 3) nil nil)))

(it-advanced-conditional-match-cases
 ("evaluates assertion conditions in the advanced matcher"
  "(?(?=a)a|b)"
  :matches ("a" "b")
  :non-matches ("c"))
 ("evaluates recursion conditions outside and inside subroutines"
  "(?(R)b|c)"
  :matches ("c")
  :non-matches ("b"))
 ("evaluates recursive named conditions"
  "(?<p>a(?:(?&p))?(?(R)b|c))"
  :matches ("ac" "aabc")
  :non-matches ("aab"))
 ("evaluates recursive index conditions"
  "(?<p>a(?:(?&p))?(?(R1)b|c))"
  :matches ("ac" "aabc")
  :non-matches ("aab"))
 ("evaluates recursive name conditions"
  "(?<p>a(?:(?&p))?(?(R&p)b|c))"
  :matches ("ac" "aabc")
  :non-matches ("aab")))

(it-scan-string-cases
 "evaluates named capture conditions in both branches"
 "(?<a>a)?(?(<a>)b|c)"
 (("ab" "ab")
  ("c" "c")
  ("ad" nil)))

(it-lookaround-cases
 "matches named lookaround forms through the public matcher surface"
 (("(*POSITIVE_LOOKAHEAD:a)a" "a" "b")
  ("(*PLA:a)a" "a" "b")
  ("(*NEGATIVE_LOOKAHEAD:b)a" "a" "b")
  ("(*NLA:b)a" "a" "b")
  ("(*POSITIVE_LOOKBEHIND:a)b" "ab" "ba")
  ("(*PLB:a)b" "ab" "ba")
  ("(*NEGATIVE_LOOKBEHIND:b)a" "a" "ba")
  ("(*NLB:b)a" "a" "ba")
  ("(*NON_ATOMIC_POSITIVE_LOOKAHEAD:a)a" "a" "b")
  ("(*NAPLA:a)a" "a" "b")
  ("(*NON_ATOMIC_POSITIVE_LOOKBEHIND:a)b" "ab" "ba")
  ("(*NAPLB:a)b" "ab" "ba")))

(it-parses-cases
 "parses recursive, relative, and conditional advanced references"
 ("(?R)"
  "(?0)"
  "(a)(?P=name)"
  "(?<word>a)(?P>word)"
  "(?<a>a)(?-1)"
  "(a)(?1)"
  "(?<p>a(?R)?b)"
  "(?(?=a)b|c)"
  "(a)?(?(<a>)b|c)"
  "(?(R)b|c)"
  "(?(R1)b|c)"
  "(?<p>a)(?(R&p)b|c)"
  "(?|a)"))
