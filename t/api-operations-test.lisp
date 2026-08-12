;;;; t/api-operations-test.lisp
;;;;
;;;; Builder options and high-level match processing.
(in-package #:cl-regex-kit/test)

(it
 "bounds NFA program size before compilation exhausts resources"
 (signals regex-syntax-error (compile-regex "a{1000}{1000}")))

(it
 "validates optional matching time limits"
 (let ((regex (compile-regex "a"))
       (regex-set (compile-regex-set '("a" "z+")))
       (non-matches (make-string 1000000 :initial-element #\b)))
   (expect (scan regex "a" :timeout 1) :to-be-truthy)
   (signals regex-timeout (scan regex non-matches :timeout 0.0001))
   (signals
    regex-timeout
    (all-matches
     regex
     (make-string 1000000 :initial-element #\a)
     :timeout
     0.0001))
   (signals
    regex-timeout
    (regex-set-matches regex-set non-matches :timeout 0.0001))
   (signals
    regex-timeout
    (regex-set-matches-into
     regex-set
     (make-array
      (regex-set-count regex-set)
      :element-type
      'bit
      :initial-element
     0)
     non-matches
     :timeout
     0.0001))
   (expect-signals-cases
    type-error
    (scan regex "a" :timeout 0)
    (all-matches regex "a" :timeout -1)
    (split regex "a" :timeout "one second"))))

(it
 "renders public regex diagnostics with actionable context"
 (expect
  (princ-to-string
   (make-condition 'regex-syntax-error :pattern "(abc" :reason "unclosed group"))
  :to-equal
  "Invalid regular expression \"(abc\": unclosed group")
 (expect
  (princ-to-string
   (make-condition
    'regex-syntax-error
    :pattern
    "[z-a]"
    :position
    2
    :reason
    "invalid range"))
  :to-equal
  "Invalid regular expression \"[z-a]\" at position 2: invalid range")
 (expect
  (princ-to-string (make-condition 'regex-timeout :seconds 0.125))
  :to-equal
  "Regular expression matching exceeded 0.125 seconds"))

(it
 "validates public matching inputs before executing the VM"
 (let ((regex (compile-regex "a"))
       (regex-set (compile-regex-set '("a"))))
   (expect-signals-cases
    type-error
    (scan nil "a")
    (scan regex nil)
    (scan regex "a" :start -1)
    (scan regex "a" :start "zero")
    (cl-regex-kit:full-match-p nil "a")
    (all-matches regex nil)
    (split regex nil)
    (replace-first regex nil "b")
    (replace-all regex nil "b")
    (regex-set-matches regex-set nil)
    (regex-set-matches regex-set "a" :start 2)
    (regex-set-matches nil "a"))
   (let ((result (scan regex "a")))
     (expect-signals-cases
      type-error
      (match-start nil)
      (match-end nil)
      (match-string result nil)
      (match-captures result nil)
      (match-group-string result 0 nil)
      (match-group-start result -1)
      (match-group-end result 1)
      (match-group-string result "missing" "a")
      (capture-location-start (regex-capture-locations regex) "zero")))))

(it
 "constrains matching to an explicit end without changing input context"
 (let ((b (compile-regex "b"))
       (plus (compile-regex "b+"))
       (end-anchor (compile-regex "b$"))
       (set (compile-regex-set '("b" "z"))))
   (expect (scan b "abc" :end 1) :to-be nil)
   (expect (match-start (scan b "abc" :end 2)) :to-equal 1)
   (expect
    (cl-regex-kit:full-match-p plus "abbbz" :start 1 :end 4)
    :to-be-truthy)
   (expect (cl-regex-kit:full-match-p plus "abz" :start 1 :end 3) :to-be nil)
   (expect (longest-match plus "abbbz" :start 1 :end 1) :to-be nil)
   (expect (match-end (longest-match plus "abbbz" :start 1 :end 3)) :to-equal 3)
   (expect (scan end-anchor "abz" :start 1 :end 2) :to-be nil)
   (expect
    (mapcar
     (lambda (result)
       (list (match-start result) (match-end result)))
     (all-matches (compile-regex ".") "abcd" :start 1 :end 3))
    :to-equal
    '((1 2) (2 3)))
   (expect (regex-set-matches set "abz" :start 1 :end 2) :to-equal '(0))
   (expect (regex-set-match-p set "abz" :start 1 :end 2) :to-be-truthy)
   (expect (split (compile-regex ",") "a,b,c" :end 2) :to-equal '("a" "b,c"))
   (expect (replace-all b "abzb" "X" :end 2) :to-equal "aXzb")
   (expect-signals-cases
    type-error
    (scan b "a" :end -1)
    (scan b "a" :end 2)
    (scan b "a" :start 1 :end 0)
    (scan b "a" :end "one")
    (cl-regex-kit:full-match-p b "a" :end -1)))
 (let ((result (full-match (compile-regex "(a|ab)") "ab")))
   (expect result :to-be-truthy)
   (expect (match-string result "ab") :to-equal "ab")
   (expect (match-group-string result 1 "ab") :to-equal "ab"))
 (expect (cl-regex-kit:full-match-p (compile-regex "a|ab") "ab") :to-be-truthy)
 (let ((result (full-match (compile-regex "a+?") "aa")))
   (expect result :to-be-truthy)
   (expect (match-end result) :to-equal 2))
 (expect (full-match (compile-regex "a|ab") "zab") :to-be nil)
 (let ((dot (compile-byte-regex "."))
       (snowman (octets #xe2 #x98 #x83)))
   (expect (scan dot snowman :end 2) :to-be nil)
   (expect (match-end (scan dot snowman :end 3)) :to-equal 3)))

(it
 "validates compiler options consistently for regexes"
 (expect-signals-for-functions
  type-error
  (list #'compile-regex #'compile-byte-regex)
  (funcall fn "a" :case-insensitive :enabled)
  (funcall fn "a" :literal :enabled)
  (funcall fn "a" :nest-limit -1)
  (funcall fn "a" :size-limit 0)
  (funcall fn "a" :line-terminator "newline")
  (funcall fn "a" :line-terminator (code-char #x80))))

(it
 "finds the shortest match at the leftmost start position"
 (expect (shortest-match (compile-regex "a+") "aaa") :to-equal 1)
 (expect (shortest-match (compile-regex "ba+") "xbaaab") :to-equal 3)
 (expect (shortest-match (compile-regex "a*") "aaa") :to-equal 0)
 (expect (shortest-match (compile-regex "a+") "baaa" :start 2) :to-equal 3)
 (expect (shortest-match-at (compile-regex "a+") "baaa" 2 :end 4) :to-equal 3)
 (expect (shortest-match-at (compile-regex "a+") "baaa" 4) :to-be nil)
 (signals type-error (shortest-match-at (compile-regex "a+") "baaa" -1))
 (expect (shortest-match (compile-regex "z+") "aaa") :to-be nil)
 (let ((text
        (make-array
         3
         :element-type
         '(unsigned-byte 8)
         :initial-contents
         '(65 65 65))))
   (expect (shortest-match (compile-byte-regex "A+") text) :to-equal 1)
   (expect (shortest-match-at (compile-byte-regex "A+") text 1) :to-equal 2)))

(it-split-cases
 "splits with Rust-compatible split operations"
 ","
 (split "one,two," ("one" "two" ""))
 (split-terminator "one,two," ("one" "two"))
 (split-terminator "one,two" ("one" "two"))
 (split-terminator "" (""))
 (split-inclusive "one,two," ("one," "two,"))
 (split-inclusive "one,two" ("one," "two"))
 (split-inclusive "" (""))
 (split-inclusive "one,two,three" ("one,two," "three") :start 4 :end 8)
 (split-terminator "a,bX" ("a" "bX") :end 2)
 (split-n "one,two,three" ("one" "two,three") 2)
 (split-n "one,two,three" ("one,two,three") 1)
 (split-n "one,two,three" nil 0)
 (split-n "one,two,three" ("one,two" "three") 2 :start 4))

(it
 "rejects invalid split arguments"
 (let ((comma (compile-regex ",")))
   (signals type-error (split-n comma "one,two" -1))
   (signals type-error (split-n comma "one,two" 1.5))
   (expect-signals-cases
    type-error
    (split-n nil "one,two" 0)
    (split-n comma nil 0)
    (split-n comma "one,two" 0 :start -1)
    (split-n comma "one,two" 0 :timeout 0)
    (split-terminator nil "one,two")
    (split-terminator comma nil)
    (split-inclusive nil "one,two")
    (split-inclusive comma nil)
    (split-inclusive comma "one,two" :start -1)
    (split-inclusive comma "one,two" :end 8)
    (split-inclusive comma "one,two" :start 2 :end 1)
    (split-inclusive comma "one,two" :timeout 0))))

(it
 "validates replacement shapes when no replacement is performed"
 (let ((string-regex (compile-regex "x"))
       (byte-regex (compile-byte-regex "x"))
       (octets
        (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(97))))
   (expect-signals-cases
    type-error
    (replace-first string-regex "a" 42)
    (replace-n string-regex "a" 42 0)
    (replace-first byte-regex octets "not octets")
    (replace-n byte-regex octets "not octets" 0))))

(it
 "handles zero-length matches once at each input position"
 (let ((empty (compile-regex "")))
   (expect-match-spans-cases
    ((all-matches empty "ab") '((0 0) (1 1) (2 2)))
    ((all-matches empty "ab" :start 1) '((1 1) (2 2))))
   (expect-equal-cases
    ((split empty "ab") '("" "a" "b" ""))
    ((split-terminator empty "ab") '("" "a" "b"))
    ((split-inclusive empty "ab") '("" "a" "b"))
    ((replace-first empty "ab" "-") "-ab")
    ((replace-n empty "ab" "-" 2) "-a-b")
    ((replace-all empty "ab" "-") "-a-b-")))
 (let ((empty (compile-byte-regex ""))
       (text (octets 65 66)))
   (expect-match-spans-cases
    ((all-matches empty text) '((0 0) (1 1) (2 2)))
    ((all-matches empty text :start 1) '((1 1) (2 2))))
   (expect-list-of-octets-cases
    ((split empty (octets #xe2 #x98 #x83)) '(nil (226) (152) (131) nil))
    ((split-terminator empty (octets #xe2 #x98 #x83)) '(nil (226) (152) (131)))
    ((split-inclusive (compile-byte-regex ",") (octets 65 44 66 44))
     '((65 44) (66 44))))
   (expect-octets-cases
    ((replace-all empty text (octets 45)) '(45 65 45 66 45)))))

(it
 "suppresses empty matches adjacent to preceding non-empty matches"
 (let ((star (compile-regex "a*")))
   (expect-match-spans-cases
    ((all-matches star "aba") '((0 1) (2 3)))
    ((all-matches star "ba") '((0 0) (1 2))))
   (let ((visited nil))
     (do-matches
      (result star "aba")
      (push (list (match-start result) (match-end result)) visited))
     (expect (nreverse visited) :to-equal '((0 1) (2 3))))
   (expect-equal-cases
    ((split star "aba") '("" "b" ""))
    ((replace-all star "aba" "-") "-b-")))
 (expect-match-spans-cases
  ((all-matches (compile-regex "a*|b") "ab") '((0 1) (2 2))))
 (let ((star (compile-byte-regex "A*"))
       (text (octets 65 66 65)))
   (expect-match-spans-cases
    ((all-matches star text) '((0 1) (2 3))))
   (expect-list-of-octets-cases
    ((split star text) '(nil (66) nil)))
   (expect-octets-cases
    ((replace-all star text (octets 45)) '(45 66 45)))))

(it
 "enumerates overlapping matches at every candidate start"
 (let ((regex (compile-regex "aa")))
   (expect-match-spans-cases
    ((all-matches-overlapping regex "aaaa") '((0 2) (1 3) (2 4)))
    ((all-matches-overlapping regex "aaaa" :start 1 :end 4) '((1 3) (2 4))))
   (let ((visited nil))
     (do-matches-overlapping
      (result regex "aaaa")
      (push (list (match-start result) (match-end result)) visited))
     (expect (nreverse visited) :to-equal '((0 2) (1 3) (2 4)))))
 (expect-match-spans-cases
  ((all-matches-overlapping (compile-regex "") "ab") '((0 0) (1 1) (2 2))))
 (let ((captures nil)
       (buffers nil))
   (do-captures-overlapping
    (locations (compile-regex "(aa)") "aaa")
    (push
     (list
      (capture-location-start locations 0)
      (capture-location-end locations 0)
      (capture-location-start locations 1)
      (capture-location-end locations 1))
     captures)
    (push locations buffers))
   (expect (nreverse captures) :to-equal '((0 2 0 2) (1 3 1 3)))
   (expect (apply #'eq buffers) :to-be-truthy))
 (let ((regex (compile-byte-regex "AA"))
       (text (octets 65 65 65)))
   (expect-match-spans-cases
    ((all-matches-overlapping regex text) '((0 2) (1 3)))
    ((all-matches-overlapping (compile-byte-regex "") text)
     '((0 0) (1 1) (2 2) (3 3))))))

(it
 "iterates captures with one reusable offset buffer"
 (let ((regex (compile-regex "(?<word>[a-z]+)-(?<number>[0-9]+)")))
   (let ((captures nil)
         (buffers nil))
     (do-captures
      (locations regex "a-1 bb-22")
      (push
       (list
        (capture-location-start locations 0)
        (capture-location-end locations 0)
        (capture-location-start locations 1)
        (capture-location-end locations 1)
        (capture-location-start locations 2)
        (capture-location-end locations 2))
       captures)
      (push locations buffers))
     (expect (nreverse captures) :to-equal '((0 3 0 1 2 3) (4 9 4 6 7 9)))
     (expect (apply #'eq buffers) :to-be-truthy)))
 (let ((spans nil))
   (do-captures
    (locations (compile-regex "a*") "aba")
    (push
     (list
      (capture-location-start locations 0)
      (capture-location-end locations 0))
     spans))
   (expect (nreverse spans) :to-equal '((0 1) (2 3))))
 (let ((optional-capture-offsets nil))
   (do-captures
    (locations (compile-regex "(a)?b") "ab b")
    (push
     (list
      (capture-location-start locations 1)
      (capture-location-end locations 1))
     optional-capture-offsets))
   (expect (nreverse optional-capture-offsets) :to-equal '((0 1) (nil nil)))))

(it-replace-cases
 "expands Rust-style replacement templates consistently for strings"
 #'compile-regex
 "(?<word>a)"
 (replace-all "a" "$$${word}-$missing-${2}-$" "$a---$")
 (replace-all "a" "$" "$")
 (replace-all "a" "$wordx" "")
 (replace-all "a" "${}" "")
 (replace-all "a" "${word" "${word")
 (replace-all "a" "$-" "$-"))

(it
 "rejects invalid string replacement values"
 (let ((regex (compile-regex "(?<word>a)")))
   (signals type-error (replace-all regex "a" 42))
   (signals
    type-error
    (replace-all
     regex
     "a"
     (lambda (result source)
       (declare (ignore result source))
       42)))))

(it-replace-cases
 "resolves edge-case named replacement templates for strings"
 #'compile-regex
 "(?<a.b>x)"
 (replace-all "x" "$a.b" ".b")
 (replace-all "x" "${a.b}" "x"))

(it-replace-cases
 "keeps unicode replacement names literal unless braced"
 #'compile-regex
 "(?<Δ>x)"
 (replace-all "x" "$Δ" "$Δ")
 (replace-all "x" "${Δ}" "x"))

(it-byte-replace-cases
 "expands Rust-style replacement templates consistently for octet vectors"
 #'compile-byte-regex
 "(?<word>A)"
 (replace-all
  (octets 65)
  (octets
   #x24
   #x24
   #x24
   #x7b
   #x77
   #x6f
   #x72
   #x64
   #x7d
   #x2d
   #x24
   #x6d
   #x69
   #x73
   #x73
   #x69
   #x6e
   #x67)
  '(36 65 45))
 (replace-all (octets 65) (octets #x24 #x7b #x77 #x6f #x72 #x64)
              '(36 123 119 111 114 100))
 (replace-all (octets 65) (octets #x24 #x2d) '(36 45))
 (replace-all (octets 65) (octets #x24) '(36))
 (replace-all (octets 65) (octets #x24 #x7b #x7d) nil)
 (replace-all (octets 65) (octets #x24 #x7b #xff #x7d) nil)
 (replace-all (octets 65) (octets #x24 #xff) '(36 255)))

(it
 "rejects invalid octet replacement values"
 (let ((regex (compile-byte-regex "(?<word>A)")))
   (signals type-error (replace-all regex (octets 65) "not octets"))
   (signals
    type-error
    (replace-all
     regex
     (octets 65)
     (lambda (result source)
       (declare (ignore result source))
       "not octets")))))

(it-byte-replace-cases
 "resolves edge-case named replacement templates for octet vectors"
 #'compile-byte-regex
 "(?<a.b>x)"
 (replace-all (octets #x78) (octets #x24 #x61 #x2e #x62) '(46 98))
 (replace-all (octets #x78)
              (octets #x24 #x7b #x61 #x2e #x62 #x7d)
              '(120)))

(it
 "exports one canonical search API for regular and fuzzy matching"
 (expect (find-symbol "REGEX-SEARCH" '#:cl-regex-kit) :to-be nil)
 (expect (find-symbol "REGEX-SEARCH-AT" '#:cl-regex-kit) :to-be nil)
 (expect (find-symbol "FUZZY-SEARCH" '#:cl-regex-kit) :to-be nil)
 (expect (find-symbol "FUZZY-SEARCH-AT" '#:cl-regex-kit) :to-be nil))

(it
 "configures compilation through builder-style keyword options"
 (expect-match-string-cases
  ((scan (compile-regex "cat" :case-insensitive t) "--CAT--") "--CAT--" "CAT")
  ((scan (compile-regex "^cat$" :multi-line t) (format nil "dog~%cat~%bird"))
   (format nil "dog~%cat~%bird")
   "cat")
  ((scan (compile-regex "a.b" :dot-matches-new-line t) (format nil "a~%b"))
   (format nil "a~%b")
   (format nil "a~%b"))
  ((scan (compile-regex "a+" :swap-greed t) "aaa") "aaa" "a")
  ((scan (compile-regex "a b" :ignore-whitespace t) "ab") "ab" "ab")
  ((scan (compile-regex "\\w+" :unicode nil) "éclair") "éclair" "clair"))
 (expect-truthy-cases
  (scan (compile-regex "(?i)k") "K")
  (scan (compile-regex "(?i)[k]") "K")
  (scan (compile-regex "." :crlf t :line-terminator #\;) ";")
  (scan (compile-regex "a" :size-limit 4) "a"))
 (expect-falsy-cases
  (scan (compile-regex "(?i-u)k") "K")
  (scan (compile-regex "(?i-u)[k]") "K")
  (scan (compile-regex "(?i-u)[K]") "k")
  (scan (compile-regex "k" :case-insensitive t :unicode nil) "K")
  (scan (compile-regex "[k]" :case-insensitive t :unicode nil) "K")
  (scan (compile-regex "." :crlf t) (string #\Return))
  (scan (compile-regex "." :line-terminator #\;) ";")
  (scan (compile-regex "." :crlf t :line-terminator #\;) (string #\Return)))
 (expect
  (match-string
   (scan
    (compile-regex "^right$" :multi-line t :line-terminator #\;)
    "left;right;tail")
   "left;right;tail")
  :to-equal
  "right")
 (let ((crlf-text
        (format
         nil
         "left~C~Cright~C~Ctail"
         #\Return
         #\Newline
         #\Return
         #\Newline)))
   (expect
    (match-string
     (scan
      (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
      crlf-text)
     crlf-text)
    :to-equal
    "right"))
 (expect-falsy-cases
  (scan
   (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
   "left;right;tail"))
 (expect-signals-cases
     regex-syntax-error
   (compile-regex "a" :size-limit 3)
   (compile-regex "a{20}" :size-limit 5)
   (compile-regex-set '("a" "b") :size-limit 9)
   (compile-regex "((a))" :nest-limit 1))
 (expect-signals-cases
     type-error
   (compile-regex "a" :size-limit 0)
   (compile-regex "a" :nest-limit -1)
   (compile-regex "a" :line-terminator "newline")))

(it-replace-cases
 "limits replacements with replace-n"
 #'compile-regex
 "a"
 (replace-n "aaaa" "b" "bbaa" 2)
 (replace-n "aaaa" "b" "aaaa" 0))

(it
 "validates replace-n limits and replacement callback behavior"
 (let ((regex (compile-regex "a")))
   (expect-signals-cases
    type-error
    (replace-n regex "aaaa" "b" -1)
    (replace-n regex "aaaa" "b" "2")
    (replace-n regex "aaaa" nil 0))
   (signals error (replace-n regex "a" "b" 0 :start 2))
   (let ((calls 0))
     (expect-equal-cases
      ((replace-n
        regex
        "aaaa"
        (lambda (result source)
          (declare (ignore result source))
          (incf calls)
          "b")
        2)
       "bbaa")
      (calls 2)))))

(it-byte-replace-cases
 "limits octet-vector replacements with replace-n"
 #'compile-byte-regex
 "A"
 (replace-n (octets 65 66 65) (octets 0) '(0 66 65) 1))

(it-property
 "escaped generated literals retain exact full-match semantics"
 ((text
   (gen-string
    :min-length
    0
    :max-length
    64
    :alphabet
    "aAbB09 .+*?()|[]{}^$\\\\#_-\t\n")))
 (expect
  (cl-regex-kit:full-match-p (compile-regex (escape text)) text)
  :to-be-truthy))

(it-property
 "fixed repetition accepts its generated exact length"
 ((count (gen-integer :min 0 :max 64)))
 (let ((text (make-string count :initial-element #\a)))
   (expect
    (cl-regex-kit:full-match-p (compile-regex (format nil "a{~D}" count)) text)
    :to-be-truthy)))

(it-property
 "streaming and collected non-overlapping matches agree"
 ((text (gen-string :min-length 0 :max-length 48 :alphabet "ab"))
  (requested-start (gen-integer :min 0 :max 48)))
 (let* ((regex (compile-regex "a*"))
        (start (min requested-start (length text)))
        (streamed nil)
        (collected (all-matches regex text :start start)))
   (do-matches
    (result regex text :start start)
    (push (list (match-start result) (match-end result)) streamed))
   (expect
    (nreverse streamed)
    :to-equal
    (mapcar
     (lambda (result)
       (list (match-start result) (match-end result)))
     collected))))

(it-fuzz
 "arbitrary bounded patterns either compile or report a syntax error"
 ((pattern
   (gen-string
    :min-length
    0
    :max-length
    80
    :alphabet
    "aAzZ09()[]{}?*+|\\\\.^$:<>,#_- \t\n")))
 (:trials 250 :timeout-per-trial 1)
 (handler-case (compile-regex pattern)
   (regex-syntax-error ()
     nil)))

(it
 "exposes the public condition hierarchy and diagnostic readers"
 (let ((without-position
        (make-condition
         (quote regex-syntax-error)
         :pattern
         "(abc"
         :reason
         "unclosed group"))
       (with-position
        (make-condition
         (quote regex-syntax-error)
         :pattern
         "[z-a]"
         :position
         2
         :reason
         "invalid range"))
       (timeout (make-condition (quote regex-timeout) :seconds 0.125)))
   (expect (typep without-position (quote cl-regex-kit-error)) :to-be-truthy)
   (expect (typep timeout (quote cl-regex-kit-error)) :to-be-truthy)
   (expect (regex-syntax-error-pattern without-position) :to-equal "(abc")
   (expect (regex-syntax-error-position without-position) :to-be nil)
   (expect
    (regex-syntax-error-reason without-position)
    :to-equal
    "unclosed group")
   (expect (regex-syntax-error-pattern with-position) :to-equal "[z-a]")
   (expect (regex-syntax-error-position with-position) :to-equal 2)
   (expect (regex-syntax-error-reason with-position) :to-equal "invalid range")
   (expect (regex-timeout-seconds timeout) :to-equal 0.125))
 (signals
  cl-regex-kit-error
  (error (quote regex-syntax-error) :pattern "(" :reason "unclosed group"))
 (signals cl-regex-kit-error (error (quote regex-timeout) :seconds 1)))
