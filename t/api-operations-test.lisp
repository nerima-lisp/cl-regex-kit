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
    (signals regex-timeout
      (scan regex non-matches :timeout 0.0001))
    (signals regex-timeout
      (all-matches regex (make-string 1000000 :initial-element #\a) :timeout 0.0001))
    (signals regex-timeout
      (regex-set-matches regex-set non-matches :timeout 0.0001))
    (signals regex-timeout
      (regex-set-matches-into
       regex-set
       (make-array (regex-set-count regex-set)
                   :element-type 'bit
                   :initial-element 0)
       non-matches
       :timeout 0.0001))
    (dolist (operation
             (list (lambda () (scan regex "a" :timeout 0))
                   (lambda () (all-matches regex "a" :timeout -1))
                   (lambda () (split regex "a" :timeout "one second"))))
        (signals type-error (funcall operation)))))

(it
  "renders public regex diagnostics with actionable context"
  (expect (princ-to-string
           (make-condition 'regex-syntax-error
                           :pattern "(abc"
                           :reason "unclosed group"))
          :to-equal
          "Invalid regular expression \"(abc\": unclosed group")
  (expect (princ-to-string
           (make-condition 'regex-syntax-error
                           :pattern "[z-a]"
                           :position 2
                           :reason "invalid range"))
          :to-equal
          "Invalid regular expression \"[z-a]\" at position 2: invalid range")
  (expect (princ-to-string (make-condition 'regex-timeout :seconds 0.125))
          :to-equal
          "Regular expression matching exceeded 0.125 seconds"))

(it
  "validates public matching inputs before executing the VM"
  (let ((regex (compile-regex "a"))
        (regex-set (compile-regex-set '("a"))))
    (dolist (operation
             (list (lambda () (scan nil "a"))
                   (lambda () (scan regex nil))
                   (lambda () (scan regex "a" :start -1))
                   (lambda () (scan regex "a" :start "zero"))
                   (lambda () (cl-regex-kit:full-match-p nil "a"))
                   (lambda () (all-matches regex nil))
                   (lambda () (split regex nil))
                   (lambda () (replace-first regex nil "b"))
                   (lambda () (replace-all regex nil "b"))
                   (lambda () (regex-set-matches regex-set nil))
                   (lambda () (regex-set-matches regex-set "a" :start 2))
                   (lambda () (regex-set-matches nil "a"))))
      (signals type-error (funcall operation)))
      (let ((result (scan regex "a")))
        (dolist (operation
               (list (lambda () (match-start nil))
                     (lambda () (match-end nil))
                     (lambda () (match-string result nil))
                     (lambda () (match-captures result nil))
                     (lambda () (match-group-string result 0 nil))
                     (lambda () (match-group-start result -1))
                     (lambda () (match-group-end result 1))
                     (lambda () (match-group-string result "missing" "a"))
                     (lambda () (capture-location-start
                                 (regex-capture-locations regex) "zero"))))
          (signals type-error (funcall operation))))))

(it
  "constrains matching to an explicit end without changing input context"
  (let ((b (compile-regex "b"))
        (plus (compile-regex "b+"))
        (end-anchor (compile-regex "b$"))
        (set (compile-regex-set '("b" "z"))))
    (expect (scan b "abc" :end 1) :to-be nil)
    (expect (match-start (scan b "abc" :end 2)) :to-equal 1)
    (expect (cl-regex-kit:full-match-p plus "abbbz" :start 1 :end 4)
            :to-be-truthy)
    (expect (cl-regex-kit:full-match-p plus "abz" :start 1 :end 3)
            :to-be nil)
    (expect (longest-match plus "abbbz" :start 1 :end 1) :to-be nil)
    (expect (match-end (longest-match plus "abbbz" :start 1 :end 3)) :to-equal 3)
    (expect (scan end-anchor "abz" :start 1 :end 2) :to-be nil)
    (expect (mapcar (lambda (result) (list (match-start result) (match-end result)))
                    (all-matches (compile-regex ".") "abcd" :start 1 :end 3))
            :to-equal '((1 2) (2 3)))
    (expect (regex-set-matches set "abz" :start 1 :end 2) :to-equal '(0))
    (expect (regex-set-match-p set "abz" :start 1 :end 2) :to-be-truthy)
    (expect (split (compile-regex ",") "a,b,c" :end 2) :to-equal '("a" "b,c"))
    (expect (replace-all b "abzb" "X" :end 2) :to-equal "aXzb")
    (dolist (operation
             (list (lambda () (scan b "a" :end -1))
                   (lambda () (scan b "a" :end 2))
                   (lambda () (scan b "a" :start 1 :end 0))
                   (lambda () (scan b "a" :end "one"))
                   (lambda () (cl-regex-kit:full-match-p b "a" :end -1))))
      (signals type-error (funcall operation))))
  (let ((result (full-match (compile-regex "(a|ab)") "ab")))
    (expect result :to-be-truthy)
    (expect (match-string result "ab") :to-equal "ab")
    (expect (match-group-string result 1 "ab") :to-equal "ab"))
  (expect (cl-regex-kit:full-match-p (compile-regex "a|ab") "ab") :to-be-truthy)
  (let ((result (full-match (compile-regex "a+?") "aa")))
    (expect result :to-be-truthy)
    (expect (match-end result) :to-equal 2))
  (expect (full-match (compile-regex "a|ab") "zab") :to-be nil)
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((dot (compile-byte-regex "."))
          (snowman (octets #xe2 #x98 #x83)))
      (expect (scan dot snowman :end 2) :to-be nil)
      (expect (match-end (scan dot snowman :end 3)) :to-equal 3))))

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
  (dolist (compiler (list #'compile-regex-set #'compile-byte-regex-set))
    (signals error (funcall compiler '() :unknown-option t))
    (signals type-error (funcall compiler '() :case-insensitive :enabled))
    (signals type-error (funcall compiler '() :literal :enabled))
    (signals type-error (funcall compiler '() :nest-limit -1))
    (signals type-error (funcall compiler '() :size-limit 0))
    (signals type-error (funcall compiler '() :line-terminator "newline"))
    (signals type-error
             (funcall compiler '() :line-terminator (code-char #x80)))))

(it
  "validates compiler options consistently for regexes"
  (dolist (compiler (list #'compile-regex #'compile-byte-regex))
    (signals type-error (funcall compiler "a" :case-insensitive :enabled))
    (signals type-error (funcall compiler "a" :literal :enabled))
    (signals type-error (funcall compiler "a" :nest-limit -1))
    (signals type-error (funcall compiler "a" :size-limit 0))
    (signals type-error (funcall compiler "a" :line-terminator "newline"))
    (signals type-error
             (funcall compiler "a" :line-terminator (code-char #x80)))))

(it
  "finds the shortest match at the leftmost start position"
  (expect (shortest-match (compile-regex "a+") "aaa") :to-equal 1)
  (expect (shortest-match (compile-regex "ba+") "xbaaab") :to-equal 3)
  (expect (shortest-match (compile-regex "a*") "aaa") :to-equal 0)
  (expect (shortest-match (compile-regex "a+") "baaa" :start 2) :to-equal 3)
  (expect (shortest-match-at (compile-regex "a+") "baaa" 2 :end 4)
          :to-equal 3)
  (expect (shortest-match-at (compile-regex "a+") "baaa" 4) :to-be nil)
  (signals type-error (shortest-match-at (compile-regex "a+") "baaa" -1))
  (expect (shortest-match (compile-regex "z+") "aaa") :to-be nil)
  (let ((text (make-array 3
                          :element-type '(unsigned-byte 8)
                          :initial-contents '(65 65 65))))
    (expect (shortest-match (compile-byte-regex "A+") text) :to-equal 1)
    (expect (shortest-match-at (compile-byte-regex "A+") text 1) :to-equal 2)))

(it
  "splits with Rust-compatible split operations"
  (let ((comma (compile-regex ",")))
    (expect (split comma "one,two,") :to-equal '("one" "two" ""))
    (expect (split-terminator comma "one,two,") :to-equal '("one" "two"))
    (expect (split-terminator comma "one,two") :to-equal '("one" "two"))
    (expect (split-terminator comma "") :to-equal '(""))
    (expect (split-inclusive comma "one,two,") :to-equal '("one," "two,"))
    (expect (split-inclusive comma "one,two") :to-equal '("one," "two"))
    (expect (split-inclusive comma "") :to-equal '(""))
    (expect (split-inclusive comma "one,two,three" :start 4 :end 8)
            :to-equal '("one,two," "three"))
    (expect (split-n comma "one,two,three" 2) :to-equal '("one" "two,three"))
    (expect (split-n comma "one,two,three" 1) :to-equal '("one,two,three"))
    (expect (split-n comma "one,two,three" 0) :to-equal nil)
    (expect (split-n comma "one,two,three" 2 :start 4) :to-equal '("one,two" "three"))
    (signals type-error (split-n comma "one,two" -1))
    (signals type-error (split-n comma "one,two" 1.5))
    (dolist (operation
             (list (lambda () (split-n nil "one,two" 0))
                   (lambda () (split-n comma nil 0))
                   (lambda () (split-n comma "one,two" 0 :start -1))
                   (lambda () (split-n comma "one,two" 0 :timeout 0))
                   (lambda () (split-terminator nil "one,two"))
                   (lambda () (split-terminator comma nil))
                   (lambda () (split-inclusive nil "one,two"))
                   (lambda () (split-inclusive comma nil))
                   (lambda () (split-inclusive comma "one,two" :start -1))
                   (lambda () (split-inclusive comma "one,two" :end 8))
                   (lambda () (split-inclusive comma "one,two" :start 2 :end 1))
                   (lambda () (split-inclusive comma "one,two" :timeout 0))))
      (signals type-error (funcall operation)))))

(it
  "validates replacement shapes when no replacement is performed"
  (let ((string-regex (compile-regex "x"))
        (byte-regex (compile-byte-regex "x"))
        (octets (make-array 1 :element-type '(unsigned-byte 8)
                              :initial-contents '(97))))
    (dolist (operation
             (list (lambda () (replace-first string-regex "a" 42))
                   (lambda () (replace-n string-regex "a" 42 0))
                   (lambda () (replace-first byte-regex octets "not octets"))
                   (lambda () (replace-n byte-regex octets "not octets" 0))))
      (signals type-error (funcall operation)))))

(it
  "handles zero-length matches once at each input position"
  (let ((empty (compile-regex "")))
    (expect (mapcar (lambda (result)
                      (list (match-start result) (match-end result)))
                    (all-matches empty "ab"))
            :to-equal '((0 0) (1 1) (2 2)))
    (expect (mapcar (lambda (result)
                      (list (match-start result) (match-end result)))
                    (all-matches empty "ab" :start 1))
            :to-equal '((1 1) (2 2)))
    (expect (split empty "ab") :to-equal '("" "a" "b" ""))
    (expect (split-terminator empty "ab") :to-equal '("" "a" "b"))
    (expect (split-inclusive empty "ab") :to-equal '("" "a" "b"))
    (expect (replace-first empty "ab" "-") :to-equal "-ab")
    (expect (replace-n empty "ab" "-" 2) :to-equal "-a-b")
    (expect (replace-all empty "ab" "-") :to-equal "-a-b-"))
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((empty (compile-byte-regex ""))
          (text (octets 65 66)))
      (expect (mapcar (lambda (result)
                        (list (match-start result) (match-end result)))
                      (all-matches empty text))
              :to-equal '((0 0) (1 1) (2 2)))
      (expect (mapcar (lambda (result)
                        (list (match-start result) (match-end result)))
                      (all-matches empty text :start 1))
              :to-equal '((1 1) (2 2)))
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split empty (octets #xe2 #x98 #x83)))
              :to-equal '(nil (226) (152) (131) nil))
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split-terminator empty (octets #xe2 #x98 #x83)))
              :to-equal '(nil (226) (152) (131)))
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split-inclusive (compile-byte-regex ",")
                                       (octets 65 44 66 44)))
              :to-equal '((65 44) (66 44)))
      (expect (coerce (replace-all empty text (octets 45)) 'list)
              :to-equal '(45 65 45 66 45)))))

(it
  "suppresses empty matches adjacent to preceding non-empty matches"
  (flet ((spans (regex text)
           (mapcar (lambda (result)
                     (list (match-start result) (match-end result)))
                   (all-matches regex text))))
    (let ((star (compile-regex "a*")))
      (expect (spans star "aba") :to-equal '((0 1) (2 3)))
      (expect (spans star "ba") :to-equal '((0 0) (1 2)))
      (let ((visited nil))
        (do-matches (result star "aba")
          (push (list (match-start result) (match-end result)) visited))
        (expect (nreverse visited) :to-equal '((0 1) (2 3))))
      (expect (split star "aba") :to-equal '("" "b" ""))
      (expect (replace-all star "aba" "-") :to-equal "-b-"))
    (expect (spans (compile-regex "a*|b") "ab")
            :to-equal '((0 1) (2 2))))
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((star (compile-byte-regex "A*"))
          (text (octets 65 66 65)))
      (expect (mapcar (lambda (result)
                        (list (match-start result) (match-end result)))
                      (all-matches star text))
              :to-equal '((0 1) (2 3)))
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split star text))
              :to-equal '(nil (66) nil))
      (expect (coerce (replace-all star text (octets 45)) 'list)
              :to-equal '(45 66 45)))))

(it
  "iterates captures with one reusable offset buffer"
  (let ((regex (compile-regex "(?<word>[a-z]+)-(?<number>[0-9]+)")))
    (let ((captures nil)
          (buffers nil))
      (do-captures (locations regex "a-1 bb-22")
        (push (list (capture-location-start locations 0)
                    (capture-location-end locations 0)
                    (capture-location-start locations 1)
                    (capture-location-end locations 1)
                    (capture-location-start locations 2)
                    (capture-location-end locations 2))
              captures)
        (push locations buffers))
      (expect (nreverse captures)
              :to-equal '((0 3 0 1 2 3) (4 9 4 6 7 9)))
      (expect (apply #'eq buffers) :to-be-truthy)))
  (let ((spans nil))
    (do-captures (locations (compile-regex "a*") "aba")
      (push (list (capture-location-start locations 0)
                  (capture-location-end locations 0))
            spans))
    (expect (nreverse spans) :to-equal '((0 1) (2 3))))
  (let ((optional-capture-offsets nil))
    (do-captures (locations (compile-regex "(a)?b") "ab b")
      (push (list (capture-location-start locations 1)
                  (capture-location-end locations 1))
            optional-capture-offsets))
    (expect (nreverse optional-capture-offsets) :to-equal '((0 1) (nil nil)))))

(it
  "expands Rust-style replacement templates consistently"
  (let ((regex (compile-regex "(?<word>a)")))
    (expect (replace-all regex "a" "$$${word}-$missing-${2}-$")
            :to-equal "$a---$")
    (expect (replace-all regex "a" "$") :to-equal "$")
    (expect (replace-all regex "a" "$wordx") :to-equal "")
    (expect (replace-all regex "a" "${}") :to-equal "")
    (expect (replace-all regex "a" "${word") :to-equal "${word")
    (expect (replace-all regex "a" "$-") :to-equal "$-")
    (signals type-error (replace-all regex "a" 42))
    (signals type-error
      (replace-all regex "a"
                   (lambda (result source)
                     (declare (ignore result source))
                     42))))
  (let ((dotted (compile-regex "(?<a.b>x)"))
        (unicode (compile-regex "(?<Δ>x)")))
    (expect (replace-all dotted "x" "$a.b") :to-equal ".b")
    (expect (replace-all dotted "x" "${a.b}") :to-equal "x")
    (expect (replace-all unicode "x" "$Δ") :to-equal "$Δ")
    (expect (replace-all unicode "x" "${Δ}") :to-equal "x"))
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((regex (compile-byte-regex "(?<word>A)")))
      (expect (coerce (replace-all regex
                                  (octets 65)
                                  (octets #x24 #x24 #x24 #x7b #x77 #x6f #x72
                                          #x64 #x7d #x2d #x24 #x6d #x69 #x73
                                          #x73 #x69 #x6e #x67))
                             'list)
              :to-equal '(36 65 45))
      (expect (coerce (replace-all regex (octets 65) (octets #x24 #x7b #x77 #x6f #x72 #x64))
                      'list)
              :to-equal '(36 123 119 111 114 100))
      (expect (coerce (replace-all regex (octets 65) (octets #x24 #x2d)) 'list)
              :to-equal '(36 45))
      (expect (coerce (replace-all regex (octets 65) (octets #x24)) 'list)
              :to-equal '(36))
      (expect (coerce (replace-all regex (octets 65) (octets #x24 #x7b #x7d)) 'list)
              :to-equal nil)
      (expect (coerce (replace-all regex (octets 65) (octets #x24 #x7b #xff #x7d)) 'list)
              :to-equal nil)
      (expect (coerce (replace-all regex (octets 65) (octets #x24 #xff)) 'list)
              :to-equal '(36 255))
      (signals type-error (replace-all regex (octets 65) "not octets"))
      (signals type-error
        (replace-all regex
                     (octets 65)
                     (lambda (result source)
                       (declare (ignore result source))
                       "not octets"))))
    (let ((dotted (compile-byte-regex "(?<a.b>x)")))
      (expect (coerce (replace-all dotted (octets #x78)
                                  (octets #x24 #x61 #x2e #x62))
                      'list)
              :to-equal '(46 98))
      (expect (coerce (replace-all dotted (octets #x78)
                                  (octets #x24 #x7b #x61 #x2e #x62 #x7d))
                      'list)
              :to-equal '(120)))))

(it
  "rejects malformed regex-set declarations before compilation"
  (signals type-error (compile-regex-set 42))
  (signals type-error (compile-regex-set "a"))
  (signals type-error (compile-byte-regex-set "a"))
  (signals error (macroexpand-1 '(regex-set "a" :case-insensitive)))
  (signals error (macroexpand-1 '(regex-set "a" case-insensitive t)))
  (signals error (macroexpand-1 '(byte-regex-set "a" :case-insensitive)))
  (signals error (macroexpand-1 '(byte-regex-set "a" case-insensitive t)))
  (signals error
    (macroexpand-1 '(byte-regex-set "a" :case-insensitive enabled))))

(it
  "handles empty regex sets as empty match collections"
  (dolist (set (list (compile-regex-set '())
                     (compile-regex-set #())
                     (compile-byte-regex-set '())))
    (expect (regex-set-matches set
                               (if (byte-regex-set-p set)
                                   (make-array 0 :element-type '(unsigned-byte 8))
                                   "text"))
            :to-equal
            '())
    (expect (regex-set-match-p set
                               (if (byte-regex-set-p set)
                                   (make-array 0 :element-type '(unsigned-byte 8))
                                   "text"))
            :to-be nil)))

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
    (signals type-error
      (regex-set-matches-into set #*00 "cat"))
    (signals type-error
      (regex-set-matches-into set #(0 1 2) "cat"))
    (signals type-error
      (regex-set-matches-into set matches nil))
    (signals type-error
      (regex-set-matches-into set matches "cat" :start -1))
    (signals type-error
      (regex-set-matches-into set matches "cat" :end 4))
    (signals type-error
      (regex-set-matches-into set matches "cat" :timeout 0))))
  (it
   "writes byte regex-set results into reusable buffers"
   (flet ((octets (&rest values)
            (make-array (length values)
                        :element-type '(unsigned-byte 8)
                        :initial-contents values)))
     (let* ((set (compile-byte-regex-set '("A" "\\C" "A")))
            (matches (make-array 3 :element-type 'bit :initial-element 0)))
       (expect (regex-set-matches-into set matches (octets 65)) :to-be matches)
       (expect (coerce matches 'list) :to-equal '(1 1 1))
       (expect (regex-set-matches-at set (octets 255 65) 1)
               :to-equal
               '(0 1 2))
       (expect (regex-set-match-at-p set (octets 255 65) 1) :to-be-truthy))))

(it
  "preserves every word-boundary form in merged regex-set execution"
  (dolist (case '(("\\bcat\\b" " cat ")
                  ("\\Bcat\\B" "scatx")
                  ("\\b{start}cat" " cat")
                  ("cat\\b{end}" "cat ")
                  ("\\b{start-half}cat" "!!cat")
                  ("cat\\b{end-half}" "cat!!")))
    (destructuring-bind (pattern text) case
      (expect (regex-set-matches (compile-regex-set (list pattern)) text)
              :to-equal
              '(0)))))

(it
  "configures compilation through builder-style keyword options"
  (expect (match-string (scan (compile-regex "cat" :case-insensitive t) "--CAT--") "--CAT--")
          :to-equal "CAT")
  (expect (scan (compile-regex "(?i)k") "K") :to-be-truthy)
  (expect (scan (compile-regex "(?i-u)k") "K") :to-be nil)
  (expect (scan (compile-regex "(?i)[k]") "K") :to-be-truthy)
  (expect (scan (compile-regex "(?i-u)[k]") "K") :to-be nil)
  (expect (scan (compile-regex "(?i-u)[K]") "k") :to-be nil)
  (expect (scan (compile-regex "k" :case-insensitive t :unicode nil) "K")
          :to-be nil)
  (expect (scan (compile-regex "[k]" :case-insensitive t :unicode nil) "K")
          :to-be nil)
  (expect (match-string (scan (compile-regex "^cat$" :multi-line t)
                               (format nil "dog~%cat~%bird"))
                        (format nil "dog~%cat~%bird"))
          :to-equal "cat")
  (expect (match-string (scan (compile-regex "a.b" :dot-matches-new-line t)
                              (format nil "a~%b"))
                        (format nil "a~%b"))
          :to-equal (format nil "a~%b"))
  (expect (match-string (scan (compile-regex "a+" :swap-greed t) "aaa") "aaa") :to-equal "a")
  (expect (match-string (scan (compile-regex "a b" :ignore-whitespace t) "ab") "ab")
          :to-equal "ab")
  (expect (match-string (scan (compile-regex "\\w+" :unicode nil) "éclair") "éclair")
          :to-equal "clair")
  (expect (scan (compile-regex "." :crlf t) (string #\Return)) :to-be nil)
  (expect (scan (compile-regex "." :line-terminator #\;) ";") :to-be nil)
  (expect (scan (compile-regex "." :crlf t :line-terminator #\;) ";")
          :to-be-truthy)
  (expect (scan (compile-regex "." :crlf t :line-terminator #\;)
                (string #\Return))
          :to-be nil)
  (expect (match-string (scan (compile-regex "^right$" :multi-line t :line-terminator #\;)
                              "left;right;tail")
                        "left;right;tail")
          :to-equal "right")
  (let ((crlf-text (format nil "left~C~Cright~C~Ctail"
                           #\Return #\Newline #\Return #\Newline)))
    (expect (match-string
             (scan (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
                   crlf-text)
             crlf-text)
            :to-equal "right"))
  (expect (scan (compile-regex "^right$" :multi-line t :crlf t :line-terminator #\;)
                "left;right;tail")
          :to-be nil)
  (expect (scan (compile-regex "a" :size-limit 4) "a") :to-be-truthy)
  (signals regex-syntax-error (compile-regex "a" :size-limit 3))
  (signals regex-syntax-error (compile-regex "a{20}" :size-limit 5))
  (signals regex-syntax-error (compile-regex-set '("a" "b") :size-limit 9))
  (signals type-error (compile-regex "a" :size-limit 0))
  (signals regex-syntax-error (compile-regex "((a))" :nest-limit 1))
  (signals type-error (compile-regex "a" :nest-limit -1))
  (signals type-error (compile-regex "a" :line-terminator "newline")))

(progn
(it
  "matches octet vectors through the byte regex API"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let* ((text (octets #xff #x61 #x0a #x62 #x80))
           (regex (compile-byte-regex "\\C(a)\\C"))
           (result (scan regex text)))
      (expect (byte-regex-p regex) :to-be-truthy)
      (expect (match-start result) :to-equal 0)
      (expect (match-end result) :to-equal 3)
      (expect (coerce (match-group-string result 1 text) 'list) :to-equal '(97))
      (expect (scan (compile-byte-regex "(?s:.)") text) :to-be-truthy)
      (expect (scan (compile-byte-regex "\\bA") (octets 255 65)) :to-be-truthy)
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split (compile-byte-regex "\\C") (octets 65 66)))
              :to-equal '(nil nil nil))
      (expect (mapcar (lambda (field) (coerce field 'list))
                      (split-n (compile-byte-regex "\\C") (octets 65 66) 2))
              :to-equal '(nil (66)))
      (expect (coerce (replace-first (compile-byte-regex "(A)")
                                     (octets #xff 65 #x80)
                                     (octets #x24 #x31 #x21))
                      'list)
              :to-equal '(255 65 33 128))
      (expect (coerce (replace-all (compile-byte-regex "A")
                                   (octets 65 66 65)
                                   (octets 0))
                      'list)
              :to-equal '(0 66 0))
      (expect (coerce (replace-first (compile-byte-regex "(?<letter>A)")
                                     (octets 65)
                                     (octets #x24 #x7b #x6c #x65 #x74 #x74
                                             #x65 #x72 #x7d))
                      'list)
              :to-equal '(65))
      (expect (coerce (replace-first (compile-byte-regex "A")
                                     (octets 65)
                                     (lambda (result source)
                                       (declare (ignore result source))
                                       (octets #xff)))
                      'list)
              :to-equal '(255))
      (signals type-error
        (replace-first (compile-byte-regex "A") (octets 65) "A"))
      (signals type-error (scan regex "a"))
      (signals type-error (scan (compile-regex "a") (octets 97)))
       (let* ((unicode-text (octets #xff #xc3 #xa9 #xfe))
              (unicode-letter (scan (compile-byte-regex "\\p{L}") unicode-text))
              (mixed (scan (compile-byte-regex "(?-u:\\xFF)\\p{L}(?-u:\\xFE)")
                           unicode-text)))
         (expect (match-start unicode-letter) :to-equal 1)
         (expect (match-end unicode-letter) :to-equal 3)
         (expect (match-start mixed) :to-equal 0)
         (expect (match-end mixed) :to-equal 4)
         (expect (match-start (scan (compile-byte-regex ".") unicode-text)) :to-equal 1)
         (expect (match-end (scan (compile-byte-regex ".") unicode-text)) :to-equal 3)
         (expect (match-start (scan (compile-byte-regex "(?-u:.)") unicode-text))
                 :to-equal 0)
         (expect (scan (compile-byte-regex "(?u:a)") (octets 97)) :to-be-truthy))
       (labels ((ascii-octets (string)
           (map (quote (vector (unsigned-byte 8))) (quote char-code) string)))
  (signals regex-syntax-error (compile-byte-regex "(?-u:\\p{L})"))
  (signals regex-syntax-error (compile-byte-regex "(?-u:[\\p{L}])"))
  (dolist (case (quote (("(?-u:[a-z&&[^aeiou]]+)" "aeiouxyz" "xyz")
                        ("(?-u:[a-z&&aeiou]+)" "aeiouxyz" "aeiou")
                        ("(?-u:[a-z--aeiou]+)" "aeiouxyz" "xyz")
                        ("(?-u:[a-f~~d-z]+)" "abcdefgh" "abc")
                        ("(?-u:\\d+)" "abc123" "123")
                        ("(?-u:[[:alpha:]]+)" "A1" "A")
                        ("(?-u:(?i:[a-z])+)" "ABC" "ABC")
                        ("(?-u:[^a])" "aB" "B")
                        ("(?-u:\\bcat\\b)" " cat " "cat")
                        ("(?-u:\\Bcat\\B)" "scatx" "cat")
                        ("(?-u:\\b{start}cat)" " cat" "cat")
                        ("(?-u:cat\\b{end})" "cat " "cat")
                        ("(?-u:\\b{start-half}cat)" "!!cat" "cat")
                        ("(?-u:cat\\b{end-half})" "cat!!" "cat"))))
    (destructuring-bind (pattern subject expected) case
      (expect (equalp (ascii-octets expected) (match-string (scan (compile-byte-regex pattern) (ascii-octets subject)) (ascii-octets subject))) :to-be-truthy)))
  (dolist (case (quote (("\\b{start}cat" " cat" "cat")
                        ("cat\\b{end}" "cat " "cat")
                        ("\\b{start-half}cat" "!!cat" "cat")
                        ("cat\\b{end-half}" "cat!!" "cat")
                        ("\\Bcat\\B" "scatx" "cat")
                        ("\\b\\p{L}+\\b" " cafe " "cafe"))))
    (destructuring-bind (pattern subject expected) case
      (expect (equalp (ascii-octets expected) (match-string (scan (compile-byte-regex pattern) (ascii-octets subject)) (ascii-octets subject))) :to-be-truthy)))
  (dolist (case
           (list (list "(?mR)^B" (format nil "A~C~CB" #\Return #\Linefeed) "B")
                 (list "(?mR)A$" (format nil "A~C~CB" #\Return #\Linefeed) "A")
                 (list "(?mR)^B" (format nil "A~CB" #\Return) "B")
                 (list "(?mR)A$" (format nil "A~CB" #\Return) "A")
                 (list "(?mR)^B" (format nil "A~CB" #\Linefeed) "B")
                 (list "(?mR)A$" (format nil "A~CB" #\Linefeed) "A")))
    (destructuring-bind (pattern subject expected) case
      (let ((text (ascii-octets subject)))
        (expect (equalp (ascii-octets expected)
                        (match-string (scan (compile-byte-regex pattern) text) text))
                :to-be-truthy))))
  (expect (scan (compile-byte-regex "(?mR)^\\n")
                (ascii-octets (format nil "~C~C" #\Return #\Linefeed)))
          :to-be nil)))))
(it
  "keeps UTF-8 scalar and Unicode-boundary semantics in byte regexes"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type (quote (unsigned-byte 8))
                       :initial-contents values)))
    (let* ((text (octets #x20 #xc3 #xa9 #xe2 #x82 #xac #xf0 #x9f #x98 #x80 #x20))
           (dot-matches (all-matches (compile-byte-regex ".") text))
           (word (scan (compile-byte-regex "\\b{start}\\p{L}+\\b{end}") text))
           (start-half (scan (compile-byte-regex "\\b{start-half}\\p{L}") text))
           (end-half (scan (compile-byte-regex "\\p{L}\\b{end-half}") text)))
      (expect (mapcar (lambda (result) (list (match-start result) (match-end result)))
                      dot-matches)
              :to-equal (quote ((0 1) (1 3) (3 6) (6 10) (10 11))))
      (expect (match-start word) :to-equal 1)
      (expect (match-end word) :to-equal 3)
      (expect (match-start start-half) :to-equal 1)
      (expect (match-end start-half) :to-equal 3)
      (expect (match-start end-half) :to-equal 1)
      (expect (match-end end-half) :to-equal 3)
      (expect (scan (compile-byte-regex "\\b{start}\\p{L}") text :start 2)
              :to-be nil)
      (dolist (invalid (quote ((#xc0 #x80)
                               (#x80)
                               (#xc2)
                               (#xe0 #x80 #x80)
                               (#xe2 #x82)
                               (#xed #xa0 #x80)
                               (#xf0 #x80 #x80 #x80)
                               (#xf0 #x9f #x98)
                               (#xf4 #x90 #x80 #x80))))
        (expect (scan (compile-byte-regex "\\p{L}") (apply #'octets invalid))
                :to-be nil))
      (let ((scalar (octets #xc3 #xa9)))
        (expect (scan (compile-byte-regex "\\b") scalar :start 1 :end 1)
                :to-be nil)
        (expect (scan (compile-byte-regex "\\B") scalar :start 1 :end 1)
                :to-be nil)))))
)

(it
  "matches octet vectors through the byte regex-set API"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type '(unsigned-byte 8)
                       :initial-contents values)))
    (let ((set (compile-byte-regex-set '("A" "\\C" "Z"))))
      (expect (regex-set-p set) :to-be-truthy)
      (expect (byte-regex-set-p set) :to-be-truthy)
      (expect (byte-regex-set-p nil) :to-be nil)
      (expect (regex-set-matches set (octets #xff 65 #x80)) :to-equal '(0 1))
      (expect (regex-set-match-p set (octets #xff)) :to-be-truthy)
      (signals type-error (regex-set-matches set "A"))
      (signals type-error (regex-set-matches set (octets 65) :start 2))
      (signals type-error (compile-byte-regex-set 42)))
    (expect (regex-set-matches
             (compile-byte-regex-set '("\\bA" "^A$") :multi-line t :crlf t)
             (octets 65))
            :to-equal
            '(0 1))
     (expect (regex-set-matches
              (compile-byte-regex-set '("^B$") :multi-line t :crlf t)
              (octets 65 13 10 66 13 10))
             :to-equal
             '(0))
    (expect (regex-set-matches
             (compile-byte-regex-set '("\\p{L}" "(?-u:\\xFF)"))
             (octets #xff #xc3 #xa9))
            :to-equal
            '(0 1)))
  (let ((set (byte-regex-set "A" "\\C")))
    (expect (regex-set-matches
             set
             (make-array 1
                         :element-type '(unsigned-byte 8)
                         :initial-contents '(65)))
            :to-equal
            '(0 1))))

(it
  "limits replacements with replace-n"
  (let ((regex (compile-regex "a")))
    (expect (replace-n regex "aaaa" "b" 2) :to-equal "bbaa")
    (expect (replace-n regex "aaaa" "b" 0) :to-equal "aaaa")
    (signals type-error (replace-n regex "aaaa" "b" -1))
    (signals type-error (replace-n regex "aaaa" "b" "2"))
    (signals error (replace-n regex "a" "b" 0 :start 2))
    (let ((calls 0))
      (expect (replace-n regex "aaaa"
                         (lambda (result source)
                           (declare (ignore result source))
                           (incf calls)
                           "b")
                         2)
              :to-equal "bbaa")
      (expect calls :to-equal 2))
    (signals type-error (replace-n regex "aaaa" nil 0))))

(it
  "limits octet-vector replacements with replace-n"
  (flet ((octets (&rest values)
           (make-array (length values)
                       :element-type (quote (unsigned-byte 8))
                       :initial-contents values)))
    (expect (coerce (replace-n (compile-byte-regex "A")
                               (octets 65 66 65)
                               (octets 0)
                               1)
                    (quote list))
            :to-equal (quote (0 66 65)))))

(it-property
  "escaped generated literals retain exact full-match semantics"
  ((text (gen-string :min-length 0
                     :max-length 64
                     :alphabet "aAbB09 .+*?()|[]{}^$\\\\#_-\t\n")))
  (expect (cl-regex-kit:full-match-p (compile-regex (escape text)) text)
          :to-be-truthy))

(it-property
  "fixed repetition accepts its generated exact length"
  ((count (gen-integer :min 0 :max 64)))
  (let ((text (make-string count :initial-element #\a)))
    (expect (cl-regex-kit:full-match-p
             (compile-regex (format nil "a{~D}" count))
             text)
            :to-be-truthy)))

(it-property
  "merged regex sets agree with their member scans for generated input"
  ((text (gen-string :min-length 0 :max-length 48 :alphabet "ab\n"))
   (requested-start (gen-integer :min 0 :max 48)))
  (let* ((patterns '("a+" "b+" "ab" "ba"))
         (start (min requested-start (length text)))
         (expected (loop for pattern in patterns
                         for index from 0
                         when (scan (compile-regex pattern) text :start start)
                           collect index)))
    (expect (regex-set-matches (compile-regex-set patterns) text :start start)
            :to-equal
            expected)))

(it-property
  "streaming and collected non-overlapping matches agree"
  ((text (gen-string :min-length 0 :max-length 48 :alphabet "ab"))
   (requested-start (gen-integer :min 0 :max 48)))
  (let* ((regex (compile-regex "a*"))
         (start (min requested-start (length text)))
         (streamed nil)
         (collected (all-matches regex text :start start)))
    (do-matches (result regex text :start start)
      (push (list (match-start result) (match-end result)) streamed))
    (expect (nreverse streamed)
            :to-equal
            (mapcar (lambda (result)
                      (list (match-start result) (match-end result)))
                    collected))))

(it-fuzz
  "arbitrary bounded patterns either compile or report a syntax error"
  ((pattern (gen-string :min-length 0
                        :max-length 80
                        :alphabet "aAzZ09()[]{}?*+|\\\\.^$:<>,#_- \t\n")))
  (:trials 250 :timeout-per-trial 1)
  (handler-case
      (compile-regex pattern)
    (regex-syntax-error () nil)))
