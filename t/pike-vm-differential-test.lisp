;;;; t/pike-vm-differential-test.lisp
;;;;
;;;; Regressions for the Pike VM's leftmost-first thread cutting, its
;;;; per-input allocation, and the byte-mode Unicode word boundary's cost, plus
;;;; differential properties that compare every regular matcher path against
;;;; RUN-ADVANCED-REGEX. The ordered-backtracking executor evaluates the AST
;;;; directly and shares no code with the NFA, the Pike VM, the lazy DFA, or
;;;; the literal prefilter, which is what makes it a usable oracle here.
(in-package #:cl-regex-kit/test)

(defun backtracking-oracle (regex text &key (start 0) end shortest-p longest-p)
  (cl-regex-kit::run-advanced-regex regex text
                                    :start start :end end
                                    :shortest-p shortest-p :longest-p longest-p
                                    :never-newline-p (regex-never-newline-p regex)))

(defun match-signature (result)
  "Return RESULT's whole-match span and capture spans as a comparable list."
  (and result
       (list (match-start result) (match-end result)
             (coerce (cl-regex-kit::match-result-groups result) 'list))))

(defun span-signature (result)
  (and result (list (match-start result) (match-end result))))

;;; -- Regression: an optional group ahead of a required literal -----------

(it
  "finds a lower-priority match while a higher-priority thread is still alive"
  (dolist (pattern '("(?:[^x]*r)?P" "(?:[^x]*r)?(P)" "(?:.*r)?P" "(?:a|[^x]*r)?P"))
    (let ((regex (compile-regex pattern))
          (bytes (compile-byte-regex pattern))
          (raw (compile-byte-regex pattern :unicode nil)))
      (expect (span-signature (scan regex "P x")) :to-equal '(0 1))
      (expect (span-signature (scan bytes (ascii-octets "P x"))) :to-equal '(0 1))
      (expect (span-signature (scan raw (ascii-octets "P x"))) :to-equal '(0 1))
      (expect (is-match-p regex "P x") :to-be-truthy)
      (expect (length (all-matches regex "P x P")) :to-equal 2)))
  (let ((result (scan (compile-regex "(?:([^x]*)r)?(P)") "P x")))
    (expect (match-group-string result 1 "P x") :to-be-null)
    (expect (match-group-string result 2 "P x") :to-equal "P"))
  (expect (match-string (scan (compile-regex "(?:[^x]*r)?P") "arP x") "arP x")
          :to-equal "arP"))

(it
  "keeps a later higher-priority match over an earlier lower-priority one"
  (let ((text "aaaba"))
    (expect (span-signature (scan (compile-regex "(?:a*b)?a") text)) :to-equal '(0 5))
    (expect (span-signature (scan (compile-regex "(?:a*c)?a") text)) :to-equal '(0 1))))

(it
  "keeps thread priority when byte instructions advance by different widths"
  ;; A non-ASCII literal in a byte regex matches its UTF-8 octets one at a
  ;; time, while a Unicode class or dot consumes the whole scalar at once.
  (let ((scalar (string-to-octets "é" :encoding :utf-8))
        (pair (string-to-octets "éa" :encoding :utf-8)))
    (let ((result (scan (compile-byte-regex "é|([^ ])") scalar)))
      (expect (span-signature result) :to-equal '(0 2))
      (expect (match-group-start result 1) :to-be-null))
    (expect (span-signature (scan (compile-byte-regex "(?:é)?(?:a|.)") pair))
            :to-equal '(0 3))))

;;; -- Differential properties against the backtracking oracle -------------

(defparameter *differential-atoms*
  '("a" "b" "r" "P" "x" " " "[^x]" "[a-c]" "[^ ]" "." "é"))

(defparameter *differential-quantifiers*
  '("?" "*" "+" "??" "*?" "+?" "{1,2}" "{0,2}" "{2}"))

(defun gen-regex-tree ()
  "Generate a small regex syntax tree: an atom string, or a list headed by
:SEQ, :ALT, :GROUP, :CAPTURE, or a quantifier string."
  (cl-weave:gen-recursive
   (gen-member *differential-atoms*)
   (lambda (self)
     (cl-weave:gen-one-of
      (cl-weave:gen-tuple (gen-member '(:seq)) self self)
      (cl-weave:gen-tuple (gen-member '(:alt)) self self)
      (cl-weave:gen-tuple (gen-member '(:group :capture)) self)
      (cl-weave:gen-tuple (gen-member *differential-quantifiers*) self)))
   :max-depth 4))

(defun render-regex-tree (tree)
  (if (stringp tree)
      tree
      (destructuring-bind (head &rest children) tree
        (let ((parts (mapcar #'render-regex-tree children)))
          (case head
            (:seq (format nil "~{~A~}" parts))
            (:alt (format nil "(?:~A|~A)" (first parts) (second parts)))
            (:group (format nil "(?:~A)" (first parts)))
            (:capture (format nil "(~A)" (first parts)))
            (otherwise (format nil "(?:~A)~A" (first parts) head)))))))

(defparameter *differential-text-alphabet* "abrPx é")

(defun gen-differential-texts (alphabet)
  "Several texts per generated pattern, so each pattern meets more inputs."
  (gen-list (gen-string :min-length 0 :max-length 10 :alphabet alphabet)
            :min-length 1 :max-length 6))

(defun check-regular-against-oracle (regex text start)
  (let ((oracle (backtracking-oracle regex text :start start)))
    (expect (match-signature (scan regex text :start start))
            :to-equal (match-signature oracle))
    (expect (not (null (is-match-p regex text :start start)))
            :to-equal (not (null oracle)))
    (expect (span-signature (longest-match regex text :start start))
            :to-equal (span-signature (backtracking-oracle regex text :start start
                                                                      :longest-p t)))
    (expect (shortest-match regex text :start start)
            :to-equal (let ((shortest (backtracking-oracle regex text :start start
                                                                     :shortest-p t)))
                        (and shortest (match-end shortest))))))

(it-property
  "string SCAN, IS-MATCH-P, and the longest/shortest variants agree with backtracking"
  ((tree (gen-regex-tree))
   (texts (gen-differential-texts *differential-text-alphabet*))
   (requested-start (gen-integer :min 0 :max 10)))
  (let ((regex (compile-regex (render-regex-tree tree))))
    (expect (regex-advanced-p regex) :to-be nil)
    (dolist (text texts)
      (check-regular-against-oracle regex text 0)
      (check-regular-against-oracle regex text (min requested-start (length text))))))

(it-property
  "byte SCAN, IS-MATCH-P, and the longest/shortest variants agree with backtracking"
  ((tree (gen-regex-tree))
   (texts (gen-differential-texts *differential-text-alphabet*)))
  (let ((regex (compile-byte-regex (render-regex-tree tree))))
    (expect (regex-advanced-p regex) :to-be nil)
    (dolist (text texts)
      (check-regular-against-oracle regex (string-to-octets text :encoding :utf-8) 0))))

(it-property
  "raw byte regexes agree with backtracking and take the lazy DFA path"
  ((tree (gen-such-that (lambda (tree) (not (search "é" (render-regex-tree tree))))
                        (gen-regex-tree)))
   (texts (gen-differential-texts "abrPx ")))
  (let ((regex (compile-byte-regex (render-regex-tree tree) :unicode nil)))
    (expect (cl-regex-kit::regex-lazy-dfa regex) :to-be-truthy)
    (dolist (text texts)
      (check-regular-against-oracle regex (ascii-octets text) 0))))

;;; -- Byte-mode Unicode word boundaries -----------------------------------

(defun whole-text-byte-unicode-word-boundary-p (text position)
  "The pre-2.1.1 definition: decode all of TEXT, then apply UAX #29 at the
scalar index whose offset is POSITION. Valid UTF-8 input only."
  (let ((characters nil)
        (offsets nil)
        (index 0))
    (loop while (< index (length text))
          do (multiple-value-bind (character end) (cl-regex-kit::utf8-character-at text index)
               (push index offsets)
               (push character characters)
               (setf index end)))
    (push (length text) offsets)
    (let ((scalar-index (position position (reverse offsets))))
      (and scalar-index
           (cl-regex-kit::%unicode-word-boundary-p (coerce (reverse characters) 'string)
                                                   scalar-index)))))

(defparameter *word-break-alphabet*
  (coerce (list #\a #\Z #\1 #\: #\' #\" #\. #\, #\_ #\Space #\Newline #\Return
                (code-char #x05D0) (code-char #x30A2) (code-char #x0301)
                (code-char #x200D) (code-char #x200B) (code-char #x1F1E6)
                (code-char #x1F1E7) (code-char #x1F600) (code-char #x00E9))
          'string)
  "Characters that exercise every UAX #29 rule %UNICODE-WORD-BOUNDARY-P
implements: letters, digits, mid-letter and mid-number punctuation, Hebrew,
Katakana, Extend, ZWJ, Format, regional indicators, an emoji, and CR/LF.")

(it-property
  "local byte-mode UAX #29 boundaries equal the whole-text decoding at every offset"
  ((text (gen-string :min-length 0 :max-length 12 :alphabet *word-break-alphabet*)))
  (let ((octets (string-to-octets text :encoding :utf-8)))
    (loop for position from 0 to (length octets)
          do (expect (not (null (cl-regex-kit::byte-unicode-word-boundary-p octets position)))
                     :to-equal
                     (not (null (whole-text-byte-unicode-word-boundary-p octets position)))))))

(it
  "falls back to the binary word test only when the inspected scalars are invalid"
  (let ((text (octets #x61 #x20 #x62 #xff)))
    (expect (cl-regex-kit::byte-unicode-word-boundary-p text 2) :to-be-truthy)
    (expect (cl-regex-kit::byte-unicode-word-boundary-p text 3) :to-be-truthy)
    (expect (cl-regex-kit::byte-unicode-word-boundary-p text 4) :to-be-null))
  (let ((text (octets #x80 #x61)))
    (expect (cl-regex-kit::byte-unicode-word-boundary-p text 0) :to-be-null)))

;;; -- Cost and allocation contracts ----------------------------------------

(defun steady-bytes-consed (thunk)
  "Return the fewest bytes THUNK conses over three runs after a warm-up run.
The minimum discards allocation by unrelated threads, which the process-wide
counter also sees."
  (funcall thunk)
  (loop repeat 3
        minimize (let ((before (sb-ext:get-bytes-consed)))
                   (funcall thunk)
                   (- (sb-ext:get-bytes-consed) before))))

(defun filler-octets (length)
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length octets)
      (setf (aref octets index) (if (zerop (mod index 8)) #x20 #x61)))))

(defparameter *allocation-slack* 65536
  "Bytes a 100x longer input may cons beyond the short one. The 2.1.0 VM
consed roughly 170 bytes per input element, so for the 30000-element inputs
below it exceeds this by about two orders of magnitude.")

(it
  "keeps Pike VM allocation independent of input length"
  (flet ((growth (function short long)
           (- (steady-bytes-consed (lambda () (funcall function long)))
              (steady-bytes-consed (lambda () (funcall function short))))))
    (let* ((digits (compile-regex "[0-9]{6}"))
           (captures (compile-regex "([0-9]{3})-([0-9]{3})"))
           (bounded (compile-regex "\\b[0-9]{6}\\b"))
           (byte-digits (compile-byte-regex "[0-9]{6}"))
           (short (concatenate 'string (make-string 300 :initial-element #\a) "123-456"))
           (long (concatenate 'string (make-string 30000 :initial-element #\a) "123-456")))
      (expect (regex-required-literals digits) :to-be-null)
      (expect (match-string (scan captures long) long) :to-equal "123-456")
      (expect (growth (lambda (text) (scan digits text)) short long)
              :to-be-less-than *allocation-slack*)
      (expect (growth (lambda (text) (scan captures text)) short long)
              :to-be-less-than *allocation-slack*)
      (expect (growth (lambda (text) (is-match-p bounded text)) short long)
              :to-be-less-than *allocation-slack*)
      (expect (growth (lambda (text) (scan byte-digits text))
                      (ascii-octets short) (ascii-octets long))
              :to-be-less-than *allocation-slack*))))

(it
  "bounds byte-mode word-boundary cost by the scanned range, not the buffer"
  (let* ((regex (compile-byte-regex "\\bfoo\\b"))
         (short (filler-octets 1024))
         (long (filler-octets 262144)))
    (replace short (ascii-octets " foo ") :start1 16)
    (replace long (ascii-octets " foo ") :start1 16)
    (expect (span-signature (scan regex long :start 0 :end 64)) :to-equal '(17 20))
    (expect (- (steady-bytes-consed (lambda () (scan regex long :start 0 :end 64)))
               (steady-bytes-consed (lambda () (scan regex short :start 0 :end 64))))
            :to-be-less-than *allocation-slack*))
  ;; A whole-buffer scan tests the boundary at every position, so this also
  ;; pins the boundary test itself to zero steady-state allocation.
  (let ((regex (compile-byte-regex "\\b[0-9]{6}\\b"))
        (short (filler-octets 300))
        (long (filler-octets 30000)))
    (expect (scan regex long) :to-be-null)
    (expect (- (steady-bytes-consed (lambda () (scan regex long)))
               (steady-bytes-consed (lambda () (scan regex short))))
            :to-be-less-than *allocation-slack*)))
