;;;; src/regex-tokenizer-escapes.lisp
;;;;
;;;; Escape-sequence decoding shared by the top-level and character-class
;;;; tokenizer dispatch (regex-tokenizer.lisp): hex/octal/Unicode-scalar
;;;; escapes, \p{...}/\P{...} Unicode properties, and \b{...} word-boundary
;;;; names. Each function here scans PATTERN from POSITION and returns
;;;; (values decoded-value next-position) -- pure lexical decoding, with no
;;;; dependency on live parser flags. Whatever legality a decoded escape has
;;;; under the *current* byte-mode/Unicode-flag state (e.g. whether \p{...}
;;;; is allowed at all) is judged later by the grammar layer, since that
;;;; state can change mid-pattern via inline (?flags) groups and so cannot be
;;;; resolved during this single up-front tokenizing pass.
(in-package #:cl-regex-kit)

(defun tokenizer-fail (pattern position reason)
  (error 'regex-syntax-error :pattern pattern :position position :reason reason))

(defun hex-digit-p (character)
  (and character (digit-char-p character 16)))

(defun octal-digit-p (character)
  (and character (digit-char-p character 8)))

(defun ascii-alphabetic-p (character)
  (and
    character
    (or
      (and (char<= #\A character) (char<= character #\Z))
      (and (char<= #\a character) (char<= character #\z)))))

(defun ascii-alphanumeric-p (character)
  (and character (or (ascii-alphabetic-p character) (digit-char-p character))))

(defun unicode-scalar-character (pattern code position)
  (if (and (unicode-scalar-code-p code)
           (< code char-code-limit)
           (code-char code))
      (code-char code)
    (tokenizer-fail pattern position "Unicode escape is not a Unicode scalar value")))

(defun scan-while (pattern position predicate)
  "Return (values matched-substring next-position) for the maximal run at
POSITION satisfying PREDICATE -- a contiguous scan with no extended-mode
whitespace skipping, since escape-internal content is never verbose-mode
sensitive in this grammar."
  (let ((end position)
        (length (length pattern)))
    (loop while (and (< end length) (funcall predicate (char pattern end)))
          do (incf end))
    (values (subseq pattern position end) end)))

(defun scan-hex-code (pattern position digits)
  (let ((beginning position))
    (multiple-value-bind (hex next) (scan-while pattern position #'hex-digit-p)
      (unless (= (length hex) digits)
        (tokenizer-fail pattern beginning "Expected hexadecimal escape digits"))
      (values
        (unicode-scalar-character pattern (parse-integer hex :radix 16) beginning)
        next))))

(defun scan-octal-code (pattern position)
  "POSITION is just past the first (already-scanned) octal digit."
  (let ((beginning (1- position)))
    (multiple-value-bind (digits next) (scan-while
        pattern
        position
        (lambda (c)
          (and (octal-digit-p c) t)))
      (declare (ignore digits))
      (let* ((end (min next (+ beginning 3)))
             (code (parse-integer pattern :start beginning :end end :radix 8)))
        (unless (<= code #o377)
          (tokenizer-fail pattern beginning "Octal escape is outside the byte range"))
        (values (code-char code) end)))))

(defun scan-escaped-character (pattern position)
  "POSITION is just past the backslash; the next character selects a control
or fixed-width hex escape. Returns (values character raw-octet-p
next-position). Callers only ever reach the #\\x/#\\u/#\\U cases here when
the braced form (`\\x{...}` etc.) has already been ruled out -- see
TOKENIZE-ESCAPE in regex-tokenizer.lisp, which intercepts and tokenizes that
form separately, since only the grammar layer can correctly apply extended-
mode whitespace tolerance inside the braces."
  (let ((escaped (char pattern position))
        (next (1+ position)))
    (case escaped
      (#\a (values (code-char 7) nil next))
      (#\e (values (code-char 27) nil next))
      (#\f (values #\Page nil next))
      (#\n (values #\Newline nil next))
      (#\r (values #\Return nil next))
      (#\t (values #\Tab nil next))
      (#\v (values (code-char 11) nil next))
      (#\x
        (multiple-value-bind (character after) (scan-hex-code pattern next 2)
          (values character t after)))
      (#\u
        (multiple-value-bind (character after) (scan-hex-code pattern next 4)
          (values character nil after)))
      (#\U
        (multiple-value-bind (character after) (scan-hex-code pattern next 8)
          (values character nil after))))))

(defun scan-unicode-property-name (pattern position)
  "POSITION is just past \\p or \\P. Returns (values descriptor negated-p next-position)."
  (labels ((resolve-or-fail (name error-position)
             (or
          (resolve-unicode-property name)
          (tokenizer-fail
            pattern
            error-position
            "Unknown or unsupported Unicode property"))))
    (if (and (< position (length pattern)) (char= (char pattern position) #\{)) (let ((body-start (1+ position)))
        (multiple-value-bind (name next) (scan-while
            pattern
            body-start
            (lambda (character)
              (not (char= character #\}))))
          (when (zerop (length name))
            (tokenizer-fail pattern body-start "Unicode property name cannot be empty"))
          (unless (< next (length pattern))
            (tokenizer-fail pattern body-start "Unclosed Unicode property"))
          (let ((not-equal-position (search "!=" name))
                (negated-p nil))
            (cond
              (not-equal-position
                (setf name (format
                    nil
                    "~A=~A"
                    (subseq name 0 not-equal-position)
                    (subseq name (+ not-equal-position 2)))
                      negated-p t))
              (t
                (let ((separator (or (position #\= name) (position #\: name))))
                  (when separator
                    (setf name (format nil "~A=~A" (subseq name 0 separator) (subseq name (1+ separator))))))))
            (values (resolve-or-fail name body-start) negated-p (1+ next)))))
      (progn
        (unless (< position (length pattern))
          (tokenizer-fail pattern position "Expected Unicode property name"))
        (values
          (resolve-or-fail (string (char pattern position)) position)
          nil
          (1+ position))))))

(defun scan-quoted-literal (pattern position)
  "POSITION is just past \\Q. Returns (values text next-position); an
unterminated \\Q runs to the end of PATTERN, matching RE2/Rust semantics."
  (let ((end position)
        (length (length pattern)))
    (loop while (and
        (< end length)
        (not
          (and
            (char= (char pattern end) #\\)
            (< (1+ end) length)
            (char= (char pattern (1+ end)) #\E))))
          do (incf end))
    (values
      (subseq pattern position end)
      (if (< end length) (+ end 2)
        end))))
(defun scan-named-character (pattern position)
  "POSITION is just past \\N{. Return (values character next-position)."
  (let ((end (position #\} pattern :start position)))
    (unless end
      (tokenizer-fail pattern position "Unclosed named character"))
    (when (= end position)
      (tokenizer-fail pattern position "Unicode character name cannot be empty"))
    (let* ((name (string-upcase
                   (substitute #\_ #\Space (subseq pattern position end))))
           (character
             (or
               (name-char name)
               (cdr
                 (assoc
                   name
                   (list
                     (cons "LINE_FEED" #\Newline)
                     (cons "CARRIAGE_RETURN" #\Return)
                     (cons "CHARACTER_TABULATION" #\Tab)
                     (cons "TAB" #\Tab)
                     (cons "FORM_FEED" #\Page)
                     (cons "BACKSPACE" (code-char 8))
                     (cons "NULL" (code-char 0))
                     (cons "ESCAPE" (code-char 27))
                     (cons "DELETE" (code-char 127))
                     (cons "NEXT_LINE" (code-char #x85)))
                   :test #'string=)))))
      (unless (characterp character)
        (tokenizer-fail pattern position "Unknown Unicode character name"))
      (values character (1+ end)))))
(defun scan-backreference (pattern position kind)
  "POSITION is just past \\g or \\k. Return (values capture-index name next-position relative-index subroutine-p)."
  (let* ((pattern-length (length pattern))
         (opening (and (< position pattern-length)
                       (char pattern position)))
         (closing (case opening
                    (#\< #\>)
                    (#\{ #\})
                    (#\' #\')
                    (otherwise nil)))
         (brace-p (and opening (char= opening #\{)))
         (quote-p (and opening (char= opening #\'))))
    (unless closing
      (tokenizer-fail pattern position
                      "Backreference must use <...>, {...}, or '...'"))
    (let* ((body-start (1+ position))
           (end (position closing pattern :start body-start)))
      (unless end
        (tokenizer-fail pattern body-start "Unclosed backreference or subroutine"))
      (when (= end body-start)
        (tokenizer-fail pattern body-start
                        "Backreference target cannot be empty"))
      (let ((body (subseq pattern body-start end)))
        (cond
          ((every (function digit-char-p) body)
           (unless (char= kind #\g)
             (tokenizer-fail pattern body-start
                             "\\k backreferences require a name"))
           (let ((capture-index (parse-integer body)))
             (when (zerop capture-index)
               (tokenizer-fail pattern body-start
                               "Backreference number must be positive"))
             (values capture-index nil (1+ end) nil
                     (and quote-p (char= kind #\g)))))
          ((and brace-p
                (> (length body) 1)
                (member (char body 0) '(#\+ #\-)
                        :test #'char=)
                (every (function digit-char-p) (subseq body 1)))
           (unless (char= kind #\g)
             (tokenizer-fail pattern body-start
                             "\\k backreferences require a name"))
           (let ((relative-index (parse-integer body)))
             (when (zerop relative-index)
               (tokenizer-fail pattern body-start
                               "Relative backreference number must be non-zero"))
             (values nil nil (1+ end) relative-index nil)))
          ((and brace-p
                (or (char= kind #\g)
                    (char= kind #\k)))
           (unless (capture-name-start-p (char body 0))
             (tokenizer-fail pattern body-start
                             "Backreference name must start with an alphabetic character or underscore"))
           (loop for character across body
                 unless (capture-name-character-p character)
                   do (tokenizer-fail pattern body-start
                                      "Invalid character in backreference name"))
           (values nil body (1+ end) nil nil))
          (brace-p
           (tokenizer-fail pattern body-start
                           "Brace backreferences require a name or number"))
          (t
           (unless (capture-name-start-p (char body 0))
             (tokenizer-fail pattern body-start
                             "Backreference name must start with an alphabetic character or underscore"))
           (loop for character across body
                 unless (capture-name-character-p character)
                   do (tokenizer-fail pattern body-start
                                      "Invalid character in backreference name"))
           (values nil body (1+ end) nil
                   (and quote-p (char= kind #\g)))))))))
