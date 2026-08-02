(in-package #:cl-regex-kit)

(defun replacement-capture (match-result text designator empty-value)
  (let ((index
        (if (integerp designator)
            designator
            (cdr (assoc designator (match-result-group-names match-result) :test #'string=)))))
    (if (and
        (integerp index)
        (<= 0 index)
        (< index (length (match-result-groups match-result))))
        (or (match-group-string match-result index text) empty-value)
      empty-value)))

(defun replacement-capture-string (match-result text designator)
  (replacement-capture match-result text designator ""))

(defun empty-octet-vector ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun ascii-octet-character (octet)
  (and (< octet 128) (code-char octet)))

(defun replacement-name-character-p (character)
  (or (digit-char-p character)
      (not (null (find character
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
                       :test #'char=)))))

(defun replacement-designator (token)
  (if (every #'digit-char-p token) (parse-integer token)
    token))

(defun replacement-digit-character-p (character)
  "Test whether CHARACTER can continue a bare backslash-syntax `\\1` reference.
Backslash syntax admits digits only, so `\\word` stays literal the way
cl-ppcre leaves it; the dollar syntax's REPLACEMENT-NAME-CHARACTER-P instead
admits letters because `$word` names a capture."
  (digit-char-p character))

(defun byte-replacement-name-character-p (octet)
  (let ((character (ascii-octet-character octet)))
    (and character (replacement-name-character-p character))))

(defun byte-replacement-digit-character-p (octet)
  (<= #x30 octet #x39))

(defun backslash-whole-match-token (character)
  "Map cl-ppcre's `\\&` whole-match alias onto the group-0 designator token."
  (and (char= character #\&) "0"))

(defun byte-backslash-whole-match-token (octet)
  (and (= octet #x26) (vector #x30)))

(defun character-template-syntax (template-syntax)
  "Return the character-domain SIGIL, NAME-CHAR-P, and ALIAS for TEMPLATE-SYNTAX."
  (ecase template-syntax
    (:dollar (values #\$ #'replacement-name-character-p nil))
    (:backslash (values #\\ #'replacement-digit-character-p #'backslash-whole-match-token))))

(defun byte-template-syntax (template-syntax)
  "Return the octet-domain SIGIL, NAME-CHAR-P, and ALIAS for TEMPLATE-SYNTAX."
  (ecase template-syntax
    (:dollar (values #x24 #'byte-replacement-name-character-p nil))
    (:backslash
     (values #x5c #'byte-replacement-digit-character-p #'byte-backslash-whole-match-token))))

(defun validate-template-syntax (template-syntax)
  "Validate TEMPLATE-SYNTAX and return it."
  (unless (member template-syntax '(:dollar :backslash))
    (error 'type-error :datum template-syntax :expected-type '(member :dollar :backslash)))
  template-syntax)

(defun expand-replacement-template-generic
    (template sigil open-brace close-brace name-char-p emit-literal emit-capture output
     &optional alias)
  "Shared `SIGIL name`/`SIGIL{name}` replacement-template scanner for both the
character (EXPAND-REPLACEMENT-TEMPLATE) and octet
(EXPAND-BYTE-REPLACEMENT-TEMPLATE) domains, and for both the Rust-style `$`
and the cl-ppcre-style `\\` syntaxes: SIGIL/OPEN-BRACE/CLOSE-BRACE are the
domain's element values for the escape introducer, `{`, and `}`, NAME-CHAR-P
tests whether an element can continue a bare `SIGIL name`, and
EMIT-LITERAL/EMIT-CAPTURE each write one element or one designator's capture
value to OUTPUT. ALIAS, when supplied, maps the single element following
SIGIL to a synthetic designator token -- this is how `\\&` reaches the same
capture lookup as `\\0` -- and returns NIL for elements it does not name.
Returns OUTPUT."
  (loop with length = (length template)
        for position from 0 below length
        for element = (elt template position)
        for alias-token = (and alias
                               (eql element sigil)
                               (< (1+ position) length)
                               (funcall alias (elt template (1+ position))))
        do (cond
             ((not (eql element sigil)) (funcall emit-literal element output))
             ((= (1+ position) length) (funcall emit-literal sigil output))
             ((eql (elt template (1+ position)) sigil)
              (funcall emit-literal sigil output)
              (incf position))
             ((eql (elt template (1+ position)) open-brace)
              (let ((end (position close-brace template :start (+ position 2))))
                (if end
                    (progn
                      (when (> end (+ position 2))
                        (funcall emit-capture (subseq template (+ position 2) end) output))
                      (setf position end))
                    (funcall emit-literal sigil output))))
             (alias-token
              (funcall emit-capture alias-token output)
              (incf position))
             (t
              (let ((end (loop for end from (1+ position) below length
                                while (funcall name-char-p (elt template end))
                                finally (return end))))
                (if (= end (1+ position))
                    (funcall emit-literal sigil output)
                    (progn
                      (funcall emit-capture (subseq template (1+ position) end) output)
                      (setf position (1- end)))))))
        finally (return output)))

(defun expand-replacement-template (template match-result text template-syntax)
  "Expand TEMPLATE's capture references for MATCH-RESULT under TEMPLATE-SYNTAX."
  (multiple-value-bind (sigil name-char-p alias) (character-template-syntax template-syntax)
    (with-output-to-string (output)
      (expand-replacement-template-generic
       template sigil #\{ #\}
       name-char-p
       (lambda (character output) (write-char character output))
       (lambda (token output)
         (write-string
          (replacement-capture-string match-result text (replacement-designator token))
          output))
       output
       alias))))

(defun expand-byte-replacement-template (template match-result text template-syntax)
  "Expand byte TEMPLATE's ASCII capture references under TEMPLATE-SYNTAX."
  (multiple-value-bind (sigil name-char-p alias) (byte-template-syntax template-syntax)
    (expand-replacement-template-generic
     template sigil #x7b #x7d
     name-char-p
     (lambda (octet output) (vector-push-extend octet output))
     (lambda (token output)
       (when (every (lambda (octet) (< octet 128)) token)
         (let ((name (map 'string #'ascii-octet-character token)))
           (loop for octet across (replacement-capture
                                    match-result text
                                    (if (every #'digit-char-p name) (parse-integer name) name)
                                    (empty-octet-vector))
                 do (vector-push-extend octet output)))))
     (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
     alias)))

(defun replacement-string (replacement match-result text template-syntax)
  (let ((value
        (if (functionp replacement)
            (funcall replacement match-result text)
            (expand-replacement-template replacement match-result text template-syntax))))
    (check-type value string)
    value))

(defun replacement-octets (replacement match-result text template-syntax)
  (let ((value
        (if (functionp replacement)
            (funcall replacement match-result text)
            (expand-byte-replacement-template replacement match-result text template-syntax))))
    (check-type value octet-vector)
    value))

(defun replacement-value (regex replacement match-result text template-syntax)
  (if (byte-regex-p regex) (replacement-octets replacement match-result text template-syntax)
    (replacement-string replacement match-result text template-syntax)))

(defun validate-replacement (regex replacement)
  "Validate REPLACEMENT's static shape for REGEX and return it."
  (check-type regex regex)
  (unless (or (functionp replacement)
              (if (byte-regex-p regex)
                  (typep replacement 'octet-vector)
                  (stringp replacement)))
    (error 'type-error
           :datum replacement
           :expected-type (if (byte-regex-p regex)
                              '(or octet-vector function)
                              '(or string function))))
  replacement)

(defun concatenate-replacement (regex prefix replacement suffix)
  (if (byte-regex-p regex) (concatenate 'octet-vector prefix replacement suffix)
    (concatenate 'string prefix replacement suffix)))

(defun replace-first (regex text replacement &key (start 0) end timeout
                                                  (template-syntax :dollar))
  "Replace the first match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a template string or a function of result and text.
TEMPLATE-SYNTAX selects the template dialect: `:dollar` (the default) for
Rust-style `$1`/`${name}`/`$$`, or `:backslash` for cl-ppcre-style
`\\1`/`\\{name}`/`\\&`/`\\\\`."
  (validate-replacement regex replacement)
  (validate-template-syntax template-syntax)
  (let ((result (scan regex text :start start :end end :timeout timeout)))
    (if result (concatenate-replacement
        regex
        (subseq text 0 (match-start result))
        (replacement-value regex replacement result text template-syntax)
        (subseq text (match-end result)))
      text)))

(defun valid-replacement-limit-p (limit)
  (or (null limit) (and (integerp limit) (not (minusp limit)))))

(defun accumulate-replacements
    (regex text replacement limit start end timeout template-syntax append)
  "Shared DO-MATCHES accumulation loop for REPLACE-UP-TO's byte/string
domains: calls (FUNCALL APPEND span) once per literal span between matches
and once per expanded REPLACEMENT value, up to LIMIT replacements. APPEND is
the one thing that differs per domain -- push octets onto an adjustable
vector, or write a string onto a stream -- so it is supplied as a
continuation rather than duplicating this loop once per domain."
  (let ((position 0) (replacement-count 0))
    (do-matches (result regex text :start start :end end :timeout timeout)
      (when (and limit (>= replacement-count limit))
        (return))
      (funcall append (subseq text position (match-start result)))
      (funcall append (replacement-value regex replacement result text template-syntax))
      (setf position (match-end result))
      (incf replacement-count))
    (funcall append (subseq text position))))

(defun replace-up-to (regex text replacement limit start end timeout template-syntax)
  (unless (valid-replacement-limit-p limit)
    (error 'type-error :datum limit :expected-type '(or null (integer 0 *))))
  (check-type regex regex)
  (validate-text-range regex text start end)
  (validate-timeout timeout)
  (validate-replacement regex replacement)
  (validate-template-syntax template-syntax)
  (when (zerop (or limit 1))
    (return-from replace-up-to text))
  (if (byte-regex-p regex)
      (let ((output (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (accumulate-replacements
         regex text replacement limit start end timeout template-syntax
         (lambda (octets) (loop for octet across octets do (vector-push-extend octet output))))
        output)
      (with-output-to-string (output)
        (accumulate-replacements
         regex text replacement limit start end timeout template-syntax
         (lambda (string) (write-string string output))))))

(defun replace-n (regex text replacement count &key (start 0) end timeout
                                                    (template-syntax :dollar))
  "Replace at most COUNT non-overlapping matches of REGEX in TEXT.
This is the CL-REGEX-KIT equivalent of Rust Regex::replacen.
TEMPLATE-SYNTAX selects the template dialect; see REPLACE-FIRST."
  (replace-up-to regex text replacement count start end timeout template-syntax))

(defun replace-all (regex text replacement &key (start 0) end timeout
                                                (template-syntax :dollar))
  "Replace every non-overlapping match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a template string or a function of result and text.
TEMPLATE-SYNTAX selects the template dialect: `:dollar` (the default) for
Rust-style `$1`/`${name}`/`$$`, or `:backslash` for cl-ppcre-style
`\\1`/`\\{name}`/`\\&`/`\\\\`."
  (replace-up-to regex text replacement nil start end timeout template-syntax))
