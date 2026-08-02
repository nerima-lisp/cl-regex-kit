(in-package #:cl-regex-kit)

;;; Unicode property normalization and matching predicates. Every static
;;; lookup table this file consults -- property/alias name lists, value
;;; aliases, and code-point range tables -- lives in
;;; unicode-property-data.lisp and unicode-binary-property-range-data.lisp
;;; instead, so this file stays pure logic over externally-defined data.
(defun range-matches-p (ranges character)
  (let ((code (char-code character)))
    (some
      (lambda (range)
        (<= (car range) code (cdr range)))
      ranges)))

(defun ascii-posix-class-ranges (name)
  "Return ASCII code point ranges for POSIX NAME, or NIL when unsupported."
  (cdr (assoc name +ascii-posix-class-ranges+ :test #'string=)))

(defun normalized-property-name (name)
  (string-upcase
    (remove-if
      (lambda (character)
        (member character '(#\_ #\- #\Space)))
      name)))

(defun property-value (property prefixes)
  (loop for prefix in prefixes
        when (and
      (>= (length property) (length prefix))
      (string= property prefix :end1 (length prefix) :end2 (length prefix)))
          do (return (subseq property (length prefix)))))

(defun canonical-script-name (name)
  "Return NAME normalized to its Unicode Script long alias."
  (and name (or (cdr (assoc name +unicode-script-aliases+ :test #'string=)) name)))

(progn
  (defparameter +unicode-runtime-domain-functions+
    (quote ((:script . sb-unicode:script)
            (:block . sb-unicode:char-block)
            (:category . sb-unicode:general-category)
            (:grapheme-break . sb-unicode:grapheme-break-class)
            (:word-break . sb-unicode:word-break-class)
            (:sentence-break . sb-unicode:sentence-break-class))))

  (defparameter *unicode-runtime-domain-indexes* nil)

  (defun unicode-runtime-member-values (function-name)
  "Extract the finite return MEMBER objects declared by FUNCTION-NAME."
  (let* ((function-type
           (sb-kernel:specifier-type
            (sb-kernel:%simple-fun-type (symbol-function function-name))))
         (return-type (sb-kernel:fun-type-returns function-type))
         (specifier (sb-kernel:type-specifier return-type)))
    (unless (and (consp specifier)
                 (eq (first specifier) (quote values))
                 (consp (rest specifier))
                 (consp (second specifier))
                 (eq (first (second specifier)) (quote member))
                 (equal (cddr specifier) (quote (&optional))))
      (error "Unsupported SBCL return type for ~S: ~S" function-name specifier))
    (rest (second specifier))))

  (defun unicode-runtime-segmentation-domain-p (domain)
    (member domain
            (quote (:grapheme-break :word-break :sentence-break))
            :test (function eq)))

  (defun make-unicode-runtime-domain-index (domain values)
  "Build a normalized lookup for authoritative VALUES in DOMAIN."
  (let ((index (make-hash-table :test (function equal))))
    (labels ((insert (name value)
               (multiple-value-bind (existing present-p) (gethash name index)
                 (when (and present-p (not (eq existing value)))
                   (error "Normalized Unicode value collision in ~S for ~A: ~S and ~S"
                          domain name existing value))
                 (setf (gethash name index) value))))
      (dolist (value values)
        (cond
          ((null value)
           (when (unicode-runtime-segmentation-domain-p domain)
             (insert "OTHER" nil)))
          ((symbolp value)
           (insert (normalized-property-name (symbol-name value)) value))
          (t
           (error "Unsupported Unicode value in ~S: ~S" domain value))))
      (when (unicode-runtime-segmentation-domain-p domain)
        (insert "OTHER" nil)))
    index))

  (defun unicode-runtime-domain-indexes ()
    (or *unicode-runtime-domain-indexes*
        (setf *unicode-runtime-domain-indexes*
              (loop for (domain . function-name) in +unicode-runtime-domain-functions+
                    collect
                    (cons domain
                          (make-unicode-runtime-domain-index
                            domain
                            (unicode-runtime-member-values function-name)))))))

  (defun unicode-runtime-domain-index (domain)
    (or (cdr (assoc domain (unicode-runtime-domain-indexes) :test (function eq)))
        (error "Unknown Unicode runtime domain: ~S" domain)))

  (defun unicode-runtime-property-value (domain name)
    (gethash name (unicode-runtime-domain-index domain)))

  (defun unicode-runtime-property-names (domain)
    (loop for name being the hash-keys of (unicode-runtime-domain-index domain)
          collect name)))
(defun canonical-segmentation-property-value (value aliases)
  "Return VALUE normalized to its UCD long alias."
  (and value (or (cdr (assoc value aliases :test (function string=))) value)))

(defun known-block-property-name (raw-property)
  "Return RAW-PROPERTY as a known Unicode block name, or NIL."
  (let ((candidate
          (or (property-value raw-property (list "BLK=" "BLOCK="))
              (and (> (length raw-property) 2)
                   (string= raw-property "IN" :end1 2 :end2 2)
                   (subseq raw-property 2)))))
    (and candidate
         (nth-value 1 (unicode-runtime-property-value :block candidate))
         candidate)))

(defun decimal-string-p (string)
  (and (plusp (length string)) (every #'digit-char-p string)))

(defun parse-age-property-value (value)
  (let* ((version
        (if (and (plusp (length value)) (char-equal (char value 0) #\V)) (subseq value 1)
          value))
         (separator
        (position-if
          (lambda (item)
            (member item '(#\. #\_)))
          version))
         (major-text (subseq version 0 separator))
         (minor-text (and separator (subseq version (1+ separator)))))
    (when (and
        (decimal-string-p major-text)
        (or (null minor-text) (decimal-string-p minor-text)))
      (values (parse-integer major-text) (and minor-text (parse-integer minor-text))))))

(defun age-property-ranges (value)
  "Return the UCD 16 ranges denoted by the Rust-compatible Age VALUE."
  (let* ((compact (remove #\_ (string-downcase value)))
         (direct
        (find
          compact
          +unicode-age-ranges+
          :key
          (lambda (entry)
            (remove #\_ (string-downcase (car entry))))
          :test
          #'string=)))
    (or
      (cdr direct)
      (multiple-value-bind (major minor) (parse-age-property-value value)
        (and
          minor
          (cdr
            (assoc (format nil "V~D_~D" major minor) +unicode-age-ranges+ :test #'string=)))))))

(defun valid-age-property-p (value)
  (not (null (age-property-ranges value))))

(defun age-property-p (value character)
  (let ((ranges (age-property-ranges value)))
    (and ranges (range-matches-p ranges character))))

(defun ascii-hex-digit-p (character)
  "Return true when CHARACTER is an ASCII hexadecimal digit."
  (or
    (char<= #\0 character #\9)
    (char<= #\a character #\f)
    (char<= #\A character #\F)))

(defun join-control-p (character)
  "Return true when CHARACTER is U+200C or U+200D."
  (member (char-code character) '(#x200c #x200d)))

(defun changes-under-case-mapping-p (mapping character)
  "Return true when CHARACTER changes under locale-independent MAPPING."
  (not
    (string= (string character) (funcall mapping (string character) :locale nil))))

(defun changes-when-casefolded-p (character)
  "Return true when CHARACTER changes under Unicode case folding."
  (not (string= (string character) (sb-unicode:casefold (string character)))))

(defun grapheme-extend-p (character)
  "Return true when CHARACTER has the Unicode Grapheme_Extend property."
  (or
    (eq (sb-unicode:grapheme-break-class character) :extend)
    (range-matches-p +other-grapheme-extend-ranges+ character)))

(progn
  (defun required-unicode-runtime-property-values (domain names)
    "Resolve NAMES in DOMAIN, failing immediately when SBCL lacks a value."
    (let ((index (unicode-runtime-domain-index domain)))
      (mapcar (lambda (name)
                (multiple-value-bind (value present-p) (gethash name index)
                  (unless (and present-p value)
                    (error "Missing required Unicode value ~A in ~S" name domain))
                  value))
              names)))
  (defparameter +unicode-id-start-category-values+
    (required-unicode-runtime-property-values
      :category (quote ("LU" "LL" "LT" "LM" "LO" "NL"))))
  (defparameter +unicode-id-continue-extra-category-values+
    (required-unicode-runtime-property-values
      :category (quote ("MN" "MC" "ND" "PC"))))
  (defparameter +unicode-number-category-values+
    (required-unicode-runtime-property-values
      :category (quote ("ND" "NL" "NO"))))
  (defun id-start-category-p (category character)
    (or (member category +unicode-id-start-category-values+ :test (function eq))
        (range-matches-p
          (quote ((#x1885 . #x1886)
                  (#x2118 . #x2118)
                  (#x212e . #x212e)
                  (#x309b . #x309c)))
          character)))
  (defun id-start-p (character)
    "Return true when CHARACTER has the Unicode ID_Start property."
    (id-start-category-p (sb-unicode:general-category character) character)))

(defun id-continue-p (character)
  "Return true when CHARACTER has the Unicode ID_Continue property."
  (let ((category (sb-unicode:general-category character)))
    (or (id-start-category-p category character)
        (member category
                +unicode-id-continue-extra-category-values+
                :test (function eq))
        (range-matches-p
          (quote ((#x00b7 . #x00b7)
                  (#x0387 . #x0387)
                  (#x1369 . #x1371)
                  (#x19da . #x19da)
                  (#x200c . #x200d)))
          character))))

(defun noncharacter-code-point-p (character)
  "Return true when CHARACTER is a Unicode noncharacter code point."
  (let ((code (char-code character)))
    (or
      (<= #xfdd0 code #xfdef)
      (and (>= code #xfffe) (member (logand code #xffff) '(#xfffe #xffff))))))

(defun extra-unicode-binary-property-p (property character)
  (let* ((canonical
        (or
          (cdr (assoc property +unicode-extra-binary-property-aliases+ :test #'string=))
          property))
         (ranges
        (cdr (assoc canonical +unicode-extra-binary-property-ranges+ :test #'string=))))
    (and ranges (range-matches-p ranges character))))

(defmacro define-unicode-range-properties (&body definitions)
  "Create a declarative alias-to-range table for binary Unicode properties."
  `(defparameter +unicode-range-property-ranges+ (list
      ,@(loop for (names ranges) in definitions
            collect `(cons ',names ,ranges)))))

(define-unicode-range-properties
  (("DASH") +dash-ranges+)
  (("HYPHEN") +hyphen-ranges+)
  (("PATTERNSYNTAX" "PATSYN") +pattern-syntax-ranges+)
  (("QUOTATIONMARK" "QMARK") +quotation-mark-ranges+)
  (("SENTENCETERMINAL" "STERM") +sentence-terminal-ranges+)
  (("TERMINALPUNCTUATION" "TERM") +terminal-punctuation-ranges+)
  (("UNIFIEDIDEOGRAPH" "UIDEO") +unified-ideograph-ranges+)
  (("GRAPHEMELINK" "GRLINK") +grapheme-link-ranges+)
  (("LOGICALORDEREXCEPTION" "LOE") +logical-order-exception-ranges+)
  (("OTHERGRAPHEMEEXTEND" "OGREXT") +other-grapheme-extend-ranges+)
  (("PREPENDEDCONCATENATIONMARK" "PCM") +prepended-concatenation-mark-ranges+)
  (("RADICAL") +radical-ranges+)
  (("BIDICONTROL" "BIDIC") +bidi-control-ranges+)
  (("DEPRECATED" "DEP") +deprecated-ranges+)
  (("IDSBINARYOPERATOR" "IDSBIN" "IDSB") +ids-binary-operator-ranges+)
  (("IDSTRINARYOPERATOR" "IDSTRI" "IDST") '((#x2ff2 . #x2ff3)))
  (("IDSUNARYOPERATOR" "IDSUNI" "IDSU") '((#x2ffe . #x2fff)))
  (("PATTERNWHITESPACE" "PATWS") +pattern-white-space-ranges+)
  (("REGIONALINDICATOR" "RI") '((#x1f1e6 . #x1f1ff)))
  (("VARIATIONSELECTOR" "VS") +variation-selector-ranges+)
  (("EMOJI") +emoji-ranges+)
  (("EMOJICOMPONENT" "ECOMP") +emoji-component-ranges+)
  (("EMOJIMODIFIER" "EMOD") +emoji-modifier-ranges+)
  (("EMOJIMODIFIERBASE" "EBASE") +emoji-modifier-base-ranges+)
  (("EMOJIPRESENTATION" "EPRES") +emoji-presentation-ranges+)
  (("EXTENDEDPICTOGRAPHIC" "EXTPICT") +extended-pictographic-ranges+)
  (("DIACRITIC" "DIA") +diacritic-ranges+)
  (("EXTENDER" "EXT") +extender-ranges+)
  (("GRAPHEMEBASE" "GRBASE") +grapheme-base-ranges+)
  (("XIDSTART" "XIDS") +xid-start-ranges+)
  (("XIDCONTINUE" "XIDC") +xid-continue-ranges+))

(defun unicode-range-property-ranges (property)
  "Return the ranges registered for PROPERTY, or NIL when it is not range-based."
  (loop for (names . ranges) in +unicode-range-property-ranges+
        when (member property names :test #'string=)
          return ranges))
