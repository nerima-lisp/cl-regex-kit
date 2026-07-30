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

(defvar *unicode-property-values* nil)

(defvar *unicode-segmentation-property-values* nil)

(defun canonical-segmentation-property-value (value aliases)
  "Return VALUE normalized to its UCD long alias."
  (and value (or (cdr (assoc value aliases :test #'string=)) value)))

(defun unicode-segmentation-class-name (class)
  "Return the UCD spelling for an SBCL segmentation CLASS."
  (if class (normalized-property-name (symbol-name class))
    "OTHER"))

(defun unicode-segmentation-property-values ()
  "Return the GCB, WB, and SB values present in SBCL's Unicode data."
  (or
    *unicode-segmentation-property-values*
    (setf *unicode-segmentation-property-values* (let ((grapheme-cluster-breaks (make-hash-table :test #'equal))
            (word-breaks (make-hash-table :test #'equal))
            (sentence-breaks (make-hash-table :test #'equal)))
        (loop for code below char-code-limit
              for character = (code-char code)
              do (setf (gethash
              (unicode-segmentation-class-name (sb-unicode:grapheme-break-class character))
              grapheme-cluster-breaks) t
                (gethash
              (unicode-segmentation-class-name (sb-unicode:word-break-class character))
              word-breaks) t
                (gethash
              (unicode-segmentation-class-name (sb-unicode:sentence-break-class character))
              sentence-breaks) t))
        (list
          (loop for value being the hash-keys of grapheme-cluster-breaks
                collect value)
          (loop for value being the hash-keys of word-breaks
                collect value)
          (loop for value being the hash-keys of sentence-breaks
                collect value))))))

(defun unicode-property-values ()
  "Return the Script, Block, and Age values present in SBCL's Unicode data."
  (or
    *unicode-property-values*
    (setf *unicode-property-values* (let ((scripts (make-hash-table :test #'equal))
            (blocks (make-hash-table :test #'equal))
            (ages (make-hash-table :test #'eql)))
        (loop for code below char-code-limit
              for character = (code-char code)
              do (setf (gethash
              (normalized-property-name (symbol-name (sb-unicode:script character)))
              scripts) t
                (gethash
              (normalized-property-name (symbol-name (sb-unicode:char-block character)))
              blocks) t
                (gethash (sb-unicode:age character) ages) t))
        (list
          (loop for value being the hash-keys of scripts
                collect value)
          (loop for value being the hash-keys of blocks
                collect value)
          (loop for value being the hash-keys of ages
                collect value))))))

(defun known-block-property-name (raw-property)
  "Return RAW-PROPERTY as a known Unicode block name, or NIL."
  (let ((candidate
        (or
          (property-value raw-property (list "BLK=" "BLOCK="))
          (and
            (> (length raw-property) 2)
            (string= raw-property "IN" :end1 2 :end2 2)
            (subseq raw-property 2)))))
    (and
      candidate
      (member candidate (second (unicode-property-values)) :test (function string=))
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
           (find compact +unicode-age-ranges+
                 :key (lambda (entry)
                        (remove #\_ (string-downcase (car entry))))
                 :test #'string=)))
    (or
      (cdr direct)
      (multiple-value-bind (major minor) (parse-age-property-value value)
        (and minor
             (cdr (assoc (format nil "V~D_~D" major minor)
                         +unicode-age-ranges+
                         :test #'string=)))))))

(defun valid-age-property-p (value)
  (not (null (age-property-ranges value))))

(defun unicode-property-name-p (name)
  "Return true if NAME denotes a property implemented by this engine."
  (let* ((raw-name (string-upcase name))
         (raw-property (normalized-property-name raw-name))
         (property
        (or (property-value raw-property (list "GC=" "GENERALCATEGORY=")) raw-property))
         (script
        (property-value raw-property (list "SC=" "SCRIPT=" "SCX=" "SCRIPTEXTENSIONS=")))
         (block (known-block-property-name raw-property))
         (age (property-value raw-name (list "AGE=")))
         (grapheme-cluster-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "GCB=" "GRAPHEMECLUSTERBREAK="))
          +unicode-grapheme-cluster-break-value-aliases+))
         (word-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "WB=" "WORDBREAK="))
          +unicode-word-break-value-aliases+))
         (sentence-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "SB=" "SENTENCEBREAK="))
          +unicode-sentence-break-value-aliases+)))
    (cond
      (script
        (member
          (canonical-script-name script)
          (first (unicode-property-values))
          :test
          (function string=)))
      (block t)
      (age (valid-age-property-p age))
      (grapheme-cluster-break
        (member
          grapheme-cluster-break
          (first (unicode-segmentation-property-values))
          :test
          (function string=)))
      (word-break
        (member
          word-break
          (second (unicode-segmentation-property-values))
          :test
          (function string=)))
      (sentence-break
        (member
          sentence-break
          (third (unicode-segmentation-property-values))
          :test
          (function string=)))
      ((member property +unicode-binary-property-names+ :test (function string=)) t)
      ((member
          property
          +unicode-general-category-property-names+
          :test
          (function string=))
        t)
      (t (member property (first (unicode-property-values)) :test (function string=))))))

(defun age-property-p (value character)
  (let ((ranges (age-property-ranges value)))
    (and ranges (range-matches-p ranges character))))

(defun ascii-hex-digit-p (character)
  "Return true when CHARACTER is an ASCII hexadecimal digit."
  (or
    (and (char<= #\0 character #\9))
    (and (char<= #\a character #\f))
    (and (char<= #\A character #\F))))

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

(defun id-start-p (character)
  "Return true when CHARACTER has the Unicode ID_Start property."
  (let ((category (symbol-name (sb-unicode:general-category character))))
    (or
      (member category '("LU" "LL" "LT" "LM" "LO" "NL") :test #'string=)
      (range-matches-p
        '((#x1885 . #x1886) (#x2118 . #x2118) (#x212e . #x212e) (#x309b . #x309c))
        character))))

(defun id-continue-p (character)
  "Return true when CHARACTER has the Unicode ID_Continue property."
  (let ((category (symbol-name (sb-unicode:general-category character))))
    (or
      (id-start-p character)
      (member category '("MN" "MC" "ND" "PC") :test #'string=)
      (range-matches-p
        '((#x00b7 . #x00b7)
          (#x0387 . #x0387)
          (#x1369 . #x1371)
          (#x19da . #x19da)
          (#x200c . #x200d))
        character))))

(defun noncharacter-code-point-p (character)
  "Return true when CHARACTER is a Unicode noncharacter code point."
  (let ((code (char-code character)))
    (or
      (<= #xfdd0 code #xfdef)
      (and (>= code #xfffe) (member (logand code #xffff) '(#xfffe #xffff))))))

(defun script-extension-p (script character)
  "Return true when CHARACTER has SCRIPT in its Script_Extensions set."
  (or
    (string=
      script
      (normalized-property-name (symbol-name (sb-unicode:script character))))
    (range-matches-p
      (cdr (assoc script +script-extension-extra-ranges+ :test #'string=))
      character)))

(defun extra-unicode-binary-property-p (property character)
  (let* ((canonical
           (or (cdr (assoc property +unicode-extra-binary-property-aliases+
                           :test #'string=))
               property))
         (ranges
           (cdr (assoc canonical +unicode-extra-binary-property-ranges+
                       :test #'string=))))
    (and ranges (range-matches-p ranges character))))

(defmacro define-unicode-range-properties (&body definitions)
  "Create a declarative alias-to-range table for binary Unicode properties."
  `(defparameter +unicode-range-property-ranges+
     (list
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

(defun unicode-property-p (name character)
  (let* ((raw-name (string-upcase name))
         (raw-property (normalized-property-name raw-name))
         (property
        (or (property-value raw-property (list "GC=" "GENERALCATEGORY=")) raw-property))
         (script
        (canonical-script-name (property-value raw-property (list "SC=" "SCRIPT="))))
         (script-extension
        (canonical-script-name
          (property-value raw-property (list "SCX=" "SCRIPTEXTENSIONS="))))
         (block (known-block-property-name raw-property))
         (age (property-value raw-name (list "AGE=")))
         (grapheme-cluster-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "GCB=" "GRAPHEMECLUSTERBREAK="))
          +unicode-grapheme-cluster-break-value-aliases+))
         (word-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "WB=" "WORDBREAK="))
          +unicode-word-break-value-aliases+))
         (sentence-break
        (canonical-segmentation-property-value
          (property-value raw-property (list "SB=" "SENTENCEBREAK="))
          +unicode-sentence-break-value-aliases+))
         (category (string-upcase (symbol-name (sb-unicode:general-category character))))
         (major-category (subseq category 0 1))
         (range-property-ranges (unicode-range-property-ranges property)))
    (cond
      (grapheme-cluster-break
        (string=
          grapheme-cluster-break
          (unicode-segmentation-class-name (sb-unicode:grapheme-break-class character))))
      (word-break
        (string=
          word-break
          (unicode-segmentation-class-name (sb-unicode:word-break-class character))))
      (sentence-break
        (string=
          sentence-break
          (unicode-segmentation-class-name (sb-unicode:sentence-break-class character))))
      (script-extension (script-extension-p script-extension character))
      (script
        (string=
          script
          (normalized-property-name (symbol-name (sb-unicode:script character)))))
      (block (string=
          block
          (normalized-property-name (symbol-name (sb-unicode:char-block character)))))
      (age (age-property-p age character))
      ((member property '("ANY") :test #'string=) t)
      ((member property '("ASCII") :test #'string=) (<= (char-code character) 127))
      ((member property
               '("IDCOMPATMATHCONTINUE" "IDCOMPATMATHSTART" "INCB"
                 "INDICCONJUNCTBREAK" "MODIFIERCOMBININGMARK" "MCM"
                 "OTHERALPHABETIC" "OALPHA"
                 "OTHERDEFAULTIGNORABLECODEPOINT" "ODI"
                 "OTHERIDCONTINUE" "OIDC" "OTHERIDSTART" "OIDS"
                 "OTHERLOWERCASE" "OLOWER" "OTHERMATH" "OMATH"
                 "OTHERUPPERCASE" "OUPPER")
               :test #'string=)
        (extra-unicode-binary-property-p property character))
      ((member property '("ASSIGNED") :test #'string=) (not (string= category "CN")))
      ((member property '("ALPHABETIC" "ALPHA") :test #'string=)
        (sb-unicode:alphabetic-p character))
      ((member property '("LOWERCASE" "LOWER") :test #'string=)
        (sb-unicode:lowercase-p character))
      ((member property '("UPPERCASE" "UPPER") :test #'string=)
        (sb-unicode:uppercase-p character))
      ((member property '("WHITESPACE" "WHITE_SPACE" "SPACE" "WSPACE") :test #'string=)
        (sb-unicode:whitespace-p character))
      ((member property '("ASCIIHEXDIGIT" "AHEX") :test #'string=)
        (ascii-hex-digit-p character))
      ((member property '("HEXDIGIT" "HEX") :test #'string=)
        (sb-unicode:hex-digit-p character))
      ((string= property "CASED") (sb-unicode:cased-p character))
      ((member property '("CASEIGNORABLE" "CI") :test #'string=)
        (sb-unicode:case-ignorable-p character))
      ((member property '("CHANGESWHENCASEFOLDED" "CWCF") :test #'string=)
        (changes-when-casefolded-p character))
      ((member property '("CHANGESWHENCASEMAPPED" "CWCM") :test #'string=)
        (or
          (changes-under-case-mapping-p #'sb-unicode:lowercase character)
          (changes-under-case-mapping-p #'sb-unicode:titlecase character)
          (changes-under-case-mapping-p #'sb-unicode:uppercase character)))
      ((member property '("CHANGESWHENLOWERCASED" "CWL") :test #'string=)
        (changes-under-case-mapping-p #'sb-unicode:lowercase character))
      ((member property '("CHANGESWHENTITLECASED" "CWT") :test #'string=)
        (changes-under-case-mapping-p #'sb-unicode:titlecase character))
      ((member property '("CHANGESWHENUPPERCASED" "CWU") :test #'string=)
        (changes-under-case-mapping-p #'sb-unicode:uppercase character))
      ((member property '("DEFAULTIGNORABLECODEPOINT" "DI") :test #'string=)
        (sb-unicode:default-ignorable-p character))
      ((member property '("IDEOGRAPHIC" "IDEO") :test #'string=)
        (sb-unicode:ideographic-p character))
      ((string= property "MATH") (sb-unicode:math-p character))
      ((member property '("SOFTDOTTED" "SD") :test #'string=)
        (sb-unicode:soft-dotted-p character))
      (range-property-ranges
        (range-matches-p range-property-ranges character))
      ((member property '("GRAPHEMEEXTEND" "GREXT") :test #'string=)
        (grapheme-extend-p character))
      ((member property '("BIDIMIRRORED" "BIDIM") :test #'string=)
        (sb-unicode:mirrored-p character))
      ((member property '("JOINCONTROL" "JOINC") :test #'string=)
        (join-control-p character))
      ((member property '("NONCHARACTERCODEPOINT" "NCHAR") :test #'string=)
        (noncharacter-code-point-p character))
      ((member property '("IDSTART" "IDS") :test #'string=) (id-start-p character))
      ((member property '("IDCONTINUE" "IDC") :test #'string=)
        (id-continue-p character))
      ((member property '("DECIMALNUMBER" "DIGIT") :test #'string=)
        (string= category "ND"))
      ((member property '("L") :test #'string=) (string= major-category "L"))
      ((member property '("M") :test #'string=) (string= major-category "M"))
      ((member property '("N") :test #'string=) (string= major-category "N"))
      ((member property '("P") :test #'string=) (string= major-category "P"))
      ((member property '("S") :test #'string=) (string= major-category "S"))
      ((member property '("Z") :test #'string=) (string= major-category "Z"))
      ((member property '("C") :test #'string=) (string= major-category "C"))
      ((member property '("LC" "CASEDLETTER") :test #'string=)
        (member category '("LU" "LL" "LT") :test #'string=))
      ((member property '("LETTER") :test #'string=) (string= major-category "L"))
      ((member property '("MARK") :test #'string=) (string= major-category "M"))
      ((member property '("NUMBER") :test #'string=) (string= major-category "N"))
      ((member property '("PUNCTUATION" "PUNCT") :test #'string=)
        (string= major-category "P"))
      ((member property '("SYMBOL") :test #'string=) (string= major-category "S"))
      ((member property '("SEPARATOR") :test #'string=) (string= major-category "Z"))
      ((member property '("OTHER") :test #'string=) (string= major-category "C"))
      ((member property '("CONTROL") :test #'string=) (string= category "CC"))
      ((member property '("FORMAT") :test #'string=) (string= category "CF"))
      ((member property '("UNASSIGNED") :test #'string=) (string= category "CN"))
      ((member property '("PRIVATEUSE") :test #'string=) (string= category "CO"))
      ((member property '("SURROGATE") :test #'string=) (string= category "CS"))
      ((member property '("UPPERCASELETTER") :test #'string=) (string= category "LU"))
      ((member property '("LOWERCASELETTER") :test #'string=) (string= category "LL"))
      ((member property '("TITLECASELETTER") :test #'string=) (string= category "LT"))
      ((member property '("MODIFIERLETTER") :test #'string=) (string= category "LM"))
      ((member property '("OTHERLETTER") :test #'string=) (string= category "LO"))
      ((member property '("NONSPACINGMARK") :test #'string=) (string= category "MN"))
      ((member property '("SPACINGMARK") :test #'string=) (string= category "MC"))
      ((member property '("ENCLOSINGMARK") :test #'string=) (string= category "ME"))
      ((member property '("DECIMALDIGITNUMBER") :test #'string=)
        (string= category "ND"))
      ((member property '("LETTERNUMBER") :test #'string=) (string= category "NL"))
      ((member property '("OTHERNUMBER") :test #'string=) (string= category "NO"))
      ((member property '("CONNECTORPUNCTUATION") :test #'string=)
        (string= category "PC"))
      ((member property '("DASHPUNCTUATION") :test #'string=) (string= category "PD"))
      ((member property '("OPENPUNCTUATION") :test #'string=) (string= category "PS"))
      ((member property '("CLOSEPUNCTUATION") :test #'string=)
        (string= category "PE"))
      ((member property '("INITIALPUNCTUATION") :test #'string=)
        (string= category "PI"))
      ((member property '("FINALPUNCTUATION") :test #'string=)
        (string= category "PF"))
      ((member property '("OTHERPUNCTUATION") :test #'string=)
        (string= category "PO"))
      ((string= property category))
      ((string=
          property
          (normalized-property-name (symbol-name (sb-unicode:script character)))))
      (t nil))))
