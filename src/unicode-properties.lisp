(in-package #:cl-regex-kit)

;;; Unicode property normalization and matching predicates.  Static lookup
;;; tables live in unicode-property-data.lisp.
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

(defparameter +unicode-binary-property-names+ '("ANY"
    "ASCII"
    "ASSIGNED"
    "ALPHABETIC"
    "ALPHA"
    "LOWERCASE"
    "LOWER"
    "UPPERCASE"
    "UPPER"
    "WHITESPACE"
    "WHITE_SPACE"
    "SPACE"
    "ASCIIHEXDIGIT"
    "AHEX"
    "HEXDIGIT"
    "CASED"
    "CASEIGNORABLE"
    "DEFAULTIGNORABLECODEPOINT"
    "IDEOGRAPHIC"
    "MATH"
    "SOFTDOTTED"
    "DASH"
    "HYPHEN"
    "PATTERNSYNTAX"
    "QUOTATIONMARK"
    "SENTENCETERMINAL"
    "TERMINALPUNCTUATION"
    "UNIFIEDIDEOGRAPH"
    "BIDIMIRRORED"
    "JOINCONTROL"
    "BIDICONTROL"
    "DEPRECATED"
    "IDSBINARYOPERATOR"
    "IDSTRINARYOPERATOR"
    "IDSUNARYOPERATOR"
    "NONCHARACTERCODEPOINT"
    "PATTERNWHITESPACE"
    "REGIONALINDICATOR"
    "VARIATIONSELECTOR"
    "EMOJI"
    "EMOJICOMPONENT"
    "EMOJIMODIFIER"
    "EMOJIMODIFIERBASE"
    "EMOJIPRESENTATION"
    "EXTENDEDPICTOGRAPHIC"
    "IDSTART"
    "IDCONTINUE"
    "GRAPHEMELINK"
    "LOGICALORDEREXCEPTION"
    "OTHERGRAPHEMEEXTEND"
    "PREPENDEDCONCATENATIONMARK"
    "RADICAL"
    "CHANGESWHENCASEFOLDED"
    "CHANGESWHENCASEMAPPED"
    "CHANGESWHENLOWERCASED"
    "CHANGESWHENTITLECASED"
    "CHANGESWHENUPPERCASED"
    "GRAPHEMEEXTEND"
    "DIACRITIC"
    "EXTENDER"
    "GRAPHEMEBASE"
    "XIDSTART"
    "XIDCONTINUE"
    "BIDIC"
    "BIDIM"
    "CI"
    "DI"
    "DEP"
    "IDSBIN"
    "IDSTRI"
    "IDSUNI"
    "NCHAR"
    "PATWS"
    "RI"
    "VS"
    "WSPACE"
    "HEX"
    "ECOMP"
    "EMOD"
    "EBASE"
    "EPRES"
    "EXTPICT"
    "IDS"
    "IDC"
    "GRLINK"
    "LOE"
    "OGREXT"
    "PCM"
    "CWCF"
    "CWCM"
    "CWL"
    "CWT"
    "CWU"
    "GREXT"
    "DIA"
    "EXT"
    "GRBASE"
    "IDEO"
    "IDSB"
    "IDST"
    "IDSU"
    "JOINC"
    "PATSYN"
    "QMARK"
    "SD"
    "STERM"
    "TERM"
    "UIDEO"
    "XIDC"
    "XIDS"
    "IDCOMPATMATHCONTINUE"
    "IDCOMPATMATHSTART"
    "INCB"
    "INDICCONJUNCTBREAK"
    "MODIFIERCOMBININGMARK"
    "MCM"
    "OTHERALPHABETIC"
    "OALPHA"
    "OTHERDEFAULTIGNORABLECODEPOINT"
    "ODI"
    "OTHERIDCONTINUE"
    "OIDC"
    "OTHERIDSTART"
    "OIDS"
    "OTHERLOWERCASE"
    "OLOWER"
    "OTHERMATH"
    "OMATH"
    "OTHERUPPERCASE"
    "OUPPER"))

(defparameter +unicode-general-category-property-names+ '("L"
    "M"
    "N"
    "P"
    "S"
    "Z"
    "C"
    "LC"
    "LU"
    "LL"
    "LT"
    "LM"
    "LO"
    "MN"
    "MC"
    "ME"
    "ND"
    "NL"
    "NO"
    "PC"
    "PD"
    "PS"
    "PE"
    "PI"
    "PF"
    "PO"
    "SM"
    "SC"
    "SK"
    "SO"
    "ZS"
    "ZL"
    "ZP"
    "CC"
    "CF"
    "CS"
    "CO"
    "CN"
    "LETTER"
    "MARK"
    "NUMBER"
    "PUNCTUATION"
    "PUNCT"
    "SYMBOL"
    "SEPARATOR"
    "OTHER"
    "CONTROL"
    "FORMAT"
    "UNASSIGNED"
    "PRIVATEUSE"
    "SURROGATE"
    "CASEDLETTER"
    "UPPERCASELETTER"
    "LOWERCASELETTER"
    "TITLECASELETTER"
    "MODIFIERLETTER"
    "OTHERLETTER"
    "NONSPACINGMARK"
    "SPACINGMARK"
    "ENCLOSINGMARK"
    "DECIMALNUMBER"
    "DIGIT"
    "DECIMALDIGITNUMBER"
    "LETTERNUMBER"
    "OTHERNUMBER"
    "CONNECTORPUNCTUATION"
    "DASHPUNCTUATION"
    "OPENPUNCTUATION"
    "CLOSEPUNCTUATION"
    "INITIALPUNCTUATION"
    "FINALPUNCTUATION"
    "OTHERPUNCTUATION"))

(defvar *unicode-property-values* nil)

(progn
  (defvar *unicode-segmentation-property-values* nil)
  (defparameter +unicode-grapheme-cluster-break-value-aliases+ '(("CN" . "CONTROL")
      ("CR" . "CR")
      ("EB" . "EBASE")
      ("EBG" . "EBASEGAZ")
      ("EM" . "EMODIFIER")
      ("EX" . "EXTEND")
      ("GAZ" . "GLUEAFTERZWJ")
      ("L" . "L")
      ("LF" . "LF")
      ("LV" . "LV")
      ("LVT" . "LVT")
      ("PP" . "PREPEND")
      ("RI" . "REGIONALINDICATOR")
      ("SM" . "SPACINGMARK")
      ("T" . "T")
      ("V" . "V")
      ("XX" . "OTHER")
      ("ZWJ" . "ZWJ")))
  (defparameter +unicode-word-break-value-aliases+ '(("CR" . "CR")
      ("DQ" . "DOUBLEQUOTE")
      ("EB" . "EBASE")
      ("EBG" . "EBASEGAZ")
      ("EM" . "EMODIFIER")
      ("EX" . "EXTENDNUMLET")
      ("EXTEND" . "EXTEND")
      ("FO" . "FORMAT")
      ("GAZ" . "GLUEAFTERZWJ")
      ("HL" . "HEBREWLETTER")
      ("KA" . "KATAKANA")
      ("LE" . "ALETTER")
      ("LF" . "LF")
      ("MB" . "MIDNUMLET")
      ("ML" . "MIDLETTER")
      ("MN" . "MIDNUM")
      ("NL" . "NEWLINE")
      ("NU" . "NUMERIC")
      ("RI" . "REGIONALINDICATOR")
      ("SQ" . "SINGLEQUOTE")
      ("WSEGSPACE" . "WSEGSPACE")
      ("XX" . "OTHER")
      ("ZWJ" . "ZWJ")))
  (defparameter +unicode-sentence-break-value-aliases+ '(("AT" . "ATERM")
      ("CL" . "CLOSE")
      ("CR" . "CR")
      ("EX" . "EXTEND")
      ("FO" . "FORMAT")
      ("LE" . "OLETTER")
      ("LF" . "LF")
      ("LO" . "LOWER")
      ("NU" . "NUMERIC")
      ("SC" . "SCONTINUE")
      ("SE" . "SEP")
      ("SP" . "SP")
      ("ST" . "STERM")
      ("UP" . "UPPER")
      ("XX" . "OTHER"))))

(progn
  (defun canonical-segmentation-property-value (value aliases)
    "Return VALUE normalized to its UCD long alias."
    (and value (or (cdr (assoc value aliases :test #'string=)) value)))
  (defun unicode-segmentation-class-name (class)
    "Return the UCD spelling for an SBCL segmentation CLASS."
    (if class (normalized-property-name (symbol-name class))
      "OTHER")))

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

(defun decode-code-point-ranges (specification)
  "Decode a space-separated hexadecimal range SPECIFICATION."
  (loop with length = (length specification)
        for start = 0 then (1+ end)
        for
        end = (or (position #\Space specification :start start) length)
        for token = (subseq specification start end)
        for separator = (position #\- token)
        collect (cons
      (parse-integer token :radix 16 :end separator)
      (if separator (parse-integer token :radix 16 :start (1+ separator))
        (parse-integer token :radix 16)))
        while (< end length)))

(defparameter +emoji-ranges+ (decode-code-point-ranges
    "23 2a 30-39 a9 ae 203c 2049 2122 2139 2194-2199 21a9-21aa 231a-231b 2328 23cf 23e9-23f3 23f8-23fa 24c2 25aa-25ab 25b6 25c0 25fb-25fe 2600-2604 260e 2611 2614-2615 2618 261d 2620 2622-2623 2626 262a 262e-262f 2638-263a 2640 2642 2648-2653 265f-2660 2663 2665-2666 2668 267b 267e-267f 2692-2697 2699 269b-269c 26a0-26a1 26a7 26aa-26ab 26b0-26b1 26bd-26be 26c4-26c5 26c8 26ce-26cf 26d1 26d3-26d4 26e9-26ea 26f0-26f5 26f7-26fa 26fd 2702 2705 2708-270d 270f 2712 2714 2716 271d 2721 2728 2733-2734 2744 2747 274c 274e 2753-2755 2757 2763-2764 2795-2797 27a1 27b0 27bf 2934-2935 2b05-2b07 2b1b-2b1c 2b50 2b55 3030 303d 3297 3299 1f004 1f0cf 1f170-1f171 1f17e-1f17f 1f18e 1f191-1f19a 1f1e6-1f1ff 1f201-1f202 1f21a 1f22f 1f232-1f23a 1f250-1f251 1f300-1f321 1f324-1f393 1f396-1f397 1f399-1f39b 1f39e-1f3f0 1f3f3-1f3f5 1f3f7-1f4fd 1f4ff-1f53d 1f549-1f54e 1f550-1f567 1f56f-1f570 1f573-1f57a 1f587 1f58a-1f58d 1f590 1f595-1f596 1f5a4-1f5a5 1f5a8 1f5b1-1f5b2 1f5bc 1f5c2-1f5c4 1f5d1-1f5d3 1f5dc-1f5de 1f5e1 1f5e3 1f5e8 1f5ef 1f5f3 1f5fa-1f64f 1f680-1f6c5 1f6cb-1f6d2 1f6d5-1f6d7 1f6dc-1f6e5 1f6e9 1f6eb-1f6ec 1f6f0 1f6f3-1f6fc 1f7e0-1f7eb 1f7f0 1f90c-1f93a 1f93c-1f945 1f947-1f9ff 1fa70-1fa7c 1fa80-1fa89 1fa8f-1fac6 1face-1fadc 1fadf-1fae9 1faf0-1faf8"))

;; Unicode 16.0 ranges generated from regex-syntax's UCD tables.
(defparameter +dash-ranges+ (decode-code-point-ranges
    "2d 58a 5be 1400 1806 2010-2015 2053 207b 208b 2212 2e17 2e1a 2e3a-2e3b 2e40 2e5d 301c 3030 30a0 fe31-fe32 fe58 fe63 ff0d 10d6e 10ead"))

(defparameter +hyphen-ranges+ (decode-code-point-ranges "2d ad 58a 1806 2010-2011 2e17 30fb fe63 ff0d ff65"))

(defparameter +pattern-syntax-ranges+ (decode-code-point-ranges
    "21-2f 3a-40 5b-5e 60 7b-7e a1-a7 a9 ab-ac ae b0-b1 b6 bb bf d7 f7 2010-2027 2030-203e 2041-2053 2055-205e 2190-245f 2500-2775 2794-2bff 2e00-2e7f 3001-3003 3008-3020 3030 fd3e-fd3f fe45-fe46"))

(defparameter +quotation-mark-ranges+ (decode-code-point-ranges
    "22 5c ab bb 2018-201f 2039-203a 2e42 300c-300f 301d-301f fe41-fe44 ff02 ff07 ff62-ff63"))

(defparameter +sentence-terminal-ranges+ (decode-code-point-ranges
    "21 2e 3f 589 61d-61f 6d4 700-702 7f9 837 839 83d-83e 964-965 104a-104b 1362 1367-1368 166e 1735-1736 17d4-17d5 1803 1809 1944-1945 1aa8-1aab 1b4e-1b4f 1b5a-1b5b 1b5e-1b5f 1b7d-1b7f 1c3b-1c3c 1c7e-1c7f 2024 203c-203d 2047-2049 2cf9-2cfb 2e2e 2e3c 2e53-2e54 3002 a4ff a60e-a60f a6f3 a6f7 a876-a877 a8ce-a8cf a92f a9c8-a9c9 aa5d-aa5f aaf0-aaf1 abeb fe12 fe15-fe16 fe52 fe56-fe57 ff01 ff0e ff1f ff61 10a56-10a57 10f55-10f59 10f86-10f89 11047-11048 110be-110c1 11141-11143 111c5-111c6 111cd 111de-111df 11238-11239 1123b-1123c 112a9 113d4-113d5 1144b-1144c 115c2-115c3 115c9-115d7 11641-11642 1173c-1173e 11944 11946 11a42-11a43 11a9b-11a9c 11c41-11c42 11ef7-11ef8 11f43-11f44 16a6e-16a6f 16af5 16b37-16b38 16b44 16d6e-16d6f 16e98 1bc9f 1da88"))

(defparameter +terminal-punctuation-ranges+ (decode-code-point-ranges
    "21 2c 2e 3a-3b 3f 37e 387 589 5c3 60c 61b 61d-61f 6d4 700-70a 70c 7f8-7f9 830-835 837-83e 85e 964-965 e5a-e5b f08 f0d-f12 104a-104b 1361-1368 166e 16eb-16ed 1735-1736 17d4-17d6 17da 1802-1805 1808-1809 1944-1945 1aa8-1aab 1b4e-1b4f 1b5a-1b5b 1b5d-1b5f 1b7d-1b7f 1c3b-1c3f 1c7e-1c7f 2024 203c-203d 2047-2049 2cf9-2cfb 2e2e 2e3c 2e41 2e4c 2e4e-2e4f 2e53-2e54 3001-3002 a4fe-a4ff a60d-a60f a6f3-a6f7 a876-a877 a8ce-a8cf a92f a9c7-a9c9 aa5d-aa5f aadf aaf0-aaf1 abeb fe12 fe15-fe16 fe50-fe52 fe54-fe57 ff01 ff0c ff0e ff1a-ff1b ff1f ff61 ff64 1039f 103d0 10857 1091f 10a56-10a57 10af0-10af5 10b3a-10b3f 10b99-10b9c 10f55-10f59 10f86-10f89 11047-1104d 110be-110c1 11141-11143 111c5-111c6 111cd 111de-111df 11238-1123c 112a9 113d4-113d5 1144b-1144d 1145a-1145b 115c2-115c5 115c9-115d7 11641-11642 1173c-1173e 11944 11946 11a42-11a43 11a9b-11a9c 11aa1-11aa2 11c41-11c43 11c71 11ef7-11ef8 11f43-11f44 12470-12474 16a6e-16a6f 16af5 16b37-16b39 16b44 16d6e-16d6f 16e97-16e98 1bc9f 1da87-1da8a"))

(defparameter +unified-ideograph-ranges+ (decode-code-point-ranges
    "3400-4dbf 4e00-9fff fa0e-fa0f fa11 fa13-fa14 fa1f fa21 fa23-fa24 fa27-fa29 20000-2a6df 2a700-2b739 2b740-2b81d 2b820-2cea1 2ceb0-2ebe0 2ebf0-2ee5d 30000-3134a 31350-323af"))

(defparameter +grapheme-link-ranges+ (decode-code-point-ranges
    "94d 9cd a4d acd b4d bcd c4d ccd d3b-d3c d4d dca e3a eba f84 1039-103a 1714-1715 1734 17d2 1a60 1b44 1baa-1bab 1bf2-1bf3 2d7f a806 a82c a8c4 a953 a9c0 aaf6 abed 10a3f 11046 11070 1107f 110b9 11133-11134 111c0 11235 112ea 1134d 113ce-113d0 11442 114c2 115bf 1163f 116b6 1172b 11839 1193d-1193e 119e0 11a34 11a47 11a99 11c3f 11d44-11d45 11d97 11f41-11f42 1612f"))

(defparameter +logical-order-exception-ranges+ (decode-code-point-ranges
    "e40-e44 ec0-ec4 19b5-19b7 19ba aab5-aab6 aab9 aabb-aabc"))

(defparameter +other-grapheme-extend-ranges+ (decode-code-point-ranges
    "9be 9d7 b3e b57 bbe bd7 cc0 cc2 cc7-cc8 cca-ccb cd5-cd6 d3e d57 dcf ddf 1715 1734 1b35 1b3b 1b3d 1b43-1b44 1baa 1bf2-1bf3 200c 302e-302f a953 a9c0 ff9e-ff9f 111c0 11235 1133e 1134d 11357 113b8 113c2 113c5 113c7-113c9 113cf 114b0 114bd 115af 116b6 11930 1193d 11f41 16ff0-16ff1 1d165-1d166 1d16d-1d172 e0020-e007f"))

(defun grapheme-extend-p (character)
  "Return true when CHARACTER has the Unicode Grapheme_Extend property."
  (or
    (eq (sb-unicode:grapheme-break-class character) :extend)
    (range-matches-p +other-grapheme-extend-ranges+ character)))

(defparameter +prepended-concatenation-mark-ranges+ (decode-code-point-ranges "600-605 6dd 70f 890-891 8e2 110bd 110cd"))

(defparameter +radical-ranges+ (decode-code-point-ranges "2e80-2e99 2e9b-2ef3 2f00-2fd5"))

(defparameter +emoji-component-ranges+ (decode-code-point-ranges
    "23 2a 30-39 200d 20e3 fe0f 1f1e6-1f1ff 1f3fb-1f3ff 1f9b0-1f9b3 e0020-e007f"))

(defparameter +emoji-modifier-ranges+ (decode-code-point-ranges "1f3fb-1f3ff"))

(defparameter +emoji-modifier-base-ranges+ (decode-code-point-ranges
    "261d 26f9 270a-270d 1f385 1f3c2-1f3c4 1f3c7 1f3ca-1f3cc 1f442-1f443 1f446-1f450 1f466-1f478 1f47c 1f481-1f483 1f485-1f487 1f48f 1f491 1f4aa 1f574-1f575 1f57a 1f590 1f595-1f596 1f645-1f647 1f64b-1f64f 1f6a3 1f6b4-1f6b6 1f6c0 1f6cc 1f90c 1f90f 1f918-1f91f 1f926 1f930-1f939 1f93c-1f93e 1f977 1f9b5-1f9b6 1f9b8-1f9b9 1f9bb 1f9cd-1f9cf 1f9d1-1f9dd 1fac3-1fac5 1faf0-1faf8"))

(defparameter +emoji-presentation-ranges+ (decode-code-point-ranges
    "231a-231b 23e9-23ec 23f0 23f3 25fd-25fe 2614-2615 2648-2653 267f 2693 26a1 26aa-26ab 26bd-26be 26c4-26c5 26ce 26d4 26ea 26f2-26f3 26f5 26fa 26fd 2705 270a-270b 2728 274c 274e 2753-2755 2757 2795-2797 27b0 27bf 2b1b-2b1c 2b50 2b55 1f004 1f0cf 1f18e 1f191-1f19a 1f1e6-1f1ff 1f201 1f21a 1f22f 1f232-1f236 1f238-1f23a 1f250-1f251 1f300-1f320 1f32d-1f335 1f337-1f37c 1f37e-1f393 1f3a0-1f3ca 1f3cf-1f3d3 1f3e0-1f3f0 1f3f4 1f3f8-1f43e 1f440 1f442-1f4fc 1f4ff-1f53d 1f54b-1f54e 1f550-1f567 1f57a 1f595-1f596 1f5a4 1f5fb-1f64f 1f680-1f6c5 1f6cc 1f6d0-1f6d2 1f6d5-1f6d7 1f6dc-1f6df 1f6eb-1f6ec 1f6f4-1f6fc 1f7e0-1f7eb 1f7f0 1f90c-1f93a 1f93c-1f945 1f947-1f9ff 1fa70-1fa7c 1fa80-1fa89 1fa8f-1fac6 1face-1fadc 1fadf-1fae9 1faf0-1faf8"))

(defparameter +extended-pictographic-ranges+ (decode-code-point-ranges
    "a9 ae 203c 2049 2122 2139 2194-2199 21a9-21aa 231a-231b 2328 2388 23cf 23e9-23f3 23f8-23fa 24c2 25aa-25ab 25b6 25c0 25fb-25fe 2600-2605 2607-2612 2614-2685 2690-2705 2708-2712 2714 2716 271d 2721 2728 2733-2734 2744 2747 274c 274e 2753-2755 2757 2763-2767 2795-2797 27a1 27b0 27bf 2934-2935 2b05-2b07 2b1b-2b1c 2b50 2b55 3030 303d 3297 3299 1f000-1f0ff 1f10d-1f10f 1f12f 1f16c-1f171 1f17e-1f17f 1f18e 1f191-1f19a 1f1ad-1f1e5 1f201-1f20f 1f21a 1f22f 1f232-1f23a 1f23c-1f23f 1f249-1f3fa 1f400-1f53d 1f546-1f64f 1f680-1f6ff 1f774-1f77f 1f7d5-1f7ff 1f80c-1f80f 1f848-1f84f 1f85a-1f85f 1f888-1f88f 1f8ae-1f8ff 1f90c-1f93a 1f93c-1f945 1f947-1faff 1fc00-1fffd"))

;; Unicode 16.0 ranges generated from the official UCD property files.
(defparameter +xid-start-ranges+ (decode-code-point-ranges
    "0041-005a 0061-007a 00aa 00b5 00ba 00c0-00d6 00d8-00f6 00f8-01ba 01bb 01bc-01bf 01c0-01c3 01c4-0293 0294 0295-02af 02b0-02c1 02c6-02d1 02e0-02e4 02ec 02ee 0370-0373 0374 0376-0377 037b-037d 037f 0386 0388-038a 038c 038e-03a1 03a3-03f5 03f7-0481 048a-052f 0531-0556 0559 0560-0588 05d0-05ea 05ef-05f2 0620-063f 0640 0641-064a 066e-066f 0671-06d3 06d5 06e5-06e6 06ee-06ef 06fa-06fc 06ff 0710 0712-072f 074d-07a5 07b1 07ca-07ea 07f4-07f5 07fa 0800-0815 081a 0824 0828 0840-0858 0860-086a 0870-0887 0889-088e 08a0-08c8 08c9 0904-0939 093d 0950 0958-0961 0971 0972-0980 0985-098c 098f-0990 0993-09a8 09aa-09b0 09b2 09b6-09b9 09bd 09ce 09dc-09dd 09df-09e1 09f0-09f1 09fc 0a05-0a0a 0a0f-0a10 0a13-0a28 0a2a-0a30 0a32-0a33 0a35-0a36 0a38-0a39 0a59-0a5c 0a5e 0a72-0a74 0a85-0a8d 0a8f-0a91 0a93-0aa8 0aaa-0ab0 0ab2-0ab3 0ab5-0ab9 0abd 0ad0 0ae0-0ae1 0af9 0b05-0b0c 0b0f-0b10 0b13-0b28 0b2a-0b30 0b32-0b33 0b35-0b39 0b3d 0b5c-0b5d 0b5f-0b61 0b71 0b83 0b85-0b8a 0b8e-0b90 0b92-0b95 0b99-0b9a 0b9c 0b9e-0b9f 0ba3-0ba4 0ba8-0baa 0bae-0bb9 0bd0 0c05-0c0c 0c0e-0c10 0c12-0c28 0c2a-0c39 0c3d 0c58-0c5a 0c5d 0c60-0c61 0c80 0c85-0c8c 0c8e-0c90 0c92-0ca8 0caa-0cb3 0cb5-0cb9 0cbd 0cdd-0cde 0ce0-0ce1 0cf1-0cf2 0d04-0d0c 0d0e-0d10 0d12-0d3a 0d3d 0d4e 0d54-0d56 0d5f-0d61 0d7a-0d7f 0d85-0d96 0d9a-0db1 0db3-0dbb 0dbd 0dc0-0dc6 0e01-0e30 0e32 0e40-0e45 0e46 0e81-0e82 0e84 0e86-0e8a 0e8c-0ea3 0ea5 0ea7-0eb0 0eb2 0ebd 0ec0-0ec4 0ec6 0edc-0edf 0f00 0f40-0f47 0f49-0f6c 0f88-0f8c 1000-102a 103f 1050-1055 105a-105d 1061 1065-1066 106e-1070 1075-1081 108e 10a0-10c5 10c7 10cd 10d0-10fa 10fc 10fd-10ff 1100-1248 124a-124d 1250-1256 1258 125a-125d 1260-1288 128a-128d 1290-12b0 12b2-12b5 12b8-12be 12c0 12c2-12c5 12c8-12d6 12d8-1310 1312-1315 1318-135a 1380-138f 13a0-13f5 13f8-13fd 1401-166c 166f-167f 1681-169a 16a0-16ea 16ee-16f0 16f1-16f8 1700-1711 171f-1731 1740-1751 1760-176c 176e-1770 1780-17b3 17d7 17dc 1820-1842 1843 1844-1878 1880-1884 1885-1886 1887-18a8 18aa 18b0-18f5 1900-191e 1950-196d 1970-1974 1980-19ab 19b0-19c9 1a00-1a16 1a20-1a54 1aa7 1b05-1b33 1b45-1b4c 1b83-1ba0 1bae-1baf 1bba-1be5 1c00-1c23 1c4d-1c4f 1c5a-1c77 1c78-1c7d 1c80-1c8a 1c90-1cba 1cbd-1cbf 1ce9-1cec 1cee-1cf3 1cf5-1cf6 1cfa 1d00-1d2b 1d2c-1d6a 1d6b-1d77 1d78 1d79-1d9a 1d9b-1dbf 1e00-1f15 1f18-1f1d 1f20-1f45 1f48-1f4d 1f50-1f57 1f59 1f5b 1f5d 1f5f-1f7d 1f80-1fb4 1fb6-1fbc 1fbe 1fc2-1fc4 1fc6-1fcc 1fd0-1fd3 1fd6-1fdb 1fe0-1fec 1ff2-1ff4 1ff6-1ffc 2071 207f 2090-209c 2102 2107 210a-2113 2115 2118 2119-211d 2124 2126 2128 212a-212d 212e 212f-2134 2135-2138 2139 213c-213f 2145-2149 214e 2160-2182 2183-2184 2185-2188 2c00-2c7b 2c7c-2c7d 2c7e-2ce4 2ceb-2cee 2cf2-2cf3 2d00-2d25 2d27 2d2d 2d30-2d67 2d6f 2d80-2d96 2da0-2da6 2da8-2dae 2db0-2db6 2db8-2dbe 2dc0-2dc6 2dc8-2dce 2dd0-2dd6 2dd8-2dde 3005 3006 3007 3021-3029 3031-3035 3038-303a 303b 303c 3041-3096 309d-309e 309f 30a1-30fa 30fc-30fe 30ff 3105-312f 3131-318e 31a0-31bf 31f0-31ff 3400-4dbf 4e00-a014 a015 a016-a48c a4d0-a4f7 a4f8-a4fd a500-a60b a60c a610-a61f a62a-a62b a640-a66d a66e a67f a680-a69b a69c-a69d a6a0-a6e5 a6e6-a6ef a717-a71f a722-a76f a770 a771-a787 a788 a78b-a78e a78f a790-a7cd a7d0-a7d1 a7d3 a7d5-a7dc a7f2-a7f4 a7f5-a7f6 a7f7 a7f8-a7f9 a7fa a7fb-a801 a803-a805 a807-a80a a80c-a822 a840-a873 a882-a8b3 a8f2-a8f7 a8fb a8fd-a8fe a90a-a925 a930-a946 a960-a97c a984-a9b2 a9cf a9e0-a9e4 a9e6 a9e7-a9ef a9fa-a9fe aa00-aa28 aa40-aa42 aa44-aa4b aa60-aa6f aa70 aa71-aa76 aa7a aa7e-aaaf aab1 aab5-aab6 aab9-aabd aac0 aac2 aadb-aadc aadd aae0-aaea aaf2 aaf3-aaf4 ab01-ab06 ab09-ab0e ab11-ab16 ab20-ab26 ab28-ab2e ab30-ab5a ab5c-ab5f ab60-ab68 ab69 ab70-abbf abc0-abe2 ac00-d7a3 d7b0-d7c6 d7cb-d7fb f900-fa6d fa70-fad9 fb00-fb06 fb13-fb17 fb1d fb1f-fb28 fb2a-fb36 fb38-fb3c fb3e fb40-fb41 fb43-fb44 fb46-fbb1 fbd3-fc5d fc64-fd3d fd50-fd8f fd92-fdc7 fdf0-fdf9 fe71 fe73 fe77 fe79 fe7b fe7d fe7f-fefc ff21-ff3a ff41-ff5a ff66-ff6f ff70 ff71-ff9d ffa0-ffbe ffc2-ffc7 ffca-ffcf ffd2-ffd7 ffda-ffdc 10000-1000b 1000d-10026 10028-1003a 1003c-1003d 1003f-1004d 10050-1005d 10080-100fa 10140-10174 10280-1029c 102a0-102d0 10300-1031f 1032d-10340 10341 10342-10349 1034a 10350-10375 10380-1039d 103a0-103c3 103c8-103cf 103d1-103d5 10400-1044f 10450-1049d 104b0-104d3 104d8-104fb 10500-10527 10530-10563 10570-1057a 1057c-1058a 1058c-10592 10594-10595 10597-105a1 105a3-105b1 105b3-105b9 105bb-105bc 105c0-105f3 10600-10736 10740-10755 10760-10767 10780-10785 10787-107b0 107b2-107ba 10800-10805 10808 1080a-10835 10837-10838 1083c 1083f-10855 10860-10876 10880-1089e 108e0-108f2 108f4-108f5 10900-10915 10920-10939 10980-109b7 109be-109bf 10a00 10a10-10a13 10a15-10a17 10a19-10a35 10a60-10a7c 10a80-10a9c 10ac0-10ac7 10ac9-10ae4 10b00-10b35 10b40-10b55 10b60-10b72 10b80-10b91 10c00-10c48 10c80-10cb2 10cc0-10cf2 10d00-10d23 10d4a-10d4d 10d4e 10d4f 10d50-10d65 10d6f 10d70-10d85 10e80-10ea9 10eb0-10eb1 10ec2-10ec4 10f00-10f1c 10f27 10f30-10f45 10f70-10f81 10fb0-10fc4 10fe0-10ff6 11003-11037 11071-11072 11075 11083-110af 110d0-110e8 11103-11126 11144 11147 11150-11172 11176 11183-111b2 111c1-111c4 111da 111dc 11200-11211 11213-1122b 1123f-11240 11280-11286 11288 1128a-1128d 1128f-1129d 1129f-112a8 112b0-112de 11305-1130c 1130f-11310 11313-11328 1132a-11330 11332-11333 11335-11339 1133d 11350 1135d-11361 11380-11389 1138b 1138e 11390-113b5 113b7 113d1 113d3 11400-11434 11447-1144a 1145f-11461 11480-114af 114c4-114c5 114c7 11580-115ae 115d8-115db 11600-1162f 11644 11680-116aa 116b8 11700-1171a 11740-11746 11800-1182b 118a0-118df 118ff-11906 11909 1190c-11913 11915-11916 11918-1192f 1193f 11941 119a0-119a7 119aa-119d0 119e1 119e3 11a00 11a0b-11a32 11a3a 11a50 11a5c-11a89 11a9d 11ab0-11af8 11bc0-11be0 11c00-11c08 11c0a-11c2e 11c40 11c72-11c8f 11d00-11d06 11d08-11d09 11d0b-11d30 11d46 11d60-11d65 11d67-11d68 11d6a-11d89 11d98 11ee0-11ef2 11f02 11f04-11f10 11f12-11f33 11fb0 12000-12399 12400-1246e 12480-12543 12f90-12ff0 13000-1342f 13441-13446 13460-143fa 14400-14646 16100-1611d 16800-16a38 16a40-16a5e 16a70-16abe 16ad0-16aed 16b00-16b2f 16b40-16b43 16b63-16b77 16b7d-16b8f 16d40-16d42 16d43-16d6a 16d6b-16d6c 16e40-16e7f 16f00-16f4a 16f50 16f93-16f9f 16fe0-16fe1 16fe3 17000-187f7 18800-18cd5 18cff-18d08 1aff0-1aff3 1aff5-1affb 1affd-1affe 1b000-1b122 1b132 1b150-1b152 1b155 1b164-1b167 1b170-1b2fb 1bc00-1bc6a 1bc70-1bc7c 1bc80-1bc88 1bc90-1bc99 1d400-1d454 1d456-1d49c 1d49e-1d49f 1d4a2 1d4a5-1d4a6 1d4a9-1d4ac 1d4ae-1d4b9 1d4bb 1d4bd-1d4c3 1d4c5-1d505 1d507-1d50a 1d50d-1d514 1d516-1d51c 1d51e-1d539 1d53b-1d53e 1d540-1d544 1d546 1d54a-1d550 1d552-1d6a5 1d6a8-1d6c0 1d6c2-1d6da 1d6dc-1d6fa 1d6fc-1d714 1d716-1d734 1d736-1d74e 1d750-1d76e 1d770-1d788 1d78a-1d7a8 1d7aa-1d7c2 1d7c4-1d7cb 1df00-1df09 1df0a 1df0b-1df1e 1df25-1df2a 1e030-1e06d 1e100-1e12c 1e137-1e13d 1e14e 1e290-1e2ad 1e2c0-1e2eb 1e4d0-1e4ea 1e4eb 1e5d0-1e5ed 1e5f0 1e7e0-1e7e6 1e7e8-1e7eb 1e7ed-1e7ee 1e7f0-1e7fe 1e800-1e8c4 1e900-1e943 1e94b 1ee00-1ee03 1ee05-1ee1f 1ee21-1ee22 1ee24 1ee27 1ee29-1ee32 1ee34-1ee37 1ee39 1ee3b 1ee42 1ee47 1ee49 1ee4b 1ee4d-1ee4f 1ee51-1ee52 1ee54 1ee57 1ee59 1ee5b 1ee5d 1ee5f 1ee61-1ee62 1ee64 1ee67-1ee6a 1ee6c-1ee72 1ee74-1ee77 1ee79-1ee7c 1ee7e 1ee80-1ee89 1ee8b-1ee9b 1eea1-1eea3 1eea5-1eea9 1eeab-1eebb 20000-2a6df 2a700-2b739 2b740-2b81d 2b820-2cea1 2ceb0-2ebe0 2ebf0-2ee5d 2f800-2fa1d 30000-3134a 31350-323af"))

(defparameter +xid-continue-ranges+ (decode-code-point-ranges
    "0030-0039 0041-005a 005f 0061-007a 00aa 00b5 00b7 00ba 00c0-00d6 00d8-00f6 00f8-01ba 01bb 01bc-01bf 01c0-01c3 01c4-0293 0294 0295-02af 02b0-02c1 02c6-02d1 02e0-02e4 02ec 02ee 0300-036f 0370-0373 0374 0376-0377 037b-037d 037f 0386 0387 0388-038a 038c 038e-03a1 03a3-03f5 03f7-0481 0483-0487 048a-052f 0531-0556 0559 0560-0588 0591-05bd 05bf 05c1-05c2 05c4-05c5 05c7 05d0-05ea 05ef-05f2 0610-061a 0620-063f 0640 0641-064a 064b-065f 0660-0669 066e-066f 0670 0671-06d3 06d5 06d6-06dc 06df-06e4 06e5-06e6 06e7-06e8 06ea-06ed 06ee-06ef 06f0-06f9 06fa-06fc 06ff 0710 0711 0712-072f 0730-074a 074d-07a5 07a6-07b0 07b1 07c0-07c9 07ca-07ea 07eb-07f3 07f4-07f5 07fa 07fd 0800-0815 0816-0819 081a 081b-0823 0824 0825-0827 0828 0829-082d 0840-0858 0859-085b 0860-086a 0870-0887 0889-088e 0897-089f 08a0-08c8 08c9 08ca-08e1 08e3-0902 0903 0904-0939 093a 093b 093c 093d 093e-0940 0941-0948 0949-094c 094d 094e-094f 0950 0951-0957 0958-0961 0962-0963 0966-096f 0971 0972-0980 0981 0982-0983 0985-098c 098f-0990 0993-09a8 09aa-09b0 09b2 09b6-09b9 09bc 09bd 09be-09c0 09c1-09c4 09c7-09c8 09cb-09cc 09cd 09ce 09d7 09dc-09dd 09df-09e1 09e2-09e3 09e6-09ef 09f0-09f1 09fc 09fe 0a01-0a02 0a03 0a05-0a0a 0a0f-0a10 0a13-0a28 0a2a-0a30 0a32-0a33 0a35-0a36 0a38-0a39 0a3c 0a3e-0a40 0a41-0a42 0a47-0a48 0a4b-0a4d 0a51 0a59-0a5c 0a5e 0a66-0a6f 0a70-0a71 0a72-0a74 0a75 0a81-0a82 0a83 0a85-0a8d 0a8f-0a91 0a93-0aa8 0aaa-0ab0 0ab2-0ab3 0ab5-0ab9 0abc 0abd 0abe-0ac0 0ac1-0ac5 0ac7-0ac8 0ac9 0acb-0acc 0acd 0ad0 0ae0-0ae1 0ae2-0ae3 0ae6-0aef 0af9 0afa-0aff 0b01 0b02-0b03 0b05-0b0c 0b0f-0b10 0b13-0b28 0b2a-0b30 0b32-0b33 0b35-0b39 0b3c 0b3d 0b3e 0b3f 0b40 0b41-0b44 0b47-0b48 0b4b-0b4c 0b4d 0b55-0b56 0b57 0b5c-0b5d 0b5f-0b61 0b62-0b63 0b66-0b6f 0b71 0b82 0b83 0b85-0b8a 0b8e-0b90 0b92-0b95 0b99-0b9a 0b9c 0b9e-0b9f 0ba3-0ba4 0ba8-0baa 0bae-0bb9 0bbe-0bbf 0bc0 0bc1-0bc2 0bc6-0bc8 0bca-0bcc 0bcd 0bd0 0bd7 0be6-0bef 0c00 0c01-0c03 0c04 0c05-0c0c 0c0e-0c10 0c12-0c28 0c2a-0c39 0c3c 0c3d 0c3e-0c40 0c41-0c44 0c46-0c48 0c4a-0c4d 0c55-0c56 0c58-0c5a 0c5d 0c60-0c61 0c62-0c63 0c66-0c6f 0c80 0c81 0c82-0c83 0c85-0c8c 0c8e-0c90 0c92-0ca8 0caa-0cb3 0cb5-0cb9 0cbc 0cbd 0cbe 0cbf 0cc0-0cc4 0cc6 0cc7-0cc8 0cca-0ccb 0ccc-0ccd 0cd5-0cd6 0cdd-0cde 0ce0-0ce1 0ce2-0ce3 0ce6-0cef 0cf1-0cf2 0cf3 0d00-0d01 0d02-0d03 0d04-0d0c 0d0e-0d10 0d12-0d3a 0d3b-0d3c 0d3d 0d3e-0d40 0d41-0d44 0d46-0d48 0d4a-0d4c 0d4d 0d4e 0d54-0d56 0d57 0d5f-0d61 0d62-0d63 0d66-0d6f 0d7a-0d7f 0d81 0d82-0d83 0d85-0d96 0d9a-0db1 0db3-0dbb 0dbd 0dc0-0dc6 0dca 0dcf-0dd1 0dd2-0dd4 0dd6 0dd8-0ddf 0de6-0def 0df2-0df3 0e01-0e30 0e31 0e32-0e33 0e34-0e3a 0e40-0e45 0e46 0e47-0e4e 0e50-0e59 0e81-0e82 0e84 0e86-0e8a 0e8c-0ea3 0ea5 0ea7-0eb0 0eb1 0eb2-0eb3 0eb4-0ebc 0ebd 0ec0-0ec4 0ec6 0ec8-0ece 0ed0-0ed9 0edc-0edf 0f00 0f18-0f19 0f20-0f29 0f35 0f37 0f39 0f3e-0f3f 0f40-0f47 0f49-0f6c 0f71-0f7e 0f7f 0f80-0f84 0f86-0f87 0f88-0f8c 0f8d-0f97 0f99-0fbc 0fc6 1000-102a 102b-102c 102d-1030 1031 1032-1037 1038 1039-103a 103b-103c 103d-103e 103f 1040-1049 1050-1055 1056-1057 1058-1059 105a-105d 105e-1060 1061 1062-1064 1065-1066 1067-106d 106e-1070 1071-1074 1075-1081 1082 1083-1084 1085-1086 1087-108c 108d 108e 108f 1090-1099 109a-109c 109d 10a0-10c5 10c7 10cd 10d0-10fa 10fc 10fd-10ff 1100-1248 124a-124d 1250-1256 1258 125a-125d 1260-1288 128a-128d 1290-12b0 12b2-12b5 12b8-12be 12c0 12c2-12c5 12c8-12d6 12d8-1310 1312-1315 1318-135a 135d-135f 1369-1371 1380-138f 13a0-13f5 13f8-13fd 1401-166c 166f-167f 1681-169a 16a0-16ea 16ee-16f0 16f1-16f8 1700-1711 1712-1714 1715 171f-1731 1732-1733 1734 1740-1751 1752-1753 1760-176c 176e-1770 1772-1773 1780-17b3 17b4-17b5 17b6 17b7-17bd 17be-17c5 17c6 17c7-17c8 17c9-17d3 17d7 17dc 17dd 17e0-17e9 180b-180d 180f 1810-1819 1820-1842 1843 1844-1878 1880-1884 1885-1886 1887-18a8 18a9 18aa 18b0-18f5 1900-191e 1920-1922 1923-1926 1927-1928 1929-192b 1930-1931 1932 1933-1938 1939-193b 1946-194f 1950-196d 1970-1974 1980-19ab 19b0-19c9 19d0-19d9 19da 1a00-1a16 1a17-1a18 1a19-1a1a 1a1b 1a20-1a54 1a55 1a56 1a57 1a58-1a5e 1a60 1a61 1a62 1a63-1a64 1a65-1a6c 1a6d-1a72 1a73-1a7c 1a7f 1a80-1a89 1a90-1a99 1aa7 1ab0-1abd 1abf-1ace 1b00-1b03 1b04 1b05-1b33 1b34 1b35 1b36-1b3a 1b3b 1b3c 1b3d-1b41 1b42 1b43-1b44 1b45-1b4c 1b50-1b59 1b6b-1b73 1b80-1b81 1b82 1b83-1ba0 1ba1 1ba2-1ba5 1ba6-1ba7 1ba8-1ba9 1baa 1bab-1bad 1bae-1baf 1bb0-1bb9 1bba-1be5 1be6 1be7 1be8-1be9 1bea-1bec 1bed 1bee 1bef-1bf1 1bf2-1bf3 1c00-1c23 1c24-1c2b 1c2c-1c33 1c34-1c35 1c36-1c37 1c40-1c49 1c4d-1c4f 1c50-1c59 1c5a-1c77 1c78-1c7d 1c80-1c8a 1c90-1cba 1cbd-1cbf 1cd0-1cd2 1cd4-1ce0 1ce1 1ce2-1ce8 1ce9-1cec 1ced 1cee-1cf3 1cf4 1cf5-1cf6 1cf7 1cf8-1cf9 1cfa 1d00-1d2b 1d2c-1d6a 1d6b-1d77 1d78 1d79-1d9a 1d9b-1dbf 1dc0-1dff 1e00-1f15 1f18-1f1d 1f20-1f45 1f48-1f4d 1f50-1f57 1f59 1f5b 1f5d 1f5f-1f7d 1f80-1fb4 1fb6-1fbc 1fbe 1fc2-1fc4 1fc6-1fcc 1fd0-1fd3 1fd6-1fdb 1fe0-1fec 1ff2-1ff4 1ff6-1ffc 200c-200d 203f-2040 2054 2071 207f 2090-209c 20d0-20dc 20e1 20e5-20f0 2102 2107 210a-2113 2115 2118 2119-211d 2124 2126 2128 212a-212d 212e 212f-2134 2135-2138 2139 213c-213f 2145-2149 214e 2160-2182 2183-2184 2185-2188 2c00-2c7b 2c7c-2c7d 2c7e-2ce4 2ceb-2cee 2cef-2cf1 2cf2-2cf3 2d00-2d25 2d27 2d2d 2d30-2d67 2d6f 2d7f 2d80-2d96 2da0-2da6 2da8-2dae 2db0-2db6 2db8-2dbe 2dc0-2dc6 2dc8-2dce 2dd0-2dd6 2dd8-2dde 2de0-2dff 3005 3006 3007 3021-3029 302a-302d 302e-302f 3031-3035 3038-303a 303b 303c 3041-3096 3099-309a 309d-309e 309f 30a1-30fa 30fb 30fc-30fe 30ff 3105-312f 3131-318e 31a0-31bf 31f0-31ff 3400-4dbf 4e00-a014 a015 a016-a48c a4d0-a4f7 a4f8-a4fd a500-a60b a60c a610-a61f a620-a629 a62a-a62b a640-a66d a66e a66f a674-a67d a67f a680-a69b a69c-a69d a69e-a69f a6a0-a6e5 a6e6-a6ef a6f0-a6f1 a717-a71f a722-a76f a770 a771-a787 a788 a78b-a78e a78f a790-a7cd a7d0-a7d1 a7d3 a7d5-a7dc a7f2-a7f4 a7f5-a7f6 a7f7 a7f8-a7f9 a7fa a7fb-a801 a802 a803-a805 a806 a807-a80a a80b a80c-a822 a823-a824 a825-a826 a827 a82c a840-a873 a880-a881 a882-a8b3 a8b4-a8c3 a8c4-a8c5 a8d0-a8d9 a8e0-a8f1 a8f2-a8f7 a8fb a8fd-a8fe a8ff a900-a909 a90a-a925 a926-a92d a930-a946 a947-a951 a952-a953 a960-a97c a980-a982 a983 a984-a9b2 a9b3 a9b4-a9b5 a9b6-a9b9 a9ba-a9bb a9bc-a9bd a9be-a9c0 a9cf a9d0-a9d9 a9e0-a9e4 a9e5 a9e6 a9e7-a9ef a9f0-a9f9 a9fa-a9fe aa00-aa28 aa29-aa2e aa2f-aa30 aa31-aa32 aa33-aa34 aa35-aa36 aa40-aa42 aa43 aa44-aa4b aa4c aa4d aa50-aa59 aa60-aa6f aa70 aa71-aa76 aa7a aa7b aa7c aa7d aa7e-aaaf aab0 aab1 aab2-aab4 aab5-aab6 aab7-aab8 aab9-aabd aabe-aabf aac0 aac1 aac2 aadb-aadc aadd aae0-aaea aaeb aaec-aaed aaee-aaef aaf2 aaf3-aaf4 aaf5 aaf6 ab01-ab06 ab09-ab0e ab11-ab16 ab20-ab26 ab28-ab2e ab30-ab5a ab5c-ab5f ab60-ab68 ab69 ab70-abbf abc0-abe2 abe3-abe4 abe5 abe6-abe7 abe8 abe9-abea abec abed abf0-abf9 ac00-d7a3 d7b0-d7c6 d7cb-d7fb f900-fa6d fa70-fad9 fb00-fb06 fb13-fb17 fb1d fb1e fb1f-fb28 fb2a-fb36 fb38-fb3c fb3e fb40-fb41 fb43-fb44 fb46-fbb1 fbd3-fc5d fc64-fd3d fd50-fd8f fd92-fdc7 fdf0-fdf9 fe00-fe0f fe20-fe2f fe33-fe34 fe4d-fe4f fe71 fe73 fe77 fe79 fe7b fe7d fe7f-fefc ff10-ff19 ff21-ff3a ff3f ff41-ff5a ff65 ff66-ff6f ff70 ff71-ff9d ff9e-ff9f ffa0-ffbe ffc2-ffc7 ffca-ffcf ffd2-ffd7 ffda-ffdc 10000-1000b 1000d-10026 10028-1003a 1003c-1003d 1003f-1004d 10050-1005d 10080-100fa 10140-10174 101fd 10280-1029c 102a0-102d0 102e0 10300-1031f 1032d-10340 10341 10342-10349 1034a 10350-10375 10376-1037a 10380-1039d 103a0-103c3 103c8-103cf 103d1-103d5 10400-1044f 10450-1049d 104a0-104a9 104b0-104d3 104d8-104fb 10500-10527 10530-10563 10570-1057a 1057c-1058a 1058c-10592 10594-10595 10597-105a1 105a3-105b1 105b3-105b9 105bb-105bc 105c0-105f3 10600-10736 10740-10755 10760-10767 10780-10785 10787-107b0 107b2-107ba 10800-10805 10808 1080a-10835 10837-10838 1083c 1083f-10855 10860-10876 10880-1089e 108e0-108f2 108f4-108f5 10900-10915 10920-10939 10980-109b7 109be-109bf 10a00 10a01-10a03 10a05-10a06 10a0c-10a0f 10a10-10a13 10a15-10a17 10a19-10a35 10a38-10a3a 10a3f 10a60-10a7c 10a80-10a9c 10ac0-10ac7 10ac9-10ae4 10ae5-10ae6 10b00-10b35 10b40-10b55 10b60-10b72 10b80-10b91 10c00-10c48 10c80-10cb2 10cc0-10cf2 10d00-10d23 10d24-10d27 10d30-10d39 10d40-10d49 10d4a-10d4d 10d4e 10d4f 10d50-10d65 10d69-10d6d 10d6f 10d70-10d85 10e80-10ea9 10eab-10eac 10eb0-10eb1 10ec2-10ec4 10efc-10eff 10f00-10f1c 10f27 10f30-10f45 10f46-10f50 10f70-10f81 10f82-10f85 10fb0-10fc4 10fe0-10ff6 11000 11001 11002 11003-11037 11038-11046 11066-1106f 11070 11071-11072 11073-11074 11075 1107f-11081 11082 11083-110af 110b0-110b2 110b3-110b6 110b7-110b8 110b9-110ba 110c2 110d0-110e8 110f0-110f9 11100-11102 11103-11126 11127-1112b 1112c 1112d-11134 11136-1113f 11144 11145-11146 11147 11150-11172 11173 11176 11180-11181 11182 11183-111b2 111b3-111b5 111b6-111be 111bf-111c0 111c1-111c4 111c9-111cc 111ce 111cf 111d0-111d9 111da 111dc 11200-11211 11213-1122b 1122c-1122e 1122f-11231 11232-11233 11234 11235 11236-11237 1123e 1123f-11240 11241 11280-11286 11288 1128a-1128d 1128f-1129d 1129f-112a8 112b0-112de 112df 112e0-112e2 112e3-112ea 112f0-112f9 11300-11301 11302-11303 11305-1130c 1130f-11310 11313-11328 1132a-11330 11332-11333 11335-11339 1133b-1133c 1133d 1133e-1133f 11340 11341-11344 11347-11348 1134b-1134d 11350 11357 1135d-11361 11362-11363 11366-1136c 11370-11374 11380-11389 1138b 1138e 11390-113b5 113b7 113b8-113ba 113bb-113c0 113c2 113c5 113c7-113ca 113cc-113cd 113ce 113cf 113d0 113d1 113d2 113d3 113e1-113e2 11400-11434 11435-11437 11438-1143f 11440-11441 11442-11444 11445 11446 11447-1144a 11450-11459 1145e 1145f-11461 11480-114af 114b0-114b2 114b3-114b8 114b9 114ba 114bb-114be 114bf-114c0 114c1 114c2-114c3 114c4-114c5 114c7 114d0-114d9 11580-115ae 115af-115b1 115b2-115b5 115b8-115bb 115bc-115bd 115be 115bf-115c0 115d8-115db 115dc-115dd 11600-1162f 11630-11632 11633-1163a 1163b-1163c 1163d 1163e 1163f-11640 11644 11650-11659 11680-116aa 116ab 116ac 116ad 116ae-116af 116b0-116b5 116b6 116b7 116b8 116c0-116c9 116d0-116e3 11700-1171a 1171d 1171e 1171f 11720-11721 11722-11725 11726 11727-1172b 11730-11739 11740-11746 11800-1182b 1182c-1182e 1182f-11837 11838 11839-1183a 118a0-118df 118e0-118e9 118ff-11906 11909 1190c-11913 11915-11916 11918-1192f 11930-11935 11937-11938 1193b-1193c 1193d 1193e 1193f 11940 11941 11942 11943 11950-11959 119a0-119a7 119aa-119d0 119d1-119d3 119d4-119d7 119da-119db 119dc-119df 119e0 119e1 119e3 119e4 11a00 11a01-11a0a 11a0b-11a32 11a33-11a38 11a39 11a3a 11a3b-11a3e 11a47 11a50 11a51-11a56 11a57-11a58 11a59-11a5b 11a5c-11a89 11a8a-11a96 11a97 11a98-11a99 11a9d 11ab0-11af8 11bc0-11be0 11bf0-11bf9 11c00-11c08 11c0a-11c2e 11c2f 11c30-11c36 11c38-11c3d 11c3e 11c3f 11c40 11c50-11c59 11c72-11c8f 11c92-11ca7 11ca9 11caa-11cb0 11cb1 11cb2-11cb3 11cb4 11cb5-11cb6 11d00-11d06 11d08-11d09 11d0b-11d30 11d31-11d36 11d3a 11d3c-11d3d 11d3f-11d45 11d46 11d47 11d50-11d59 11d60-11d65 11d67-11d68 11d6a-11d89 11d8a-11d8e 11d90-11d91 11d93-11d94 11d95 11d96 11d97 11d98 11da0-11da9 11ee0-11ef2 11ef3-11ef4 11ef5-11ef6 11f00-11f01 11f02 11f03 11f04-11f10 11f12-11f33 11f34-11f35 11f36-11f3a 11f3e-11f3f 11f40 11f41 11f42 11f50-11f59 11f5a 11fb0 12000-12399 12400-1246e 12480-12543 12f90-12ff0 13000-1342f 13440 13441-13446 13447-13455 13460-143fa 14400-14646 16100-1611d 1611e-16129 1612a-1612c 1612d-1612f 16130-16139 16800-16a38 16a40-16a5e 16a60-16a69 16a70-16abe 16ac0-16ac9 16ad0-16aed 16af0-16af4 16b00-16b2f 16b30-16b36 16b40-16b43 16b50-16b59 16b63-16b77 16b7d-16b8f 16d40-16d42 16d43-16d6a 16d6b-16d6c 16d70-16d79 16e40-16e7f 16f00-16f4a 16f4f 16f50 16f51-16f87 16f8f-16f92 16f93-16f9f 16fe0-16fe1 16fe3 16fe4 16ff0-16ff1 17000-187f7 18800-18cd5 18cff-18d08 1aff0-1aff3 1aff5-1affb 1affd-1affe 1b000-1b122 1b132 1b150-1b152 1b155 1b164-1b167 1b170-1b2fb 1bc00-1bc6a 1bc70-1bc7c 1bc80-1bc88 1bc90-1bc99 1bc9d-1bc9e 1ccf0-1ccf9 1cf00-1cf2d 1cf30-1cf46 1d165-1d166 1d167-1d169 1d16d-1d172 1d17b-1d182 1d185-1d18b 1d1aa-1d1ad 1d242-1d244 1d400-1d454 1d456-1d49c 1d49e-1d49f 1d4a2 1d4a5-1d4a6 1d4a9-1d4ac 1d4ae-1d4b9 1d4bb 1d4bd-1d4c3 1d4c5-1d505 1d507-1d50a 1d50d-1d514 1d516-1d51c 1d51e-1d539 1d53b-1d53e 1d540-1d544 1d546 1d54a-1d550 1d552-1d6a5 1d6a8-1d6c0 1d6c2-1d6da 1d6dc-1d6fa 1d6fc-1d714 1d716-1d734 1d736-1d74e 1d750-1d76e 1d770-1d788 1d78a-1d7a8 1d7aa-1d7c2 1d7c4-1d7cb 1d7ce-1d7ff 1da00-1da36 1da3b-1da6c 1da75 1da84 1da9b-1da9f 1daa1-1daaf 1df00-1df09 1df0a 1df0b-1df1e 1df25-1df2a 1e000-1e006 1e008-1e018 1e01b-1e021 1e023-1e024 1e026-1e02a 1e030-1e06d 1e08f 1e100-1e12c 1e130-1e136 1e137-1e13d 1e140-1e149 1e14e 1e290-1e2ad 1e2ae 1e2c0-1e2eb 1e2ec-1e2ef 1e2f0-1e2f9 1e4d0-1e4ea 1e4eb 1e4ec-1e4ef 1e4f0-1e4f9 1e5d0-1e5ed 1e5ee-1e5ef 1e5f0 1e5f1-1e5fa 1e7e0-1e7e6 1e7e8-1e7eb 1e7ed-1e7ee 1e7f0-1e7fe 1e800-1e8c4 1e8d0-1e8d6 1e900-1e943 1e944-1e94a 1e94b 1e950-1e959 1ee00-1ee03 1ee05-1ee1f 1ee21-1ee22 1ee24 1ee27 1ee29-1ee32 1ee34-1ee37 1ee39 1ee3b 1ee42 1ee47 1ee49 1ee4b 1ee4d-1ee4f 1ee51-1ee52 1ee54 1ee57 1ee59 1ee5b 1ee5d 1ee5f 1ee61-1ee62 1ee64 1ee67-1ee6a 1ee6c-1ee72 1ee74-1ee77 1ee79-1ee7c 1ee7e 1ee80-1ee89 1ee8b-1ee9b 1eea1-1eea3 1eea5-1eea9 1eeab-1eebb 1fbf0-1fbf9 20000-2a6df 2a700-2b739 2b740-2b81d 2b820-2cea1 2ceb0-2ebe0 2ebf0-2ee5d 2f800-2fa1d 30000-3134a 31350-323af e0100-e01ef"))

(defparameter +grapheme-base-ranges+ (decode-code-point-ranges
    "0020 0021-0023 0024 0025-0027 0028 0029 002a 002b 002c 002d 002e-002f 0030-0039 003a-003b 003c-003e 003f-0040 0041-005a 005b 005c 005d 005e 005f 0060 0061-007a 007b 007c 007d 007e 00a0 00a1 00a2-00a5 00a6 00a7 00a8 00a9 00aa 00ab 00ac 00ae 00af 00b0 00b1 00b2-00b3 00b4 00b5 00b6-00b7 00b8 00b9 00ba 00bb 00bc-00be 00bf 00c0-00d6 00d7 00d8-00f6 00f7 00f8-01ba 01bb 01bc-01bf 01c0-01c3 01c4-0293 0294 0295-02af 02b0-02c1 02c2-02c5 02c6-02d1 02d2-02df 02e0-02e4 02e5-02eb 02ec 02ed 02ee 02ef-02ff 0370-0373 0374 0375 0376-0377 037a 037b-037d 037e 037f 0384-0385 0386 0387 0388-038a 038c 038e-03a1 03a3-03f5 03f6 03f7-0481 0482 048a-052f 0531-0556 0559 055a-055f 0560-0588 0589 058a 058d-058e 058f 05be 05c0 05c3 05c6 05d0-05ea 05ef-05f2 05f3-05f4 0606-0608 0609-060a 060b 060c-060d 060e-060f 061b 061d-061f 0620-063f 0640 0641-064a 0660-0669 066a-066d 066e-066f 0671-06d3 06d4 06d5 06de 06e5-06e6 06e9 06ee-06ef 06f0-06f9 06fa-06fc 06fd-06fe 06ff 0700-070d 0710 0712-072f 074d-07a5 07b1 07c0-07c9 07ca-07ea 07f4-07f5 07f6 07f7-07f9 07fa 07fe-07ff 0800-0815 081a 0824 0828 0830-083e 0840-0858 085e 0860-086a 0870-0887 0888 0889-088e 08a0-08c8 08c9 0903 0904-0939 093b 093d 093e-0940 0949-094c 094e-094f 0950 0958-0961 0964-0965 0966-096f 0970 0971 0972-0980 0982-0983 0985-098c 098f-0990 0993-09a8 09aa-09b0 09b2 09b6-09b9 09bd 09bf-09c0 09c7-09c8 09cb-09cc 09ce 09dc-09dd 09df-09e1 09e6-09ef 09f0-09f1 09f2-09f3 09f4-09f9 09fa 09fb 09fc 09fd 0a03 0a05-0a0a 0a0f-0a10 0a13-0a28 0a2a-0a30 0a32-0a33 0a35-0a36 0a38-0a39 0a3e-0a40 0a59-0a5c 0a5e 0a66-0a6f 0a72-0a74 0a76 0a83 0a85-0a8d 0a8f-0a91 0a93-0aa8 0aaa-0ab0 0ab2-0ab3 0ab5-0ab9 0abd 0abe-0ac0 0ac9 0acb-0acc 0ad0 0ae0-0ae1 0ae6-0aef 0af0 0af1 0af9 0b02-0b03 0b05-0b0c 0b0f-0b10 0b13-0b28 0b2a-0b30 0b32-0b33 0b35-0b39 0b3d 0b40 0b47-0b48 0b4b-0b4c 0b5c-0b5d 0b5f-0b61 0b66-0b6f 0b70 0b71 0b72-0b77 0b83 0b85-0b8a 0b8e-0b90 0b92-0b95 0b99-0b9a 0b9c 0b9e-0b9f 0ba3-0ba4 0ba8-0baa 0bae-0bb9 0bbf 0bc1-0bc2 0bc6-0bc8 0bca-0bcc 0bd0 0be6-0bef 0bf0-0bf2 0bf3-0bf8 0bf9 0bfa 0c01-0c03 0c05-0c0c 0c0e-0c10 0c12-0c28 0c2a-0c39 0c3d 0c41-0c44 0c58-0c5a 0c5d 0c60-0c61 0c66-0c6f 0c77 0c78-0c7e 0c7f 0c80 0c82-0c83 0c84 0c85-0c8c 0c8e-0c90 0c92-0ca8 0caa-0cb3 0cb5-0cb9 0cbd 0cbe 0cc1 0cc3-0cc4 0cdd-0cde 0ce0-0ce1 0ce6-0cef 0cf1-0cf2 0cf3 0d02-0d03 0d04-0d0c 0d0e-0d10 0d12-0d3a 0d3d 0d3f-0d40 0d46-0d48 0d4a-0d4c 0d4e 0d4f 0d54-0d56 0d58-0d5e 0d5f-0d61 0d66-0d6f 0d70-0d78 0d79 0d7a-0d7f 0d82-0d83 0d85-0d96 0d9a-0db1 0db3-0dbb 0dbd 0dc0-0dc6 0dd0-0dd1 0dd8-0dde 0de6-0def 0df2-0df3 0df4 0e01-0e30 0e32-0e33 0e3f 0e40-0e45 0e46 0e4f 0e50-0e59 0e5a-0e5b 0e81-0e82 0e84 0e86-0e8a 0e8c-0ea3 0ea5 0ea7-0eb0 0eb2-0eb3 0ebd 0ec0-0ec4 0ec6 0ed0-0ed9 0edc-0edf 0f00 0f01-0f03 0f04-0f12 0f13 0f14 0f15-0f17 0f1a-0f1f 0f20-0f29 0f2a-0f33 0f34 0f36 0f38 0f3a 0f3b 0f3c 0f3d 0f3e-0f3f 0f40-0f47 0f49-0f6c 0f7f 0f85 0f88-0f8c 0fbe-0fc5 0fc7-0fcc 0fce-0fcf 0fd0-0fd4 0fd5-0fd8 0fd9-0fda 1000-102a 102b-102c 1031 1038 103b-103c 103f 1040-1049 104a-104f 1050-1055 1056-1057 105a-105d 1061 1062-1064 1065-1066 1067-106d 106e-1070 1075-1081 1083-1084 1087-108c 108e 108f 1090-1099 109a-109c 109e-109f 10a0-10c5 10c7 10cd 10d0-10fa 10fb 10fc 10fd-10ff 1100-1248 124a-124d 1250-1256 1258 125a-125d 1260-1288 128a-128d 1290-12b0 12b2-12b5 12b8-12be 12c0 12c2-12c5 12c8-12d6 12d8-1310 1312-1315 1318-135a 1360-1368 1369-137c 1380-138f 1390-1399 13a0-13f5 13f8-13fd 1400 1401-166c 166d 166e 166f-167f 1680 1681-169a 169b 169c 16a0-16ea 16eb-16ed 16ee-16f0 16f1-16f8 1700-1711 171f-1731 1735-1736 1740-1751 1760-176c 176e-1770 1780-17b3 17b6 17be-17c5 17c7-17c8 17d4-17d6 17d7 17d8-17da 17db 17dc 17e0-17e9 17f0-17f9 1800-1805 1806 1807-180a 1810-1819 1820-1842 1843 1844-1878 1880-1884 1887-18a8 18aa 18b0-18f5 1900-191e 1923-1926 1929-192b 1930-1931 1933-1938 1940 1944-1945 1946-194f 1950-196d 1970-1974 1980-19ab 19b0-19c9 19d0-19d9 19da 19de-19ff 1a00-1a16 1a19-1a1a 1a1e-1a1f 1a20-1a54 1a55 1a57 1a61 1a63-1a64 1a6d-1a72 1a80-1a89 1a90-1a99 1aa0-1aa6 1aa7 1aa8-1aad 1b04 1b05-1b33 1b3e-1b41 1b45-1b4c 1b4e-1b4f 1b50-1b59 1b5a-1b60 1b61-1b6a 1b74-1b7c 1b7d-1b7f 1b82 1b83-1ba0 1ba1 1ba6-1ba7 1bae-1baf 1bb0-1bb9 1bba-1be5 1be7 1bea-1bec 1bee 1bfc-1bff 1c00-1c23 1c24-1c2b 1c34-1c35 1c3b-1c3f 1c40-1c49 1c4d-1c4f 1c50-1c59 1c5a-1c77 1c78-1c7d 1c7e-1c7f 1c80-1c8a 1c90-1cba 1cbd-1cbf 1cc0-1cc7 1cd3 1ce1 1ce9-1cec 1cee-1cf3 1cf5-1cf6 1cf7 1cfa 1d00-1d2b 1d2c-1d6a 1d6b-1d77 1d78 1d79-1d9a 1d9b-1dbf 1e00-1f15 1f18-1f1d 1f20-1f45 1f48-1f4d 1f50-1f57 1f59 1f5b 1f5d 1f5f-1f7d 1f80-1fb4 1fb6-1fbc 1fbd 1fbe 1fbf-1fc1 1fc2-1fc4 1fc6-1fcc 1fcd-1fcf 1fd0-1fd3 1fd6-1fdb 1fdd-1fdf 1fe0-1fec 1fed-1fef 1ff2-1ff4 1ff6-1ffc 1ffd-1ffe 2000-200a 2010-2015 2016-2017 2018 2019 201a 201b-201c 201d 201e 201f 2020-2027 202f 2030-2038 2039 203a 203b-203e 203f-2040 2041-2043 2044 2045 2046 2047-2051 2052 2053 2054 2055-205e 205f 2070 2071 2074-2079 207a-207c 207d 207e 207f 2080-2089 208a-208c 208d 208e 2090-209c 20a0-20c0 2100-2101 2102 2103-2106 2107 2108-2109 210a-2113 2114 2115 2116-2117 2118 2119-211d 211e-2123 2124 2125 2126 2127 2128 2129 212a-212d 212e 212f-2134 2135-2138 2139 213a-213b 213c-213f 2140-2144 2145-2149 214a 214b 214c-214d 214e 214f 2150-215f 2160-2182 2183-2184 2185-2188 2189 218a-218b 2190-2194 2195-2199 219a-219b 219c-219f 21a0 21a1-21a2 21a3 21a4-21a5 21a6 21a7-21ad 21ae 21af-21cd 21ce-21cf 21d0-21d1 21d2 21d3 21d4 21d5-21f3 21f4-22ff 2300-2307 2308 2309 230a 230b 230c-231f 2320-2321 2322-2328 2329 232a 232b-237b 237c 237d-239a 239b-23b3 23b4-23db 23dc-23e1 23e2-2429 2440-244a 2460-249b 249c-24e9 24ea-24ff 2500-25b6 25b7 25b8-25c0 25c1 25c2-25f7 25f8-25ff 2600-266e 266f 2670-2767 2768 2769 276a 276b 276c 276d 276e 276f 2770 2771 2772 2773 2774 2775 2776-2793 2794-27bf 27c0-27c4 27c5 27c6 27c7-27e5 27e6 27e7 27e8 27e9 27ea 27eb 27ec 27ed 27ee 27ef 27f0-27ff 2800-28ff 2900-2982 2983 2984 2985 2986 2987 2988 2989 298a 298b 298c 298d 298e 298f 2990 2991 2992 2993 2994 2995 2996 2997 2998 2999-29d7 29d8 29d9 29da 29db 29dc-29fb 29fc 29fd 29fe-2aff 2b00-2b2f 2b30-2b44 2b45-2b46 2b47-2b4c 2b4d-2b73 2b76-2b95 2b97-2bff 2c00-2c7b 2c7c-2c7d 2c7e-2ce4 2ce5-2cea 2ceb-2cee 2cf2-2cf3 2cf9-2cfc 2cfd 2cfe-2cff 2d00-2d25 2d27 2d2d 2d30-2d67 2d6f 2d70 2d80-2d96 2da0-2da6 2da8-2dae 2db0-2db6 2db8-2dbe 2dc0-2dc6 2dc8-2dce 2dd0-2dd6 2dd8-2dde 2e00-2e01 2e02 2e03 2e04 2e05 2e06-2e08 2e09 2e0a 2e0b 2e0c 2e0d 2e0e-2e16 2e17 2e18-2e19 2e1a 2e1b 2e1c 2e1d 2e1e-2e1f 2e20 2e21 2e22 2e23 2e24 2e25 2e26 2e27 2e28 2e29 2e2a-2e2e 2e2f 2e30-2e39 2e3a-2e3b 2e3c-2e3f 2e40 2e41 2e42 2e43-2e4f 2e50-2e51 2e52-2e54 2e55 2e56 2e57 2e58 2e59 2e5a 2e5b 2e5c 2e5d 2e80-2e99 2e9b-2ef3 2f00-2fd5 2ff0-2fff 3000 3001-3003 3004 3005 3006 3007 3008 3009 300a 300b 300c 300d 300e 300f 3010 3011 3012-3013 3014 3015 3016 3017 3018 3019 301a 301b 301c 301d 301e-301f 3020 3021-3029 3030 3031-3035 3036-3037 3038-303a 303b 303c 303d 303e-303f 3041-3096 309b-309c 309d-309e 309f 30a0 30a1-30fa 30fb 30fc-30fe 30ff 3105-312f 3131-318e 3190-3191 3192-3195 3196-319f 31a0-31bf 31c0-31e5 31ef 31f0-31ff 3200-321e 3220-3229 322a-3247 3248-324f 3250 3251-325f 3260-327f 3280-3289 328a-32b0 32b1-32bf 32c0-33ff 3400-4dbf 4dc0-4dff 4e00-a014 a015 a016-a48c a490-a4c6 a4d0-a4f7 a4f8-a4fd a4fe-a4ff a500-a60b a60c a60d-a60f a610-a61f a620-a629 a62a-a62b a640-a66d a66e a673 a67e a67f a680-a69b a69c-a69d a6a0-a6e5 a6e6-a6ef a6f2-a6f7 a700-a716 a717-a71f a720-a721 a722-a76f a770 a771-a787 a788 a789-a78a a78b-a78e a78f a790-a7cd a7d0-a7d1 a7d3 a7d5-a7dc a7f2-a7f4 a7f5-a7f6 a7f7 a7f8-a7f9 a7fa a7fb-a801 a803-a805 a807-a80a a80c-a822 a823-a824 a827 a828-a82b a830-a835 a836-a837 a838 a839 a840-a873 a874-a877 a880-a881 a882-a8b3 a8b4-a8c3 a8ce-a8cf a8d0-a8d9 a8f2-a8f7 a8f8-a8fa a8fb a8fc a8fd-a8fe a900-a909 a90a-a925 a92e-a92f a930-a946 a952 a95f a960-a97c a983 a984-a9b2 a9b4-a9b5 a9ba-a9bb a9be-a9bf a9c1-a9cd a9cf a9d0-a9d9 a9de-a9df a9e0-a9e4 a9e6 a9e7-a9ef a9f0-a9f9 a9fa-a9fe aa00-aa28 aa2f-aa30 aa33-aa34 aa40-aa42 aa44-aa4b aa4d aa50-aa59 aa5c-aa5f aa60-aa6f aa70 aa71-aa76 aa77-aa79 aa7a aa7b aa7d aa7e-aaaf aab1 aab5-aab6 aab9-aabd aac0 aac2 aadb-aadc aadd aade-aadf aae0-aaea aaeb aaee-aaef aaf0-aaf1 aaf2 aaf3-aaf4 aaf5 ab01-ab06 ab09-ab0e ab11-ab16 ab20-ab26 ab28-ab2e ab30-ab5a ab5b ab5c-ab5f ab60-ab68 ab69 ab6a-ab6b ab70-abbf abc0-abe2 abe3-abe4 abe6-abe7 abe9-abea abeb abec abf0-abf9 ac00-d7a3 d7b0-d7c6 d7cb-d7fb f900-fa6d fa70-fad9 fb00-fb06 fb13-fb17 fb1d fb1f-fb28 fb29 fb2a-fb36 fb38-fb3c fb3e fb40-fb41 fb43-fb44 fb46-fbb1 fbb2-fbc2 fbd3-fd3d fd3e fd3f fd40-fd4f fd50-fd8f fd92-fdc7 fdcf fdf0-fdfb fdfc fdfd-fdff fe10-fe16 fe17 fe18 fe19 fe30 fe31-fe32 fe33-fe34 fe35 fe36 fe37 fe38 fe39 fe3a fe3b fe3c fe3d fe3e fe3f fe40 fe41 fe42 fe43 fe44 fe45-fe46 fe47 fe48 fe49-fe4c fe4d-fe4f fe50-fe52 fe54-fe57 fe58 fe59 fe5a fe5b fe5c fe5d fe5e fe5f-fe61 fe62 fe63 fe64-fe66 fe68 fe69 fe6a-fe6b fe70-fe74 fe76-fefc ff01-ff03 ff04 ff05-ff07 ff08 ff09 ff0a ff0b ff0c ff0d ff0e-ff0f ff10-ff19 ff1a-ff1b ff1c-ff1e ff1f-ff20 ff21-ff3a ff3b ff3c ff3d ff3e ff3f ff40 ff41-ff5a ff5b ff5c ff5d ff5e ff5f ff60 ff61 ff62 ff63 ff64-ff65 ff66-ff6f ff70 ff71-ff9d ffa0-ffbe ffc2-ffc7 ffca-ffcf ffd2-ffd7 ffda-ffdc ffe0-ffe1 ffe2 ffe3 ffe4 ffe5-ffe6 ffe8 ffe9-ffec ffed-ffee fffc-fffd 10000-1000b 1000d-10026 10028-1003a 1003c-1003d 1003f-1004d 10050-1005d 10080-100fa 10100-10102 10107-10133 10137-1013f 10140-10174 10175-10178 10179-10189 1018a-1018b 1018c-1018e 10190-1019c 101a0 101d0-101fc 10280-1029c 102a0-102d0 102e1-102fb 10300-1031f 10320-10323 1032d-10340 10341 10342-10349 1034a 10350-10375 10380-1039d 1039f 103a0-103c3 103c8-103cf 103d0 103d1-103d5 10400-1044f 10450-1049d 104a0-104a9 104b0-104d3 104d8-104fb 10500-10527 10530-10563 1056f 10570-1057a 1057c-1058a 1058c-10592 10594-10595 10597-105a1 105a3-105b1 105b3-105b9 105bb-105bc 105c0-105f3 10600-10736 10740-10755 10760-10767 10780-10785 10787-107b0 107b2-107ba 10800-10805 10808 1080a-10835 10837-10838 1083c 1083f-10855 10857 10858-1085f 10860-10876 10877-10878 10879-1087f 10880-1089e 108a7-108af 108e0-108f2 108f4-108f5 108fb-108ff 10900-10915 10916-1091b 1091f 10920-10939 1093f 10980-109b7 109bc-109bd 109be-109bf 109c0-109cf 109d2-109ff 10a00 10a10-10a13 10a15-10a17 10a19-10a35 10a40-10a48 10a50-10a58 10a60-10a7c 10a7d-10a7e 10a7f 10a80-10a9c 10a9d-10a9f 10ac0-10ac7 10ac8 10ac9-10ae4 10aeb-10aef 10af0-10af6 10b00-10b35 10b39-10b3f 10b40-10b55 10b58-10b5f 10b60-10b72 10b78-10b7f 10b80-10b91 10b99-10b9c 10ba9-10baf 10c00-10c48 10c80-10cb2 10cc0-10cf2 10cfa-10cff 10d00-10d23 10d30-10d39 10d40-10d49 10d4a-10d4d 10d4e 10d4f 10d50-10d65 10d6e 10d6f 10d70-10d85 10d8e-10d8f 10e60-10e7e 10e80-10ea9 10ead 10eb0-10eb1 10ec2-10ec4 10f00-10f1c 10f1d-10f26 10f27 10f30-10f45 10f51-10f54 10f55-10f59 10f70-10f81 10f86-10f89 10fb0-10fc4 10fc5-10fcb 10fe0-10ff6 11000 11002 11003-11037 11047-1104d 11052-11065 11066-1106f 11071-11072 11075 11082 11083-110af 110b0-110b2 110b7-110b8 110bb-110bc 110be-110c1 110d0-110e8 110f0-110f9 11103-11126 1112c 11136-1113f 11140-11143 11144 11145-11146 11147 11150-11172 11174-11175 11176 11182 11183-111b2 111b3-111b5 111bf 111c1-111c4 111c5-111c8 111cd 111ce 111d0-111d9 111da 111db 111dc 111dd-111df 111e1-111f4 11200-11211 11213-1122b 1122c-1122e 11232-11233 11238-1123d 1123f-11240 11280-11286 11288 1128a-1128d 1128f-1129d 1129f-112a8 112a9 112b0-112de 112e0-112e2 112f0-112f9 11302-11303 11305-1130c 1130f-11310 11313-11328 1132a-11330 11332-11333 11335-11339 1133d 1133f 11341-11344 11347-11348 1134b-1134c 11350 1135d-11361 11362-11363 11380-11389 1138b 1138e 11390-113b5 113b7 113b9-113ba 113ca 113cc-113cd 113d1 113d3 113d4-113d5 113d7-113d8 11400-11434 11435-11437 11440-11441 11445 11447-1144a 1144b-1144f 11450-11459 1145a-1145b 1145d 1145f-11461 11480-114af 114b1-114b2 114b9 114bb-114bc 114be 114c1 114c4-114c5 114c6 114c7 114d0-114d9 11580-115ae 115b0-115b1 115b8-115bb 115be 115c1-115d7 115d8-115db 11600-1162f 11630-11632 1163b-1163c 1163e 11641-11643 11644 11650-11659 11660-1166c 11680-116aa 116ac 116ae-116af 116b8 116b9 116c0-116c9 116d0-116e3 11700-1171a 1171e 11720-11721 11726 11730-11739 1173a-1173b 1173c-1173e 1173f 11740-11746 11800-1182b 1182c-1182e 11838 1183b 118a0-118df 118e0-118e9 118ea-118f2 118ff-11906 11909 1190c-11913 11915-11916 11918-1192f 11931-11935 11937-11938 1193f 11940 11941 11942 11944-11946 11950-11959 119a0-119a7 119aa-119d0 119d1-119d3 119dc-119df 119e1 119e2 119e3 119e4 11a00 11a0b-11a32 11a39 11a3a 11a3f-11a46 11a50 11a57-11a58 11a5c-11a89 11a97 11a9a-11a9c 11a9d 11a9e-11aa2 11ab0-11af8 11b00-11b09 11bc0-11be0 11be1 11bf0-11bf9 11c00-11c08 11c0a-11c2e 11c2f 11c3e 11c40 11c41-11c45 11c50-11c59 11c5a-11c6c 11c70-11c71 11c72-11c8f 11ca9 11cb1 11cb4 11d00-11d06 11d08-11d09 11d0b-11d30 11d46 11d50-11d59 11d60-11d65 11d67-11d68 11d6a-11d89 11d8a-11d8e 11d93-11d94 11d96 11d98 11da0-11da9 11ee0-11ef2 11ef5-11ef6 11ef7-11ef8 11f02 11f03 11f04-11f10 11f12-11f33 11f34-11f35 11f3e-11f3f 11f43-11f4f 11f50-11f59 11fb0 11fc0-11fd4 11fd5-11fdc 11fdd-11fe0 11fe1-11ff1 11fff 12000-12399 12400-1246e 12470-12474 12480-12543 12f90-12ff0 12ff1-12ff2 13000-1342f 13441-13446 13460-143fa 14400-14646 16100-1611d 1612a-1612c 16130-16139 16800-16a38 16a40-16a5e 16a60-16a69 16a6e-16a6f 16a70-16abe 16ac0-16ac9 16ad0-16aed 16af5 16b00-16b2f 16b37-16b3b 16b3c-16b3f 16b40-16b43 16b44 16b45 16b50-16b59 16b5b-16b61 16b63-16b77 16b7d-16b8f 16d40-16d42 16d43-16d6a 16d6b-16d6c 16d6d-16d6f 16d70-16d79 16e40-16e7f 16e80-16e96 16e97-16e9a 16f00-16f4a 16f50 16f51-16f87 16f93-16f9f 16fe0-16fe1 16fe2 16fe3 17000-187f7 18800-18cd5 18cff-18d08 1aff0-1aff3 1aff5-1affb 1affd-1affe 1b000-1b122 1b132 1b150-1b152 1b155 1b164-1b167 1b170-1b2fb 1bc00-1bc6a 1bc70-1bc7c 1bc80-1bc88 1bc90-1bc99 1bc9c 1bc9f 1cc00-1ccef 1ccf0-1ccf9 1cd00-1ceb3 1cf50-1cfc3 1d000-1d0f5 1d100-1d126 1d129-1d164 1d16a-1d16c 1d183-1d184 1d18c-1d1a9 1d1ae-1d1ea 1d200-1d241 1d245 1d2c0-1d2d3 1d2e0-1d2f3 1d300-1d356 1d360-1d378 1d400-1d454 1d456-1d49c 1d49e-1d49f 1d4a2 1d4a5-1d4a6 1d4a9-1d4ac 1d4ae-1d4b9 1d4bb 1d4bd-1d4c3 1d4c5-1d505 1d507-1d50a 1d50d-1d514 1d516-1d51c 1d51e-1d539 1d53b-1d53e 1d540-1d544 1d546 1d54a-1d550 1d552-1d6a5 1d6a8-1d6c0 1d6c1 1d6c2-1d6da 1d6db 1d6dc-1d6fa 1d6fb 1d6fc-1d714 1d715 1d716-1d734 1d735 1d736-1d74e 1d74f 1d750-1d76e 1d76f 1d770-1d788 1d789 1d78a-1d7a8 1d7a9 1d7aa-1d7c2 1d7c3 1d7c4-1d7cb 1d7ce-1d7ff 1d800-1d9ff 1da37-1da3a 1da6d-1da74 1da76-1da83 1da85-1da86 1da87-1da8b 1df00-1df09 1df0a 1df0b-1df1e 1df25-1df2a 1e030-1e06d 1e100-1e12c 1e137-1e13d 1e140-1e149 1e14e 1e14f 1e290-1e2ad 1e2c0-1e2eb 1e2f0-1e2f9 1e2ff 1e4d0-1e4ea 1e4eb 1e4f0-1e4f9 1e5d0-1e5ed 1e5f0 1e5f1-1e5fa 1e5ff 1e7e0-1e7e6 1e7e8-1e7eb 1e7ed-1e7ee 1e7f0-1e7fe 1e800-1e8c4 1e8c7-1e8cf 1e900-1e943 1e94b 1e950-1e959 1e95e-1e95f 1ec71-1ecab 1ecac 1ecad-1ecaf 1ecb0 1ecb1-1ecb4 1ed01-1ed2d 1ed2e 1ed2f-1ed3d 1ee00-1ee03 1ee05-1ee1f 1ee21-1ee22 1ee24 1ee27 1ee29-1ee32 1ee34-1ee37 1ee39 1ee3b 1ee42 1ee47 1ee49 1ee4b 1ee4d-1ee4f 1ee51-1ee52 1ee54 1ee57 1ee59 1ee5b 1ee5d 1ee5f 1ee61-1ee62 1ee64 1ee67-1ee6a 1ee6c-1ee72 1ee74-1ee77 1ee79-1ee7c 1ee7e 1ee80-1ee89 1ee8b-1ee9b 1eea1-1eea3 1eea5-1eea9 1eeab-1eebb 1eef0-1eef1 1f000-1f02b 1f030-1f093 1f0a0-1f0ae 1f0b1-1f0bf 1f0c1-1f0cf 1f0d1-1f0f5 1f100-1f10c 1f10d-1f1ad 1f1e6-1f202 1f210-1f23b 1f240-1f248 1f250-1f251 1f260-1f265 1f300-1f3fa 1f3fb-1f3ff 1f400-1f6d7 1f6dc-1f6ec 1f6f0-1f6fc 1f700-1f776 1f77b-1f7d9 1f7e0-1f7eb 1f7f0 1f800-1f80b 1f810-1f847 1f850-1f859 1f860-1f887 1f890-1f8ad 1f8b0-1f8bb 1f8c0-1f8c1 1f900-1fa53 1fa60-1fa6d 1fa70-1fa7c 1fa80-1fa89 1fa8f-1fac6 1face-1fadc 1fadf-1fae9 1faf0-1faf8 1fb00-1fb92 1fb94-1fbef 1fbf0-1fbf9 20000-2a6df 2a700-2b739 2b740-2b81d 2b820-2cea1 2ceb0-2ebe0 2ebf0-2ee5d 2f800-2fa1d 30000-3134a 31350-323af"))

(defparameter +diacritic-ranges+ (decode-code-point-ranges
    "005e 0060 00a8 00af 00b4 00b7 00b8 02b0-02c1 02c2-02c5 02c6-02d1 02d2-02df 02e0-02e4 02e5-02eb 02ec 02ed 02ee 02ef-02ff 0300-034e 0350-0357 035d-0362 0374 0375 037a 0384-0385 0483-0487 0559 0591-05a1 05a3-05bd 05bf 05c1-05c2 05c4 064b-0652 0657-0658 06df-06e0 06e5-06e6 06ea-06ec 0730-074a 07a6-07b0 07eb-07f3 07f4-07f5 0818-0819 0898-089f 08c9 08ca-08d2 08e3-08fe 093c 094d 0951-0954 0971 09bc 09cd 0a3c 0a4d 0abc 0acd 0afd-0aff 0b3c 0b4d 0b55 0bcd 0c3c 0c4d 0cbc 0ccd 0d3b-0d3c 0d4d 0dca 0e3a 0e47-0e4c 0e4e 0eba 0ec8-0ecc 0f18-0f19 0f35 0f37 0f39 0f3e-0f3f 0f82-0f84 0f86-0f87 0fc6 1037 1039-103a 1063-1064 1069-106d 1087-108c 108d 108f 109a-109b 135d-135f 1714 1715 1734 17c9-17d3 17dd 1939-193b 1a60 1a75-1a7c 1a7f 1ab0-1abd 1abe 1ac1-1acb 1b34 1b44 1b6b-1b73 1baa 1bab 1be6 1bf2-1bf3 1c36-1c37 1c78-1c7d 1cd0-1cd2 1cd3 1cd4-1ce0 1ce1 1ce2-1ce8 1ced 1cf4 1cf7 1cf8-1cf9 1d2c-1d6a 1dc4-1dcf 1df5-1dff 1fbd 1fbf-1fc1 1fcd-1fcf 1fdd-1fdf 1fed-1fef 1ffd-1ffe 2cef-2cf1 2e2f 302a-302d 302e-302f 3099-309a 309b-309c 30fc a66f a67c-a67d a67f a69c-a69d a6f0-a6f1 a700-a716 a717-a71f a720-a721 a788 a789-a78a a7f8-a7f9 a806 a82c a8c4 a8e0-a8f1 a92b-a92d a92e a953 a9b3 a9c0 a9e5 aa7b aa7c aa7d aabf aac0 aac1 aac2 aaf6 ab5b ab5c-ab5f ab69 ab6a-ab6b abec abed fb1e fe20-fe2f ff3e ff40 ff70 ff9e-ff9f ffe3 102e0 10780-10785 10787-107b0 107b2-107ba 10a38-10a3a 10a3f 10ae5-10ae6 10d22-10d23 10d24-10d27 10d4e 10d69-10d6d 10efd-10eff 10f46-10f50 10f82-10f85 11046 11070 110b9-110ba 11133-11134 11173 111c0 111ca-111cc 11235 11236 112e9-112ea 1133b-1133c 1134d 11366-1136c 11370-11374 113ce 113cf 113d0 113d2 113d3 113e1-113e2 11442 11446 114c2-114c3 115bf-115c0 1163f 116b6 116b7 1172b 11839-1183a 1193d 1193e 11943 119e0 11a34 11a47 11a99 11c3f 11d42 11d44-11d45 11d97 11f41 11f42 11f5a 13447-13455 1612f 16af0-16af4 16b30-16b36 16d6b-16d6c 16f8f-16f92 16f93-16f9f 16ff0-16ff1 1aff0-1aff3 1aff5-1affb 1affd-1affe 1cf00-1cf2d 1cf30-1cf46 1d167-1d169 1d16d-1d172 1d17b-1d182 1d185-1d18b 1d1aa-1d1ad 1e030-1e06d 1e130-1e136 1e2ae 1e2ec-1e2ef 1e5ee-1e5ef 1e8d0-1e8d6 1e944-1e946 1e948-1e94a"))

(defparameter +extender-ranges+ (decode-code-point-ranges
    "00b7 02d0-02d1 0640 07fa 0a71 0afb 0b55 0e46 0ec6 180a 1843 1aa7 1c36 1c7b 3005 3031-3035 309d-309e 30fc-30fe a015 a60c a9cf a9e6 aa70 aadd aaf3-aaf4 ff70 10781-10782 10d4e 10d6a 10d6f 11237 1135d 113d2 113d3 115c6-115c8 11a98 16b42-16b43 16fe0-16fe1 16fe3 1e13c-1e13d 1e5ef 1e944-1e946"))

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

(defparameter +bidi-control-ranges+ '((#x061c . #x061c) (#x200e . #x200f) (#x202a . #x202e) (#x2066 . #x2069)))

(defparameter +deprecated-ranges+ '((#x0149 . #x0149)
    (#x0673 . #x0673)
    (#x0f77 . #x0f77)
    (#x0f79 . #x0f79)
    (#x17a3 . #x17a4)
    (#x206a . #x206f)
    (#x2329 . #x232a)
    (#xe0001 . #xe0001)))

(defparameter +ids-binary-operator-ranges+ '((#x2ff0 . #x2ff1) (#x2ff4 . #x2ffd) (#x31ef . #x31ef)))

(defparameter +pattern-white-space-ranges+ '((#x0009 . #x000d)
    (#x0020 . #x0020)
    (#x0085 . #x0085)
    (#x200e . #x200f)
    (#x2028 . #x2029)))

(defparameter +variation-selector-ranges+ '((#x180b . #x180d) (#x180f . #x180f) (#xfe00 . #xfe0f) (#xe0100 . #xe01ef)))

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

(defparameter +unicode-extra-binary-property-aliases+
  '(("INDICCONJUNCTBREAK" . "INCB")
    ("MCM" . "MODIFIERCOMBININGMARK")
    ("OALPHA" . "OTHERALPHABETIC")
    ("ODI" . "OTHERDEFAULTIGNORABLECODEPOINT")
    ("OIDC" . "OTHERIDCONTINUE")
    ("OIDS" . "OTHERIDSTART")
    ("OLOWER" . "OTHERLOWERCASE")
    ("OMATH" . "OTHERMATH")
    ("OUPPER" . "OTHERUPPERCASE")))

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
