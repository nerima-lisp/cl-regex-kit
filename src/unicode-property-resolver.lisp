(in-package #:cl-regex-kit)

(defun pack-unicode-property-ranges (ranges)
  (let ((packed (make-array (* 2 (length ranges)) :element-type '(unsigned-byte 32)))
        (previous-upper nil))
    (loop for range in ranges
          for index from 0 by 2
          for lower = (car range)
          for upper = (if (consp (cdr range)) (cadr range)
        (cdr range))
          do (unless (and
          (integerp lower)
          (integerp upper)
          (<= 0 lower upper)
          (< upper char-code-limit)
          (or (null previous-upper) (< previous-upper lower)))
        (error "Invalid Unicode property range: ~S" range)) (setf (aref packed index) lower
            (aref packed (1+ index)) upper
            previous-upper upper))
    packed))

(defun make-unicode-property-descriptor (kind &optional payload)
  (%make-unicode-property-descriptor kind payload))

(defun range-property-descriptor (ranges)
  (and
    ranges
    (make-unicode-property-descriptor :ranges (pack-unicode-property-ranges ranges))))

(defun changes-when-case-mapped-p (character)
  (or
    (changes-under-case-mapping-p #'sb-unicode:lowercase character)
    (changes-under-case-mapping-p #'sb-unicode:titlecase character)
    (changes-under-case-mapping-p #'sb-unicode:uppercase character)))

(defun changes-when-lowercased-p (character)
  (changes-under-case-mapping-p #'sb-unicode:lowercase character))

(defun changes-when-titlecased-p (character)
  (changes-under-case-mapping-p #'sb-unicode:titlecase character))

(defun changes-when-uppercased-p (character)
  (changes-under-case-mapping-p #'sb-unicode:uppercase character))

(defparameter +unicode-property-predicates+ '(("ALPHABETIC" . sb-unicode:alphabetic-p)
    ("ALPHA" . sb-unicode:alphabetic-p)
    ("LOWERCASE" . sb-unicode:lowercase-p)
    ("LOWER" . sb-unicode:lowercase-p)
    ("UPPERCASE" . sb-unicode:uppercase-p)
    ("UPPER" . sb-unicode:uppercase-p)
    ("WHITESPACE" . sb-unicode:whitespace-p)
    ("SPACE" . sb-unicode:whitespace-p)
    ("WSPACE" . sb-unicode:whitespace-p)
    ("ASCIIHEXDIGIT" . ascii-hex-digit-p)
    ("AHEX" . ascii-hex-digit-p)
    ("HEXDIGIT" . sb-unicode:hex-digit-p)
    ("HEX" . sb-unicode:hex-digit-p)
    ("CASED" . sb-unicode:cased-p)
    ("CASEIGNORABLE" . sb-unicode:case-ignorable-p)
    ("CI" . sb-unicode:case-ignorable-p)
    ("CHANGESWHENCASEFOLDED" . changes-when-casefolded-p)
    ("CWCF" . changes-when-casefolded-p)
    ("CHANGESWHENCASEMAPPED" . changes-when-case-mapped-p)
    ("CWCM" . changes-when-case-mapped-p)
    ("CHANGESWHENLOWERCASED" . changes-when-lowercased-p)
    ("CWL" . changes-when-lowercased-p)
    ("CHANGESWHENTITLECASED" . changes-when-titlecased-p)
    ("CWT" . changes-when-titlecased-p)
    ("CHANGESWHENUPPERCASED" . changes-when-uppercased-p)
    ("CWU" . changes-when-uppercased-p)
    ("DEFAULTIGNORABLECODEPOINT" . sb-unicode:default-ignorable-p)
    ("DI" . sb-unicode:default-ignorable-p)
    ("IDEOGRAPHIC" . sb-unicode:ideographic-p)
    ("IDEO" . sb-unicode:ideographic-p)
    ("MATH" . sb-unicode:math-p)
    ("SOFTDOTTED" . sb-unicode:soft-dotted-p)
    ("SD" . sb-unicode:soft-dotted-p)
    ("GRAPHEMEEXTEND" . grapheme-extend-p)
    ("GREXT" . grapheme-extend-p)
    ("BIDIMIRRORED" . sb-unicode:mirrored-p)
    ("BIDIM" . sb-unicode:mirrored-p)
    ("JOINCONTROL" . join-control-p)
    ("JOINC" . join-control-p)
    ("NONCHARACTERCODEPOINT" . noncharacter-code-point-p)
    ("NCHAR" . noncharacter-code-point-p)
    ("IDSTART" . id-start-p)
    ("IDS" . id-start-p)
    ("IDCONTINUE" . id-continue-p)
    ("IDC" . id-continue-p)))

(defparameter +unicode-major-categories+ '(("L" . #\L)
    ("LETTER" . #\L)
    ("M" . #\M)
    ("MARK" . #\M)
    ("N" . #\N)
    ("NUMBER" . #\N)
    ("P" . #\P)
    ("PUNCTUATION" . #\P)
    ("PUNCT" . #\P)
    ("S" . #\S)
    ("SYMBOL" . #\S)
    ("Z" . #\Z)
    ("SEPARATOR" . #\Z)
    ("C" . #\C)
    ("OTHER" . #\C)))

(defparameter +unicode-category-aliases+ '(("CONTROL" . "CC")
    ("FORMAT" . "CF")
    ("UNASSIGNED" . "CN")
    ("PRIVATEUSE" . "CO")
    ("SURROGATE" . "CS")
    ("UPPERCASELETTER" . "LU")
    ("LOWERCASELETTER" . "LL")
    ("TITLECASELETTER" . "LT")
    ("MODIFIERLETTER" . "LM")
    ("OTHERLETTER" . "LO")
    ("NONSPACINGMARK" . "MN")
    ("SPACINGMARK" . "MC")
    ("ENCLOSINGMARK" . "ME")
    ("DECIMALNUMBER" . "ND")
    ("DIGIT" . "ND")
    ("DECIMALDIGITNUMBER" . "ND")
    ("LETTERNUMBER" . "NL")
    ("OTHERNUMBER" . "NO")
    ("CONNECTORPUNCTUATION" . "PC")
    ("DASHPUNCTUATION" . "PD")
    ("OPENPUNCTUATION" . "PS")
    ("CLOSEPUNCTUATION" . "PE")
    ("INITIALPUNCTUATION" . "PI")
    ("FINALPUNCTUATION" . "PF")
    ("OTHERPUNCTUATION" . "PO")))

(progn
  (defun unicode-runtime-property-descriptor (kind domain name)
    (multiple-value-bind (value present-p)
        (unicode-runtime-property-value domain name)
      (and present-p
           (make-unicode-property-descriptor kind value))))

  (defun unicode-runtime-category-values (names)
    (mapcar (lambda (name)
              (multiple-value-bind (value present-p)
                  (unicode-runtime-property-value :category name)
                (unless present-p
                  (error "Unknown Unicode general category: ~A" name))
                value))
            names))

  (defun unicode-runtime-major-category-values (major-category)
    (unicode-runtime-category-values
      (remove-if-not
        (lambda (name)
          (char= major-category (char name 0)))
        (unicode-runtime-property-names :category))))

  (defun extra-binary-property-ranges (name)
    (cdr (assoc name +unicode-extra-binary-property-ranges+ :test (function string=)))))

(defun resolve-unicode-property (name)
  (let* ((raw-name (string-upcase name))
         (raw-property (normalized-property-name raw-name))
         (property
           (or (property-value raw-property (list "GC=" "GENERALCATEGORY="))
               raw-property))
         (script
           (canonical-script-name
             (property-value raw-property (list "SC=" "SCRIPT="))))
         (script-extension
           (canonical-script-name
             (property-value raw-property (list "SCX=" "SCRIPTEXTENSIONS="))))
         (block (known-block-property-name raw-property))
         (age (property-value raw-name (list "AGE=")))
         (grapheme-break
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
         (predicate
           (cdr (assoc property +unicode-property-predicates+ :test (function string=))))
         (major (cdr (assoc property +unicode-major-categories+ :test (function string=))))
         (category
           (or (cdr (assoc property +unicode-category-aliases+ :test (function string=)))
               (and (= (length property) 2) property)))
         (ranges
           (or (unicode-range-property-ranges property)
               (extra-binary-property-ranges property))))
    (cond
      (grapheme-break
       (unicode-runtime-property-descriptor
         :grapheme-break :grapheme-break grapheme-break))
      (word-break
       (unicode-runtime-property-descriptor :word-break :word-break word-break))
      (sentence-break
       (unicode-runtime-property-descriptor
         :sentence-break :sentence-break sentence-break))
      (script-extension
       (multiple-value-bind (value present-p)
           (unicode-runtime-property-value :script script-extension)
         (and present-p
              (make-unicode-property-descriptor
                :script-extension
                (cons value
                      (pack-unicode-property-ranges
                        (cdr (assoc script-extension
                                    +script-extension-extra-ranges+
                                    :test (function string=)))))))))
      (script
       (unicode-runtime-property-descriptor :script :script script))
      (block
       (unicode-runtime-property-descriptor :block :block block))
      (age
       (range-property-descriptor (age-property-ranges age)))
      ((string= property "ANY")
       (make-unicode-property-descriptor :any))
      ((string= property "ASCII")
       (make-unicode-property-descriptor :ascii))
      ((string= property "ASSIGNED")
       (make-unicode-property-descriptor
         :assigned
         (first (unicode-runtime-category-values (list "CN")))))
      (ranges
       (range-property-descriptor ranges))
      (predicate
       (make-unicode-property-descriptor :predicate predicate))
      (major
       (make-unicode-property-descriptor
         :major-category
         (unicode-runtime-major-category-values major)))
      ((member property (list "LC" "CASEDLETTER") :test (function string=))
       (make-unicode-property-descriptor
         :cased-letter
         (unicode-runtime-category-values (list "LU" "LL" "LT"))))
      (category
       (unicode-runtime-property-descriptor :category :category category))
      (t
       (unicode-runtime-property-descriptor :script :script property)))))
