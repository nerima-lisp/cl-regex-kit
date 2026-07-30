(in-package #:cl-regex-kit)

(defun range-matches-p (ranges character)
  (let ((code (char-code character)))
    (some
      (lambda (range)
        (<= (car range) code (cdr range)))
      ranges)))

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

(defun age-property-p (value character)
  (let* ((version
        (if (and (> (length value) 0) (char= (char value 0) #\V)) (subseq value 1)
          value))
         (separator
        (position-if
          (lambda (item)
            (member item '(#\. #\_)))
          version))
         (major (subseq version 0 separator)))
    (and
      (plusp (length major))
      (= (parse-integer major) (sb-unicode:age character)))))

(defun unicode-property-p (name character)
  (let* ((raw-name (string-upcase name))
         (raw-property (normalized-property-name raw-name))
         (property
        (or (property-value raw-property '("GC=" "GENERALCATEGORY=")) raw-property))
         (script (property-value raw-property '("SC=" "SCRIPT=")))
         (block (or
          (property-value raw-property '("BLK=" "BLOCK="))
          (and
            (> (length raw-property) 2)
            (string= raw-property "IN" :end1 2 :end2 2)
            (subseq raw-property 2))))
         (age (property-value raw-name '("AGE=")))
         (category (string-upcase (symbol-name (sb-unicode:general-category character))))
         (major-category (subseq category 0 1)))
    (cond
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
      ((member property '("ASSIGNED") :test #'string=) (not (string= category "CN")))
      ((member property '("ALPHABETIC" "ALPHA") :test #'string=)
        (sb-unicode:alphabetic-p character))
      ((member property '("LOWERCASE" "LOWER") :test #'string=)
        (sb-unicode:lowercase-p character))
      ((member property '("UPPERCASE" "UPPER") :test #'string=)
        (sb-unicode:uppercase-p character))
      ((member property '("WHITESPACE" "WHITE_SPACE" "SPACE") :test #'string=)
        (sb-unicode:whitespace-p character))
      ((member property '("DECIMALNUMBER" "DIGIT") :test #'string=)
        (string= category "ND"))
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

(defun matcher-matches-p (matcher character)
  (case (first matcher)
    (:ranges (range-matches-p (second matcher) character))
    (:property (unicode-property-p (second matcher) character))
    (:union
      (some
        (lambda (part)
          (matcher-matches-p part character))
        (rest matcher)))
    (:intersection
      (and
        (matcher-matches-p (second matcher) character)
        (matcher-matches-p (third matcher) character)))
    (:difference
      (and
        (matcher-matches-p (second matcher) character)
        (not (matcher-matches-p (third matcher) character))))
    (:negate (not (matcher-matches-p (second matcher) character)))
    (otherwise (error "Unknown character-class matcher: ~S" matcher))))

(defun class-matches-p (node character)
  (labels ((matches-character-p (candidate)
             (or
          (and
            (char-class-node-matcher node)
            (matcher-matches-p (char-class-node-matcher node) candidate))
          (range-matches-p (char-class-node-ranges node) candidate))))
    (let* ((candidates
          (if (char-class-node-case-insensitive-p node) (remove-duplicates
              (list character (char-downcase character) (char-upcase character))
              :test
              #'char=)
            (list character)))
           (matches-p (some #'matches-character-p candidates)))
      (if (char-class-node-negated-p node) (not matches-p)
        matches-p))))

(defun word-character-p (character unicode-p)
  (and
    character
    (if unicode-p (let ((category (string-upcase (symbol-name (sb-unicode:general-category character)))))
        (or
          (sb-unicode:alphabetic-p character)
          (member category '("MN" "MC" "ME" "ND" "PC") :test #'string=)
          (member (char-code character) '(#x200c #x200d))))
      (or
        (and (char<= #\a character #\z))
        (and (char<= #\A character #\Z))
        (and (char<= #\0 character #\9))
        (char= character #\_)))))

(defun word-boundary-p (text position unicode-p)
  (not
    (eq
      (word-character-p (and (> position 0) (char text (1- position))) unicode-p)
      (word-character-p
        (and (< position (length text)) (char text position))
        unicode-p))))

(defun word-start-p (text position unicode-p)
  (and
    (not
      (word-character-p (and (> position 0) (char text (1- position))) unicode-p))
    (word-character-p
      (and (< position (length text)) (char text position))
      unicode-p)))

(defun word-end-p (text position unicode-p)
  (and
    (word-character-p (and (> position 0) (char text (1- position))) unicode-p)
    (not
      (word-character-p
        (and (< position (length text)) (char text position))
        unicode-p))))

(defun word-start-half-p (text position unicode-p)
  (not
    (word-character-p (and (> position 0) (char text (1- position))) unicode-p)))

(defun word-end-half-p (text position unicode-p)
  (not
    (word-character-p
      (and (< position (length text)) (char text position))
      unicode-p)))

(defun line-start-p (text position crlf-p)
  (if crlf-p (or
      (and
        (> position 0)
        (char= (char text (1- position)) #\Return)
        (or (= position (length text)) (not (char= (char text position) #\Newline))))
      (and (> position 0) (char= (char text (1- position)) #\Newline)))
    (and (> position 0) (char= (char text (1- position)) #\Newline))))

(defun line-end-p (text position crlf-p)
  (if crlf-p (or
      (and (< position (length text)) (char= (char text position) #\Return))
      (and
        (< position (length text))
        (char= (char text position) #\Newline)
        (or (= position 0) (not (char= (char text (1- position)) #\Return)))))
    (and (< position (length text)) (char= (char text position) #\Newline))))
