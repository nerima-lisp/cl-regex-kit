(in-package #:cl-regex-kit)

;;; Matcher composition and boundary predicates. Static Unicode case-folding
;;; data lives in unicode-case-folding-data.lisp; property data lives in
;;; unicode-properties.lisp.
(defmacro define-set-matcher (name (matcher-var element-var) &key ranges property otherwise)
  "Define NAME as a character-class matcher evaluator over ELEMENT-VAR.

RANGES and PROPERTY are forms evaluated for the corresponding matcher leaves;
OTHERWISE is evaluated when MATCHER-VAR's operator is unrecognized. The
:union/:intersection/:difference/:symmetric-difference/:negate combinators
are boolean-algebra logic shared by every matcher domain, so they are
generated once here instead of once per domain."
  `(defun ,name (,matcher-var ,element-var)
     (case (first ,matcher-var)
       (:ranges ,ranges)
       (:property ,property)
       (:union
         (some (lambda (part) (,name part ,element-var)) (rest ,matcher-var)))
       (:intersection
         (every (lambda (part) (,name part ,element-var)) (rest ,matcher-var)))
       (:difference
         (and (,name (second ,matcher-var) ,element-var)
              (not (,name (third ,matcher-var) ,element-var))))
       (:symmetric-difference
         (not (eq (not (null (,name (second ,matcher-var) ,element-var)))
                  (not (null (,name (third ,matcher-var) ,element-var))))))
       (:negate (not (,name (second ,matcher-var) ,element-var)))
       (otherwise ,otherwise))))

(define-set-matcher matcher-matches-p (matcher character)
  :ranges (range-matches-p (second matcher) character)
  :property (unicode-property-p (second matcher) character)
  :otherwise (error "Unknown character-class matcher: ~S" matcher))


(defun unicode-simple-case-fold-candidates (character)
  (cons character (gethash character *unicode-simple-case-folding*)))

(defun unicode-case-insensitive-char= (left right)
  "Return true when LEFT and RIGHT share a Unicode simple case-fold class."
  (member right (unicode-simple-case-fold-candidates left) :test #'char=))

(defun ascii-case-insensitive-char= (left right)
  "Return true when LEFT and RIGHT are equal under ASCII case folding."
  (or (char= left right)
      (and (alpha-char-p left)
           (alpha-char-p right)
           (< (char-code left) 128)
           (< (char-code right) 128)
           (char= (char-downcase left) (char-downcase right)))))

(defun case-insensitive-class-candidates (character)
  "Return Unicode simple case-fold variants for range and property tests."
  (unicode-simple-case-fold-candidates character))

(defun ascii-case-insensitive-class-candidates (character)
  "Return ASCII case variants of CHARACTER suitable for class tests."
  (if (and (alpha-char-p character) (< (char-code character) 128))
      (remove-duplicates
       (list character (char-downcase character) (char-upcase character))
       :test #'char=)
      (list character)))

(defun class-matches-p (node character)
  (labels ((matches-character-p (candidate)
             (or
          (and
            (char-class-node-matcher node)
            (matcher-matches-p (char-class-node-matcher node) candidate))
          (range-matches-p (char-class-node-ranges node) candidate))))
    (let* ((candidates
          (if (char-class-node-case-insensitive-p node)
              (if (char-class-node-unicode-p node)
                  (case-insensitive-class-candidates character)
                  (ascii-case-insensitive-class-candidates character))
              (list character)))
           (matches-p (some #'matches-character-p candidates)))
      (if (char-class-node-negated-p node) (not matches-p)
        matches-p))))

(defun ascii-fold-octet (octet)
  "Return OCTET folded to lower case when it is an ASCII letter."
  (if (or (<= #x41 octet #x5a))
      (+ octet #x20)
      octet))

(defun octet-range-bound (bound)
  "Return BOUND as an octet-compatible integer."
  (if (characterp bound)
      (char-code bound)
      bound))

(defun range-upper-bound (range)
  "Return the upper bound of RANGE in list or dotted-pair form."
  (let ((tail (cdr range)))
    (if (consp tail)
        (car tail)
        tail)))

(define-set-matcher byte-matcher-matches-p (matcher octet)
  :ranges (some (lambda (range)
                  (<= (octet-range-bound (car range))
                      octet
                      (octet-range-bound (range-upper-bound range))))
                (second matcher))
  :property nil
  :otherwise nil)

(defun class-matches-octet-p (node octet)
  "Return true when byte-mode class NODE accepts OCTET."
  (let* ((candidates
           (if (char-class-node-case-insensitive-p node)
               (remove-duplicates
                (list octet
                      (ascii-fold-octet octet)
                      (if (<= #x61 octet #x7a)
                          (- octet #x20)
                          octet)))
               (list octet)))
         (matches-p
           (some
            (lambda (candidate)
              (or (and (char-class-node-matcher node)
                       (byte-matcher-matches-p
                        (char-class-node-matcher node)
                        candidate))
                  (some (lambda (range)
                          (<= (octet-range-bound (car range))
                              candidate
                              (octet-range-bound
                               (range-upper-bound range))))
                        (char-class-node-ranges node))))
            candidates)))
    (if (char-class-node-negated-p node)
        (not matches-p)
        matches-p)))

(defmacro define-boundary-predicates (prefix lambda-list &body compute-neighbors)
  "Define PREFIX-BOUNDARY-P/-START-P/-END-P/-START-HALF-P/-END-HALF-P over
LAMBDA-LIST. COMPUTE-NEIGHBORS is evaluated once per call, in each generated
function, and must return (values LEFT-WORD-P RIGHT-WORD-P GUARD-P):
GUARD-P gates every predicate (true unless the domain has a precondition
narrower than \"in bounds\", as byte-mode Unicode boundaries do), and
LEFT-WORD-P/RIGHT-WORD-P are whether a word character precedes/follows the
position. The five predicates are the same boundary/start/end/half algebra
in every domain this file supports (ASCII bytes, Unicode-aware bytes,
strings) -- only how a domain decides \"is there a word character here\"
differs, which is exactly what COMPUTE-NEIGHBORS captures."
  (flet ((name (suffix) (intern (format nil "~A-~A" prefix suffix))))
    `(progn
       (defun ,(name "BOUNDARY-P") ,lambda-list
         (multiple-value-bind (left right guard-p) (progn ,@compute-neighbors)
           (and guard-p (not (eq left right)))))
       (defun ,(name "START-P") ,lambda-list
         (multiple-value-bind (left right guard-p) (progn ,@compute-neighbors)
           (and guard-p (not left) right)))
       (defun ,(name "END-P") ,lambda-list
         (multiple-value-bind (left right guard-p) (progn ,@compute-neighbors)
           (and guard-p left (not right))))
       (defun ,(name "START-HALF-P") ,lambda-list
         (multiple-value-bind (left right guard-p) (progn ,@compute-neighbors)
           (declare (ignore right))
           (and guard-p (not left))))
       (defun ,(name "END-HALF-P") ,lambda-list
         (multiple-value-bind (left right guard-p) (progn ,@compute-neighbors)
           (declare (ignore left))
           (and guard-p (not right)))))))

(defun byte-word-character-p (octet)
  "Return true when OCTET is an ASCII word character."
  (and octet
       (or (<= #x61 octet #x7a) (<= #x41 octet #x5a)
           (<= #x30 octet #x39) (= octet #x5f))))

(define-boundary-predicates byte-word (text position)
  (values (byte-word-character-p (and (> position 0) (aref text (1- position))))
          (byte-word-character-p (and (< position (length text)) (aref text position)))
          t))

(defun utf8-continuation-octet-p (octet)
  (and octet (<= #x80 octet #xbf)))

(defun utf8-character-at (text position)
  "Decode one valid UTF-8 scalar at POSITION in octet-vector TEXT.
Returns CHARACTER, the exclusive end index, and true; otherwise three NILs."
  (let ((length (length text)))
    (when (< position length)
      (let ((first (aref text position)))
        (cond
          ((<= first #x7f) (values (code-char first) (1+ position) t))
          ((and (<= #xc2 first #xdf) (< (+ position 1) length))
           (let ((second (aref text (1+ position))))
             (when (utf8-continuation-octet-p second)
               (values (code-char (+ (ash (logand first #x1f) 6)
                                    (logand second #x3f)))
                       (+ position 2) t))))
          ((and (<= #xe0 first #xef) (< (+ position 2) length))
           (let ((second (aref text (1+ position)))
                 (third (aref text (+ position 2))))
             (when (and (utf8-continuation-octet-p second)
                        (utf8-continuation-octet-p third)
                        (not (and (= first #xe0) (< second #xa0)))
                        (not (and (= first #xed) (> second #x9f))))
               (values (code-char (+ (ash (logand first #x0f) 12)
                                    (ash (logand second #x3f) 6)
                                    (logand third #x3f)))
                       (+ position 3) t))))
          ((and (<= #xf0 first #xf4) (< (+ position 3) length))
           (let ((second (aref text (1+ position)))
                 (third (aref text (+ position 2)))
                 (fourth (aref text (+ position 3))))
             (when (and (utf8-continuation-octet-p second)
                        (utf8-continuation-octet-p third)
                        (utf8-continuation-octet-p fourth)
                        (not (and (= first #xf0) (< second #x90)))
                        (not (and (= first #xf4) (> second #x8f))))
               (values (code-char (+ (ash (logand first #x07) 18)
                                    (ash (logand second #x3f) 12)
                                    (ash (logand third #x3f) 6)
                                    (logand fourth #x3f)))
                       (+ position 4) t)))))))))

(defun utf8-character-before (text position)
  "Decode the valid UTF-8 scalar ending at POSITION, if any."
  (loop for beginning from (max 0 (- position 4)) below position do
    (multiple-value-bind (character end valid-p)
        (utf8-character-at text beginning)
      (when (and valid-p (= end position))
        (return (values character beginning t))))
    finally (return (values nil nil nil))))

(defun byte-unicode-non-boundary-position-p (text position)
  "Return true when Unicode \\B can evaluate without splitting UTF-8."
  (and (or (= position 0)
           (nth-value 2 (utf8-character-before text position)))
       (or (= position (length text))
           (nth-value 2 (utf8-character-at text position)))))

(defun byte-unicode-boundary-characters (text position)
  "Return surrounding Unicode characters and whether POSITION is a UTF-8 boundary."
  (multiple-value-bind (left ignored-left valid-left)
      (if (= position 0)
          (values nil nil nil)
          (utf8-character-before text position))
    (declare (ignore ignored-left valid-left))
    (multiple-value-bind (right ignored-right valid-right)
        (if (= position (length text))
            (values nil nil nil)
            (utf8-character-at text position))
      (declare (ignore ignored-right valid-right))
      (values left right
              (or (= position (length text))
                  (not (utf8-continuation-octet-p (aref text position))))))))

(define-boundary-predicates byte-unicode-word (text position)
  (multiple-value-bind (left right boundary-p) (byte-unicode-boundary-characters text position)
    (values (word-character-p left t) (word-character-p right t) boundary-p)))

(defun byte-line-start-p (text position crlf-p line-terminator)
  (let ((terminator (char-code line-terminator)))
    (if crlf-p
        (or (and (> position 0) (= (aref text (1- position)) #x0d)
                 (or (= position (length text)) (/= (aref text position) #x0a)))
            (and (> position 0) (= (aref text (1- position)) #x0a)))
        (and (> position 0) (= (aref text (1- position)) terminator)))))

(defun byte-line-end-p (text position crlf-p line-terminator)
  (let ((terminator (char-code line-terminator)))
    (if crlf-p
        (or (and (< position (length text)) (= (aref text position) #x0d))
            (and (< position (length text)) (= (aref text position) #x0a)
                 (or (= position 0) (/= (aref text (1- position)) #x0d))))
        (and (< position (length text)) (= (aref text position) terminator)))))

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

(define-boundary-predicates word (text position unicode-p)
  (values (word-character-p (and (> position 0) (char text (1- position))) unicode-p)
          (word-character-p (and (< position (length text)) (char text position)) unicode-p)
          t))

(defun line-start-p (text position crlf-p line-terminator)
  (if crlf-p (or
      (and
        (> position 0)
        (char= (char text (1- position)) #\Return)
        (or (= position (length text)) (not (char= (char text position) #\Newline))))
      (and (> position 0) (char= (char text (1- position)) #\Newline)))
    (and (> position 0) (char= (char text (1- position)) line-terminator))))

(defun line-end-p (text position crlf-p line-terminator)
  (if crlf-p (or
      (and (< position (length text)) (char= (char text position) #\Return))
      (and
        (< position (length text))
        (char= (char text position) #\Newline)
        (or (= position 0) (not (char= (char text (1- position)) #\Return)))))
    (and (< position (length text)) (char= (char text position) line-terminator))))
