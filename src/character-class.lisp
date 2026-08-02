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
        (some
          (lambda (part)
            (,name part ,element-var))
          (rest ,matcher-var)))
      (:intersection
        (every
          (lambda (part)
            (,name part ,element-var))
          (rest ,matcher-var)))
      (:difference
        (and
          (,name (second ,matcher-var) ,element-var)
          (not (,name (third ,matcher-var) ,element-var))))
      (:symmetric-difference
        (not
          (eq
            (not (null (,name (second ,matcher-var) ,element-var)))
            (not (null (,name (third ,matcher-var) ,element-var))))))
      (:negate (not (,name (second ,matcher-var) ,element-var)))
      (otherwise ,otherwise))))

(define-set-matcher
  matcher-matches-p
  (matcher character)
  :ranges
  (range-matches-p (second matcher) character)
  :property
  (unicode-property-descriptor-matches-p (second matcher) character)
  :otherwise
  (error "Unknown character-class matcher: ~S" matcher))

(defun unicode-case-insensitive-char= (left right)
  "Return true when LEFT and RIGHT are equal under Unicode simple case folding."
  (or (char= left right)
      (member right
              (gethash left *unicode-simple-case-folding*)
              :test (function char=))))

(defun ascii-case-insensitive-char= (left right)
  "Return true when LEFT and RIGHT are equal under ASCII case folding."
  (or
    (char= left right)
    (and
      (alpha-char-p left)
      (alpha-char-p right)
      (< (char-code left) 128)
      (< (char-code right) 128)
      (char= (char-downcase left) (char-downcase right)))))

(defun class-matches-p (node character)
  (labels ((matches-character-p (candidate)
             (or (and (char-class-node-matcher node)
                      (matcher-matches-p (char-class-node-matcher node) candidate))
                 (range-matches-p (char-class-node-ranges node) candidate))))
    (let ((matches-p
            (or (matches-character-p character)
                (and (char-class-node-case-insensitive-p node)
                     (if (char-class-node-unicode-p node)
                         (some (function matches-character-p)
                               (gethash character *unicode-simple-case-folding*))
                         (and (alpha-char-p character)
                              (< (char-code character) 128)
                              (let ((lower (char-downcase character))
                                    (upper (char-upcase character)))
                                (or (and (not (char= lower character))
                                         (matches-character-p lower))
                                    (and (not (char= upper character))
                                         (not (char= upper lower))
                                         (matches-character-p upper))))))))))
      (if (char-class-node-negated-p node)
          (not matches-p)
          matches-p))))

(defun ascii-fold-octet (octet)
  "Return OCTET folded to lower case when it is an ASCII letter."
  (if (<= #x41 octet #x5a) (+ octet #x20)
    octet))

(defun octet-range-bound (bound)
  "Return BOUND as an octet-compatible integer."
  (if (characterp bound) (char-code bound)
    bound))

(defun range-upper-bound (range)
  "Return the upper bound of RANGE in list or dotted-pair form."
  (let ((tail (cdr range)))
    (if (consp tail) (car tail)
      tail)))

(define-set-matcher
  byte-matcher-matches-p
  (matcher octet)
  :ranges
  (some
    (lambda (range)
      (<=
        (octet-range-bound (car range))
        octet
        (octet-range-bound (range-upper-bound range))))
    (second matcher))
  :property
  nil
  :otherwise
  nil)

(defun class-matches-octet-p (node octet)
  "Return true when byte-mode class NODE accepts OCTET."
  (labels ((matches-octet-p (candidate)
             (or (and (char-class-node-matcher node)
                      (byte-matcher-matches-p
                        (char-class-node-matcher node) candidate))
                 (some (lambda (range)
                         (<= (octet-range-bound (car range))
                             candidate
                             (octet-range-bound (range-upper-bound range))))
                       (char-class-node-ranges node)))))
    (let* ((original-matches-p (matches-octet-p octet))
           (matches-p
             (or original-matches-p
                 (and (char-class-node-case-insensitive-p node)
                      (let ((lower (ascii-fold-octet octet))
                            (upper (if (<= #x61 octet #x7a)
                                       (- octet #x20)
                                       octet)))
                        (or (and (/= lower octet)
                                 (matches-octet-p lower))
                            (and (/= upper octet)
                                 (/= upper lower)
                                 (matches-octet-p upper))))))))
      (if (char-class-node-negated-p node) (not matches-p) matches-p))))
