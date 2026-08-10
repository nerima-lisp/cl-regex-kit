(in-package #:cl-regex-kit)

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
  (flet ((name (suffix)
           (intern (format nil "~A-~A" prefix suffix))))
    `(progn
      (defun ,(name "BOUNDARY-P") ,lambda-list
        (multiple-value-bind (left right guard-p) (progn
            ,@compute-neighbors)
          (and guard-p (not (eq left right)))))
      (defun ,(name "START-P") ,lambda-list
        (multiple-value-bind (left right guard-p) (progn
            ,@compute-neighbors)
          (and guard-p (not left) right)))
      (defun ,(name "END-P") ,lambda-list
        (multiple-value-bind (left right guard-p) (progn
            ,@compute-neighbors)
          (and guard-p left (not right))))
      (defun ,(name "START-HALF-P") ,lambda-list
        (multiple-value-bind (left right guard-p) (progn
            ,@compute-neighbors)
          (declare (ignore right))
          (and guard-p (not left))))
      (defun ,(name "END-HALF-P") ,lambda-list
        (multiple-value-bind (left right guard-p) (progn
            ,@compute-neighbors)
          (declare (ignore left))
          (and guard-p (not right)))))))

(defun byte-word-character-p (octet)
  "Return true when OCTET is an ASCII word character."
  (and
    octet
    (or
      (<= #x61 octet #x7a)
      (<= #x41 octet #x5a)
      (<= #x30 octet #x39)
      (= octet #x5f))))

(define-boundary-predicates
  byte-word
  (text position)
  (values
    (byte-word-character-p (and (plusp position) (aref text (1- position))))
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
          ((and (<= #xc2 first #xdf) (< (1+ position) length))
            (let ((second (aref text (1+ position))))
              (when (utf8-continuation-octet-p second)
                (values
                  (code-char (+ (ash (logand first #x1f) 6) (logand second #x3f)))
                  (+ position 2)
                  t))))
          ((and (<= #xe0 first #xef) (< (+ position 2) length))
            (let ((second (aref text (1+ position)))
                  (third (aref text (+ position 2))))
              (when (and
                  (utf8-continuation-octet-p second)
                  (utf8-continuation-octet-p third)
                  (not (and (= first #xe0) (< second #xa0)))
                  (not (and (= first #xed) (> second #x9f))))
                (values
                  (code-char
                    (+
                      (ash (logand first #x0f) 12)
                      (ash (logand second #x3f) 6)
                      (logand third #x3f)))
                  (+ position 3)
                  t))))
          ((and (<= #xf0 first #xf4) (< (+ position 3) length))
            (let ((second (aref text (1+ position)))
                  (third (aref text (+ position 2)))
                  (fourth (aref text (+ position 3))))
              (when (and
                  (utf8-continuation-octet-p second)
                  (utf8-continuation-octet-p third)
                  (utf8-continuation-octet-p fourth)
                  (not (and (= first #xf0) (< second #x90)))
                  (not (and (= first #xf4) (> second #x8f))))
                (values
                  (code-char
                    (+
                      (ash (logand first #x07) 18)
                      (ash (logand second #x3f) 12)
                      (ash (logand third #x3f) 6)
                      (logand fourth #x3f)))
                  (+ position 4)
                  t)))))))))

(defun utf8-character-before (text position)
  "Decode the valid UTF-8 scalar ending at POSITION, if any."
  (loop for beginning from (max 0 (- position 4)) below position
        do (multiple-value-bind (character end valid-p) (utf8-character-at text beginning)
      (when (and valid-p (= end position))
        (return (values character beginning t))))
        finally (return (values nil nil nil))))

(defun byte-unicode-non-boundary-position-p (text position)
  "Return true when Unicode \\B can evaluate without splitting UTF-8."
  (and
    (or (zerop position) (nth-value 2 (utf8-character-before text position)))
    (or (= position (length text)) (nth-value 2 (utf8-character-at text position)))))

(defun byte-unicode-boundary-characters (text position)
  "Return surrounding Unicode characters and whether POSITION is a UTF-8 boundary."
  (multiple-value-bind (left ignored-left valid-left) (if (zerop position) (values nil nil nil)
      (utf8-character-before text position))
    (declare (ignore ignored-left valid-left))
    (multiple-value-bind (right ignored-right valid-right) (if (= position (length text)) (values nil nil nil)
        (utf8-character-at text position))
      (declare (ignore ignored-right valid-right))
      (values
        left
        right
        (or
          (= position (length text))
          (not (utf8-continuation-octet-p (aref text position))))))))

(define-boundary-predicates
  byte-unicode-word
  (text position)
  (multiple-value-bind (left right boundary-p) (byte-unicode-boundary-characters text position)
    (values (word-character-p left t) (word-character-p right t) boundary-p)))

(defun byte-line-start-p (text position crlf-p line-terminator)
  (let ((terminator (char-code line-terminator)))
    (if crlf-p (or
        (and
          (plusp position)
          (= (aref text (1- position)) #x0d)
          (or (= position (length text)) (/= (aref text position) #x0a)))
        (and (plusp position) (= (aref text (1- position)) #x0a)))
      (and (plusp position) (= (aref text (1- position)) terminator)))))

(defun byte-line-end-p (text position crlf-p line-terminator)
  (let ((terminator (char-code line-terminator)))
    (if crlf-p (or
        (and (< position (length text)) (= (aref text position) #x0d))
        (and
          (< position (length text))
          (= (aref text position) #x0a)
          (or (zerop position) (/= (aref text (1- position)) #x0d))))
      (and (< position (length text)) (= (aref text position) terminator)))))

(progn
  (defparameter *unicode-word-general-categories* nil)

  (defun unicode-word-general-categories ()
    (or *unicode-word-general-categories*
        (setf *unicode-word-general-categories*
              (unicode-runtime-category-values
               (quote ("MN" "MC" "ME" "ND" "PC"))))))

  (defun word-character-p (character unicode-p)
    "Return exactly T or NIL -- never MEMBER's matching-tail cons -- so
DEFINE-BOUNDARY-PREDICATES's generated BOUNDARY-P, which compares this
function's result across two positions with EQ, does not see two
otherwise-equal word-character classifications as different objects."
    (and
      character
      (not
        (null
          (if unicode-p
              (let ((category (sb-unicode:general-category character)))
                (or
                  (sb-unicode:alphabetic-p character)
                  (member category (unicode-word-general-categories) :test (function eq))
                  (member (char-code character) (quote (#x200c #x200d)))))
            (or
              (char<= #\a character #\z)
              (char<= #\A character #\Z)
              (char<= #\0 character #\9)
              (char= character #\_))))))))

(define-boundary-predicates
  word
  (text position unicode-p)
  (values
    (word-character-p (and (plusp position) (char text (1- position))) unicode-p)
    (word-character-p
      (and (< position (length text)) (char text position))
      unicode-p)
    t))

(defun line-start-p (text position crlf-p line-terminator)
  (if crlf-p (or
      (and
        (plusp position)
        (char= (char text (1- position)) #\Return)
        (or (= position (length text)) (not (char= (char text position) #\Newline))))
      (and (plusp position) (char= (char text (1- position)) #\Newline)))
    (and (plusp position) (char= (char text (1- position)) line-terminator))))

(defun line-end-p (text position crlf-p line-terminator)
  (if crlf-p (or
      (and (< position (length text)) (char= (char text position) #\Return))
      (and
        (< position (length text))
        (char= (char text position) #\Newline)
        (or (zerop position) (not (char= (char text (1- position)) #\Return)))))
    (and (< position (length text)) (char= (char text position) line-terminator))))

(progn
  (defun %unicode-word-break-ignored-class-p (class)
    (member class (quote (:extend :format :zwj)) :test (function eq)))

  (defun %unicode-word-boundary-p (text position)
    (labels
        ((class-at (index)
           (unicode-word-break-class (aref text index)))
         (newline-class-p (class)
           (member class (quote (:cr :lf :newline)) :test (function eq)))
         (ah-letter-p (class)
           (member class (quote (:aletter :hebrew-letter)) :test (function eq)))
         (middle-letter-p (class)
           (member class (quote (:midletter :midnumlet :single-quote))
                   :test (function eq)))
         (middle-number-p (class)
           (member class (quote (:midnum :midnumlet :single-quote))
                   :test (function eq)))
         (word-like-p (class)
           (member class (quote (:aletter :hebrew-letter :numeric :katakana
                                    :extendnumlet))
                   :test (function eq)))
         (significant-before (index)
           (loop for cursor downfrom index downto 0
                 for class = (class-at cursor)
                 unless (%unicode-word-break-ignored-class-p class)
                   do (return (values cursor class))
                 finally (return (values nil nil))))
         (significant-after (index)
           (loop for cursor from index below (length text)
                 for class = (class-at cursor)
                 unless (%unicode-word-break-ignored-class-p class)
                   do (return (values cursor class))
                 finally (return (values nil nil))))
         (regional-indicator-count-before (index)
           (loop with count = 0
                 for cursor downfrom index downto 0
                 for class = (class-at cursor)
                 unless (%unicode-word-break-ignored-class-p class)
                   do (if (eq class :regional-indicator)
                          (incf count)
                          (return count))
                 finally (return count))))
      (cond
        ((or (zerop position) (= position (length text))) t)
        (t
         (let ((immediate-left-class (class-at (1- position)))
               (immediate-right-class (class-at position)))
           (cond
             ((and (eq immediate-left-class :cr)
                   (eq immediate-right-class :lf))
              nil)
             ((or (newline-class-p immediate-left-class)
                  (newline-class-p immediate-right-class))
              t)
             ((and (eq immediate-left-class :wsegspace)
                   (eq immediate-right-class :wsegspace))
              nil)
             ((and (eq immediate-left-class :zwj)
                   (range-matches-p +extended-pictographic-ranges+
                                    (aref text position)))
              nil)
             ((%unicode-word-break-ignored-class-p immediate-right-class)
              nil)
             (t
              (multiple-value-bind (left-index left-class)
                  (significant-before (1- position))
                (multiple-value-bind (right-index right-class)
                    (significant-after position)
                  (if (or (null left-index) (null right-index))
                      t
                      (cond
                        ((and (eq left-class :regional-indicator)
                              (eq right-class :regional-indicator)
                              (oddp
                               (regional-indicator-count-before left-index)))
                         nil)
                        ((and (eq left-class :hebrew-letter)
                              (eq right-class :single-quote))
                         nil)
                        ((and (eq left-class :hebrew-letter)
                              (eq right-class :double-quote))
                         (multiple-value-bind (next-index next-class)
                             (significant-after (1+ right-index))
                           (not (and next-index
                                     (eq next-class :hebrew-letter)))))
                        ((and (eq left-class :double-quote)
                              (eq right-class :hebrew-letter))
                         (multiple-value-bind (previous-index previous-class)
                             (significant-before (1- left-index))
                           (not (and previous-index
                                     (eq previous-class :hebrew-letter)))))
                        ((and (ah-letter-p left-class)
                              (ah-letter-p right-class))
                         nil)
                        ((and (ah-letter-p left-class)
                              (middle-letter-p right-class))
                         (multiple-value-bind (next-index next-class)
                             (significant-after (1+ right-index))
                           (not (and next-index (ah-letter-p next-class)))))
                        ((and (middle-letter-p left-class)
                              (ah-letter-p right-class))
                         (multiple-value-bind (previous-index previous-class)
                             (significant-before (1- left-index))
                           (not (and previous-index
                                     (ah-letter-p previous-class)))))
                        ((and (eq left-class :numeric)
                              (eq right-class :numeric))
                         nil)
                        ((and (ah-letter-p left-class)
                              (eq right-class :numeric))
                         nil)
                        ((and (eq left-class :numeric)
                              (ah-letter-p right-class))
                         nil)
                        ((and (eq left-class :numeric)
                              (middle-number-p right-class))
                         (multiple-value-bind (next-index next-class)
                             (significant-after (1+ right-index))
                           (not (and next-index
                                     (eq next-class :numeric)))))
                        ((and (middle-number-p left-class)
                              (eq right-class :numeric))
                         (multiple-value-bind (previous-index previous-class)
                             (significant-before (1- left-index))
                           (not (and previous-index
                                     (eq previous-class :numeric)))))
                        ((and (eq left-class :katakana)
                              (eq right-class :katakana))
                         nil)
                        ((and (word-like-p left-class)
                              (eq right-class :extendnumlet))
                         nil)
                        ((and (eq left-class :extendnumlet)
                              (word-like-p right-class))
                         nil)
                        (t t))))))))))))

  (defun %byte-unicode-binary-word-boundary-p (text position)
    (multiple-value-bind (left right boundary-p)
        (byte-unicode-boundary-characters text position)
      (and boundary-p
           (not (eq (word-character-p left t)
                    (word-character-p right t))))))

  (defun %byte-unicode-word-boundary-p (text position)
    (let ((characters (make-array 0 :adjustable t :fill-pointer 0))
          (offsets (make-array 0 :adjustable t :fill-pointer 0))
          (index 0))
      (loop while (< index (length text))
            do (vector-push-extend index offsets)
               (multiple-value-bind (character end valid-p)
                   (utf8-character-at text index)
                 (unless valid-p
                   (return-from %byte-unicode-word-boundary-p
                     (%byte-unicode-binary-word-boundary-p text position)))
                 (vector-push-extend character characters)
                 (setf index end)))
      (vector-push-extend (length text) offsets)
      (loop for character-index from 0 below (length offsets)
            when (= position (aref offsets character-index))
              do (return (%unicode-word-boundary-p characters character-index))
            finally (return nil))))

  (setf (symbol-function 'byte-unicode-word-boundary-p) (lambda (text position) (%byte-unicode-word-boundary-p text position)))

  (setf (symbol-function 'word-boundary-p) (lambda (text position unicode-p) (if unicode-p (%unicode-word-boundary-p text position) (not (eq (word-character-p (and (plusp position) (char text (1- position))) nil) (word-character-p (and (< position (length text)) (char text position)) nil))))))
)
