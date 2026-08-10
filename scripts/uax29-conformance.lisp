;;;; Run the Unicode UAX #29 boundary conformance fixtures.
;;;;
;;;; Usage:
;;;;   sbcl --script scripts/uax29-conformance.lisp \
;;;;     path/to/GraphemeBreakTest.txt \
;;;;     path/to/WordBreakTest.txt \
;;;;     path/to/SentenceBreakTest.txt

(require :asdf)

(defun script-directory ()
  (make-pathname :name nil
                 :type nil
                 :defaults (or *load-truename*
                               *compile-file-truename*
                               (error "Unable to determine the script location"))))

(let ((root (uiop:pathname-parent-directory-pathname (script-directory))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,root)
     :inherit-configuration)))

(asdf:load-system "cl-regex-kit")

(in-package #:cl-regex-kit)

(defun split-whitespace (string)
  (loop with start = 0
        for position from 0 to (length string)
        when (or (= position (length string))
                 (find (char string position)
                       (list #\Space #\Tab #\Return #\Newline)
                       :test #'char=))
          unless (= start position)
            collect (subseq string start position)
          and do (setf start (1+ position))))

(defun parse-uax29-line (line)
  (let* ((comment-position (position #\# line))
         (content (if comment-position
                      (subseq line 0 comment-position)
                      line))
         (tokens (split-whitespace content)))
    (when tokens
      (let ((characters (make-array 0 :adjustable t :fill-pointer 0))
            (expected (make-array 0 :adjustable t :fill-pointer 0)))
        (loop for token in tokens
              for token-index from 0
              do (if (evenp token-index)
                     (progn
                       (unless (member token '("÷" "×") :test #'string=)
                         (error "Invalid UAX #29 boundary marker ~S in ~S"
                                token line))
                       (vector-push-extend (string= token "÷") expected))
                     (let ((code-point (parse-integer token :radix 16)))
                       (unless (<= code-point #x10FFFF)
                         (error "Invalid Unicode scalar value ~A in ~S"
                                token line))
                       (vector-push-extend (or (code-char code-point)
                                              (error "Cannot represent U+~X"
                                                     code-point))
                                            characters))))
        (unless (= (length expected) (1+ (length characters)))
          (error "Malformed UAX #29 case: ~S" line))
        (values (coerce characters 'string)
                (coerce expected 'vector)
                t)))))

(defun uax29-fixture-kind (pathname)
  (let ((name (pathname-name (pathname pathname))))
    (cond
      ((search "GraphemeBreakTest" name :test #'char-equal) :grapheme)
      ((search "WordBreakTest" name :test #'char-equal) :word)
      ((search "SentenceBreakTest" name :test #'char-equal) :sentence)
      (t
       (error "Cannot infer UAX #29 fixture kind from ~S" pathname)))))

(defun make-uax29-context (text)
  (make-advanced-context
   :text text
   :search-start 0
   :limit (length text)
   :text-length (length text)
   :byte-mode-p nil
   :never-newline-p nil
   :root nil
   :group-count 0
   :group-names nil
   :step-limit most-positive-fixnum
   :steps 0
   :nest-limit most-positive-fixnum
   :callout nil))

(defun uax29-boundary-p (kind text position context)
  (ecase kind
    (:grapheme
     (%advanced-grapheme-boundary-p context position t))
    (:word
     (word-boundary-p text position t))
    (:sentence
     (%advanced-sentence-boundary-p context position t))))

(defun text-code-points (text)
  (loop for character across text collect (char-code character)))

(defun check-uax29-fixture (pathname)
  (let ((kind (uax29-fixture-kind pathname))
        (case-count 0)
        (boundary-count 0)
        (mismatches nil))
    (with-open-file (stream pathname :direction :input :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            for line-number from 1
            while line
            do (multiple-value-bind (text expected present-p)
                   (parse-uax29-line line)
                 (when present-p
                   (incf case-count)
                   (let ((context (make-uax29-context text)))
                     (loop for position from 0 below (length expected)
                           do (incf boundary-count)
                              (let ((actual
                                      (uax29-boundary-p
                                       kind text position context))
                                    (wanted (aref expected position)))
                                (unless (eql actual wanted)
                                  (push (list line-number position wanted actual
                                              (text-code-points text))
                                        mismatches))))))))
    )
    (unless (plusp case-count)
      (error "UAX #29 fixture ~S contained no test cases" pathname))
    (format t "~A: ~D cases, ~D boundary checks~%"
            kind case-count boundary-count)
    (dolist (mismatch (reverse mismatches))
      (destructuring-bind (line-number position wanted actual code-points)
          mismatch
        (format t "  line ~D position ~D expected ~S got ~S (~{U+~4,'0X~^ ~})~%"
                line-number position wanted actual code-points)))
    (values case-count boundary-count (length mismatches))))

(let ((arguments (uiop:command-line-arguments)))
  (unless arguments
    (format *error-output*
            "Usage: sbcl --script scripts/uax29-conformance.lisp FIXTURE...~%")
    (uiop:quit 2))
  (let ((case-count 0)
        (boundary-count 0)
        (mismatch-count 0))
    (dolist (argument arguments)
      (multiple-value-bind (cases boundaries mismatches)
          (check-uax29-fixture argument)
        (incf case-count cases)
        (incf boundary-count boundaries)
        (incf mismatch-count mismatches)))
    (format t "UAX #29 total: ~D cases, ~D boundary checks, ~D mismatches~%"
            case-count boundary-count mismatch-count)
    (uiop:quit (if (zerop mismatch-count) 0 1))))
