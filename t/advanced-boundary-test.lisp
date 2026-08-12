;;;; t/advanced-boundary-test.lisp

(in-package #:cl-regex-kit/test)

(it "caches advanced boundary values and evaluates the producer once"
  (let ((context (cl-regex-kit::make-advanced-context))
        (calls 0))
    (flet ((produce ()
             (incf calls)
             :cached))
      (expect
       (cl-regex-kit::%advanced-boundary-cache context :sentence #'produce)
       :to-equal
       :cached)
      (expect
       (cl-regex-kit::%advanced-boundary-cache context :sentence #'produce)
       :to-equal
       :cached)
      (expect calls :to-equal 1)
      (expect (hash-table-p
               (cl-regex-kit::advanced-context-boundary-cache context))
              :to-be-truthy))))

(it "keeps sentence-prefix classification explicit across ignored units"
  (labels ((units (&rest classes)
             (coerce
              (mapcar
               (lambda (class)
                 (cl-regex-kit::make-advanced-sentence-unit
                  :start 0 :end 1 :class class))
               classes)
              'vector)))
    (expect
     (cl-regex-kit::%advanced-sentence-significant-before
      (units :extend :format)
      10)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-significant-before
      (units :extend :upper)
      10)
     :to-equal
     1)
    (expect
     (cl-regex-kit::%advanced-sentence-significant-before (units) 0)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :aterm :extend)
      1
      nil)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :aterm :sp)
      1
      t)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :aterm :sp)
      1
      nil)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :aterm :close)
      1
      nil)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :upper)
      0
      nil)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-terminal-prefix-p
      (units :aterm)
      -1
      nil)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm :extend)
      1)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm :other)
      1)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm :oletter)
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm :sep)
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm :sterm)
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :upper)
      0)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-aterm-before-lower-p
      (units :aterm)
      -1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-upper-aterm-before-upper-p
      (units :upper :aterm :upper)
      1)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-upper-aterm-before-upper-p
      (units :aterm :upper)
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-upper-aterm-before-upper-p
      (units :numeric :aterm :upper)
      2)
     :to-be-null)))

(it "applies each sentence boundary rule in its precedence order"
  (labels ((units (&rest classes)
             (coerce
              (mapcar
               (lambda (class)
                 (cl-regex-kit::make-advanced-sentence-unit
                  :start 0 :end 1 :class class))
               classes)
              'vector))
           (rule (classes left-index right-index)
             (cl-regex-kit::%advanced-sentence-boundary-rule
              (apply #'units classes)
              left-index
              right-index)))
    (expect (rule '(:cr :lf) 0 1) :to-be-null)
    (expect (rule '(:lf :upper) 0 1) :to-be-truthy)
    (expect (rule '(:aterm :numeric) 0 1) :to-be-null)
    (expect (rule '(:upper :aterm :upper) 1 2) :to-be-null)
    (expect (rule '(:aterm :lower) 0 1) :to-be-null)
    (expect (rule '(:aterm :sp :scontinue) 1 2) :to-be-null)
    (expect (rule '(:aterm :close) 0 1) :to-be-null)
    (expect (rule '(:aterm :sp :sep) 1 2) :to-be-null)
    (expect (rule '(:aterm :sp :upper) 1 2) :to-be-truthy)
    (expect (rule '(:aterm :other :lower) 1 2) :to-be-null)
    ;; SB8 also suppresses the boundary before an intermediate Close run
    ;; when that run eventually reaches Lower.
    (expect
     (rule '(:aterm :close :close :sp :close :close :lower) 3 4)
     :to-be-null)
    (expect
     (rule '(:aterm :close :close :sp :close :close :upper) 3 4)
     :to-be-truthy)
    (expect
     (rule '(:aterm :close :close :sp :format :close :format :close
             :format :lower)
           3
           4)
     :to-be-null)
    ;; SB8 only suppresses a break when its negated class can consume the
    ;; intervening unit.  OLetter therefore leaves the default SB998
    ;; no-break result unchanged, while ParaSep and SATerm are handled by
    ;; their earlier paragraph/terminal rules.
    (expect (rule '(:aterm :oletter :lower) 1 2) :to-be-null)
    (expect (rule '(:aterm :sep :lower) 1 2) :to-be-truthy)
    (expect (rule '(:aterm :sterm :lower) 1 2) :to-be-truthy)
    (expect (rule '(:aterm :upper) 0 1) :to-be-truthy)
    (expect (rule '(:upper :lower) 0 1) :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-boundary-between-p
      (apply #'units '(:cr :lf))
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-boundary-between-p
      (apply #'units '(:lf :upper))
      1)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-sentence-boundary-between-p
      (apply #'units '(:upper :extend))
      1)
     :to-be-null)
    (expect
     (cl-regex-kit::%advanced-sentence-boundary-between-p
      (apply #'units '(:upper :lower))
      1)
     :to-be-null)))

(it "builds sentence boundary vectors and falls back safely for invalid UTF-8"
  (let* ((text "A. B")
         (context (cl-regex-kit::make-advanced-context
                   :text text
                   :limit (length text)))
         (boundaries (cl-regex-kit::%advanced-sentence-boundaries context))
         (invalid (make-array 1
                              :element-type '(unsigned-byte 8)
                              :initial-contents '(#x80)))
         (invalid-context
           (cl-regex-kit::make-advanced-context
            :text invalid
            :limit 1
            :byte-mode-p t))
         (invalid-boundaries
           (cl-regex-kit::%advanced-sentence-boundaries invalid-context)))
    (expect (length boundaries) :to-equal 5)
    (expect (aref boundaries 0) :to-be-truthy)
    (expect (aref boundaries 3) :to-be-truthy)
    (expect (aref boundaries 4) :to-be-truthy)
    (expect (length invalid-boundaries) :to-equal 2)
    (expect (every #'identity invalid-boundaries) :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-p context 0 nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-p context 3 t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-p context -1 t)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-sentence-boundary-p context 5 t)
            :to-be-null)))

(it "handles grapheme boundaries at scalar, invalid, and partial-byte positions"
  (let* ((context (cl-regex-kit::make-advanced-context
                   :text "ab"
                   :limit 2))
         (invalid (make-array 3
                              :element-type '(unsigned-byte 8)
                              :initial-contents '(#x61 #x80 #x62)))
         (invalid-context
           (cl-regex-kit::make-advanced-context
            :text invalid
            :limit 3
            :byte-mode-p t))
         (multibyte (make-array 2
                                :element-type '(unsigned-byte 8)
                                :initial-contents '(#xc3 #xa9)))
         (multibyte-context
           (cl-regex-kit::make-advanced-context
            :text multibyte
            :limit 2
            :byte-mode-p t)))
    (expect (cl-regex-kit::%advanced-grapheme-boundary-p context 1 nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-boundary-p context 0 t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-boundary-p context 2 t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-boundary-p context -1 t)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-grapheme-boundary-p context 3 t)
            :to-be-null)
    (expect
     (cl-regex-kit::%advanced-grapheme-boundary-p invalid-context 2 t)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-grapheme-boundary-p multibyte-context 1 t)
     :to-be-null)))

(it "applies grapheme rules to an abstract stream of boundary units"
  (labels ((unit (class &key (indic "NONE") (extended nil))
             (cl-regex-kit::make-advanced-grapheme-unit
              :class class
              :indic-conjunct-break indic
              :extended-pictographic-p extended))
           (break-p (units next &optional (extended-p t))
             (cl-regex-kit::%advanced-grapheme-break-p
              units next extended-p)))
    (expect (break-p (list (unit "CR")) (unit "LF")) :to-be-null)
    (expect (break-p (list (unit "CONTROL")) (unit "OTHER"))
            :to-be-truthy)
    (expect (break-p (list (unit "L")) (unit "L")) :to-be-null)
    (expect (break-p (list (unit "LV")) (unit "V")) :to-be-null)
    (expect (break-p (list (unit "LVT")) (unit "T")) :to-be-null)
    (expect (break-p (list (unit "OTHER")) (unit "EXTEND")) :to-be-null)
    (expect (break-p (list (unit "OTHER")) (unit "SPACINGMARK"))
            :to-be-null)
    (expect (break-p (list (unit "PREPEND")) (unit "OTHER"))
            :to-be-null)
    (expect
     (break-p (list (unit "OTHER" :indic "LINKER")
                    (unit "OTHER" :indic "CONSONANT"))
              (unit "OTHER" :indic "CONSONANT"))
     :to-be-null)
    (expect
     (break-p (list (unit "ZWJ") (unit "OTHER" :extended t))
              (unit "OTHER" :extended t))
     :to-be-null)
    (expect (break-p (list (unit "RI")) (unit "RI")) :to-be-null)
    (expect (break-p (list (unit "RI") (unit "RI")) (unit "RI"))
            :to-be-truthy)
    (expect (break-p (list (unit "OTHER")) (unit "OTHER") nil)
            :to-be-truthy)
    (expect (break-p (list (unit "OTHER")) (unit "OTHER"))
            :to-be-truthy)))

(it "classifies Indic grapheme rules by their semantic input categories"
  (expect
   (cl-regex-kit::%advanced-indic-conjunct-break-class nil "OTHER")
   :to-equal
   "NONE")
  (expect
   (cl-regex-kit::%advanced-indic-conjunct-break-class
    (code-char #x094d) "OTHER")
   :to-equal
   "LINKER")
  (expect
   (cl-regex-kit::%advanced-indic-conjunct-break-class
    (code-char #x0915) "OTHER")
   :to-equal
   "CONSONANT")
  (expect
   (cl-regex-kit::%advanced-indic-conjunct-break-class
    (code-char #x0301) "EXTEND")
   :to-equal
   "EXTEND")
  (expect
   (cl-regex-kit::%advanced-indic-conjunct-break-class
    (code-char #x200c) "EXTEND")
   :to-equal
   "NONE"))

(it "selects the correct word-boundary implementation for each input mode"
  (let* ((string-context (cl-regex-kit::make-advanced-context
                          :text "a!"
                          :limit 2))
         (octets (make-array 2
                             :element-type '(unsigned-byte 8)
                             :initial-contents '(97 98)))
         (byte-context
           (cl-regex-kit::make-advanced-context
            :text octets
            :limit 2
            :byte-mode-p t)))
    (expect (cl-regex-kit::%advanced-word-boundary-p string-context 1 t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-word-boundary-p byte-context 0 nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-word-boundary-p byte-context 1 nil)
            :to-be-null)
    (expect (cl-regex-kit::%advanced-word-boundary-p byte-context 2 nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-word-boundary-p byte-context 0 t)
            :to-be-truthy)))

(it "dispatches special boundary anchors without treating unknown kinds as matches"
  (let ((context (cl-regex-kit::make-advanced-context
                  :text "A. B"
                  :limit 4)))
    (expect
     (cl-regex-kit::%advanced-special-boundary-p
      (make-instance 'cl-regex-kit::anchor-node
                     :kind :grapheme-boundary
                     :unicode-p t)
      context
      1)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-special-boundary-p
      (make-instance 'cl-regex-kit::anchor-node
                     :kind :word-boundary-unicode
                     :unicode-p t)
      context
      0)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-special-boundary-p
      (make-instance 'cl-regex-kit::anchor-node
                     :kind :sentence-boundary
                     :unicode-p t)
      context
      3)
     :to-be-truthy)
    (expect
     (cl-regex-kit::%advanced-special-boundary-p
     (make-instance 'cl-regex-kit::anchor-node :kind :unknown)
      context
      0)
     :to-be-null)))
