(in-package #:cl-regex-kit/test)

(it "covers sentence, grapheme, and special boundary contracts"
  (with-advanced-boundary-fixtures (context sentence-units grapheme-unit)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :cr :lf) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :cr :upper) 0 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :aterm :numeric) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :upper :aterm :upper) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :aterm :lower) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :scontinue) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :close) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :sep) 1 2)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :sp :upper) 1 2)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :sterm :upper) 0 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-rule
             (sentence-units :upper :lower) 0 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :cr :lf) 1)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :cr :upper) 1)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-sentence-boundary-between-p
             (sentence-units :upper :extend :lower) 1)
            :to-be nil)
    (let ((calls 0)
          (ctx (context "A. B")))
      (expect (cl-regex-kit::%advanced-boundary-cache
               ctx :test (lambda () (incf calls)))
              :to-equal 1)
      (expect (cl-regex-kit::%advanced-boundary-cache
               ctx :test (lambda () (incf calls)))
              :to-equal 1)
      (expect calls :to-equal 1)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx -1 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 100 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p ctx 100 nil)
              :to-be-truthy))
    (let ((invalid
            (context (make-array 1
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xFF))
                     :byte-mode-p t)))
      (expect (cl-regex-kit::%advanced-sentence-boundary-p invalid 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-sentence-boundary-p invalid 1 t)
              :to-be-truthy))
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "CR")) (grapheme-unit "LF") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LF")) (grapheme-unit "OTHER") t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "L")) (grapheme-unit "V") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LV")) (grapheme-unit "T") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LVT")) (grapheme-unit "T") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "EXTEND") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "ZWJ") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "SPACINGMARK") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "PREPEND")) (grapheme-unit "OTHER") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "LINKER" :indic "LINKER")
                   (grapheme-unit "EXTEND" :indic "EXTEND")
                   (grapheme-unit "OTHER" :indic "CONSONANT"))
             (grapheme-unit "OTHER" :indic "CONSONANT") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "ZWJ")
                   (grapheme-unit "EXTEND")
                   (grapheme-unit "OTHER" :pictographic-p t))
             (grapheme-unit "OTHER" :pictographic-p t) t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "RI")) (grapheme-unit "RI") t)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "RI") (grapheme-unit "RI"))
             (grapheme-unit "RI") t)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "EXTEND") nil)
            :to-be nil)
    (expect (cl-regex-kit::%advanced-grapheme-break-p
             (list (grapheme-unit "OTHER")) (grapheme-unit "OTHER") nil)
            :to-be-truthy)
    (expect (cl-regex-kit::%advanced-grapheme-class 65) :to-equal "OTHER")
    (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
             (code-char #x0301) "EXTEND")
            :to-equal "EXTEND")
    (expect (cl-regex-kit::%advanced-indic-conjunct-break-class
             #\a "OTHER")
            :to-equal "NONE")
    (let ((string-context (context "a "))
          (invalid-byte-context
            (context (make-array 2
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents '(#xFF 65))
                     :byte-mode-p t)))
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context -1 t)
              :to-be nil)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 0 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 2 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               invalid-byte-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-grapheme-boundary-p
               string-context 1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               string-context 1 t)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context "a ") 1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context (make-array 2
                                    :element-type '(unsigned-byte 8)
                                    :initial-contents '(65 32))
                        :byte-mode-p t)
               1 nil)
              :to-be-truthy)
      (expect (cl-regex-kit::%advanced-word-boundary-p
               (context (make-array 2
                                    :element-type '(unsigned-byte 8)
                                    :initial-contents '(65 32))
                        :byte-mode-p t)
               1 t)
              :to-be-truthy)
      (dolist (node
               (list
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :grapheme-boundary :unicode-p t)
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :word-boundary-unicode :unicode-p t)
                (make-instance 'cl-regex-kit::anchor-node
                               :kind :sentence-boundary :unicode-p t)))
        (expect (cl-regex-kit::%advanced-special-boundary-p
                 node string-context 0)
                :to-be-truthy))
      (expect (cl-regex-kit::%advanced-special-boundary-p
               (make-instance 'cl-regex-kit::anchor-node :kind :unknown)
               string-context 0)
              :to-be nil))))
