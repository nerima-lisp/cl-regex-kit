(in-package #:cl-regex-kit/test)

(defun utf8-octets (string)
  "Encode STRING to UTF-8 octets through cl-codec-kit for test fixtures."
  (string-to-octets string :encoding :utf-8))

(defmacro it-utf8-decode-at-cases (description cases)
  "Register one UTF-8 forward-decoding specification from declarative cases.

Each case is (OCTETS POSITION EXPECTED-VALUES). EXPECTED-VALUES is compared
against MULTIPLE-VALUE-LIST of CL-REGEX-KIT::UTF8-CHARACTER-AT."
  (let ((octets-values (gensym "OCTETS-VALUES"))
        (position (gensym "POSITION"))
        (expected (gensym "EXPECTED"))
        (octets-fn (gensym "OCTETS"))
        (decode-at-fn (gensym "DECODE-AT")))
    `(it ,description
       (flet ((,octets-fn (&rest values)
                (make-array (length values)
                            :element-type '(unsigned-byte 8)
                            :initial-contents values))
              (,decode-at-fn (text position)
                (multiple-value-list
                 (cl-regex-kit::utf8-character-at text position))))
         (dolist (case ',cases)
           (destructuring-bind (,octets-values ,position ,expected) case
             (expect (,decode-at-fn (apply #',octets-fn ,octets-values) ,position)
                     :to-equal
                     ,expected)))))))

(defmacro it-utf8-decode-before-cases (description cases)
  "Register one UTF-8 backward-decoding specification from declarative cases.

Each case is (OCTETS POSITION EXPECTED-VALUES). EXPECTED-VALUES is compared
against MULTIPLE-VALUE-LIST of CL-REGEX-KIT::UTF8-CHARACTER-BEFORE."
  (let ((octets-values (gensym "OCTETS-VALUES"))
        (position (gensym "POSITION"))
        (expected (gensym "EXPECTED"))
        (octets-fn (gensym "OCTETS"))
        (decode-before-fn (gensym "DECODE-BEFORE")))
    `(it ,description
       (flet ((,octets-fn (&rest values)
                (make-array (length values)
                            :element-type '(unsigned-byte 8)
                            :initial-contents values))
              (,decode-before-fn (text position)
                (multiple-value-list
                 (cl-regex-kit::utf8-character-before text position))))
         (dolist (case ',cases)
           (destructuring-bind (,octets-values ,position ,expected) case
             (expect (,decode-before-fn (apply #',octets-fn ,octets-values) ,position)
                     :to-equal
                     ,expected)))))))

(defmacro it-unicode-property-equivalence-cases (description cases)
  "Register one Unicode-property alias equivalence specification.

Each case is (LEFT RIGHT CHARACTER)."
  (let ((left (gensym "LEFT"))
        (right (gensym "RIGHT"))
        (character (gensym "CHARACTER"))
        (matches-p-fn (gensym "MATCHES-P"))
        (descriptor-var (gensym "DESCRIPTOR")))
    `(it ,description
       (labels ((,matches-p-fn (property character)
                  (let ((,descriptor-var
                          (or (cl-regex-kit::resolve-unicode-property property)
                              (error
                               "Unicode property did not resolve: ~A"
                               property))))
                    (not
                     (null
                      (cl-regex-kit::unicode-property-descriptor-matches-p
                       ,descriptor-var
                       character))))))
         (dolist (case ',cases)
           (destructuring-bind (,left ,right ,character) case
             (expect (,matches-p-fn ,left ,character)
                     :to-equal
                     (,matches-p-fn ,right ,character))))))))
