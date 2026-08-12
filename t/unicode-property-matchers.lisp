(in-package #:cl-regex-kit/test)

(defmacro it-unicode-property-positive-cases (description cases)
  "Register positive Unicode property match cases.

Each case is (PROPERTY CODE-POINT)."
  `(it-each ,cases
       ,description
       (property code-point)
     (expect (match (format nil "\\p{~A}" property)
                    (string (code-char code-point)))
             :to-be-truthy)))

(defmacro it-unicode-property-negative-cases (description cases)
  "Register negative Unicode property match cases.

Each case is (PROPERTY CODE-POINT)."
  `(it-each ,cases
       ,description
       (property code-point)
     (expect (match (format nil "\\p{~A}" property)
                    (string (code-char code-point)))
             :to-be-null)))

(defmacro it-unicode-property-alias-cases (description cases)
  "Register Unicode property alias cases with positive and negative probes.

Each case is (PROPERTY MATCHING-CODE NONMATCHING-CODE)."
  (let ((matching-text-name (gensym "MATCHING-TEXT"))
        (nonmatching-text-name (gensym "NONMATCHING-TEXT")))
    `(it-each ,cases
         ,description
         (property matching-code nonmatching-code)
       (let ((,matching-text-name (string (code-char matching-code)))
             (,nonmatching-text-name (string (code-char nonmatching-code))))
         (expect (match (format nil "\\p{~A}" property) ,matching-text-name)
                 :to-be-truthy)
         (expect (match (format nil "\\P{~A}" property) ,nonmatching-text-name)
                 :to-be-truthy)
         (expect (match (format nil "\\p{~A}" property) ,nonmatching-text-name)
                 :to-be-null)))))

(defmacro it-unicode-property-normalization-cases (description cases)
  "Register normalized Unicode property selector cases.

Each case is (PROPERTY CODE-POINT EXPECTED)."
  (let ((result-name (gensym "RESULT")))
    `(it-each ,cases
         ,description
         (property code-point expected)
       (let ((,result-name (match (format nil "\\p{~A}" property)
                                  (string (code-char code-point)))))
         (if expected
             (expect ,result-name :to-be-truthy)
             (expect ,result-name :to-be-null))))))

(defmacro it-invalid-unicode-property-cases (description cases)
  "Register invalid Unicode property selector cases.

Each case is (PROPERTY)."
  `(it-each ,cases
       ,description
       (property)
     (signals regex-syntax-error
       (compile-regex (format nil "\\p{~A}" property)))))
