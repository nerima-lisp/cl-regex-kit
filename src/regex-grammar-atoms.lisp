(in-package #:cl-regex-kit)

(defun parse-number ()
  (let ((beginning (current-source-position))
        (digits nil))
    (unless (and (peek-token) (digit-token-p (peek-token)))
      (fail "Expected a decimal number"))
    (loop while (and (peek-token) (digit-token-p (peek-token)))
          do (push (token-value (take-token)) digits))
    (let ((count (parse-integer (coerce (nreverse digits) 'string))))
      (when (> count +maximum-repeat-count+)
        (fail "Repetition count exceeds 1000" beginning))
      count)))

(defun word-anchor (kind)
  (make-instance 'anchor-node :kind kind :unicode-p (flag-p +flag-unicode+)))

(defun closing-brace-p (token)
  "Whether TOKEN is a `}` -- :RBRACE outside a character class, or a plain
:CHAR with that value inside one, where `}` has no structural meaning of its
own. COLLECT-BRACED-HEX runs in both contexts (`\\x{...}` is valid inside
`[...]` too), so it tests the character rather than the token type."
  (and token (char= (token-value token) #\})))

(defun parse-word-boundary-suffix ()
  "Resolve the optional `{name}` suffix on a bare `\\b` word-boundary escape,
using the same extended-mode-aware significant-token cursor as
COLLECT-BRACED-HEX. Unlike a braced hex/Unicode-property escape, an empty
name is not an error: `\\b{2}` is a repeated bare word-boundary, so a `{`
with no alphabetic-or-hyphen name behind it must be left completely
unconsumed for the quantifier grammar to see -- simply not taking the
:LBRACE token accomplishes exactly that, with no position to rewind."
  (if (peek-type :lbrace)
      (let ((saved *regex-token-position*)
            (name-characters nil))
        (take-token)
        (loop while (and (peek-type :char)
                         (or (ascii-alphabetic-p (peek-value))
                             (char= (peek-value) #\-)))
              do (push (token-value (take-token)) name-characters))
        (if name-characters
            (let ((name (coerce (nreverse name-characters) 'string)))
              (unless (closing-brace-p (peek-token))
                (fail "Unclosed word boundary"))
              (take-token)
              (cond
                ((string= name "start") :word-start)
                ((string= name "end") :word-end)
                ((string= name "start-half") :word-start-half)
                ((string= name "end-half") :word-end-half)
                ((string= name "g") :grapheme-boundary)
                ((string= name "wb") :word-boundary-unicode)
                ((string= name "sb") :sentence-boundary)
                (t (fail "Unknown word boundary"))))
            (progn
              (setf *regex-token-position* saved)
              :word-boundary)))
      :word-boundary))

(defun collect-braced-hex (&optional (radix 16) (description "hexadecimal"))
  "Collect a braced numeric escape using the grammar significant-token cursor."
  (let ((beginning (current-source-position))
        (digits nil))
    (loop while (and (peek-type :char) (digit-char-p (peek-value) radix))
          do (push (token-value (take-token)) digits))
    (unless digits
      (fail (format nil "Expected ~A escape digits" description) beginning))
    (unless (closing-brace-p (peek-token))
      (fail (format nil "Unclosed ~A escape" description) beginning))
    (take-token)
    (let ((code (parse-integer (coerce (nreverse digits) (quote string)) :radix radix)))
      (if (and (< code char-code-limit) (not (<= #xD800 code #xDFFF)))
          (code-char code)
          (fail (format nil "~A escape is not a Unicode scalar value" description)
                beginning)))))

(defun build-escape-atom (token)
  (let ((escape (token-value token)))
    (ecase (getf escape :kind)
      (:absolute-start (make-instance (quote anchor-node) :kind :absolute-start))
      (:absolute-end (make-instance (quote anchor-node) :kind :absolute-end))
      (:match-start (make-instance (quote anchor-node) :kind :match-start))
      (:end-before-final-newline
       (make-instance (quote anchor-node) :kind :end-before-final-newline))
      (:reset-match-start (make-instance (quote reset-match-start-node)))
      (:word-boundary (word-anchor (parse-word-boundary-suffix)))
      (:not-word-boundary
       (make-instance (quote anchor-node) :kind :not-word-boundary
                      :unicode-p (flag-p +flag-unicode+)))
      (:word-start (word-anchor :word-start))
      (:word-end (word-anchor :word-end))
      (:any-byte
       (make-instance (quote any-char-node) :dotall-p t :crlf-p nil
                      :line-terminator *regex-line-terminator* :unicode-p nil))
      (:not-newline
       (make-instance (quote any-char-node) :dotall-p nil
                      :crlf-p (flag-p +flag-crlf+)
                      :line-terminator *regex-line-terminator*
                      :unicode-p (flag-p +flag-unicode+)))
      (:line-break
       (make-instance (quote line-break-node)
                      :unicode-p (flag-p +flag-unicode+)))
      (:backreference
       (let* ((relative-index (getf escape :relative-index))
              (capture-index
                (or (getf escape :capture-index)
                    (and relative-index
                         (+ *regex-group-count* 1 relative-index)))))
         (when (and relative-index
                    (or (not (plusp capture-index))
                        (> capture-index *regex-group-count*)))
           (fail "Relative backreference refers to an unavailable capture"
                 (token-start token)))
         (make-instance (quote backreference-node)
                        :capture-index capture-index
                        :name (getf escape :name)
                        :case-insensitive-p
                        (flag-p +flag-case-insensitive+)
                        :unicode-p (flag-p +flag-unicode+))))
      (:subroutine
       (let ((capture-index (getf escape :capture-index))
             (name (getf escape :name)))
         (make-instance (quote subroutine-node)
                        :target (or name capture-index)
                        :name name
                        :capture-index capture-index
                        :recursive-p nil)))
      (:numeric-reference
       (let ((capture-index (getf escape :capture-index)))
         (cond
           ((and (plusp capture-index)
                 (<= capture-index *regex-group-count*))
            (make-instance (quote backreference-node)
                           :capture-index capture-index
                           :name nil
                           :case-insensitive-p
                           (flag-p +flag-case-insensitive+)
                           :unicode-p (flag-p +flag-unicode+)))
           ((getf escape :octal-candidate-p)
            (unless *regex-octal-p*
              (fail "Octal escapes are disabled" (token-start token)))
            (let ((character (getf escape :octal-char)))
              (if character
                  (make-literal character
                                :raw-octet-p
                                (not (flag-p +flag-unicode+)))
                  (fail "Octal escape is outside the byte range"
                        (token-start token)))))
           (t
            (fail "Numeric backreference refers to an unavailable capture"
                  (token-start token))))))
      (:grapheme
       (make-instance (quote grapheme-node)
                      :extended-p t
                      :unicode-p (flag-p +flag-unicode+)))
      (:unicode-property
       (when (and *regex-byte-mode-p* (not (flag-p +flag-unicode+)))
         (fail "Unicode properties are not available in byte patterns" (token-start token)))
       (make-class nil
                   (not (eq (getf escape :from-p) (getf escape :negated-p)))
                   (list :property (getf escape :descriptor))))
      (:named-character (make-literal (getf escape :char)))
      (:quoted-literal
       (make-concat
        (map (quote list) (lambda (c) (make-literal c)) (getf escape :text))))
      (:control (make-literal (getf escape :char)))
      (:hex (make-literal (getf escape :char)
                          :raw-octet-p
                          (and (getf escape :raw-octet-p)
                               (not (flag-p +flag-unicode+)))))
      (:shorthand
       (let ((which (getf escape :which)))
         (make-class nil (member which (quote (#\D #\W #\S #\H)))
                     (shorthand-matcher which))))
      (:octal (make-literal (getf escape :char)
                            :raw-octet-p (not (flag-p +flag-unicode+))))
      (:literal (make-literal (getf escape :char))))))

(defun parse-quantifier (node)
  (let ((minimum nil)
        (maximum nil)
        (quantified-p nil))
    (case (peek-type)
      (:star
        (take-token)
        (setf minimum 0
              maximum nil
              quantified-p t))
      (:plus
        (take-token)
        (setf minimum 1
              maximum nil
              quantified-p t))
      (:question
        (take-token)
        (setf minimum 0
              maximum 1
              quantified-p t))
      (:lbrace
        (take-token)
        (setf minimum (parse-number)
              maximum minimum
              quantified-p t)
        (cond
          ((peek-type :comma)
            (take-token)
            (setf maximum (unless (peek-type :rbrace)
                            (parse-number))))
          ((peek-type :rbrace) nil)
          (t (fail "Expected , or } in repetition")))
        (unless (peek-type :rbrace)
          (fail "Unclosed repetition"))
        (take-token)
        (when (and maximum (> minimum maximum))
          (fail "Repetition minimum exceeds maximum"))))
    (if quantified-p
        (let ((greedy-p (not (flag-p +flag-ungreedy+)))
              (lazy-p nil)
              (possessive-p nil))
          (when (peek-type :question)
            (take-token)
            (setf greedy-p (not greedy-p)
                  lazy-p t))
          (when (peek-type :plus)
            (take-token)
            (setf possessive-p t))
          (when (and lazy-p possessive-p)
            (fail "A repetition cannot be both lazy and possessive"))
          (when (member (peek-type) '(:star :plus :question :lbrace))
            (fail "Repeated repetition operator"))
          (if possessive-p
              (make-instance
               'possessive-repetition-node
               :child node
               :min minimum
               :max maximum
               :greedy-p greedy-p
               :possessive-p t)
              (make-instance
               'repetition-node
               :child node
               :min minimum
               :max maximum
               :greedy-p greedy-p)))
        node)))
