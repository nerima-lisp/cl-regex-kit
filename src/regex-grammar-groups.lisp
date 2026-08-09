;;;; src/regex-grammar-groups.lisp
;;;;
;;;; Group-level grammar: captures, lookarounds, control verbs, callouts,
;;;; subroutines, conditionals, and branch-reset groups. Keeping these
;;;; constructs separate from the regular expression precedence tiers makes
;;;; the stateful parts of the grammar easier to audit.
(in-package #:cl-regex-kit)

(defun %group-token-at-offset (offset)
  (let ((position (+ *regex-token-position* offset)))
    (and (< position (length *regex-tokens*))
         (aref *regex-tokens* position))))

(defun %group-token-value-at (offset)
  (let ((token (%group-token-at-offset offset)))
    (and token (token-value token))))

(defun %special-group-prefix-p ()
  (when (peek-type :question)
    (let ((first (%group-token-value-at 1))
          (second (%group-token-value-at 2))
          (following (%group-token-at-offset 2)))
      (and (characterp first)
           (or (member first '(#\= #\! #\> #\( #\& #\C #\* #\[)
                           :test #'char=)
               (and (member first '(#\+ #\-) :test #'char=)
                    (digit-char-p second))
               (char= first #\|)
               (digit-char-p first)
               (and (char= first #\<)
                    (characterp second)
                    (member second '(#\= #\! #\*) :test #'char=))
               (and (char= first #\P)
                    (characterp second)
                    (member second '(#\> #\=) :test #'char=))
               (and (char= first #\R)
                    (eq (and following (token-type following)) :rparen)))))))

(defun %parse-callout ()
  (take-token)
  (unless (and (peek-type :char)
               (char= (peek-value) #\C))
    (fail "Invalid callout prefix"))
  (take-token)
  (let ((number 0)
        (tag nil))
    (cond
      ((peek-type :rparen))
      ((digit-token-p (peek-token))
       (let ((digits nil))
         (loop while (digit-token-p (peek-token))
               do (push (token-value (take-token)) digits))
         (setf number
               (parse-integer
                (coerce (nreverse digits) 'string)))))
      ((and (peek-token)
            (member (token-value (peek-token))
                    '(#\" #\' #\^ #\% #\# #\$ #\{)
                    :test #'char=))
       (let* ((delimiter (token-value (take-token)))
              (closing (if (char= delimiter #\{) #\} delimiter))
              (characters nil))
         (loop
           (cond
             ((null (peek-token))
              (fail "Unclosed callout tag"))
             ((and (characterp (token-value (peek-token)))
                   (char= (token-value (peek-token)) closing))
              (take-token)
              (setf tag (coerce (nreverse characters) 'string))
              (return))
             ((characterp (token-value (peek-token)))
              (push (token-value (take-token)) characters))
             (t
              (fail "Invalid callout tag"))))))
      (t
       (fail "Invalid callout")))
    (unless (peek-type :rparen)
      (fail "Unclosed callout"))
    (take-token)
    (make-instance 'callout-node :number number :tag tag)))

(defun %read-reference-name (terminator)
  (let ((characters nil))
    (unless (and (peek-type :char)
                 (capture-name-start-p (peek-value)))
      (fail "Reference name must start with an alphabetic character or underscore"))
    (loop while (and (peek-type :char)
                     (capture-name-character-p (peek-value)))
          do (push (token-value (take-token)) characters))
    (let ((name (coerce (nreverse characters) 'string)))
      (if (eq terminator :rparen)
          (unless (peek-type :rparen)
            (fail "Unclosed reference"))
          (unless (and (peek-type :char)
                       (char= (peek-value) terminator))
            (fail "Unclosed reference")))
      (take-token)
      name)))

(defun %named-lookaround-kind (verb-name)
  (cond
    ((member verb-name
             '("POSITIVE_LOOKAHEAD" "PLA"
               "NON_ATOMIC_POSITIVE_LOOKAHEAD" "NAPLA")
             :test #'string=)
     :lookahead)
    ((member verb-name '("NEGATIVE_LOOKAHEAD" "NLA")
             :test #'string=)
     :negative-lookahead)
    ((member verb-name
             '("POSITIVE_LOOKBEHIND" "PLB"
               "NON_ATOMIC_POSITIVE_LOOKBEHIND" "NAPLB")
             :test #'string=)
     :lookbehind)
    ((member verb-name '("NEGATIVE_LOOKBEHIND" "NLB")
             :test #'string=)
     :negative-lookbehind)
    (t nil)))

(defun %non-atomic-lookaround-verb-p (verb-name)
  (not (null
        (member verb-name
                '("NON_ATOMIC_POSITIVE_LOOKAHEAD" "NAPLA"
                  "NON_ATOMIC_POSITIVE_LOOKBEHIND" "NAPLB")
                :test #'string=))))

(defun %parse-named-lookaround (verb-name kind)
  (unless (and (peek-type :char)
               (char= (peek-value) #\:))
    (fail "Named lookaround requires a colon"))
  (take-token)
  (let ((child (parse-alternation)))
    (unless (peek-type :rparen)
      (fail "Unclosed named lookaround"))
    (take-token)
    (make-instance
     'lookaround-node
     :kind (if (member kind '(:lookbehind :negative-lookbehind))
               :lookbehind
               :lookahead)
     :child child
     :negative-p
     (not (null (member kind '(:negative-lookahead :negative-lookbehind))))
     :direction (if (member kind '(:lookbehind :negative-lookbehind))
                    :backward
                    :forward)
     :fixed-length nil
     :non-atomic-p (%non-atomic-lookaround-verb-p verb-name))))

(defun %control-verb-kind (verb-name)
  (cond
    ((zerop (length verb-name)) :mark)
    ((member verb-name '("FAIL" "F") :test #'string=) :fail)
    ((member verb-name '("SKIP" "S") :test #'string=) :skip)
    ((member verb-name '("PRUNE" "P") :test #'string=) :prune)
    ((member verb-name '("COMMIT" "C") :test #'string=) :commit)
    ((string= verb-name "THEN") :then)
    ((member verb-name '("ACCEPT" "A") :test #'string=) :accept)
    ((string= verb-name "MARK") :mark)
    (t (fail "Unknown control verb"))))

(defun %parse-control-verb-argument (verb)
  (when (and (peek-type :char)
             (char= (peek-value) #\:))
    (when (eq verb :fail)
      (fail "FAIL control verb does not accept an argument"))
    (take-token)
    (let ((characters nil))
      (loop while (and (peek-token)
                       (not (peek-type :rparen)))
            do (unless (peek-type :char)
                 (fail "Invalid control verb argument"))
               (push (token-value (take-token)) characters))
      (when (null characters)
        (fail "Control verb argument requires a non-empty tag"))
      (coerce (nreverse characters) 'string))))

(defun %parse-control-verb ()
  (take-token)
  (let ((characters nil))
    (loop while (and (peek-token)
                     (peek-type :char)
                     (not (char= (peek-value) #\:)))
          do (push (token-value (take-token)) characters))
    (let* ((verb-name (string-upcase (coerce (nreverse characters) 'string)))
           (lookaround-kind (%named-lookaround-kind verb-name)))
      (if lookaround-kind
          (%parse-named-lookaround verb-name lookaround-kind)
          (let* ((verb (%control-verb-kind verb-name))
                 (argument (%parse-control-verb-argument verb)))
            (when (and (eq verb :mark) (null argument))
              (fail "MARK control verb requires :tag"))
            (unless (peek-type :rparen)
              (fail "Unclosed control verb"))
            (take-token)
            (make-instance 'control-verb-node
                           :verb verb
                           :argument argument))))))

(defun %parse-lookaround-prefix ()
  (take-token)
  (let ((backward-p nil)
        (negative-p nil)
        (non-atomic-p nil))
    (cond
      ((and (peek-type :char)
            (char= (peek-value) #\*))
       (setf non-atomic-p t)
       (take-token))
      ((and (peek-type :char)
            (member (peek-value) '(#\= #\!) :test #'char=))
       (setf negative-p (char= (peek-value) #\!))
       (take-token))
      ((and (peek-type :char)
            (char= (peek-value) #\<))
       (setf backward-p t)
       (take-token)
       (cond
         ((and (peek-type :char)
               (char= (peek-value) #\*))
          (setf non-atomic-p t)
          (take-token))
         ((and (peek-type :char)
               (member (peek-value) '(#\= #\!) :test #'char=))
          (setf negative-p (char= (peek-value) #\!))
          (take-token))
         (t
          (fail "Lookbehind must use <=, <!, or <*"))))
      (t
       (fail "Invalid lookaround prefix")))
    (when (and non-atomic-p negative-p)
      (fail "Non-atomic lookaround must be positive"))
    (values backward-p negative-p non-atomic-p)))

(defun %parse-lookaround ()
  (multiple-value-bind (backward-p negative-p non-atomic-p)
      (%parse-lookaround-prefix)
    (let ((child (parse-alternation)))
      (unless (peek-type :rparen)
        (fail "Unclosed lookaround"))
      (take-token)
      (make-instance 'lookaround-node
                     :kind (if backward-p :lookbehind :lookahead)
                     :child child
                     :negative-p negative-p
                     :direction (if backward-p :backward :forward)
                     :fixed-length nil
                     :non-atomic-p non-atomic-p))))

(defun %parse-atomic ()
  (take-token)
  (unless (and (peek-type :char)
               (char= (peek-value) #\>))
    (fail "Atomic group must use ?>"))
  (take-token)
  (let ((child (parse-alternation)))
    (unless (peek-type :rparen)
      (fail "Unclosed atomic group"))
    (take-token)
    (make-instance 'atomic-node :child child)))

(defun %parse-subroutine ()
  (take-token)
  (cond
    ((and (peek-type :char) (char= (peek-value) #\&))
     (take-token)
     (let ((name (%read-reference-name :rparen)))
       (make-instance 'subroutine-node
                      :target name
                      :name name
                      :capture-index nil
                      :recursive-p nil)))
    ((and (peek-type :char) (char= (peek-value) #\R))
     (take-token)
     (unless (peek-type :rparen)
       (fail "Recursion reference must use (?R)"))
     (take-token)
     (make-instance 'recursion-node
                    :target nil
                    :name nil
                    :capture-index nil
                    :recursive-p t))
    ((and (peek-type :char)
          (char= (peek-value) #\P)
          (let ((next (%group-token-at-offset 1)))
            (and next
                 (eq (token-type next) :char)
                 (char= (token-value next) #\=))))
     (take-token)
     (take-token)
     (let ((name (%read-reference-name :rparen)))
       (make-instance 'backreference-node
                      :capture-index nil
                      :name name
                      :case-insensitive-p (flag-p +flag-case-insensitive+)
                      :unicode-p (flag-p +flag-unicode+))))
    ((and (peek-type :char)
          (char= (peek-value) #\P)
          (let ((next (%group-token-at-offset 1)))
            (and next
                 (eq (token-type next) :char)
                 (char= (token-value next) #\>))))
     (take-token)
     (take-token)
     (let ((name (%read-reference-name :rparen)))
       (make-instance 'subroutine-node
                      :target name
                      :name name
                      :capture-index nil
                      :recursive-p nil)))
    ((and (or (peek-type :plus)
              (and (peek-type :char)
                   (char= (peek-value) #\-)))
          (digit-char-p (%group-token-value-at 1)))
     (let ((sign (token-value (take-token)))
           (digits nil))
       (loop while (digit-token-p (peek-token))
             do (push (token-value (take-token)) digits))
       (unless (peek-type :rparen)
         (fail "Unclosed relative subroutine reference"))
       (take-token)
       (let* ((relative-index
                (parse-integer
                 (coerce (cons sign (nreverse digits)) 'string)))
              (target (+ (1+ *regex-group-count*) relative-index)))
         (unless (and (plusp target)
                      (<= target *regex-group-count*))
           (fail "Relative subroutine reference is unavailable"))
         (make-instance 'subroutine-node
                        :target target
                        :name nil
                        :capture-index target
                        :recursive-p nil))))
    ((digit-token-p (peek-token))
     (let ((digits nil))
       (loop while (digit-token-p (peek-token))
             do (push (token-value (take-token)) digits))
       (unless (peek-type :rparen)
         (fail "Unclosed subroutine reference"))
       (take-token)
       (let ((target
               (parse-integer
                (coerce (nreverse digits) 'string))))
         (if (zerop target)
             (make-instance 'recursion-node
                            :target target
                            :name nil
                            :capture-index target
                            :recursive-p t)
             (make-instance 'subroutine-node
                            :target target
                            :name nil
                            :capture-index target
                            :recursive-p nil)))))
    (t
     (fail "Invalid subroutine reference"))))

(defun %parse-conditional-condition ()
  (cond
    ((peek-type :question)
     (%parse-lookaround))
    ((digit-token-p (peek-token))
     (let ((digits nil))
       (loop while (digit-token-p (peek-token))
             do (push (token-value (take-token)) digits))
       (unless (peek-type :rparen)
         (fail "Unclosed conditional condition"))
       (take-token)
       (list :capture-index
             (parse-integer (coerce (nreverse digits) 'string)))))
    ((and (peek-type :char)
          (char= (peek-value) #\<))
     (take-token)
     (let ((name (%read-reference-name #\>)))
       (unless (peek-type :rparen)
         (fail "Unclosed conditional condition"))
       (take-token)
       (list :name name)))
    ((and (peek-type :char)
          (char= (peek-value) #\R))
     (take-token)
     (cond
       ((peek-type :rparen)
        (take-token)
        :recursion)
       ((digit-token-p (peek-token))
        (let ((digits nil))
          (loop while (digit-token-p (peek-token))
                do (push (token-value (take-token)) digits))
          (unless (peek-type :rparen)
            (fail "Unclosed recursive condition"))
          (take-token)
          (list :recursion-index
                (parse-integer (coerce (nreverse digits) 'string)))))
       ((and (peek-type :char)
             (char= (peek-value) #\&))
        (take-token)
        (list :recursion-name (%read-reference-name :rparen)))
       (t
        (fail "Invalid recursive condition"))))
    ((and (peek-type :char)
          (capture-name-start-p (peek-value)))
     (let ((name (%read-reference-name :rparen)))
       (if (string= name "DEFINE")
           :define
           (list :name name))))
    (t
     (fail "Conditional group requires a capture reference"))))

(defun %parse-conditional ()
  (take-token)
  (unless (peek-type :lparen)
    (fail "Conditional group must start with a condition"))
  (take-token)
  (let ((condition (%parse-conditional-condition)))
    (let ((yes-branch (parse-concatenation))
          (no-branch (if (peek-type :pipe)
                         (progn
                           (take-token)
                           (parse-concatenation))
                         (make-concat nil))))
      (unless (peek-type :rparen)
        (fail "Unclosed conditional group"))
      (take-token)
      (make-instance 'conditional-node
                     :condition condition
                     :yes-branch yes-branch
                     :no-branch no-branch))))

(defun %parse-branch-reset ()
  (take-token)
  (take-token)
  (let ((base *regex-group-count*)
        (maximum *regex-group-count*)
        (branches nil))
    (loop
      (setf *regex-group-count* base)
      (push (parse-concatenation) branches)
      (setf maximum (max maximum *regex-group-count*))
      (if (peek-type :pipe)
          (take-token)
          (return)))
    (setf *regex-group-count* maximum)
    (unless (peek-type :rparen)
      (fail "Unclosed branch-reset group"))
    (take-token)
    (if (cdr branches)
        (make-instance 'alternation-node :branches (nreverse branches))
        (car branches))))

(defun %parse-special-group ()
  (let ((first (%group-token-value-at 1)))
    (cond
      ((not (characterp first))
       (fail "Invalid special group prefix"))
      ((char= first #\[) (parse-extended-class))
      ((member first '(#\= #\!) :test #'char=) (%parse-lookaround))
      ((char= first #\C) (%parse-callout))
      ((char= first #\*) (%parse-lookaround))
      ((char= first #\<) (%parse-lookaround))
      ((char= first #\>) (%parse-atomic))
      ((char= first #\() (%parse-conditional))
      ((char= first #\|) (%parse-branch-reset))
      ((or (char= first #\&)
           (char= first #\P)
           (char= first #\R)
           (char= first #\+)
           (char= first #\-)
           (digit-char-p first))
       (%parse-subroutine))
      (t
       (fail "Invalid special group prefix")))))

(defun parse-group ()
  (take-token)
  (with-parser-nesting
    (*regex-nesting-depth*
      *regex-nest-limit*
      (fail "Regular expression exceeds the configured nesting limit"))
    (cond
      ((peek-type :star)
       (%parse-control-verb))
      ((%special-group-prefix-p)
       (%parse-special-group))
      (t
       (multiple-value-bind (capturing-p name scoped-flags bare-flags-p balance-name)
           (parse-group-prefix)
         (if bare-flags-p
             (make-concat nil)
             (let ((capture-index
                     (when capturing-p
                       (incf *regex-group-count*)))
                   (child (parse-alternation)))
               (unless (peek-type :rparen)
                 (fail "Unclosed group"))
               (take-token)
               (when scoped-flags
                 (setf *regex-flags* scoped-flags))
               (make-instance 'group-node
                              :child child
                              :capture-index capture-index
                              :name name
                              :balance-name balance-name))))))))
