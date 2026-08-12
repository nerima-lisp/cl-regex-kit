(in-package #:cl-regex-kit)

(defun parse-group-token-at-offset (offset)
  (let ((position (+ *regex-token-position* offset)))
    (and (< position (length *regex-tokens*))
         (aref *regex-tokens* position))))

(defun parse-group-token-value-at (offset)
  (let ((token (parse-group-token-at-offset offset)))
    (and token (token-value token))))

(defun parse-flags ()
  (let ((enabled-p t)
        (seen-p nil)
        (disabled-p nil)
        (seen-flags nil))
    (loop while (and
                 (peek-type :char)
                 (or (member (peek-value) (list #\i #\m #\s #\R #\U #\x #\u #\n #\J)
                             :test #'char=)
                     (char= (peek-value) #\-)))
          do (if (char= (peek-value) #\-)
                 (progn
                   (when disabled-p
                     (fail "Inline flags may contain only one -"))
                   (take-token)
                   (setf enabled-p nil
                         disabled-p t))
                 (let ((character (token-value (take-token))))
                   (when (member character seen-flags :test #'char=)
                     (fail "Duplicate inline flag"))
                   (push character seen-flags)
                   (setf *regex-flags*
                         (update-parser-flag *regex-flags* character enabled-p))
                   (setf seen-p t))))
    (unless seen-p
      (fail "Expected inline flags"))))

(defun read-balanced-group-name (&optional (terminator #\>))
  (let ((beginning (current-source-position))
        (primary nil)
        (target nil)
        (characters nil))
    (when (and (peek-type :char)
               (char= (peek-value) #\-))
      (take-token)
      (setf target :leading))
    (unless (and (peek-token)
                 (not (eq (peek-type) :escape))
                 (capture-name-start-p (peek-value)))
      (fail "Capture name must start with an alphabetic character or underscore"))
    (loop while (and (peek-token)
                     (not (eq (peek-type) :escape))
                     (capture-name-character-p (peek-value)))
          do (push (token-value (take-token)) characters))
    (if (eq target :leading)
        (setf target (coerce (nreverse characters) 'string))
        (progn
          (setf primary (coerce (nreverse characters) 'string))
          (when (and (peek-type :char)
                     (char= (peek-value) #\-))
            (take-token)
            (setf characters nil)
            (unless (and (peek-token)
                         (not (eq (peek-type) :escape))
                         (capture-name-start-p (peek-value)))
              (fail "Balance target must start with an alphabetic character or underscore"))
            (loop while (and (peek-token)
                             (not (eq (peek-type) :escape))
                             (capture-name-character-p (peek-value)))
                  do (push (token-value (take-token)) characters))
            (setf target (coerce (nreverse characters) 'string)))))
    (unless (peek-type :char)
      (fail "Unclosed capture name"))
    (unless (char= (peek-value) terminator)
      (fail "Unclosed capture name"))
    (take-token)
    (when (and primary
               (not (flag-p +flag-duplicate-names+))
               (member primary *regex-group-names* :test #'string=))
      (fail "Duplicate capture name" beginning))
    (when primary
      (push primary *regex-group-names*))
    (values primary target)))

(defun parse-group-prefix ()
  "Parse the optional group introducer after the opening parenthesis.
Returns CAPTURING-P, NAME, SCOPED-FLAGS, BARE-FLAGS-P, and BALANCE-NAME.
BALANCE-NAME identifies a private capture history to pop before the child is
evaluated."
  (let ((capturing-p (and (not *regex-never-capture-p*)
                          (not (flag-p +flag-no-auto-capture+))))
        (name nil)
        (balance-name nil)
        (scoped-flags nil))
    (when (peek-type :question)
      (take-token)
      (if (and (peek-type :char)
               (member (peek-value) (list #\: #\< #\P #\') :test #'char=))
          (cond
            ((char= (peek-value) #\:)
             (take-token)
             (setf capturing-p nil))
            ((char= (peek-value) #\<)
             (take-token)
             (multiple-value-setq (name balance-name)
               (read-balanced-group-name))
             (setf capturing-p (not (null name))))
            ((char= (peek-value) #\')
             (take-token)
             (multiple-value-setq (name balance-name)
               (read-balanced-group-name #\'))
             (setf capturing-p (not (null name))))
            ((char= (peek-value) #\P)
             (take-token)
             (unless (peek-type :char)
               (fail "Expected < or quote after ?P"))
             (cond
               ((char= (peek-value) #\<)
                (take-token)
                (multiple-value-setq (name balance-name)
                  (read-balanced-group-name))
                (setf capturing-p (not (null name))))
               ((char= (peek-value) #\')
                (take-token)
                (multiple-value-setq (name balance-name)
                  (read-balanced-group-name #\'))
                (setf capturing-p (not (null name))))
               (t
                (fail "Expected < or quote after ?P")))))
          (let ((saved-flags *regex-flags*))
            (parse-flags)
            (cond
              ((peek-type :rparen)
               (take-token)
               (return-from parse-group-prefix
                 (values capturing-p name nil t balance-name)))
              ((and (peek-type :char) (char= (peek-value) #\:))
               (take-token)
               (setf capturing-p nil
                     scoped-flags saved-flags))
              (t
               (fail "Expected : or ) after inline flags"))))))
    (values capturing-p name scoped-flags nil balance-name)))

(defun special-group-prefix-p ()
  (when (peek-type :question)
    (let ((first (parse-group-token-value-at 1))
          (second (parse-group-token-value-at 2))
          (following (parse-group-token-at-offset 2)))
      (and (characterp first)
           (or (member first (list #\= #\! #\> #\( #\& #\C #\*)
                       :test #'char=)
               (and (member first (list #\+ #\-) :test #'char=)
                    (digit-char-p second))
               (char= first #\|)
               (digit-char-p first)
               (and (char= first #\<)
                    (characterp second)
                    (member second (list #\= #\! #\*) :test #'char=))
               (and (char= first #\P)
                    (characterp second)
                    (member second (list #\> #\=) :test #'char=))
               (and (char= first #\R)
                    (eq (and following (token-type following)) :rparen)))))))

(defun parse-callout ()
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
                    (list #\" (code-char 39) #\^ #\% #\# #\$ #\{)
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
    (make-instance 'callout-node
                   :number number
                   :tag tag)))

(defun read-reference-name (terminator)
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

(defun parse-control-verb ()
  (take-token)
  (let ((characters nil))
    (loop while (and (peek-token)
                     (peek-type :char)
                     (not (char= (peek-value) #\:)))
          do (push (token-value (take-token)) characters))
    (let* ((verb-name (string-upcase (coerce (nreverse characters) 'string)))
           (lookaround-kind (named-lookaround-kind verb-name)))
      (if lookaround-kind
          (progn
            (unless (and (peek-type :char) (char= (peek-value) #\:))
              (fail "Named lookaround requires a colon"))
            (take-token)
            (let ((child (parse-alternation)))
              (unless (peek-type :rparen)
                (fail "Unclosed named lookaround"))
              (take-token)
              (make-instance 'lookaround-node
                             :kind (if (member lookaround-kind
                                               (list :lookbehind :negative-lookbehind))
                                        :lookbehind
                                        :lookahead)
                             :child child
                             :negative-p (not (null (member lookaround-kind
                                                           (list :negative-lookahead
                                                                 :negative-lookbehind))))
                             :direction (if (member lookaround-kind
                                                    (list :lookbehind
                                                          :negative-lookbehind))
                                            :backward
                                            :forward)
                             :fixed-length nil
                             :non-atomic-p (non-atomic-lookaround-verb-p verb-name))))
          (let ((verb (control-verb-kind verb-name))
                (argument nil))
            (when (and (peek-type :char)
                       (char= (peek-value) #\:))
              (when (eq verb :fail)
                (fail "FAIL control verb does not accept an argument"))
              (take-token)
              (let ((tag-characters nil))
                (loop while (and (peek-token)
                                 (not (peek-type :rparen)))
                      do (unless (peek-type :char)
                           (fail "Invalid control verb argument"))
                         (push (token-value (take-token)) tag-characters))
                (when (null tag-characters)
                  (fail "Control verb argument requires a non-empty tag"))
                (setf argument (coerce (nreverse tag-characters) 'string))))
            (when (and argument
                       (not (control-verb-accepts-argument-p verb)))
              (fail (format nil "~A control verb does not accept an argument"
                            verb-name)))
            (when (and (control-verb-requires-argument-p verb)
                       (null argument))
              (fail "MARK control verb requires :tag"))
            (unless (peek-type :rparen)
              (fail "Unclosed control verb"))
            (take-token)
            (make-instance 'control-verb-node
                           :verb verb
                           :argument argument))))))

(defun parse-lookaround ()
  (take-token)
  (let ((backward-p nil)
        (negative-p nil)
        (non-atomic-p nil))
    (cond
      ((or (peek-type :star)
           (and (peek-type :char)
                (char= (peek-value) #\*)))
       (setf non-atomic-p t)
       (take-token))
      ((and (peek-type :char)
            (member (peek-value) (list #\= #\!) :test #'char=))
       (setf negative-p (char= (peek-value) #\!))
       (take-token))
      ((and (peek-type :char)
            (char= (peek-value) #\<))
       (setf backward-p t)
       (take-token)
       (cond
         ((or (peek-type :star)
              (and (peek-type :char)
                   (char= (peek-value) #\*)))
          (setf non-atomic-p t)
          (take-token))
         ((and (peek-type :char)
               (member (peek-value) (list #\= #\!) :test #'char=))
          (setf negative-p (char= (peek-value) #\!))
          (take-token))
         (t
          (fail "Lookbehind must use <=, <!, or <*"))))
      (t
       (fail "Invalid lookaround prefix")))
    (when (and non-atomic-p negative-p)
      (fail "Non-atomic lookaround must be positive"))
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

(defun parse-atomic ()
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

(defun parse-subroutine ()
  (take-token)
  (cond
    ((and (peek-type :char)
          (char= (peek-value) #\&))
     (take-token)
     (let ((name (read-reference-name :rparen)))
       (make-instance 'subroutine-node
                      :target name
                      :name name
                      :capture-index nil
                      :recursive-p nil)))
    ((and (peek-type :char)
          (char= (peek-value) #\R))
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
          (let ((next (parse-group-token-at-offset 1)))
            (and next
                 (eq (token-type next) :char)
                 (char= (token-value next) #\=))))
     (take-token)
     (take-token)
     (let ((name (read-reference-name :rparen)))
       (make-instance 'backreference-node
                      :capture-index nil
                      :name name
                      :case-insensitive-p (flag-p +flag-case-insensitive+)
                      :unicode-p (flag-p +flag-unicode+))))
    ((and (peek-type :char)
          (char= (peek-value) #\P)
          (let ((next (parse-group-token-at-offset 1)))
            (and next
                 (eq (token-type next) :char)
                 (char= (token-value next) #\>))))
     (take-token)
     (take-token)
     (let ((name (read-reference-name :rparen)))
       (make-instance 'subroutine-node
                      :target name
                      :name name
                      :capture-index nil
                      :recursive-p nil)))
    ((and (or (peek-type :plus)
              (and (peek-type :char)
                   (char= (peek-value) #\-)))
          (digit-char-p (parse-group-token-value-at 1)))
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
       (let ((target (parse-integer (coerce (nreverse digits) 'string))))
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

(defun parse-conditional ()
  (take-token)
  (unless (peek-type :lparen)
    (fail "Conditional group must start with a condition"))
  (take-token)
  (let ((condition
          (cond
            ((peek-type :question)
             (parse-lookaround))
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
                  (char= (peek-value) (code-char 60)))
             (take-token)
             (let ((name (read-reference-name (code-char 62))))
               (unless (peek-type :rparen)
                 (fail "Unclosed conditional condition"))
               (take-token)
               (list :name name)))
            ((and (peek-type :char)
                  (char= (peek-value) (code-char 82)))
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
                     (char= (peek-value) (code-char 38)))
                (take-token)
                (list :recursion-name
                      (read-reference-name :rparen)))
               (t
                (fail "Invalid recursive condition"))))
            ((and (peek-type :char)
                  (capture-name-start-p (peek-value)))
             (let ((name (read-reference-name :rparen)))
               (cond
                 ((string= name "DEFINE") :define)
                 (t (list :name name)))))
            (t
             (fail "Conditional group requires a capture reference")))))
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

(defun parse-branch-reset ()
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
        (make-instance 'alternation-node
                       :branches (nreverse branches))
        (car branches))))

(defun parse-special-group ()
  (let ((first (parse-group-token-value-at 1)))
    (unless (characterp first)
      (fail "Invalid special group prefix"))
    (dispatch-on-character first
      ((#\= #\! #\* #\<)
       (return-from parse-special-group
         (parse-lookaround)))
      ((#\C)
       (return-from parse-special-group
         (parse-callout)))
      ((#\>)
       (return-from parse-special-group
         (parse-atomic)))
      ((#\()
       (return-from parse-special-group
         (parse-conditional)))
      ((#\|)
       (return-from parse-special-group
         (parse-branch-reset))))
    (when (or (digit-char-p first)
              (member first (list #\& #\P #\R #\+ #\-) :test #'char=))
      (return-from parse-special-group
        (parse-subroutine)))
    (fail "Invalid special group prefix")))

(defun parse-group ()
  (take-token)
  (with-parser-nesting
    (*regex-nesting-depth*
     *regex-nest-limit*
     (fail "Regular expression exceeds the configured nesting limit"))
    (cond
      ((peek-type :star)
       (parse-control-verb))
      ((special-group-prefix-p)
       (parse-special-group))
      (t
       (multiple-value-bind (capturing-p name scoped-flags bare-flags-p balance-name)
           (parse-group-prefix)
         (if bare-flags-p
             (make-concat nil)
             (let ((capture-index
                     (when capturing-p (incf *regex-group-count*)))
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
