;;;; src/regex-grammar.lisp
;;;;
;;;; PARSE-REGEX and the top-level grammar (alternation, concatenation,
;;;; repetition, groups, inline flags, atoms) over the token vector
;;;; TOKENIZE-REGEX-PATTERN produces. Character-class bodies are
;;;; regex-grammar-classes.lisp's concern.
;;;;
;;;; Shared parser state -- position in the token stream, accumulated flags,
;;;; capture bookkeeping -- lives in the dynamically-bound special variables
;;;; DEFVAR'd in parser-syntax.lisp, exactly as it did when this parser
;;;; walked characters directly (see the original rationale, preserved here:
;;;; binding them once in PARSE-REGEX and letting every other function read
;;;; and mutate them directly is the same technique CL-PPCRE's recursive-
;;;; descent parser uses). Swapping the character scanner for
;;;; CL-PARSER-KIT's token/span model changes what *REGEX-TOKEN-POSITION*
;;;; indexes and moves every escape/hex/octal/Unicode-property/POSIX-class
;;;; scan into the tokenizer, but does not change this shape: this grammar's
;;;; alternation/concatenation/repetition tiers have no genuine backtracking
;;;; ambiguity (every branch point resolves on one token of lookahead), so
;;;; they stay hand-written recursive descent over the token vector rather
;;;; than combinator pipelines -- CL-PARSER-KIT's own tokenizer/pratt/
;;;; combinator layers are a poor fit for a context-sensitive, single-
;;;; lookahead, error-position-precise grammar like this one, per its own
;;;; documented design center (token-stream languages with real operator
;;;; precedence). What CL-PARSER-KIT contributes here is its TOKEN/SPAN data
;;;; model and the tokenizer built on it.
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

(defun read-group-name (&optional (terminator #\>))
  (let ((beginning (current-source-position))
        (characters nil))
    (unless (and
        (peek-token)
        (not (eq (peek-type) :escape))
        (capture-name-start-p (peek-value)))
      (fail "Capture name must start with an alphabetic character or underscore"))
    (loop while (and
        (peek-token)
        (not (eq (peek-type) :escape))
        (capture-name-character-p (peek-value)))
          do (push (token-value (take-token)) characters))
    (let ((name (coerce (nreverse characters) 'string)))
      (unless (peek-type :char)
        (fail "Unclosed capture name"))
      (unless (char= (peek-value) terminator)
        (fail "Unclosed capture name"))
      (take-token)
      (when (and (not (flag-p +flag-duplicate-names+)) (member name *regex-group-names* :test #'string=)) (fail "Duplicate capture name" beginning))
      (push name *regex-group-names*)
      name)))

(defun parse-flags ()
  (let ((enabled-p t)
        (seen-p nil)
        (disabled-p nil)
        (seen-flags nil))
    (loop while (and
        (peek-type :char)
        (or
          (member (peek-value) (list #\i #\m #\s #\R #\U #\x #\u #\n #\J)
                  :test #'char=)
          (char= (peek-value) #\-)))
          do (if (char= (peek-value) #\-) (progn
          (when disabled-p
            (fail "Inline flags may contain only one -"))
          (take-token)
          (setf enabled-p nil
                disabled-p t))
        (let ((character (token-value (take-token))))
          (when (member character seen-flags :test #'char=)
            (fail "Duplicate inline flag"))
          (push character seen-flags)
          (setf *regex-flags* (update-parser-flag *regex-flags* character enabled-p))
          (setf seen-p t))))
    (unless seen-p
      (fail "Expected inline flags"))))

(defun parse-group-prefix ()
  "Parse the optional group introducer after the opening parenthesis.
Returns CAPTURING-P, NAME, SCOPED-FLAGS, BARE-FLAGS-P, and BALANCE-NAME.
BALANCE-NAME identifies a private capture history to pop before the child is
evaluated."
  (let ((capturing-p (and (not *regex-never-capture-p*) (not (flag-p +flag-no-auto-capture+))))
        (name nil)
        (balance-name nil)
        (scoped-flags nil))
    (when (peek-type :question)
      (take-token)
      (if (and (peek-type :char)
               (member (peek-value)
                       (list (code-char 58) (code-char 60) (code-char 80) (code-char 39))
                       :test (function char=)))
          (cond
            ((char= (peek-value) (code-char 58))
             (take-token)
             (setf capturing-p nil))
            ((char= (peek-value) (code-char 60))
             (take-token)
             (multiple-value-setq (name balance-name)
               (read-balanced-group-name))
             (setf capturing-p (not (null name))))
            ((char= (peek-value) (code-char 39))
             (take-token)
             (multiple-value-setq (name balance-name)
               (read-balanced-group-name (code-char 39)))
             (setf capturing-p (not (null name))))
            ((char= (peek-value) (code-char 80))
             (take-token)
             (unless (peek-type :char)
               (fail "Expected < or quote after ?P"))
             (cond
               ((char= (peek-value) (code-char 60))
                (take-token)
                (multiple-value-setq (name balance-name)
                  (read-balanced-group-name))
                (setf capturing-p (not (null name))))
               ((char= (peek-value) (code-char 39))
                (take-token)
                (multiple-value-setq (name balance-name)
                  (read-balanced-group-name (code-char 39)))
                (setf capturing-p (not (null name))))
               (t (fail "Expected < or quote after ?P")))))
          (let ((saved-flags *regex-flags*))
            (parse-flags)
            (cond
              ((peek-type :rparen)
               (take-token)
               (return-from parse-group-prefix
                 (values capturing-p name nil t balance-name)))
              ((and (peek-type :char) (char= (peek-value) (code-char 58)))
               (take-token)
               (setf capturing-p nil scoped-flags saved-flags))
              (t (fail "Expected : or ) after inline flags"))))))
    (values capturing-p name scoped-flags nil balance-name)))
(defun read-balanced-group-name (&optional (terminator (code-char 62)))
  (let ((beginning (current-source-position))
        (primary nil)
        (target nil)
        (characters nil))
    (when (and (peek-type :char)
               (char= (peek-value) (code-char 45)))
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
        (setf target (coerce (nreverse characters) (quote string)))
        (progn
          (setf primary (coerce (nreverse characters) (quote string)))
          (when (and (peek-type :char)
                     (char= (peek-value) (code-char 45)))
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
            (setf target (coerce (nreverse characters) (quote string))))))
    (unless (peek-type :char)
      (fail "Unclosed capture name"))
    (unless (char= (peek-value) terminator)
      (fail "Unclosed capture name"))
    (take-token)
    (when (and primary
               (not (flag-p +flag-duplicate-names+))
               (member primary *regex-group-names* :test (function string=)))
      (fail "Duplicate capture name" beginning))
    (when primary
      (push primary *regex-group-names*))
    (values primary target)))

(defun parse-group ()
  (take-token)
  (with-parser-nesting
    (*regex-nesting-depth*
      *regex-nest-limit*
      (fail "Regular expression exceeds the configured nesting limit"))
    (labels
        ((token-at-offset (offset)
           (let ((position (+ *regex-token-position* offset)))
             (and (< position (length *regex-tokens*))
                  (aref *regex-tokens* position))))
         (token-value-at (offset)
           (let ((token (token-at-offset offset)))
             (and token (token-value token))))
         (special-group-prefix-p () (when (peek-type :question) (let ((first (token-value-at 1)) (second (token-value-at 2)) (following (token-at-offset 2))) (and (characterp first) (or (member first (list #\= #\! #\> #\( #\& #\C #\* #\[) :test (function char=)) (and (member first (list #\+ #\-) :test (function char=)) (digit-char-p second)) (char= first #\|) (digit-char-p first) (and (char= first #\<) (characterp second) (member second (list #\= #\! #\*) :test (function char=))) (and (char= first #\P) (characterp second) (member second (list #\> #\=) :test (function char=))) (and (char= first #\R) (eq (and following (token-type following)) :rparen)))))))
         (parse-callout ()
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
                (coerce (nreverse digits) (quote string))))))
      ((and (peek-token)
            (member (token-value (peek-token))
                    (list #\" (code-char 39) #\^ #\% #\# #\$ #\{)
                    :test (function char=)))
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
              (setf tag
                    (coerce (nreverse characters) (quote string)))
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
    (make-instance (quote callout-node)
                   :number number
                   :tag tag)))
         (read-reference-name (terminator)
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
         (parse-control-verb ()
  (take-token)
  (let ((characters nil))
    (loop while (and (peek-token)
                     (peek-type :char)
                     (not (char= (peek-value) #\:)))
          do (push (token-value (take-token)) characters))
    (let* ((verb-name (string-upcase (coerce (nreverse characters) (quote string))))
           (lookaround-kind
            (cond
              ((member verb-name (list "POSITIVE_LOOKAHEAD" "PLA" "NON_ATOMIC_POSITIVE_LOOKAHEAD" "NAPLA") :test (function string=))
               :lookahead)
              ((member verb-name (list "NEGATIVE_LOOKAHEAD" "NLA")
                       :test (function string=))
               :negative-lookahead)
              ((member verb-name (list "POSITIVE_LOOKBEHIND" "PLB" "NON_ATOMIC_POSITIVE_LOOKBEHIND" "NAPLB") :test (function string=))
               :lookbehind)
              ((member verb-name (list "NEGATIVE_LOOKBEHIND" "NLB")
                       :test (function string=))
               :negative-lookbehind)
              (t nil))))
      (if lookaround-kind
          (progn (unless (and (peek-type :char) (char= (peek-value) #\:)) (fail "Named lookaround requires a colon")) (take-token) (let ((child (parse-alternation))) (unless (peek-type :rparen) (fail "Unclosed named lookaround")) (take-token) (make-instance (quote lookaround-node) :kind (if (member lookaround-kind (list :lookbehind :negative-lookbehind)) :lookbehind :lookahead) :child child :negative-p (not (null (member lookaround-kind (list :negative-lookahead :negative-lookbehind)))) :direction (if (member lookaround-kind (list :lookbehind :negative-lookbehind)) :backward :forward) :fixed-length nil :non-atomic-p (member verb-name (list "NON_ATOMIC_POSITIVE_LOOKAHEAD" "NAPLA" "NON_ATOMIC_POSITIVE_LOOKBEHIND" "NAPLB") :test (function string=)))))
          (let ((verb
                  (cond
                    ((zerop (length verb-name)) :mark)
                    ((member verb-name (list "FAIL" "F")
                             :test (function string=))
                     :fail)
                    ((member verb-name (list "SKIP" "S")
                             :test (function string=))
                     :skip)
                    ((member verb-name (list "PRUNE" "P")
                             :test (function string=))
                     :prune)
                    ((member verb-name (list "COMMIT" "C")
                             :test (function string=))
                     :commit)
                    ((string= verb-name "THEN") :then)
                    ((member verb-name (list "ACCEPT" "A")
                             :test (function string=))
                     :accept)
                    ((string= verb-name "MARK") :mark)
                    (t (fail "Unknown control verb"))))
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
                (setf argument (coerce (nreverse tag-characters)
                                       (quote string)))))
            (when (and (eq verb :mark) (null argument))
              (fail "MARK control verb requires :tag"))
            (unless (peek-type :rparen)
              (fail "Unclosed control verb"))
            (take-token)
            (make-instance (quote control-verb-node)
                           :verb verb
                           :argument argument))))))
         (parse-lookaround () (take-token) (let ((backward-p nil) (negative-p nil) (non-atomic-p nil)) (cond ((and (peek-type :char) (char= (peek-value) #\*)) (setf non-atomic-p t) (take-token)) ((and (peek-type :char) (member (peek-value) (list #\= #\!) :test (function char=))) (setf negative-p (char= (peek-value) #\!)) (take-token)) ((and (peek-type :char) (char= (peek-value) #\<)) (setf backward-p t) (take-token) (cond ((and (peek-type :char) (char= (peek-value) #\*)) (setf non-atomic-p t) (take-token)) ((and (peek-type :char) (member (peek-value) (list #\= #\!) :test (function char=))) (setf negative-p (char= (peek-value) #\!)) (take-token)) (t (fail "Lookbehind must use <=, <!, or <*")))) (t (fail "Invalid lookaround prefix"))) (when (and non-atomic-p negative-p) (fail "Non-atomic lookaround must be positive")) (let ((child (parse-alternation))) (unless (peek-type :rparen) (fail "Unclosed lookaround")) (take-token) (make-instance (quote lookaround-node) :kind (if backward-p :lookbehind :lookahead) :child child :negative-p negative-p :direction (if backward-p :backward :forward) :fixed-length nil :non-atomic-p non-atomic-p))))
         (parse-atomic ()
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
         (parse-subroutine ()
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
          (let ((next (token-at-offset 1)))
            (and next
                 (eq (token-type next) :char)
                 (char= (token-value next) #\=))))
     (take-token)
     (take-token)
     (let ((name (read-reference-name :rparen)))
       (make-instance 'backreference-node
                      :capture-index nil
                      :name name
                      :case-insensitive-p
                      (flag-p +flag-case-insensitive+)
                      :unicode-p (flag-p +flag-unicode+))))
    ((and (peek-type :char)
          (char= (peek-value) #\P)
          (let ((next (token-at-offset 1)))
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
          (digit-char-p (token-value-at 1)))
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
         (parse-conditional ()
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
                              (parse-integer
                               (coerce (nreverse digits) (quote string))))))
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
                                 (parse-integer
                                  (coerce (nreverse digits) (quote string))))))
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
         (parse-special-group () (let ((first (token-value-at 1))) (cond ((not (characterp first)) (fail "Invalid special group prefix")) ((char= first #\[) (parse-extended-class)) ((char= first #\=) (parse-lookaround)) ((char= first #\!) (parse-lookaround)) ((char= first #\C) (parse-callout)) ((char= first #\*) (parse-lookaround)) ((char= first #\<) (parse-lookaround)) ((char= first #\>) (parse-atomic)) ((char= first #\() (parse-conditional)) ((char= first #\|) (take-token) (take-token) (let ((base *regex-group-count*) (maximum *regex-group-count*) (branches nil)) (loop (setf *regex-group-count* base) (push (parse-concatenation) branches) (setf maximum (max maximum *regex-group-count*)) (if (peek-type :pipe) (take-token) (return))) (setf *regex-group-count* maximum) (unless (peek-type :rparen) (fail "Unclosed branch-reset group")) (take-token) (if (cdr branches) (make-instance (quote alternation-node) :branches (nreverse branches)) (car branches)))) ((or (char= first #\&) (char= first #\P) (char= first #\R) (char= first #\+) (char= first #\-) (digit-char-p first)) (parse-subroutine)) (t (fail "Invalid special group prefix"))))))
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
                 (make-instance (quote group-node)
                                :child child
                                :capture-index capture-index
                                :name name
                                :balance-name balance-name)))))))))

(defun parse-atom ()
  (when (at-end-p)
    (fail "Expected an expression"))
  (let ((token (peek-token)))
    (case (token-type token)
      (:lparen (parse-group))
      (:lbracket (parse-class))
      (:dot
        (take-token)
        (make-instance
          (quote any-char-node)
          :dotall-p
          (flag-p +flag-dotall+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*
          :unicode-p
          (flag-p +flag-unicode+)))
      (:caret
        (take-token)
        (make-instance
          (quote anchor-node)
          :kind
          :start
          :multiline-p
          (flag-p +flag-multiline+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*))
      (:dollar
        (take-token)
        (make-instance
          (quote anchor-node)
          :kind
          :end
          :multiline-p
          (flag-p +flag-multiline+)
          :crlf-p
          (flag-p +flag-crlf+)
          :line-terminator
          *regex-line-terminator*))
      (:escape
        (take-token)
        (build-escape-atom token))
      (:hex-brace-open
        (take-token)
        (make-literal (collect-braced-hex)))
      (:octal-brace-open
        (take-token)
        (make-literal
          (ensure-byte-character (collect-braced-hex 8 "octal"))
          :raw-octet-p (not (flag-p +flag-unicode+))))
      ((:star :plus :question :rbrace :pipe :rparen)
        (fail "Unexpected metacharacter"))
      (otherwise
        (take-token)
        (make-literal (token-value token))))))

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

(defun parse-concatenation ()
  (let ((children nil))
    (loop until (or (at-end-p) (member (peek-type) '(:pipe :rparen)))
          do (push (parse-quantifier (parse-atom)) children))
    (make-concat (nreverse children))))

(defun parse-alternation ()
  (let ((branches (list (parse-concatenation))))
    (loop while (peek-type :pipe)
          do (take-token) (push (parse-concatenation) branches))
    (if (cdr branches) (make-instance 'alternation-node :branches (nreverse branches)) (car branches))))

(defun parse-regex (pattern
    &key
    (initial-flags +flag-unicode+)
    byte-mode
    literal
    never-capture
    (octal t)
    (nest-limit +default-nest-limit+)
    (line-terminator #\Newline))
  "Parse PATTERN into a REGEX-NODE tree.
Signals REGEX-SYNTAX-ERROR on malformed input."
  (unless (stringp pattern)
    (error 'regex-syntax-error :pattern pattern :reason "Pattern must be a string"))
  (check-type initial-flags (integer 0 *))
  (check-type byte-mode boolean)
  (check-type literal boolean)
  (check-type never-capture boolean)
  (check-type octal boolean)
  (check-type nest-limit (integer 0 *))
  (check-type line-terminator character)
  (let* ((*regex-pattern* pattern)
         (*regex-length* (length pattern))
         (*regex-byte-mode-p* byte-mode)
         (*regex-octal-p* octal)
         (*regex-tokens* (tokenize-regex-pattern pattern))
         (*regex-token-position* 0)
         (*regex-group-count* 0)
         (*regex-group-names* nil)
         (*regex-flags* initial-flags)
         (*regex-nesting-depth* 0)
         (*regex-never-capture-p* never-capture)
         (*regex-nest-limit* nest-limit)
         (*regex-line-terminator* line-terminator))
    (let ((ast
          (if literal (prog1
              (make-concat
                (loop for character across pattern
                      collect (make-literal character)))
              (setf *regex-token-position* (length *regex-tokens*)))
            (parse-alternation))))
      (unless (at-end-p)
        (fail "Unexpected trailing input"))
      ast)))
