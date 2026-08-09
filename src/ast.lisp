;;;; src/ast.lisp
;;;;
;;;; The parse tree PARSER produces and NFA compiles. Every syntax feature the
;;;; engine supports has exactly one node class here; there is no separate
;;;; "optimized" tree, since Thompson construction compiles this shape
;;;; directly.
(in-package #:cl-regex-kit)

(defmacro define-regex-node (name superclasses slots &optional documentation)
  "Define a REGEX-NODE subclass NAME from a compact SLOTS spec.

Each entry in SLOTS is (SLOT-NAME &key initform documentation): SLOT-NAME
gets a keyword initarg of the same name and a reader NAME-SLOT-NAME, exactly
as a hand-written DEFCLASS would. This keeps every node's shape declarative
data while the class-generation logic lives in one place."
  `(defclass ,name (,@superclasses)
     (,@(mapcar
         (lambda (slot)
           (destructuring-bind (slot-name &key (initform nil initform-p) documentation)
               slot
             `(,slot-name
               :initarg ,(intern (symbol-name slot-name) :keyword)
               :reader ,(intern (format nil "~A-~A" name slot-name))
               ,@(when initform-p (list :initform initform))
               ,@(when documentation (list :documentation documentation)))))
         slots))
     ,@(when documentation `((:documentation ,documentation)))))

(define-regex-node regex-node () ()
  "Base class for every regex AST node.")

(define-regex-node literal-node (regex-node)
  ((char)
   (raw-octet-p :initform nil
                :documentation "True when CHAR came from a raw byte escape.")
   (case-insensitive-p :initform nil)
   (unicode-p :initform t))
  "A single literal character.")

(define-regex-node concat-node (regex-node)
  ((children))
  "CHILDREN matched one after another.")

(define-regex-node alternation-node (regex-node)
  ((branches))
  "The first BRANCH that matches wins: `a|b|c`.")

(define-regex-node repetition-node (regex-node)
  ((child)
   (min)
   (max :initform nil
        :documentation "NIL means unbounded, as in `*` and `+`.")
   (greedy-p :initform t))
  "CHILD repeated between MIN and MAX times: `*`, `+`, `?`, `{m,n}`.")

(define-regex-node group-node (regex-node) ((child) (capture-index :initform nil :documentation "NIL for a non-capturing group `(?:...)`.") (name :initform nil) (balance-name :initform nil)) "A parenthesized group, capturing or not.")

(define-regex-node char-class-node (regex-node)
  ((ranges :documentation "A list of (START . END) char-code ranges, inclusive.")
   (matcher :initform nil
            :documentation "A Boolean character-set matcher expression, when required.")
   (negated-p :initform nil)
   (case-insensitive-p :initform nil)
   (unicode-p :initform t))
  "A character class: `[abc]`, `[a-z]`, `[^0-9]`, or a property/set expression.")

(define-regex-node any-char-node (regex-node)
  ((dotall-p :initform nil)
   (crlf-p :initform nil)
   (line-terminator :initform #\Newline)
   (unicode-p :initform t))
  "`.` -- matches any character except newline.")
(define-regex-node line-break-node (regex-node)
  ((unicode-p :initform t))
  "A consuming Unicode-aware line-break sequence.")

(define-regex-node anchor-node (regex-node)
  ((kind :documentation "An anchor or boundary keyword.")
   (multiline-p :initform nil)
   (crlf-p :initform nil)
   (line-terminator :initform #\Newline)
   (unicode-p :initform nil))
  "A zero-width position assertion.")
(define-regex-node reset-match-start-node (regex-node) () "Reset the reported match start at the current position.")

(define-regex-node possessive-repetition-node (regex-node)
  ((child)
   (min)
   (max :initform nil
        :documentation "NIL means unbounded, as in \`*\` and \`+\`.")
   (greedy-p :initform t)
   (possessive-p :initform t))
  "CHILD repeated between MIN and MAX times without backtracking.")

(define-regex-node assertion-node (regex-node)
  ((kind :initform :lookahead
         :documentation "The assertion kind, such as :LOOKAHEAD or :LOOKBEHIND.")
   (child :initform nil)
   (negative-p :initform nil)
   (direction :initform :forward
              :documentation "The matching direction, :FORWARD or :BACKWARD.")
   (fixed-length :initform nil :documentation "Known fixed width for a lookbehind, when available.") (non-atomic-p :initform nil :documentation "True when a positive assertion may backtrack into its child."))
  "A zero-width assertion with optional CHILD.")

(define-regex-node lookaround-node (assertion-node)
  ()
  "A zero-width lookahead or lookbehind assertion.")

(define-regex-node atomic-node (regex-node)
  ((child))
  "A group whose successful match is not backtracked into.")

(define-regex-node backreference-node (regex-node)
  ((capture-index :initform nil
                  :documentation "Numeric capture identifier, when present.")
   (name :initform nil
         :documentation "Named capture identifier, when present.")
   (case-insensitive-p :initform nil)
   (unicode-p :initform t))
  "A reference to text captured by a named or numbered group.")

(define-regex-node grapheme-node (regex-node)
  ((extended-p :initform t
               :documentation "True when matching an extended grapheme cluster.")
   (unicode-p :initform t))
  "A Unicode grapheme cluster.")

(define-regex-node conditional-node (regex-node)
  ((condition)
   (yes-branch :initform nil
               :documentation "Branch selected when CONDITION is true.")
   (no-branch :initform nil
              :documentation "Branch selected when CONDITION is false."))
  "A conditional expression with optional yes and no branches.")

(define-regex-node subroutine-node (regex-node)
  ((target :initform nil
           :documentation "A resolved node or symbolic subpattern target.")
   (name :initform nil)
   (capture-index :initform nil)
   (recursive-p :initform nil))
  "A call to a named or numbered subpattern.")

(define-regex-node recursion-node (subroutine-node)
  ((recursive-p :initform t))
  "A recursive subroutine call.")

(define-regex-node control-verb-node (regex-node)
  ((verb)
   (argument :initform nil
             :documentation "Optional control-verb argument."))
  "A backtracking control verb such as (*SKIP) or (*FAIL).")
(define-regex-node callout-node (regex-node) ((number :initform 0) (tag :initform nil :documentation "Optional callout tag.")) "A PCRE2-style zero-width callout.")

(defun utf8-octets-for-character (character)
  "Return the UTF-8 encoding of CHARACTER as a list of octets."
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) (list code))
      ((<= code #x7ff)
       (list (logior #xc0 (ash code -6))
             (logior #x80 (logand code #x3f))))
      ((<= code #xffff)
       (list (logior #xe0 (ash code -12))
             (logior #x80 (logand (ash code -6) #x3f))
             (logior #x80 (logand code #x3f))))
      (t
       (list (logior #xf0 (ash code -18))
             (logior #x80 (logand (ash code -12) #x3f))
             (logior #x80 (logand (ash code -6) #x3f))
             (logior #x80 (logand code #x3f)))))))

(defun normalize-byte-literals (node &optional (byte-mode-p t))
  "Encode source literals in raw byte scopes as their UTF-8 octets."
  (labels ((normalize-child (child)
             (and child (normalize-byte-literals child byte-mode-p)))
           (normalize-children (children)
             (let ((changed-p nil))
               (values
                (mapcar (lambda (child)
                          (let ((normalized (normalize-byte-literals child byte-mode-p)))
                            (unless (eq normalized child)
                              (setf changed-p t))
                            normalized))
                        children)
                changed-p)))
           (normalize-assertion (assertion)
             (let ((old-child (assertion-node-child assertion))
                   (child (normalize-child (assertion-node-child assertion))))
               (if (eq child old-child)
                   assertion
                   (make-instance (class-name (class-of assertion)) :kind (assertion-node-kind assertion) :child child :negative-p (assertion-node-negative-p assertion) :direction (assertion-node-direction assertion) :fixed-length (assertion-node-fixed-length assertion) :non-atomic-p (assertion-node-non-atomic-p assertion)))))
           (normalize-literal (literal)
             (if (and byte-mode-p (not (literal-node-raw-octet-p literal)) (> (char-code (literal-node-char literal)) 127))
                 (let ((octets (utf8-octets-for-character (literal-node-char literal))))
                   (make-instance 'concat-node
                                  :children
                                  (mapcar (lambda (octet) (make-instance 'literal-node :char (code-char octet) :raw-octet-p t :case-insensitive-p (literal-node-case-insensitive-p literal) :unicode-p nil))
                                          octets)))
                 literal)))
    (typecase node
      (literal-node
       (normalize-literal node))
      ((or char-class-node any-char-node anchor-node line-break-node reset-match-start-node)
       node)
      (concat-node
       (multiple-value-bind (children changed-p)
           (normalize-children (concat-node-children node))
         (if changed-p
             (make-instance 'concat-node :children children)
             node)))
      (alternation-node
       (multiple-value-bind (branches changed-p)
           (normalize-children (alternation-node-branches node))
         (if changed-p
             (make-instance 'alternation-node :branches branches)
             node)))
      (repetition-node
       (let ((old-child (repetition-node-child node))
             (child (normalize-child (repetition-node-child node))))
         (if (eq child old-child)
             node
             (make-instance 'repetition-node
                            :child child
                            :min (repetition-node-min node)
                            :max (repetition-node-max node)
                            :greedy-p (repetition-node-greedy-p node)))))
      (possessive-repetition-node
       (let ((old-child (possessive-repetition-node-child node))
             (child (normalize-child (possessive-repetition-node-child node))))
         (if (eq child old-child)
             node
             (make-instance 'possessive-repetition-node
                            :child child
                            :min (possessive-repetition-node-min node)
                            :max (possessive-repetition-node-max node)
                            :greedy-p (possessive-repetition-node-greedy-p node)
                            :possessive-p
                            (possessive-repetition-node-possessive-p node)))))
      (group-node
       (let ((old-child (group-node-child node))
             (child (normalize-child (group-node-child node))))
         (if (eq child old-child)
             node
             (make-instance 'group-node :child child :capture-index (group-node-capture-index node) :name (group-node-name node) :balance-name (group-node-balance-name node)))))
      ((or assertion-node lookaround-node)
       (normalize-assertion node))
      (atomic-node
       (let ((old-child (atomic-node-child node))
             (child (normalize-child (atomic-node-child node))))
         (if (eq child old-child)
             node
             (make-instance 'atomic-node :child child))))
      (conditional-node
       (let* ((old-yes (conditional-node-yes-branch node))
              (old-no (conditional-node-no-branch node))
              (yes (normalize-child old-yes))
              (no (normalize-child old-no)))
         (if (and (eq yes old-yes) (eq no old-no))
             node
             (make-instance 'conditional-node
                            :condition (conditional-node-condition node)
                            :yes-branch yes
                            :no-branch no))))
      (subroutine-node
       (let* ((old-target (subroutine-node-target node))
              (target (if (typep old-target 'regex-node)
                          (normalize-child old-target)
                          old-target)))
         (if (eq target old-target)
             node
             (make-instance (class-name (class-of node))
                            :target target
                            :name (subroutine-node-name node)
                            :capture-index
                            (subroutine-node-capture-index node)
                            :recursive-p
                            (subroutine-node-recursive-p node)))))
      (otherwise
       node))))

(progn
  (defun ast-contains-advanced-p (node)
  "Return true when NODE requires ordered backtracking execution."
  (typecase node
    ((or backreference-node grapheme-node assertion-node atomic-node
         possessive-repetition-node conditional-node subroutine-node
         control-verb-node callout-node reset-match-start-node)
     t)
    (anchor-node
     (member (anchor-node-kind node)
             '(:match-start :match-end :end-before-final-newline
               :grapheme-boundary :word-boundary-unicode
               :sentence-boundary)
             :test #'eq))
    (concat-node
     (some #'ast-contains-advanced-p (concat-node-children node)))
    (alternation-node
     (some #'ast-contains-advanced-p (alternation-node-branches node)))
    (repetition-node
     (ast-contains-advanced-p (repetition-node-child node)))
    (group-node
     (or (group-node-balance-name node)
         (ast-contains-advanced-p (group-node-child node))))
    (otherwise nil)))
  (defun ast-fixed-length (node byte-mode-p)
  "Return NODE's exact consumed width, or NIL when its width is unknown."
  (typecase node
    (literal-node
     (if (and byte-mode-p
              (literal-node-unicode-p node)
              (> (char-code (literal-node-char node)) #x7f))
         nil
         1))
    (char-class-node
     (if (and byte-mode-p (char-class-node-unicode-p node))
         nil
         1))
    (any-char-node
     (if (or (any-char-node-crlf-p node)
             (and byte-mode-p (any-char-node-unicode-p node)))
         nil
         1))
    (line-break-node nil)
    ((or anchor-node reset-match-start-node assertion-node) 0)
    (concat-node
     (loop with total = 0
           for child in (concat-node-children node)
           for width = (ast-fixed-length child byte-mode-p)
           unless (integerp width)
             do (return nil)
           do (incf total width)
           finally (return total)))
    (alternation-node
     (let ((widths (mapcar (lambda (branch)
                             (ast-fixed-length branch byte-mode-p))
                           (alternation-node-branches node))))
       (when (and widths
                  (every (function integerp) widths)
                  (apply (function =) widths))
         (first widths))))
    (repetition-node
     (let ((width (ast-fixed-length (repetition-node-child node) byte-mode-p))
           (max (repetition-node-max node)))
       (when (and (integerp width)
                  (integerp max)
                  (= (repetition-node-min node) max))
         (* (repetition-node-min node) width))))
    (possessive-repetition-node
     (let ((width (ast-fixed-length
                   (possessive-repetition-node-child node)
                   byte-mode-p))
           (max (possessive-repetition-node-max node)))
       (when (and (integerp width)
                  (integerp max)
                  (= (possessive-repetition-node-min node) max))
         (* (possessive-repetition-node-min node) width))))
    (group-node (ast-fixed-length (group-node-child node) byte-mode-p))
    (atomic-node (ast-fixed-length (atomic-node-child node) byte-mode-p))
    (otherwise nil)))
  (defun annotate-lookbehind-lengths (node byte-mode-p)
  "Annotate lookbehind nodes after byte-mode AST normalization."
  (let ((seen (make-hash-table :test (function eq))))
    (labels ((visit (current)
               (when (and current
                          (not (gethash current seen)))
                 (setf (gethash current seen) t)
                 (typecase current
                   (assertion-node
                    (when (eq (assertion-node-direction current) :backward)
                      (setf (slot-value current (quote fixed-length))
                            (ast-fixed-length
                             (assertion-node-child current)
                             byte-mode-p)))
                    (visit (assertion-node-child current)))
                   (concat-node
                    (mapc (function visit) (concat-node-children current)))
                   (alternation-node
                    (mapc (function visit) (alternation-node-branches current)))
                   (repetition-node
                    (visit (repetition-node-child current)))
                   (possessive-repetition-node
                    (visit (possessive-repetition-node-child current)))
                   (group-node
                    (visit (group-node-child current)))
                   (atomic-node
                    (visit (atomic-node-child current)))
                   (conditional-node
                    (let ((condition (conditional-node-condition current)))
                      (when (typep condition (quote regex-node))
                        (visit condition)))
                    (visit (conditional-node-yes-branch current))
                    (visit (conditional-node-no-branch current)))
                   (subroutine-node
                    (let ((target (subroutine-node-target current)))
                      (when (typep target (quote regex-node))
                        (visit target))))
                   (otherwise nil)))))
      (visit node)))
  node)

  (defun ast-group-count (node)
    "Return the highest capture index present in NODE."
    (labels ((maximum (children)
               (loop with count = 0
                     for child in children do
                       (setf count (max count (ast-group-count child)))
                     finally (return count))))
      (typecase node
        (group-node
         (max (or (group-node-capture-index node) 0)
              (ast-group-count (group-node-child node))))
        (concat-node (maximum (concat-node-children node)))
        (alternation-node (maximum (alternation-node-branches node)))
        (repetition-node (ast-group-count (repetition-node-child node)))
        (possessive-repetition-node
         (ast-group-count (possessive-repetition-node-child node)))
        (assertion-node (ast-group-count (assertion-node-child node)))
        (atomic-node (ast-group-count (atomic-node-child node)))
        (conditional-node
         (max (ast-group-count (conditional-node-yes-branch node))
              (ast-group-count (conditional-node-no-branch node))))
        (subroutine-node
         (ast-group-count (subroutine-node-target node)))
        (otherwise 0))))

  (defun ast-static-capture-count (node)
  "Return the fixed participating capture count for NODE and a success flag.

The count excludes the implicit whole-match capture.  The flag is false when
different successful matches can contain different numbers of captures."
  (labels ((sum-children (children)
             (loop with count = 0
                   for child in children do
                     (multiple-value-bind (child-count static-p)
                         (ast-static-capture-count child)
                       (unless static-p
                         (return (values nil nil)))
                       (incf count child-count))
                   finally (return (values count t))))
           (matching-branch-count (branches)
             (multiple-value-bind (count static-p)
                 (ast-static-capture-count (car branches))
               (unless static-p
                 (return-from matching-branch-count (values nil nil)))
               (dolist (branch (cdr branches) (values count t))
                 (multiple-value-bind (branch-count branch-static-p)
                     (ast-static-capture-count branch)
                   (unless (and branch-static-p (= branch-count count))
                     (return (values nil nil)))))))
           (repetition-count (min max child)
             (cond
               ((and max (= min max))
                (if (zerop min)
                    (values 0 t)
                    (ast-static-capture-count child)))
               ((zerop min)
                (multiple-value-bind (child-count static-p)
                    (ast-static-capture-count child)
                  (if (and static-p (zerop child-count))
                      (values 0 t)
                      (values nil nil))))
               (t
                (ast-static-capture-count child)))))
    (typecase node
      (concat-node (sum-children (concat-node-children node)))
      (alternation-node
       (matching-branch-count (alternation-node-branches node)))
      (repetition-node
       (repetition-count (repetition-node-min node)
                         (repetition-node-max node)
                         (repetition-node-child node)))
      (possessive-repetition-node
       (repetition-count (possessive-repetition-node-min node)
                         (possessive-repetition-node-max node)
                         (possessive-repetition-node-child node)))
      (group-node
       (multiple-value-bind (child-count static-p)
           (ast-static-capture-count (group-node-child node))
         (if static-p
             (values (+ child-count
                        (if (group-node-capture-index node) 1 0))
                     t)
             (values nil nil))))
      (assertion-node
       (if (assertion-node-negative-p node)
           (values 0 t)
           (ast-static-capture-count (assertion-node-child node))))
      (atomic-node
       (ast-static-capture-count (atomic-node-child node)))
      (conditional-node
       (multiple-value-bind (yes-count yes-static-p)
           (ast-static-capture-count (conditional-node-yes-branch node))
         (multiple-value-bind (no-count no-static-p)
             (ast-static-capture-count (conditional-node-no-branch node))
           (if (and yes-static-p no-static-p (= yes-count no-count))
               (values yes-count t)
               (values nil nil)))))
      (otherwise (values 0 t))))))
