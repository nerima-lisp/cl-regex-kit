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

(define-regex-node group-node (regex-node)
  ((child)
   (capture-index :initform nil
                  :documentation "NIL for a non-capturing group `(?:...)`.")
   (name :initform nil))
  "A parenthesized group, capturing or not.")

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

(define-regex-node anchor-node (regex-node)
  ((kind :documentation "An anchor or boundary keyword.")
   (multiline-p :initform nil)
   (crlf-p :initform nil)
   (line-terminator :initform #\Newline)
   (unicode-p :initform nil))
  "A zero-width position assertion.")

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

(defun normalize-byte-literals (node)
  "Encode source literals in raw byte scopes as their UTF-8 octets.

Explicit byte escapes retain their single-octet meaning."
  (labels ((normalize-children (children)
             (mapcar #'normalize-byte-literals children))
           (changed-children-p (old new)
             (not (every #'eq old new)))
           (normalize-literal (literal)
             (if (and (not (literal-node-unicode-p literal))
                      (not (literal-node-raw-octet-p literal))
                      (> (char-code (literal-node-char literal)) #x7f))
                 (make-instance
                  'concat-node
                  :children
                  (mapcar (lambda (octet)
                            (make-instance 'literal-node :char (code-char octet)
                                           :raw-octet-p t
                                           :case-insensitive-p
                                           (literal-node-case-insensitive-p literal)
                                           :unicode-p nil))
                          (utf8-octets-for-character (literal-node-char literal))))
                 literal)))
    (typecase node
      (literal-node (normalize-literal node))
      (concat-node
       (let ((children (normalize-children (concat-node-children node))))
         (if (changed-children-p (concat-node-children node) children)
             (make-instance 'concat-node :children children)
             node)))
      (alternation-node
       (let ((branches (normalize-children (alternation-node-branches node))))
         (if (changed-children-p (alternation-node-branches node) branches)
             (make-instance 'alternation-node :branches branches)
             node)))
      (repetition-node
       (let ((child (normalize-byte-literals (repetition-node-child node))))
         (if (eq child (repetition-node-child node))
             node
             (make-instance 'repetition-node :child child
                              :min (repetition-node-min node)
                              :max (repetition-node-max node)
                              :greedy-p (repetition-node-greedy-p node)))))
      (group-node
       (let ((child (normalize-byte-literals (group-node-child node))))
         (if (eq child (group-node-child node))
             node
             (make-instance 'group-node :child child
                              :capture-index (group-node-capture-index node)
                              :name (group-node-name node)))))
      (otherwise node))))

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
                     (return (values nil nil))))))))
    (typecase node
      (concat-node (sum-children (concat-node-children node)))
      (alternation-node (matching-branch-count (alternation-node-branches node)))
      (repetition-node
       (cond
         ((and (repetition-node-max node)
               (= (repetition-node-min node) (repetition-node-max node)))
          (if (zerop (repetition-node-min node))
              (values 0 t)
              (ast-static-capture-count (repetition-node-child node))))
         ((zerop (repetition-node-min node))
          (multiple-value-bind (child-count static-p)
              (ast-static-capture-count (repetition-node-child node))
            (if (and static-p (zerop child-count))
                (values 0 t)
                (values nil nil))))
         (t (ast-static-capture-count (repetition-node-child node)))))
      (group-node
       (multiple-value-bind (child-count static-p)
           (ast-static-capture-count (group-node-child node))
         (if static-p
             (values (+ child-count (if (group-node-capture-index node) 1 0)) t)
             (values nil nil))))
      (otherwise (values 0 t)))))
