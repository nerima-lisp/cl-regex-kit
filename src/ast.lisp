;;;; src/ast.lisp
;;;;
;;;; The parse tree PARSER produces and NFA compiles. Every syntax feature the
;;;; engine supports has exactly one node class here; there is no separate
;;;; "optimized" tree, since Thompson construction compiles this shape
;;;; directly.
(in-package #:cl-regex-kit)

(defclass regex-node () ()
  (:documentation "Base class for every regex AST node."))

(defclass literal-node (regex-node)
  ((char :initarg :char :reader literal-node-char))
  (:documentation "A single literal character."))

(defclass concat-node (regex-node)
  ((children :initarg :children :reader concat-node-children))
  (:documentation "CHILDREN matched one after another."))

(defclass alternation-node (regex-node)
  ((branches :initarg :branches :reader alternation-node-branches))
  (:documentation "The first BRANCH that matches wins: `a|b|c`."))

(defclass repetition-node (regex-node)
  ((child :initarg :child :reader repetition-node-child)
   (min :initarg :min :reader repetition-node-min)
   (max :initarg :max :initform nil :reader repetition-node-max
        :documentation "NIL means unbounded, as in `*` and `+`.")
   (greedy-p :initarg :greedy-p :initform t :reader repetition-node-greedy-p))
  (:documentation "CHILD repeated between MIN and MAX times: `*`, `+`, `?`, `{m,n}`."))

(defclass group-node (regex-node)
  ((child :initarg :child :reader group-node-child)
   (capture-index :initarg :capture-index :initform nil :reader group-node-capture-index
                  :documentation "NIL for a non-capturing group `(?:...)`."))
  (:documentation "A parenthesized group, capturing or not."))

(defclass char-class-node (regex-node)
  ((ranges :initarg :ranges :reader char-class-node-ranges
           :documentation "A list of (START . END) char-code ranges, inclusive.")
   (negated-p :initarg :negated-p :initform nil :reader char-class-node-negated-p))
  (:documentation "A character class: `[abc]`, `[a-z]`, `[^0-9]`."))

(defclass any-char-node (regex-node) ()
  (:documentation "`.` -- matches any character except newline."))

(defclass anchor-node (regex-node)
  ((kind :initarg :kind :reader anchor-node-kind
         :documentation ":START for `^`, :END for `$`."))
  (:documentation "A zero-width position assertion."))
