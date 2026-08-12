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
  (let ((invalid-position (first-non-unicode-scalar-position pattern)))
    (when invalid-position
      (error 'regex-syntax-error
             :pattern pattern
             :position invalid-position
             :reason "Pattern contains a non-Unicode-scalar character")))
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
