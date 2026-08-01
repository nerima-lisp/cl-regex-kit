(in-package #:cl-regex-kit)

;; DEFVAR'd here, ahead of regex-tokenizer.lisp/regex-grammar.lisp/
;; regex-grammar-classes.lisp, purely for load order: the tokenizer reads
;; *REGEX-BYTE-MODE-P*/*REGEX-OCTAL-P* (fixed for the whole parse) and every
;; grammar function reads and mutates the rest, but PARSE-REGEX (in
;; regex-grammar.lisp) is what actually binds them, once, per call.
(defvar *regex-pattern*)

(defvar *regex-tokens*)

(defvar *regex-token-position*)

(defvar *regex-length*)

(defvar *regex-group-count*)

(defvar *regex-group-names*)

(defvar *regex-flags*)

(defvar *regex-nesting-depth*)

(defvar *regex-byte-mode-p*)

(defvar *regex-never-capture-p*)

(defvar *regex-octal-p*)

(defvar *regex-nest-limit*)

(defvar *regex-line-terminator*)

(defconstant +flag-case-insensitive+ #b0001)

(defconstant +flag-multiline+ #b0010)

(defconstant +flag-dotall+ #b0100)

(defconstant +flag-ungreedy+ #b1000)

(defconstant +flag-extended+ #b10000)

(defconstant +flag-unicode+ #b100000)

(defconstant +flag-crlf+ #b1000000)

(defconstant +maximum-repeat-count+ 1000
  "Largest finite repetition bound accepted by the parser.")

(defconstant +default-nest-limit+ 250
  "Maximum nested group depth accepted by default.")

(defun capture-name-start-p (character)
  "Return whether CHARACTER can start a Rust-compatible capture name."
  (and character (or (sb-unicode:alphabetic-p character) (char= character #\_))))

(defun capture-name-character-p (character)
  "Return whether CHARACTER can continue a Rust-compatible capture name."
  (and character
       (or (capture-name-start-p character)
           (member (sb-unicode:general-category character)
                   +unicode-number-category-values+
                   :test (function eq))
           (find character ".[]" :test (function char=)))))

(defun make-parser-flags (&key
    case-insensitive
    multi-line
    dot-matches-new-line
    swap-greed
    ignore-whitespace
    (unicode t)
    crlf)
  "Construct an initial parser flag set from public compilation options."
  (check-type case-insensitive boolean)
  (check-type multi-line boolean)
  (check-type dot-matches-new-line boolean)
  (check-type swap-greed boolean)
  (check-type ignore-whitespace boolean)
  (check-type unicode boolean)
  (check-type crlf boolean)
  (logior
    (if case-insensitive +flag-case-insensitive+
      0)
    (if multi-line +flag-multiline+
      0)
    (if dot-matches-new-line +flag-dotall+
      0)
    (if swap-greed +flag-ungreedy+
      0)
    (if ignore-whitespace +flag-extended+
      0)
    (if unicode +flag-unicode+
      0)
    (if crlf +flag-crlf+
      0)))

(defun update-parser-flag (flags character enabled-p)
  "Return FLAGS with supported inline flag CHARACTER enabled or disabled."
  (let ((flag
        (ecase character
          (#\i +flag-case-insensitive+)
          (#\m +flag-multiline+)
          (#\s +flag-dotall+)
          (#\U +flag-ungreedy+)
          (#\x +flag-extended+)
          (#\u +flag-unicode+)
          (#\R +flag-crlf+))))
    (if enabled-p (logior flags flag)
      (logandc2 flags flag))))

(defmacro with-parser-nesting ((depth limit overflow-form) &body body)
  "Execute BODY at one parser nesting level and always restore DEPTH."
  `(progn
    (when (>= ,depth ,limit)
      ,overflow-form)
    (incf ,depth)
    (unwind-protect (progn
        ,@body)
      (decf ,depth))))
