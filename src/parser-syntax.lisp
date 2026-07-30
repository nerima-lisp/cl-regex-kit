(in-package #:cl-regex-kit)

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
  (and character
       (or (unicode-property-p "Alphabetic" character)
           (char= character #\_))))

(defun capture-name-character-p (character)
  "Return whether CHARACTER can continue a Rust-compatible capture name."
  (and character
       (or (capture-name-start-p character)
           (unicode-property-p "N" character)
           (find character ".[]" :test #'char=))))

(defun make-parser-flags (&key case-insensitive multi-line dot-matches-new-line
                            swap-greed ignore-whitespace (unicode t) crlf)
  "Construct an initial parser flag set from public compilation options."
  (check-type case-insensitive boolean)
  (check-type multi-line boolean)
  (check-type dot-matches-new-line boolean)
  (check-type swap-greed boolean)
  (check-type ignore-whitespace boolean)
  (check-type unicode boolean)
  (check-type crlf boolean)
  (logior (if case-insensitive +flag-case-insensitive+ 0)
          (if multi-line +flag-multiline+ 0)
          (if dot-matches-new-line +flag-dotall+ 0)
          (if swap-greed +flag-ungreedy+ 0)
          (if ignore-whitespace +flag-extended+ 0)
          (if unicode +flag-unicode+ 0)
          (if crlf +flag-crlf+ 0)))
