;;;; t/tokenizer-edge-test.lisp
(in-package #:cl-regex-kit/test)

(it "decodes fixed-width escapes through the lexical scanner"
  (dolist (case
           '(("\\a" 7 nil)
             ("\\f" 12 nil)
             ("\\n" 10 nil)
             ("\\r" 13 nil)
             ("\\t" 9 nil)
             ("\\v" 11 nil)
             ("\\x41" 65 t)
             ("\\u0041" 65 nil)
             ("\\U00000041" 65 nil)))
    (destructuring-bind (pattern expected raw-p) case
      (multiple-value-bind (character raw-octet-p next)
          (cl-regex-kit::scan-escaped-character pattern 1)
        (expect (char-code character) :to-be expected)
        (expect raw-octet-p :to-be raw-p)
        (expect next :to-be (length pattern))))))

(it "decodes octal escapes and rejects values outside the byte range"
  (multiple-value-bind (character next)
      (cl-regex-kit::scan-octal-code "\\141" 2)
    (expect character :to-be #\a)
    (expect next :to-be 4))
  (signals regex-syntax-error
    (cl-regex-kit::scan-octal-code "\\400" 2)))

(it "rejects malformed fixed-width and scalar escapes"
  (signals regex-syntax-error
    (cl-regex-kit::scan-escaped-character "\\x4" 1))
  (signals regex-syntax-error
    (cl-regex-kit::scan-escaped-character "\\uD800" 1))
  (signals regex-syntax-error
    (cl-regex-kit::scan-escaped-character "\\U00110000" 1)))

(it "resolves braced, negated, and single-character Unicode properties"
  (multiple-value-bind (descriptor negated-p next)
      (cl-regex-kit::scan-unicode-property-name "\\p{General_Category=Lu}" 2)
    (expect descriptor :not :to-be nil)
    (expect negated-p :to-be nil)
    (expect next :to-be (length "\\p{General_Category=Lu}")))
  (multiple-value-bind (descriptor negated-p next)
      (cl-regex-kit::scan-unicode-property-name "\\p{General_Category!=Lu}" 2)
    (expect descriptor :not :to-be nil)
    (expect negated-p :to-be t)
    (expect next :to-be (length "\\p{General_Category!=Lu}")))
  (multiple-value-bind (descriptor negated-p next)
      (cl-regex-kit::scan-unicode-property-name "\\pL" 2)
    (expect descriptor :not :to-be nil)
    (expect negated-p :to-be nil)
    (expect next :to-be 3)))

(it "rejects malformed Unicode property names"
  (dolist (pattern '("\\p{}" "\\p{General_Category=Lu" "\\p{NotAProperty}" "\\p"))
    (signals regex-syntax-error
      (cl-regex-kit::scan-unicode-property-name pattern 2))))

(it "scans quoted literals and named characters"
  (multiple-value-bind (text next)
      (cl-regex-kit::scan-quoted-literal "\\Q[a.]\\E" 2)
    (expect text :to-equal "[a.]")
    (expect next :to-be (length "\\Q[a.]\\E")))
  (multiple-value-bind (text next)
      (cl-regex-kit::scan-quoted-literal "\\Q[a." 2)
    (expect text :to-equal "[a.")
    (expect next :to-be (length "\\Q[a.")))
  (multiple-value-bind (character next)
      (cl-regex-kit::scan-named-character "\\N{TAB}" 3)
    (expect character :to-be #\Tab)
    (expect next :to-be (length "\\N{TAB}")))
  (signals regex-syntax-error
    (cl-regex-kit::scan-named-character "\\N{}" 3))
  (signals regex-syntax-error
    (cl-regex-kit::scan-named-character "\\N{UNKNOWN_CHARACTER}" 3))
  (signals regex-syntax-error
    (cl-regex-kit::scan-named-character "\\N{TAB" 3)))

(it "scans numeric, relative, named, and subroutine references"
  (multiple-value-bind (index name next relative subroutine-p)
      (cl-regex-kit::scan-backreference "\\g<12>" 2 #\g)
    (expect index :to-be 12)
    (expect name :to-be nil)
    (expect next :to-be 6)
    (expect relative :to-be nil)
    (expect subroutine-p :to-be nil))
  (multiple-value-bind (index name next relative subroutine-p)
      (cl-regex-kit::scan-backreference "\\g{+2}" 2 #\g)
    (expect index :to-be nil)
    (expect name :to-be nil)
    (expect next :to-be 6)
    (expect relative :to-be 2)
    (expect subroutine-p :to-be nil))
  (multiple-value-bind (index name next relative subroutine-p)
      (cl-regex-kit::scan-backreference "\\k<name>" 2 #\k)
    (expect index :to-be nil)
    (expect name :to-equal "name")
    (expect next :to-be 8)
    (expect relative :to-be nil)
    (expect subroutine-p :to-be nil))
  (multiple-value-bind (index name next relative subroutine-p)
      (cl-regex-kit::scan-backreference "\\g'target'" 2 #\g)
    (expect index :to-be nil)
    (expect name :to-equal "target")
    (expect next :to-be 10)
    (expect relative :to-be nil)
    (expect subroutine-p :to-be t)))

(it "rejects malformed backreference targets at the scanner boundary"
  (dolist (case '(("\\g?" 2 #\g)
                  ("\\g<name" 2 #\g)
                  ("\\g<>" 2 #\g)
                  ("\\k<1>" 2 #\k)
                  ("\\g<0>" 2 #\g)
                  ("\\k{+1}" 2 #\k)
                  ("\\g{0}" 2 #\g)
                  ("\\g{+0}" 2 #\g)
                  ("\\g{1-name}" 2 #\g)
                  ("\\g{target-name}" 2 #\g)
                  ("\\g{1foo}" 2 #\g)
                  ("\\g{name" 2 #\g)
                  ("\\g'a-b'" 2 #\g)))
    (destructuring-bind (pattern position kind) case
      (signals regex-syntax-error
        (cl-regex-kit::scan-backreference pattern position kind)))))

(it "rejects escapes that are unavailable inside character classes"
  (dolist (pattern '("[\\R]" "[\\X]" "[\\g<1>]" "[\\k<1>]"))
    (signals regex-syntax-error
      (compile-regex pattern)))
  (signals regex-syntax-error
    (compile-regex "(?#")))

(it "keeps class-local anchor escapes literal and honors the octal option"
  (expect (match "[\\G]" "G") :to-be-truthy)
  (expect (match "[\\K]" "K") :to-be-truthy)
  (expect (match "\\141" "a") :to-be-truthy)
  (signals regex-syntax-error
    (compile-regex "\\141" :octal nil)))
