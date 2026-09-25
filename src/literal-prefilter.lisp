;;;; src/literal-prefilter.lisp
;;;;
;;;; Conservative required-literal extraction over the REGEX-NODE AST, and
;;;; the internal existence prefilter SCAN/ALL-MATCHES/IS-MATCH-P consult
;;;; through CALL-WITH-VALIDATED-MATCH to skip a search range no match can
;;;; occupy, entirely without invoking the Pike VM, the lazy DFA, or the
;;;; advanced executor.
;;;;
;;;; REQUIRED-LITERALS-FOR-NODE walks only AST positions that are mandatory,
;;;; case-sensitive, and free of alternation: concatenation, non-optional
;;;; groups and repetitions (MIN >= 1), atomic groups, and the child of a
;;;; positive lookahead (sound because a positive lookahead's child is
;;;; required to appear in TEXT at that position whenever the overall match
;;;; succeeds, even though it contributes no width to the reported match).
;;;; Every other construct -- alternation, optional (MIN 0) repetition,
;;;; case-insensitive literals, character classes, backreferences,
;;;; subroutines, negative assertions, and lookbehind -- contributes nothing
;;;; rather than an unsound guess, per NFR-005's conservativeness requirement.
(in-package #:cl-regex-kit)

(defun literal-run-char (node)
  "Return NODE's character when NODE is a plain, case-sensitive literal.

Returns NIL for a case-insensitive literal, since the text at that position
may use either case and the character is therefore not a required literal
byte/character."
  (and (typep node 'literal-node)
       (not (literal-node-case-insensitive-p node))
       (literal-node-char node)))

(defun flush-literal-run (buffer)
  "Return BUFFER, a list of characters accumulated most-recent-first, as a
fresh string, or NIL when BUFFER is empty."
  (when buffer
    (coerce (nreverse buffer) 'string)))

(defun required-literals-in-sequence (children)
  "Return required literal substrings for CHILDREN, matched in order.

Consecutive plain literal characters merge into one substring; every other
child is inspected on its own via REQUIRED-LITERALS-FOR-NODE, so a mandatory
literal nested inside a non-literal child (a non-optional group, say) is
still found, just not merged across the boundary."
  (let ((buffer nil)
        (results nil))
    (dolist (child children)
      (let ((char (literal-run-char child)))
        (if char
            (push char buffer)
            (progn
              (let ((flushed (flush-literal-run buffer)))
                (setf buffer nil)
                (when flushed (push flushed results)))
              (dolist (literal (required-literals-for-node child))
                (push literal results))))))
    (let ((flushed (flush-literal-run buffer)))
      (when flushed (push flushed results)))
    (nreverse results)))

(defun required-literals-for-node (node)
  "Return a list of literal strings that every match through NODE must
contain. NIL means no requirement could be conservatively derived, not that
none exists."
  (typecase node
    (literal-node
     (let ((char (literal-run-char node)))
       (and char (list (string char)))))
    (concat-node
     (required-literals-in-sequence (concat-node-children node)))
    (group-node
     (required-literals-for-node (group-node-child node)))
    (atomic-node
     (required-literals-for-node (atomic-node-child node)))
    (repetition-node
     (and (plusp (repetition-node-min node))
          (required-literals-for-node (repetition-node-child node))))
    (possessive-repetition-node
     (and (plusp (possessive-repetition-node-min node))
          (required-literals-for-node (possessive-repetition-node-child node))))
    (assertion-node
     (and (eq (assertion-node-kind node) :lookahead)
          (not (assertion-node-negative-p node))
          (assertion-node-child node)
          (required-literals-for-node (assertion-node-child node))))
    (otherwise nil)))

(defun regex-required-literals (regex)
  "Return a fresh list of literal strings that every match of REGEX must
contain, longest first, or NIL when no literal requirement can be
conservatively derived.

Each returned string is guaranteed present in TEXT whenever REGEX matches
TEXT anywhere in TEXT: derivation only follows mandatory, case-sensitive,
non-alternated AST positions -- concatenation, non-optional groups and
repetitions, atomic groups, and positive lookahead -- and stops,
contributing nothing, at alternation, optional repetition, case-insensitive
literals, character classes, backreferences, negative assertions,
lookbehind, subroutines, and every other content-dependent or variable
construct. For a byte regex, each string's CHAR-CODEs are the required
octet values, not Unicode scalar values."
  (check-type regex regex)
  (let ((literals (remove-duplicates (required-literals-for-node (regex-ast regex))
                                      :test #'string=)))
    (when literals
      (sort literals #'> :key #'length))))

(defun literal-as-element-sequence (literal byte-mode-p)
  "Return LITERAL as a sequence in the same element domain as REGEX's input:
an octet vector when BYTE-MODE-P, otherwise LITERAL itself."
  (if byte-mode-p
      (map '(vector (unsigned-byte 8)) #'char-code literal)
      literal))

(defun regex-required-literal-needles (regex)
  "Return REGEX's cached prefilter needle sequences, built on first use.

Each needle is LITERAL-AS-ELEMENT-SEQUENCE of one of REGEX-REQUIRED-LITERALS,
computed once since a compiled REGEX's AST never changes. A benign race
under concurrent first use may compute this twice; both results are equal
and either is safe to publish, since neither observably mutates anything
this cache does not itself own."
  (let ((cached (slot-value regex '%required-literal-needles)))
    (if (eq cached :unbuilt)
        (let* ((byte-mode-p (byte-regex-p regex))
               (needles (mapcar (lambda (literal) (literal-as-element-sequence literal byte-mode-p))
                                 (regex-required-literals regex))))
          (setf (slot-value regex '%required-literal-needles) needles)
          needles)
        cached)))

(defun text-range-contains-needle-p (needle text start limit)
  "Return true when NEEDLE occurs in TEXT within [START, LIMIT)."
  (and (<= (length needle) (- limit start))
       (not (null (search needle text :start2 start :end2 limit)))))

(defun regex-literal-prefilter-blocks-p (regex text start limit)
  "Return true when REGEX's required-literal set proves no match of REGEX can
occupy any position in [START, LIMIT) of TEXT.

Conservative in one direction only: a false return never means REGEX
matches TEXT, only that the cheap literal check could not rule it out, so
the real matcher must still run. A true return is a sound proof of absence,
since every element of REGEX-REQUIRED-LITERALS is independently required by
every match, so a single missing one already rules out a match anywhere in
range."
  (let ((needles (regex-required-literal-needles regex)))
    (and needles
         (notevery (lambda (needle) (text-range-contains-needle-p needle text start limit))
                   needles))))
