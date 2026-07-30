;;;; src/pike-vm.lisp
;;;;
;;;; Executes a PROGRAM (from NFA's COMPILE-TO-NFA) against TEXT by advancing
;;;; a deduplicated set of active NFA threads one character at a time (Rob
;;;; Pike's VM, as used by RE2 and Go's and Rust's `regexp`/`regex` engines).
;;;; Each thread carries its own capture-slot vector; thread priority at a
;;;; :SPLIT instruction reproduces Perl-style leftmost-first, greedy
;;;; semantics without ever re-scanning input.
;;;;
;;;; NOT YET IMPLEMENTED. Left as a stub so the system loads and the test
;;;; harness runs; replace the body below with a real thread simulation.
(in-package #:cl-regex-kit)

(defstruct match-result
  "The result of a successful match. START and END bound the whole match;
GROUPS is a simple-vector indexed by capture-group number, each element a
\(START . END) cons or NIL if that group did not participate."
  start end groups)

(defun run-pike-vm (program text &key (start 0))
  "Simulate PROGRAM against TEXT starting at START.
Returns a MATCH-RESULT for the leftmost-first match, or NIL."
  (declare (ignore program text start))
  (error "RUN-PIKE-VM is not yet implemented."))
