;;;; src/lazy-dfa.lisp
;;;;
;;;; A lazy (on-demand subset-construction) DFA cache for boolean match
;;;; detection, consulted only from IS-MATCH-P/IS-MATCH-AT. Every other
;;;; public matcher keeps using RUN-PIKE-VM/RUN-ADVANCED-REGEX directly, and
;;;; captures never come from this path: a DFA state here is a set of live
;;;; program counters with no capture slots at all.
;;;;
;;;; Construction. Consider the classic unanchored-search simulation
;;;; RUN-PIKE-VM-BOOLEAN already performs: at each position it seeds a fresh
;;;; thread at PC 0, adds the survivors carried over from the previous
;;;; position, and epsilon-closes the result. Because that reseeding happens
;;;; unconditionally at every position, the set of live PCs *after closure*
;;;; is a pure function of the previous closed set and the one input element
;;;; just consumed -- exactly a DFA transition, with the previous-position
;;;; reseed folded into the transition function itself rather than the
;;;; caller's loop. DFA-STATE-FOR-SEEDS builds that closed set once per
;;;; distinct set of raw seeds and caches it under LAZY-DFA-STATES; a
;;;; transition looks up its target by (state, element) in
;;;; DFA-STATE-TRANSITIONS, computing and caching it on first use.
;;;;
;;;; Eligibility. A DFA transition table is keyed only by the current state
;;;; and the single input element just consumed, so it cannot represent an
;;;; instruction whose match decision depends on surrounding text the
;;;; element alone does not carry: an anchor or word/line boundary (:BOL,
;;;; :EOL, :BOS, :EOS, :BOUNDARY, :NON-BOUNDARY, :WORD-START, :WORD-END,
;;;; :WORD-START-HALF, :WORD-END-HALF), or a byte-mode line break (:LINE-BREAK,
;;;; which reads a possible CRLF pair) or Unicode literal/class/any (which
;;;; decodes a variable 1-4 octet run rather than one fixed-width element).
;;;; LAZY-DFA-ELIGIBLE-P excludes every program containing one of those, and
;;;; RUN-PIKE-VM-BOOLEAN remains fully correct and available for them.
(in-package #:cl-regex-kit)

(defconstant +lazy-dfa-max-states+ 4096
  "Upper bound on cached subset-construction states per compiled REGEX.

A state beyond this bound is still computed correctly, by the same
PIKE-VM-BOOLEAN-CLOSURE epsilon-closure step RUN-PIKE-VM-BOOLEAN performs
per position; it is simply not retained, so exceeding the bound degrades to
ordinary per-step NFA work rather than growing the cache without limit.")

(defstruct dfa-state
  "One subset-construction state: PCS is the closed set of live program
counters reachable at this point of an unanchored search; ACCEPT-P is
whether that set already contains a :MATCH instruction; TRANSITIONS caches
this state's outgoing (state, element) -> DFA-STATE steps."
  (pcs nil :type list :read-only t)
  (accept-p nil :read-only t)
  (transitions (make-hash-table :test 'eql) :read-only t))

(defstruct lazy-dfa
  "Per-REGEX cache of DFA-STATE objects. LOCK serializes every access to
STATES and to any DFA-STATE's TRANSITIONS, since a compiled REGEX (and this
cache with it) is shared across concurrent IS-MATCH-P calls: the underlying
hash tables and the shared closure WORKSPACE are ordinary mutable structures
with no thread-safety of their own."
  (program nil :read-only t :type simple-vector)
  (never-newline-p nil :read-only t)
  (byte-mode-p nil :read-only t)
  (workspace nil :read-only t)
  (states (make-hash-table :test 'equal))
  (state-count 0)
  (lock (cl-concurrent-kit:make-lock :name "cl-regex-kit lazy-dfa")))

(defun lazy-dfa-supported-op-p (op)
  "Return true when OP's match decision depends only on the current state
and the single input element about to be consumed."
  (member op '(:save :split :jmp :match :char :class :any) :test #'eq))

(defun lazy-dfa-eligible-p (program byte-mode-p)
  "Return true when PROGRAM can be matched by a fixed (state, element)
transition table -- see this file's header for why zero-width and
variable-width instructions cannot."
  (declare (type simple-vector program))
  (loop for instruction across program
        always (case (inst-op instruction)
                 ((:char :class :any)
                  (or (not byte-mode-p) (not (instruction-unicode-p instruction))))
                 (otherwise (lazy-dfa-supported-op-p (inst-op instruction))))))

(defun dfa-instruction-accepts-p (instruction element never-newline-p)
  "Return true when consuming INSTRUCTION accepts ELEMENT.

Mirrors INSTRUCTION-MATCH-END's acceptance rule minus its position/limit
bookkeeping, which the lazy DFA's caller-side loop already owns."
  (and (not (and never-newline-p (newline-element-p element)))
       (instruction-matches-p instruction element)))

(defun canonicalize-dfa-key (pcs program-length)
  "Return PCS, a list of program counters, as an ELEMENT-wise-comparable
EQUAL hash key: a bit vector, since EQUAL compares bit vectors elementwise
but falls back to EQ for a general vector of fixnums."
  (let ((bits (make-array program-length :element-type 'bit :initial-element 0)))
    (dolist (pc pcs bits)
      (setf (sbit bits pc) 1))))

(defun %dfa-state-for-seeds (dfa text position length seeds)
  "Return the DFA-STATE reached by epsilon-closing SEEDS, from cache when
possible. The caller must hold DFA's lock. Always computes a correct state;
only caching beyond +LAZY-DFA-MAX-STATES+ is skipped."
  (let* ((program (lazy-dfa-program dfa))
         (closed (pike-vm-boolean-closure program text position length
                                           (lazy-dfa-byte-mode-p dfa) seeds
                                           :workspace (lazy-dfa-workspace dfa)))
         (key (canonicalize-dfa-key closed (length program))))
    (or (gethash key (lazy-dfa-states dfa))
        (let ((state (make-dfa-state
                       :pcs closed
                       :accept-p (some (lambda (pc) (eq (inst-op (aref program pc)) :match))
                                        closed))))
          (when (< (lazy-dfa-state-count dfa) +lazy-dfa-max-states+)
            (setf (gethash key (lazy-dfa-states dfa)) state)
            (incf (lazy-dfa-state-count dfa)))
          state))))

(defun lazy-dfa-initial-state (dfa text start length)
  "Return the DFA-STATE at the start of an unanchored search from START."
  (cl-concurrent-kit:with-lock-held ((lazy-dfa-lock dfa))
    (%dfa-state-for-seeds dfa text start length (list 0))))

(defun dfa-transition (dfa state text position length element)
  "Return the DFA-STATE reached from STATE by consuming ELEMENT at POSITION,
including the fresh PC-0 seed every position of an unanchored search adds."
  (cl-concurrent-kit:with-lock-held ((lazy-dfa-lock dfa))
    (or (gethash element (dfa-state-transitions state))
        (let ((successors (list 0))
              (program (lazy-dfa-program dfa))
              (never-newline-p (lazy-dfa-never-newline-p dfa)))
          (dolist (pc (dfa-state-pcs state))
            (let ((instruction (aref program pc)))
              (when (and (member (inst-op instruction) '(:char :class :any) :test #'eq)
                         (dfa-instruction-accepts-p instruction element never-newline-p))
                (push (inst-b instruction) successors))))
          (let ((next (%dfa-state-for-seeds dfa text (1+ position) length successors)))
            (setf (gethash element (dfa-state-transitions state)) next)
            next)))))

(defun run-lazy-dfa-boolean (dfa text start limit)
  "Return true when DFA's program has an unanchored match in TEXT within
[START, LIMIT). Equivalent to RUN-PIKE-VM-BOOLEAN for any program
LAZY-DFA-ELIGIBLE-P accepted, memoizing subset-construction states across
calls sharing the same compiled REGEX."
  (let ((length (length text)))
    (loop with state = (lazy-dfa-initial-state dfa text start length)
          for position from start
          do (when (dfa-state-accept-p state)
               (return t))
             (when (>= position limit)
               (return nil))
             (setf state (dfa-transition dfa state text position length (aref text position))))))

(defun regex-lazy-dfa (regex)
  "Return REGEX's cached LAZY-DFA, or NIL when its program is ineligible or
REGEX is advanced. Built once per REGEX on first use; a benign race under
concurrent first use may build it twice, and the loser's result is simply
discarded, since a freshly built LAZY-DFA is self-contained and either
instance is correct."
  (let ((cached (slot-value regex '%lazy-dfa)))
    (if (eq cached :unbuilt)
        (let* ((program (and (not (regex-advanced-p regex)) (regex-program regex)))
               (byte-mode-p (byte-regex-p regex))
               (built (and program
                           (lazy-dfa-eligible-p program byte-mode-p)
                           (make-lazy-dfa
                            :program program
                            :never-newline-p (regex-never-newline-p regex)
                            :byte-mode-p byte-mode-p
                            :workspace (make-pike-vm-closure-workspace (length program))))))
          (setf (slot-value regex '%lazy-dfa) built)
          built)
        cached)))
