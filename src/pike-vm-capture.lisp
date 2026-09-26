;;;; src/pike-vm-capture.lisp
;;;;
;;;; RUN-PIKE-VM and RUN-PIKE-VM-BOOLEAN: the capture-aware and capture-free
;;;; Pike VM loops.
;;;;
;;;; Both loops keep every per-position structure in a PIKE-VM-SCRATCH that is
;;;; allocated once per call and grows only to the peak number of live
;;;; threads, never with the input length: a thread is a program counter, the
;;;; position it resumes at, and a row of capture slots inside a VM-QUEUE; the
;;;; epsilon closure walks an explicit stack whose :SAVE frames restore the one
;;;; scratch slot they changed; and a surviving thread is copied into the next
;;;; queue rather than allocated.
;;;;
;;;; Thread lists stay in priority order at every position, which is what
;;;; leftmost-first selection depends on. A byte-mode Unicode instruction
;;;; consumes a 1-4 octet UTF-8 scalar and a CRLF line break consumes two
;;;; elements, while a raw octet instruction consumes one, so threads can
;;;; advance by different widths from the same position. A thread that
;;;; advanced past the next position is carried through each intermediate list
;;;; at its own priority rank, as a waiting entry that is not closed, rather
;;;; than parked in a separate per-position queue whose threads would later be
;;;; merged in arrival order.
(in-package #:cl-regex-kit)

(defstruct match-result start
  end
  groups
  group-names
  mark
  (edit-distance 0))

(defun slot-count-for-program (program)
  (declare (type simple-vector program))
  (loop for instruction across program
        when (eq (inst-op instruction) :save)
          maximize (1+ (inst-a instruction)) into maximum
        finally (return (or maximum 0))))

(defun make-match-result-from-slots (slots slot-count &optional mark (edit-distance 0))
  (let ((groups (make-array (/ slot-count 2) :initial-element nil)))
    (dotimes (index (length groups))
      (let ((from (aref slots (* 2 index)))
            (to (aref slots (1+ (* 2 index)))))
        (when (and from to)
          (setf (aref groups index) (cons from to)))))
    (make-match-result
     :start (aref slots 0)
     :end (aref slots 1)
     :groups groups
     :mark mark
     :edit-distance edit-distance)))

(defstruct (vm-queue (:constructor %make-vm-queue (stride pcs resumes rows)))
  "A priority-ordered list of threads. Thread I has program counter PCS[I],
resumes matching at position RESUMES[I], and holds its STRIDE capture slots
at ROWS[I*STRIDE, (I+1)*STRIDE). Grows by doubling and never shrinks, so a
reused queue stops allocating once it has held the peak thread count."
  (stride 0 :type fixnum :read-only t)
  (pcs (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (resumes (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (rows #() :type simple-vector)
  (fill 0 :type fixnum))

(defun make-vm-queue (stride &optional (capacity 8))
  (%make-vm-queue stride
                  (make-array capacity :element-type 'fixnum :initial-element 0)
                  (make-array capacity :element-type 'fixnum :initial-element 0)
                  (make-array (* capacity stride) :initial-element nil)))

(defun vm-queue-push (queue pc resume source source-start)
  "Append a thread at PC resuming at RESUME whose slots are
SOURCE[SOURCE-START, +STRIDE), or all NIL when SOURCE is NIL."
  (declare (type vm-queue queue) (type fixnum pc resume source-start))
  (let ((count (vm-queue-fill queue))
        (stride (vm-queue-stride queue)))
    (when (= count (length (vm-queue-pcs queue)))
      (let* ((capacity (* 2 (max 1 count)))
             (pcs (make-array capacity :element-type 'fixnum :initial-element 0))
             (resumes (make-array capacity :element-type 'fixnum :initial-element 0))
             (rows (make-array (* capacity stride) :initial-element nil)))
        (replace pcs (vm-queue-pcs queue))
        (replace resumes (vm-queue-resumes queue))
        (replace rows (vm-queue-rows queue))
        (setf (vm-queue-pcs queue) pcs
              (vm-queue-resumes queue) resumes
              (vm-queue-rows queue) rows)))
    (setf (aref (vm-queue-pcs queue) count) pc
          (aref (vm-queue-resumes queue) count) resume)
    (let ((offset (* count stride))
          (rows (vm-queue-rows queue)))
      (if source
          (replace rows source :start1 offset
                               :start2 source-start :end2 (+ source-start stride))
          (fill rows nil :start offset :end (+ offset stride))))
    (setf (vm-queue-fill queue) (1+ count))))

(defstruct (pike-vm-scratch (:constructor %make-pike-vm-scratch))
  "Every mutable structure one RUN-PIKE-VM/RUN-PIKE-VM-BOOLEAN call needs.
SLOTS is the closure walk's working slot row; STACK-ENTRIES/STACK-VALUES hold
its explicit frames (a nonnegative entry is a program counter still to visit;
a negative entry -(I+1) restores SLOTS[I] to the paired value); CURRENT
receives one position's closed thread list and PENDING the list for the
positions after it; BEST holds the selected match's slots."
  (program #() :type simple-vector :read-only t)
  (stride 0 :type fixnum :read-only t)
  (workspace nil :read-only t)
  (slots #() :type simple-vector :read-only t)
  (stack-entries (make-array 16 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (stack-values (make-array 16 :initial-element nil) :type simple-vector)
  (current (make-vm-queue 0) :type vm-queue :read-only t)
  (pending (make-vm-queue 0) :type vm-queue :read-only t)
  (best #() :type simple-vector :read-only t))

(defun make-pike-vm-scratch (program stride)
  (declare (type simple-vector program) (type fixnum stride))
  (%make-pike-vm-scratch
   :program program
   :stride stride
   :workspace (make-pike-vm-closure-workspace (length program))
   :slots (make-array stride :initial-element nil)
   :current (make-vm-queue stride)
   :pending (make-vm-queue stride)
   :best (make-array stride :initial-element nil)))

(defun pike-vm-scratch-closure (scratch text position length byte-mode-p fresh)
  "Epsilon-close SCRATCH's PENDING threads at POSITION into its CURRENT, then
empty PENDING.

A pending thread that resumes after POSITION is copied unchanged, at its
rank. FRESH is :FIRST or :LAST to add a new all-NIL thread at program counter
0 before or after the pending ones, or NIL for none. Visits program counters
in the same depth-first order as PIKE-VM-CLOSURE, deduplicating by program
counter across the whole position, and tracks capture slots only when
SCRATCH's STRIDE is positive."
  (declare (type pike-vm-scratch scratch) (type fixnum position length))
  (let* ((program (pike-vm-scratch-program scratch))
         (stride (pike-vm-scratch-stride scratch))
         (slots (pike-vm-scratch-slots scratch))
         (seeds (pike-vm-scratch-pending scratch))
         (current (pike-vm-scratch-current scratch))
         (depth 0))
    (declare (type simple-vector program slots) (type fixnum stride depth))
    (setf (vm-queue-fill current) 0)
    (multiple-value-bind (marks generation)
        (advance-pike-vm-closure-workspace (pike-vm-scratch-workspace scratch) (length program))
      (declare (type (simple-array fixnum (*)) marks) (type fixnum generation))
      (labels ((push-frame (entry value)
                 (declare (type fixnum entry))
                 (when (= depth (length (pike-vm-scratch-stack-entries scratch)))
                   (let* ((capacity (* 2 depth))
                          (entries (make-array capacity :element-type 'fixnum :initial-element 0))
                          (frame-values (make-array capacity :initial-element nil)))
                     (replace entries (pike-vm-scratch-stack-entries scratch))
                     (replace frame-values (pike-vm-scratch-stack-values scratch))
                     (setf (pike-vm-scratch-stack-entries scratch) entries
                           (pike-vm-scratch-stack-values scratch) frame-values)))
                 (setf (aref (pike-vm-scratch-stack-entries scratch) depth) entry
                       (svref (pike-vm-scratch-stack-values scratch) depth) value)
                 (incf depth))
               (walk (pc)
                 (declare (type fixnum pc))
                 (loop
                   (when (= (aref marks pc) generation)
                     (return))
                   (setf (aref marks pc) generation)
                   (let ((instruction (svref program pc)))
                     (case (inst-op instruction)
                       (:split
                        (push-frame (inst-b instruction) nil)
                        (setf pc (inst-a instruction)))
                       (:jmp (setf pc (inst-a instruction)))
                       (:save
                        (when (plusp stride)
                          (let ((slot (inst-a instruction)))
                            (push-frame (- -1 slot) (svref slots slot))
                            (setf (svref slots slot) position)))
                        (setf pc (inst-b instruction)))
                       ((:bol :eol :bos :eos :boundary :non-boundary
                         :word-start :word-end :word-start-half :word-end-half)
                        (if (zero-width-instruction-matches-p instruction text position length
                                                              byte-mode-p)
                            (setf pc (inst-a instruction))
                            (return)))
                       (otherwise
                        (vm-queue-push current pc position slots 0)
                        (return))))))
               (close-seed (pc)
                 (declare (type fixnum pc))
                 (walk pc)
                 (loop while (plusp depth)
                       do (decf depth)
                          (let ((entry (aref (pike-vm-scratch-stack-entries scratch) depth)))
                            (if (minusp entry)
                                (setf (svref slots (- -1 entry))
                                      (svref (pike-vm-scratch-stack-values scratch) depth))
                                (walk entry)))))
               (close-fresh ()
                 (fill slots nil)
                 (close-seed 0)))
        (when (eq fresh :first)
          (close-fresh))
        (let ((pcs (vm-queue-pcs seeds))
              (resumes (vm-queue-resumes seeds))
              (rows (vm-queue-rows seeds)))
          (dotimes (index (vm-queue-fill seeds))
            (let ((resume (aref resumes index))
                  (offset (* index stride)))
              (if (> resume position)
                  (vm-queue-push current (aref pcs index) resume rows offset)
                  (progn
                    (replace slots rows :start2 offset)
                    (close-seed (aref pcs index)))))))
        (when (eq fresh :last)
          (close-fresh))))
    (setf (vm-queue-fill seeds) 0)
    current))

(defun validate-pike-vm-range (text start limit)
  (unless (and (integerp start) (integerp limit) (<= 0 start limit (length text)))
    (error "START and END must define a range within TEXT")))

(defun run-pike-vm-boolean (program text &key (start 0) end never-newline-p)
  "Return true when PROGRAM matches TEXT anywhere within [START, END)."
  (declare (type simple-vector program))
  (let* ((length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text))))
    (validate-pike-vm-range text start limit)
    (let* ((scratch (make-pike-vm-scratch program 0))
           (pending (pike-vm-scratch-pending scratch)))
      (loop for position from start to limit
            do (let* ((current (pike-vm-scratch-closure scratch text position length
                                                        byte-mode-p :first))
                      (pcs (vm-queue-pcs current))
                      (resumes (vm-queue-resumes current)))
                 (dotimes (index (vm-queue-fill current))
                   (let ((pc (aref pcs index))
                         (resume (aref resumes index)))
                     (if (> resume position)
                         (vm-queue-push pending pc resume nil 0)
                         (let ((instruction (svref program pc)))
                           (case (inst-op instruction)
                             (:match (return-from run-pike-vm-boolean t))
                             ((:char :class :any :line-break)
                              (multiple-value-bind (next-position matched-p)
                                  (instruction-match-end instruction text position limit
                                                         byte-mode-p never-newline-p)
                                (when matched-p
                                  (vm-queue-push pending (inst-b instruction) next-position
                                                 nil 0))))))))))))
    nil))

(defun run-pike-vm (program text &key (start 0) end shortest-p longest-p
                                      never-newline-p boolean-p slot-count)
  "Run PROGRAM against TEXT and return its leftmost-first match, if any.

When SHORTEST-P is true, select the earliest ending match at the leftmost
start position instead of the usual greedy/lazy branch priority. When
LONGEST-P is true, select the longest match at the leftmost start position,
retaining the usual branch priority to resolve equal-length paths.

Leftmost-first selection records a :MATCH thread as the current best and
drops every lower-priority thread at that position, but keeps running the
higher-priority threads ahead of it, since one of them may still reach a
preferred match. Once any match is recorded no new start position is seeded,
and the run ends when no thread remains."
  (declare (type simple-vector program))
  (when (and shortest-p longest-p)
    (error "SHORTEST-P and LONGEST-P cannot both be true"))
  (check-type never-newline-p boolean)
  (let* ((slot-count (or slot-count (slot-count-for-program program)))
         (length (length text))
         (limit (or end length))
         (byte-mode-p (not (stringp text))))
    (when (and boolean-p (= slot-count 2))
      (return-from run-pike-vm
        (run-pike-vm-boolean program text :start start :end end
                             :never-newline-p never-newline-p)))
    (validate-pike-vm-range text start limit)
    (let* ((scratch (make-pike-vm-scratch program slot-count))
           (pending (pike-vm-scratch-pending scratch))
           (best (pike-vm-scratch-best scratch))
           (found-p nil))
      (declare (type simple-vector best))
      (loop for position from start to limit
            do (let* ((current (pike-vm-scratch-closure scratch text position length byte-mode-p
                                                        (cond (found-p nil)
                                                              (boolean-p :first)
                                                              (t :last))))
                      (pcs (vm-queue-pcs current))
                      (resumes (vm-queue-resumes current))
                      (rows (vm-queue-rows current)))
                 (loop for index from 0 below (vm-queue-fill current)
                       for pc = (aref pcs index)
                       for resume = (aref resumes index)
                       for offset = (* index slot-count)
                       for instruction = (svref program pc)
                       do (cond
                            ;; A shortest match can only be displaced by a thread
                            ;; that started earlier.
                            ((and shortest-p found-p
                                  (>= (svref rows offset) (svref best 0))))
                            ((> resume position)
                             (vm-queue-push pending pc resume rows offset))
                            ((eq (inst-op instruction) :match)
                             (cond
                               (boolean-p (return-from run-pike-vm t))
                               (longest-p
                                (when (or (not found-p)
                                          (< (svref rows offset) (svref best 0))
                                          (and (= (svref rows offset) (svref best 0))
                                               (> (svref rows (1+ offset)) (svref best 1))))
                                  (replace best rows :start2 offset)
                                  (setf found-p t)))
                               (shortest-p
                                (replace best rows :start2 offset)
                                (setf found-p t))
                               (t
                                (replace best rows :start2 offset)
                                (setf found-p t)
                                (loop-finish))))
                            ((case (inst-op instruction) ((:char :class :any :line-break) t))
                             (multiple-value-bind (next-position matched-p)
                                 (instruction-match-end instruction text position limit
                                                        byte-mode-p never-newline-p)
                               (when matched-p
                                 (vm-queue-push pending (inst-b instruction) next-position
                                                rows offset))))))
                 (when (and found-p (zerop (vm-queue-fill pending)))
                   (loop-finish))))
      (and found-p
           (make-match-result-from-slots best slot-count)))))
