(in-package #:cl-regex-kit)

(defparameter +incremental-consuming-ops+ '(:char :class :any))

(defclass incremental-regex-stream ()
  ((regex
     :initarg :regex
     :reader incremental-regex-stream-regex)
   (start
     :initarg :start
     :reader incremental-regex-stream-start)
   (timeout
     :initarg :timeout
     :reader incremental-regex-stream-timeout)
   (position
     :initarg :position
     :reader incremental-regex-stream-position)
   (unicode-byte-p
     :initarg :unicode-byte-p
     :reader %incremental-regex-stream-unicode-byte-p
     :initform nil)
   (pending-octets
     :initform #()
     :accessor %incremental-regex-stream-pending-octets)
   (active
     :initform nil
     :accessor %incremental-regex-stream-active)
   (workspace
     :initarg :workspace
     :reader %incremental-regex-stream-workspace)
   (finished-p
     :initform nil
     :accessor incremental-regex-stream-finished-p)
   (final-result
     :initform nil
     :accessor %incremental-regex-stream-final-result)))

(defun incremental-regex-stream-p (object)
  "Return true when OBJECT is an incremental regular-expression stream."
  (typep object 'incremental-regex-stream))

(defun %incremental-stream-error (regex format-control &rest arguments)
  (error 'regex-syntax-error
         :pattern (regex-source regex)
         :reason (apply #'format nil format-control arguments)))

(defun %incremental-stream-consuming-p (instruction)
  (member (inst-op instruction) +incremental-consuming-ops+ :test #'eq))

(defun %validate-incremental-regex (regex)
  (when (regex-advanced-p regex)
    (%incremental-stream-error
     regex
     "Incremental streaming supports ordinary NFA expressions only"))
  (let ((has-consuming-p nil)
        (unicode-consuming-p nil)
        (raw-consuming-p nil))
    (loop for instruction across (regex-program regex)
          do (let ((operation (inst-op instruction)))
               (unless (or (eq operation :split)
                           (eq operation :jmp)
                           (eq operation :save)
                           (eq operation :match)
                           (%incremental-stream-consuming-p instruction))
                 (%incremental-stream-error
                  regex
                  "Incremental streaming does not support VM operation ~S"
                  operation))
               (when (%incremental-stream-consuming-p instruction)
                 (setf has-consuming-p t)
                 (when (byte-regex-p regex)
                   (if (instruction-unicode-p instruction)
                       (setf unicode-consuming-p t)
                       (setf raw-consuming-p t))))))
    (unless has-consuming-p
      (%incremental-stream-error
       regex
       "Incremental streaming requires a non-empty consuming expression"))
    (let ((empty-text
            (if (byte-regex-p regex)
                (make-array 0 :element-type '(unsigned-byte 8))
                "")))
      (when (run-pike-vm
             (regex-program regex)
             empty-text
             :start 0
             :end 0
             :never-newline-p (regex-never-newline-p regex))
        (%incremental-stream-error
         regex
         "Incremental streaming does not support expressions that match the empty string")))
    (when (and (byte-regex-p regex)
               unicode-consuming-p
               raw-consuming-p)
      (%incremental-stream-error
       regex
       "Incremental byte streaming does not support mixing raw-octet and Unicode-scalar consuming instructions"))
    (values regex
            (and (byte-regex-p regex)
                 unicode-consuming-p))))

(defun make-incremental-regex-stream (regex &key (start 0) timeout)
  "Create a bounded-memory stream for the ordinary NFA subset of REGEX.

FEED accepts string chunks for character regexes and octet vectors for byte
regexes.  It returns newly finalized, non-overlapping MATCH-RESULT objects;
FINISH returns the final pending result, if any.  Positions are absolute
stream offsets beginning at START.  The stream deliberately rejects advanced,
zero-width, and line-break expressions.  Unicode-aware byte streams retain at
most an incomplete trailing UTF-8 prefix; mixed raw-octet and Unicode-scalar
byte expressions are rejected because their consuming widths cannot share one
bounded frontier."
  (check-type regex regex)
  (unless (and (integerp start) (<= 0 start))
    (error 'type-error :datum start :expected-type '(integer 0 *)))
  (validate-timeout timeout)
  (multiple-value-bind (validated-regex unicode-byte-p)
      (%validate-incremental-regex regex)
    (make-instance
     'incremental-regex-stream
     :regex validated-regex
     :start start
     :timeout timeout
     :position start
     :unicode-byte-p unicode-byte-p
     :workspace (make-pike-vm-closure-workspace
                 (length (regex-program validated-regex))))))

(defun %incremental-stream-fresh-thread (stream)
  (make-vm-thread
   :pc 0
   :slots (make-array
           (slot-count-for-program
            (regex-program (incremental-regex-stream-regex stream)))
           :initial-element nil)))

(defun %incremental-stream-closure (stream active position)
  (let ((regex (incremental-regex-stream-regex stream)))
    (pike-vm-closure
     (regex-program regex)
     nil
     position
     0
     (byte-regex-p regex)
     (append active (list (%incremental-stream-fresh-thread stream)))
     :workspace (%incremental-regex-stream-workspace stream))))

(defun %incremental-stream-result (stream slots)
  (let* ((regex (incremental-regex-stream-regex stream))
         (result
           (make-match-result-from-slots
            slots
            (slot-count-for-program (regex-program regex)))))
    (setf (match-result-group-names result) (regex-group-names regex))
    result))

(defun %incremental-stream-utf8-width (octet)
  (cond
    ((<= #x00 octet #x7f) 1)
    ((<= #xc2 octet #xdf) 2)
    ((<= #xe0 octet #xef) 3)
    ((<= #xf0 octet #xf4) 4)
    (t nil)))

(defun %incremental-stream-utf8-continuation-p (octet)
  (<= #x80 octet #xbf))

(defun %incremental-stream-utf8-prefix-valid-p (octets index limit width)
  (let* ((first (aref octets index))
         (available (- limit index)))
    (and
     (or
      (< available 2)
      (let ((second (aref octets (1+ index))))
        (and
         (%incremental-stream-utf8-continuation-p second)
         (or (/= first #xe0) (>= second #xa0))
         (or (/= first #xed) (<= second #x9f))
         (or (/= first #xf0) (>= second #x90))
         (or (/= first #xf4) (<= second #x8f)))))
     (or
      (<= width 2)
      (< available 3)
      (%incremental-stream-utf8-continuation-p
       (aref octets (+ index 2))))
     (or
      (<= width 3)
      (< available 4)
      (%incremental-stream-utf8-continuation-p
       (aref octets (+ index 3)))))))

(defun %incremental-stream-utf8-process-limit (octets start limit)
  (let ((index start))
    (loop while (< index limit)
          do (let* ((width
                     (%incremental-stream-utf8-width
                      (aref octets index)))
                    (available (- limit index)))
               (cond
                 ((null width)
                  (incf index))
                 ((%incremental-stream-utf8-prefix-valid-p
                   octets index limit width)
                  (if (>= available width)
                      (incf index width)
                    (return index)))
                 (t
                  (incf index))))
          finally (return limit))))

(defun %incremental-stream-combine-octets (pending octets start limit)
  (let* ((pending-length (length pending))
         (chunk-length (- limit start))
         (combined
           (make-array (+ pending-length chunk-length)
                       :element-type '(unsigned-byte 8))))
    (replace combined pending)
    (replace combined octets
             :start1 pending-length
             :start2 start
             :end2 limit)
    combined))

(defun %incremental-stream-next-index (octets index limit unicode-byte-p)
  (if unicode-byte-p
      (multiple-value-bind (character end valid-p)
          (utf8-character-at octets index)
        (declare (ignore character))
        (if (and valid-p (<= end limit)) end (1+ index)))
    (1+ index)))

(defun %incremental-stream-step (stream chunk index limit position active)
  (let* ((regex (incremental-regex-stream-regex stream))
         (byte-mode-p (byte-regex-p regex))
         (never-newline-p (regex-never-newline-p regex))
         (blocking-p nil)
         (next nil)
         (next-index
           (%incremental-stream-next-index
            chunk
            index
            limit
            (%incremental-regex-stream-unicode-byte-p stream))))
    (dolist (thread (%incremental-stream-closure stream active position))
      (let ((instruction (aref (regex-program regex) (vm-thread-pc thread))))
        (case (inst-op instruction)
          (:match
           (unless blocking-p
             (return-from %incremental-stream-step
               (values
               nil
                (%incremental-stream-result stream (vm-thread-slots thread))
                t
                nil))))
          ((:char :class :any)
           (multiple-value-bind (next-position matched-p)
               (instruction-match-end
                instruction
                chunk
                index
                limit
                byte-mode-p
                never-newline-p)
             (when matched-p
               (setf blocking-p t
                     next-index next-position)
               (push
                (make-vm-thread
                 :pc (inst-b instruction)
                 :slots (vm-thread-slots thread))
                next)))))))
    (values (nreverse next) nil nil next-index)))

(defun %incremental-stream-boundary-result (stream active position)
  "Return a match that cannot be extended by a future input element."
  (let ((blocking-p nil)
        (regex (incremental-regex-stream-regex stream)))
    (dolist (thread (%incremental-stream-closure stream active position))
      (let ((instruction (aref (regex-program regex) (vm-thread-pc thread))))
        (case (inst-op instruction)
          (:match
           (unless blocking-p
             (return-from %incremental-stream-boundary-result
               (%incremental-stream-result stream (vm-thread-slots thread)))))
          ((:char :class :any)
           (setf blocking-p t)))))
  nil))

(defun %incremental-stream-end-result (stream active position)
  (let ((regex (incremental-regex-stream-regex stream)))
    (dolist (thread (%incremental-stream-closure stream active position))
      (when (eq (inst-op (aref (regex-program regex) (vm-thread-pc thread)))
                :match)
        (return-from %incremental-stream-end-result
          (%incremental-stream-result stream (vm-thread-slots thread)))))
    nil))

(defun incremental-regex-stream-feed (stream chunk &key (start 0) end)
  "Feed CHUNK and return newly finalized non-overlapping matches.

No input is retained by this API.  The caller must retain or assemble the
logical input represented by the chunks when it wants to use MATCH-STRING on a
returned MATCH-RESULT; result positions are absolute offsets beginning at
START.  A timeout leaves the stream's position and active matcher state
unchanged."
  (check-type stream incremental-regex-stream)
  (when (incremental-regex-stream-finished-p stream)
    (error "Cannot feed an incremental regex stream after FINISH"))
  (let* ((regex (incremental-regex-stream-regex stream))
         (limit (validate-text-range regex chunk start end))
         (unicode-byte-p
           (%incremental-regex-stream-unicode-byte-p stream))
         (pending
           (if unicode-byte-p
               (%incremental-regex-stream-pending-octets stream)
             #()))
         (processing-chunk
           (if unicode-byte-p
               (%incremental-stream-combine-octets
                pending
                chunk
                start
                limit)
             chunk))
         (processing-start (if unicode-byte-p 0 start))
         (processing-limit
           (if unicode-byte-p
               (%incremental-stream-utf8-process-limit
                processing-chunk
                processing-start
                (length processing-chunk))
             limit))
         (new-pending
           (if unicode-byte-p
               (subseq processing-chunk
                       processing-limit
                       (length processing-chunk))
             #())))
    (call-with-timeout
     (incremental-regex-stream-timeout stream)
     (lambda ()
       (let ((active (%incremental-regex-stream-active stream))
             (position (incremental-regex-stream-position stream))
             (results nil))
         (let ((index processing-start))
           (loop while (< index processing-limit)
                 do (loop
                      (multiple-value-bind
                            (next result matched-p next-index)
                          (%incremental-stream-step
                           stream
                           processing-chunk
                           index
                           processing-limit
                           position
                           active)
                        (if matched-p
                            (progn
                              (push result results)
                              (setf active nil))
                          (progn
                            (setf active next
                                  position (+ position (- next-index index))
                                  index next-index)
                            (return))))))
               (unless (and unicode-byte-p
                            (> (length new-pending) 0))
                 (let ((result
                         (%incremental-stream-boundary-result
                          stream
                          active
                          position)))
                   (when result
                     (push result results)
                     (setf active nil)))))
         (setf (%incremental-regex-stream-active stream) active
               (slot-value stream 'position) position
               (%incremental-regex-stream-pending-octets stream) new-pending)
         (nreverse results))))))

(defun incremental-regex-stream-finish (stream)
  "Finalize STREAM and return its final pending match, if any.

FINISH is idempotent and returns a fresh result-list copy on every call."
  (check-type stream incremental-regex-stream)
  (if (incremental-regex-stream-finished-p stream)
      (copy-list (%incremental-regex-stream-final-result stream))
    (call-with-timeout
     (incremental-regex-stream-timeout stream)
     (lambda ()
       (let ((active (%incremental-regex-stream-active stream))
             (pending (%incremental-regex-stream-pending-octets stream))
             (position (incremental-regex-stream-position stream))
             (results nil))
         (let ((index 0)
               (limit (length pending)))
           (loop while (< index limit)
                 do (loop
                      (multiple-value-bind
                            (next result matched-p next-index)
                          (%incremental-stream-step
                           stream
                           pending
                           index
                           limit
                           position
                           active)
                        (if matched-p
                            (progn
                              (push result results)
                              (setf active nil))
                          (progn
                            (setf active next
                                  position (+ position (- next-index index))
                                  index next-index)
                            (return))))))
         (let ((result
                 (%incremental-stream-end-result
                  stream
                  active
                  position)))
           (when result
             (push result results)))
         (setf (incremental-regex-stream-finished-p stream) t
               (%incremental-regex-stream-active stream) nil
               (%incremental-regex-stream-pending-octets stream) #()
               (slot-value stream 'position) position
               (%incremental-regex-stream-final-result stream)
               (nreverse results))
         (copy-list (%incremental-regex-stream-final-result stream))))))))

(defun incremental-regex-stream-reset (stream)
  "Reset STREAM to its initial logical offset and clear its matcher state."
  (check-type stream incremental-regex-stream)
  (setf (%incremental-regex-stream-active stream) nil
        (slot-value stream 'position) (incremental-regex-stream-start stream)
        (%incremental-regex-stream-pending-octets stream) #()
        (incremental-regex-stream-finished-p stream) nil
        (%incremental-regex-stream-final-result stream) nil)
  stream)
