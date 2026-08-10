(in-package #:cl-regex-kit)

;;;; Chunked input adapter
;;;;
;;;; The Pike VM and the advanced executor currently require a complete input
;;;; value.  This API owns that value while callers deliver it in chunks.  It
;;;; deliberately has an explicit FINISH barrier: arbitrary lookaround,
;;;; backreferences, and end-of-input anchors cannot be reported correctly at
;;;; a chunk boundary without more engine state than either executor exposes.

(defclass regex-stream ()
  ((regex
    :initarg :regex
    :reader regex-stream-regex)
   (buffer
    :initarg :buffer
    :reader %regex-stream-buffer)
   (start
    :initarg :start
    :reader regex-stream-start)
   (timeout
    :initarg :timeout
    :reader regex-stream-timeout)
   (finished-p
    :initform nil
    :accessor regex-stream-finished-p)
   (matches
    :initform nil
    :accessor %regex-stream-matches)
   (overlapping-matches
    :initform nil
    :accessor %regex-stream-overlapping-matches)
   (matches-ready-p
    :initform nil
    :accessor %regex-stream-matches-ready-p)
   (overlapping-matches-ready-p
    :initform nil
    :accessor %regex-stream-overlapping-matches-ready-p)
   (matches-callback-index
    :initform 0
    :accessor %regex-stream-matches-callback-index)
   (overlapping-matches-callback-index
    :initform 0
    :accessor %regex-stream-overlapping-matches-callback-index)))

(defun regex-stream-p (object)
  "Return true when OBJECT is a chunked input state created by MAKE-REGEX-STREAM."
  (typep object 'regex-stream))

(defun %make-regex-stream-buffer (regex)
  (make-array 0
              :element-type (if (byte-regex-p regex)
                                '(unsigned-byte 8)
                                'character)
              :adjustable t
              :fill-pointer 0))

(defun make-regex-stream (regex &key (start 0) timeout)
  "Create a chunked input state for REGEX.

FEED appends string or octet-vector chunks, and FINISH performs the complete
match operation.  START and TIMEOUT use the same coordinate and timeout
contracts as ALL-MATCHES.  The stream owns the accumulated input until RESET
or garbage collection; match offsets remain meaningful against
REGEX-STREAM-TEXT until then."
  (check-type regex regex)
  (unless (and (integerp start) (not (minusp start)))
    (error 'type-error :datum start :expected-type '(integer 0 *)))
  (validate-timeout timeout)
  (make-instance 'regex-stream
                 :regex regex
                 :buffer (%make-regex-stream-buffer regex)
                 :start start
                 :timeout timeout))

(defun regex-stream-length (stream)
  "Return the number of input units received by STREAM."
  (check-type stream regex-stream)
  (length (%regex-stream-buffer stream)))

(defun regex-stream-text (stream)
  "Return a copy of the input received by STREAM so far.

The copy prevents callers from mutating the buffer used by cached match
results."
  (check-type stream regex-stream)
  (copy-seq (%regex-stream-buffer stream)))

(defun regex-stream-feed (stream chunk &key (start 0) end)
  "Append CHUNK, or its START..END range, to STREAM.

CHUNK must be a string for a character REGEX and an octet vector for a byte
REGEX.  FEED returns STREAM.  Feeding after FINISH is an error; call RESET to
reuse the state."
  (check-type stream regex-stream)
  (when (regex-stream-finished-p stream)
    (error "Cannot feed a finalized regex stream; call REGEX-STREAM-RESET first."))
  (let* ((regex (regex-stream-regex stream))
         (limit (validate-text-range regex chunk start end))
         (buffer (%regex-stream-buffer stream)))
    (loop for position from start below limit
          do (vector-push-extend (aref chunk position) buffer))
    stream))

(defun %regex-stream-finish-matches (stream overlapping-p)
  (if overlapping-p
      (if (%regex-stream-overlapping-matches-ready-p stream)
          (%regex-stream-overlapping-matches stream)
          (prog1
              (setf (%regex-stream-overlapping-matches stream)
                    (all-matches-overlapping
                     (regex-stream-regex stream)
                     (%regex-stream-buffer stream)
                     :start (regex-stream-start stream)
                     :timeout nil))
            (setf (%regex-stream-overlapping-matches-ready-p stream) t)))
      (if (%regex-stream-matches-ready-p stream)
          (%regex-stream-matches stream)
          (prog1
              (setf (%regex-stream-matches stream)
                    (all-matches
                     (regex-stream-regex stream)
                     (%regex-stream-buffer stream)
                     :start (regex-stream-start stream)
                     :timeout nil))
            (setf (%regex-stream-matches-ready-p stream) t)))))

(defun regex-stream-finish (stream &key overlapping-p callback)
  "Finalize STREAM and return its matches in left-to-right order.

When OVERLAPPING-P is true, overlapping matches are returned.  CALLBACK, when
non-NIL, is called for each cached match that has not already been delivered
successfully.  FINISH is idempotent, and a callback that signals can be retried
without re-delivering earlier successful callbacks.  The returned list is a
fresh list, so changing its conses does not change the stream cache."
  (check-type stream regex-stream)
  (check-type callback (or null function))
  (call-with-timeout
   (regex-stream-timeout stream)
   (lambda ()
     (let ((matches (%regex-stream-finish-matches stream overlapping-p)))
       ;; Keep the state feedable when matching signals (for example, on a
       ;; timeout).  A successful empty result is distinguished from an absent
       ;; cache by the READY-P slots above.  Once matching succeeds, the input
       ;; is finalized even if a callback later signals; the callback index
       ;; makes a retry resume at the first undelivered result.
       (setf (regex-stream-finished-p stream) t)
       (when callback
         (if overlapping-p
             (loop for index from (%regex-stream-overlapping-matches-callback-index
                                   stream)
                   below (length matches)
                   do (funcall callback (nth index matches))
                      (setf (%regex-stream-overlapping-matches-callback-index
                              stream)
                            (1+ index)))
             (loop for index from (%regex-stream-matches-callback-index stream)
                   below (length matches)
                   do (funcall callback (nth index matches))
                      (setf (%regex-stream-matches-callback-index stream)
                            (1+ index)))))
       (copy-list matches)))))

(defun regex-stream-reset (stream)
  "Discard STREAM's buffered input and cached matches, returning STREAM."
  (check-type stream regex-stream)
  (setf (fill-pointer (%regex-stream-buffer stream)) 0
        (regex-stream-finished-p stream) nil
        (%regex-stream-matches stream) nil
        (%regex-stream-overlapping-matches stream) nil
        (%regex-stream-matches-ready-p stream) nil
        (%regex-stream-overlapping-matches-ready-p stream) nil
        (%regex-stream-matches-callback-index stream) 0
        (%regex-stream-overlapping-matches-callback-index stream) 0)
  stream)

(defun %read-regex-stream (regex input-stream chunk-size start end)
  (check-type input-stream stream)
  (check-type chunk-size (integer 1 *))
  (unless (or (null end) (and (integerp end) (<= start end)))
    (error 'type-error :datum end :expected-type `(or null (integer ,start *))))
    (let* ((state (make-regex-stream regex :start start :timeout nil))
         (element-type (if (byte-regex-p regex)
                           '(unsigned-byte 8)
                           'character))
         (chunk (make-array chunk-size :element-type element-type))
         (total 0))
    (loop while (or (null end) (< total end))
          for available = (if end (min chunk-size (- end total)) chunk-size)
          for count = (read-sequence chunk input-stream :start 0 :end available)
          while (plusp count)
          do (incf total count)
             (regex-stream-feed state chunk :end count))
    (when (and end (< total end))
      (error 'type-error :datum end :expected-type `(integer ,start ,total)))
    state))

(defun %stream-matches-from-input (regex input-stream chunk-size start end
                                    timeout overlapping-p)
  (call-with-timeout
   timeout
   (lambda ()
     (regex-stream-finish
      (%read-regex-stream regex input-stream chunk-size start end)
      :overlapping-p overlapping-p))))

(defun all-stream-matches (regex input-stream
                           &key (chunk-size 4096) (start 0) end timeout)
  "Read INPUT-STREAM in chunks and return non-overlapping matches.

The input stream is not closed.  Matching starts only after EOF, which keeps
the result correct for patterns whose match depends on later input.  TIMEOUT
is applied around the read loop and matching; an arbitrary blocking external
or foreign stream read may not be interruptible."
  (%stream-matches-from-input regex input-stream chunk-size start end timeout nil))

(defun all-stream-matches-overlapping (regex input-stream
                                       &key (chunk-size 4096) (start 0) end timeout)
  "Read INPUT-STREAM in chunks and return overlapping matches at EOF."
  (%stream-matches-from-input regex input-stream chunk-size start end timeout t))

(defun scan-stream (regex input-stream
                    &key (chunk-size 4096) (start 0) end timeout)
  "Read INPUT-STREAM in chunks and return its first match, or NIL."
  (first (all-stream-matches regex input-stream
                             :chunk-size chunk-size
                             :start start
                             :end end
                             :timeout timeout)))

(defmacro do-stream-matches ((match regex input-stream
                              &key (chunk-size 4096) (start 0) end timeout)
                             &body body)
  "Evaluate BODY for each non-overlapping match read from INPUT-STREAM."
  `(dolist (,match
             (all-stream-matches ,regex ,input-stream
                                 :chunk-size ,chunk-size
                                 :start ,start
                                 :end ,end
                                 :timeout ,timeout))
     ,@body))

(defmacro do-stream-matches-overlapping ((match regex input-stream
                                          &key (chunk-size 4096) (start 0)
                                            end timeout)
                                         &body body)
  "Evaluate BODY for each overlapping match read from INPUT-STREAM."
  `(dolist (,match
             (all-stream-matches-overlapping
              ,regex
              ,input-stream
              :chunk-size ,chunk-size
              :start ,start
              :end ,end
              :timeout ,timeout))
     ,@body))

(defmacro do-stream-captures ((locations regex input-stream
                               &key (chunk-size 4096) (start 0) end timeout)
                              &body body)
  "Evaluate BODY for each streamed match with reusable capture offsets."
  (let ((regex-var (gensym "REGEX-"))
        (locations-var (gensym "LOCATIONS-")))
    `(let* ((,regex-var ,regex)
            (,locations-var (regex-capture-locations ,regex-var)))
       (do-stream-matches (result ,regex-var ,input-stream
                                  :chunk-size ,chunk-size
                                  :start ,start
                                  :end ,end
                                  :timeout ,timeout)
         (copy-match-result-to-capture-locations result ,locations-var)
         (let ((,locations ,locations-var))
           ,@body)))))

(defmacro do-stream-captures-overlapping ((locations regex input-stream
                                           &key (chunk-size 4096) (start 0)
                                             end timeout)
                                          &body body)
  "Evaluate BODY for each overlapping streamed match with reusable offsets."
  (let ((regex-var (gensym "REGEX-"))
        (locations-var (gensym "LOCATIONS-")))
    `(let* ((,regex-var ,regex)
            (,locations-var (regex-capture-locations ,regex-var)))
       (do-stream-matches-overlapping
        (result ,regex-var ,input-stream
                :chunk-size ,chunk-size
                :start ,start
                :end ,end
                :timeout ,timeout)
         (copy-match-result-to-capture-locations result ,locations-var)
         (let ((,locations ,locations-var))
           ,@body)))))
