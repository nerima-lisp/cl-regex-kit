;;;; t/streaming-test.lisp
;;;;
;;;; Chunked input adapters must agree with materialized-input operations.
(in-package #:cl-regex-kit/test)

(it
 "matches character chunks only after an explicit finish"
 (let* ((regex (compile-regex "a+b"))
        (stream (make-regex-stream regex)))
   (regex-stream-feed stream "a")
   (regex-stream-feed stream "a")
   (expect (regex-stream-finished-p stream) :to-be-null)
   (regex-stream-feed stream "b")
   (let* ((matches (regex-stream-finish stream))
          (result (first matches)))
     (expect (length matches) :to-equal 1)
     (expect (match-start result) :to-equal 0)
     (expect (match-end result) :to-equal 3)
     (expect (match-string result (regex-stream-text stream)) :to-equal "aab")
     (expect (regex-stream-finished-p stream) :to-be-truthy)
     (expect (regex-stream-finish stream) :to-equal matches))))

(it
 "exposes stream state and validates input bounds"
 (let* ((regex (compile-regex "a"))
        (stream (make-regex-stream regex :start 3 :timeout nil)))
   (expect (regex-stream-p stream) :to-be-truthy)
   (expect (regex-stream-p nil) :to-be nil)
   (expect (regex-stream-regex stream) :to-equal regex)
   (expect (regex-stream-start stream) :to-equal 3)
   (expect (regex-stream-timeout stream) :to-be nil)
   (expect (regex-stream-length stream) :to-equal 0))
 (signals type-error
   (make-regex-stream (compile-regex "a") :start -1))
 (signals type-error
   (make-regex-stream (compile-regex "a") :start "0"))
 (signals type-error
   (all-stream-matches (compile-regex "a")
                       (make-string-input-stream "a")
                       :start 2
                       :end 1))
 (signals type-error
   (all-stream-matches (compile-regex "a")
                       (make-string-input-stream "a")
                       :end 2))
 (signals type-error
   (all-stream-matches (compile-regex "a")
                       (make-string-input-stream "a")
                       :end "2")))

(it
 "keeps overlapping matches across chunk boundaries"
 (let ((stream (make-regex-stream (compile-regex "aa"))))
   (regex-stream-feed stream "a")
   (regex-stream-feed stream "aa")
   (expect (length (regex-stream-finish stream)) :to-equal 1)
   (expect (length (regex-stream-finish stream :overlapping-p t)) :to-equal 2)))

(it
 "delivers overlapping callbacks and caches their results"
 (let ((stream (make-regex-stream (compile-regex "aa")))
       (starts nil))
   (regex-stream-feed stream "aaa")
   (regex-stream-finish
    stream
    :overlapping-p t
    :callback (lambda (result)
                (push (match-start result) starts)))
   (expect (nreverse starts) :to-equal '(0 1))
   (expect (mapcar #'match-start
                   (regex-stream-finish stream :overlapping-p t))
           :to-equal
           '(0 1))))

(it
 "supports byte chunks and ranged feeds"
 (let* ((regex (compile-byte-regex "ab"))
        (stream (make-regex-stream regex))
        (chunk (make-array 3
                           :element-type '(unsigned-byte 8)
                           :initial-contents '(97 98 99))))
   (regex-stream-feed stream chunk :start 0 :end 2)
   (let ((result (first (regex-stream-finish stream))))
     (expect (match-start result) :to-equal 0)
     (expect (match-end result) :to-equal 2)
     (expect (regex-stream-length stream) :to-equal 2)
     (expect (coerce (regex-stream-text stream) 'list) :to-equal '(97 98)))))

(it
 "reads byte input streams without changing their ownership"
 (let ((path (merge-pathnames
              (format nil "cl-regex-kit-stream-~A.bin" (gensym))
              (uiop:temporary-directory))))
   (unwind-protect
        (progn
          (with-open-file (output path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type '(unsigned-byte 8))
            (write-sequence
             (make-array 3
                         :element-type '(unsigned-byte 8)
                         :initial-contents '(120 97 98))
             output))
          (with-open-file (input path
                                 :direction :input
                                 :element-type '(unsigned-byte 8))
            (let ((result (first
                            (all-stream-matches
                             (compile-byte-regex "ab")
                             input
                             :chunk-size 1))))
              (expect (list (match-start result) (match-end result))
                      :to-equal
                      '(1 3))
              (expect (read-byte input nil :eof) :to-equal :eof))))
     (when (probe-file path)
       (delete-file path)))))

(it
 "adapts character input streams without closing them"
 (let* ((input (make-string-input-stream "a aa"))
        (regex (compile-regex "a+"))
        (matches (all-stream-matches regex input :chunk-size 1)))
   (expect (mapcar (lambda (result) (match-string result "a aa")) matches)
           :to-equal '("a" "aa"))
   (expect (read-char input nil :eof) :to-equal :eof)))

(it
 "provides callback and capture variants for streamed input"
 (let ((seen nil)
       (captures nil))
   (do-stream-matches
    (result (compile-regex "(a)(b)") (make-string-input-stream "ab ab")
            :chunk-size 1)
    (push (match-string result "ab ab") seen))
   (do-stream-captures
    (locations (compile-regex "(a)(b)") (make-string-input-stream "ab")
               :chunk-size 1)
    (push (list (capture-location-start locations 1)
                (capture-location-end locations 1)
                (capture-location-start locations 2)
                (capture-location-end locations 2))
          captures))
   (expect (nreverse seen) :to-equal '("ab" "ab"))
   (expect (nreverse captures) :to-equal '((0 1 1 2)))))

(it
 "resets a finalized stream and rejects further feeds until reset"
 (let ((stream (make-regex-stream (compile-regex "a"))))
   (regex-stream-feed stream "a")
   (regex-stream-finish stream)
   (signals error (regex-stream-feed stream "a"))
   (regex-stream-reset stream)
   (expect (regex-stream-length stream) :to-equal 0)
   (regex-stream-feed stream "a")
   (expect (length (regex-stream-finish stream)) :to-equal 1)))

(it
 "caches empty results and remains open when finalization fails"
 (let ((empty (make-regex-stream (compile-regex "z")))
       (late (make-regex-stream (compile-regex "a") :start 2)))
   (expect (regex-stream-finish empty) :to-be-null)
   (expect (cl-regex-kit::%regex-stream-matches-ready-p empty) :to-be-truthy)
   (expect (regex-stream-finish empty) :to-be-null)
   (regex-stream-feed late "a")
   (signals type-error (regex-stream-finish late))
   (expect (regex-stream-finished-p late) :to-be-null)
   (regex-stream-feed late "aa")
   (expect (length (regex-stream-finish late)) :to-equal 1)))

(it
 "reports the first streamed match through scan-stream"
 (let ((result (scan-stream (compile-regex "b")
                            (make-string-input-stream "a b")
                            :chunk-size 1)))
   (expect (match-start result) :to-equal 2)
   (expect (match-end result) :to-equal 3)))

(it
 "supports bounded and overlapping stream helpers"
 (let* ((regex (compile-regex "aa"))
        (input (make-string-input-stream "aaa"))
        (matches (all-stream-matches-overlapping regex input
                                                  :chunk-size 1
                                                  :end 3))
        (starts nil))
   (expect (mapcar #'match-start matches) :to-equal '(0 1))
   (expect (read-char input nil :eof) :to-equal :eof)
   (do-stream-matches-overlapping
    (result (compile-regex "aa") (make-string-input-stream "aaa")
            :chunk-size 1 :end 3)
    (push (match-start result) starts))
   (expect (nreverse starts) :to-equal '(0 1))
   (let ((capture-starts nil))
     (do-stream-captures-overlapping
      (locations (compile-regex "(aa)") (make-string-input-stream "aaa")
                 :chunk-size 1 :end 3)
      (push (capture-location-start locations 1) capture-starts))
     (expect (nreverse capture-starts) :to-equal '(0 1)))))

(it
 "resumes callbacks after a callback failure"
 (let ((stream (make-regex-stream (compile-regex "a")))
       (calls 0)
       (seen nil))
   (regex-stream-feed stream "a a a")
   (signals simple-error
     (regex-stream-finish
      stream
      :callback (lambda (result)
                  (incf calls)
                  (when (= calls 2)
                    (error "callback failed"))
                  (push (match-start result) seen))))
   (expect (regex-stream-finished-p stream) :to-be-truthy)
   (regex-stream-finish
    stream
    :callback (lambda (result)
                (push (match-start result) seen)))
   (expect (nreverse seen) :to-equal '(0 2 4))))

(it
 "returns result-list copies independent of the stream cache"
 (let ((stream (make-regex-stream (compile-regex "a"))))
   (regex-stream-feed stream "aa")
   (let ((first-result (regex-stream-finish stream)))
     (setf (cdr first-result) nil)
     (expect (length (regex-stream-finish stream)) :to-equal 2))))
