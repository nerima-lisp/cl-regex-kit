;;;; t/incremental-streaming-test.lisp
;;;;
;;;; Incremental input must finalize only matches that no future chunk can extend.
(in-package #:cl-regex-kit/test)

(defun %incremental-test-spans (results)
  (mapcar (lambda (result)
            (list (match-start result) (match-end result)))
          results))

(it
 "agrees with materialized matching across chunk boundaries"
 (let* ((regex (compile-regex "ab|a"))
        (stream (make-incremental-regex-stream regex))
        (incremental
          (append (incremental-regex-stream-feed stream "a")
                  (incremental-regex-stream-feed stream "ba")
                  (incremental-regex-stream-feed stream "ba")
                  (incremental-regex-stream-finish stream))))
   (expect (%incremental-test-spans incremental)
           :to-equal
           (%incremental-test-spans (all-matches regex "ababa")))))

(it
 "exposes bounded stream state and validates construction arguments"
 (let* ((regex (compile-regex "a"))
        (stream (make-incremental-regex-stream regex
                                               :start 3
                                               :timeout nil)))
   (expect (incremental-regex-stream-p stream) :to-be-truthy)
   (expect (incremental-regex-stream-p nil) :to-be nil)
   (expect (incremental-regex-stream-regex stream) :to-equal regex)
   (expect (incremental-regex-stream-start stream) :to-equal 3)
   (expect (incremental-regex-stream-timeout stream) :to-be nil)
   (expect (incremental-regex-stream-position stream) :to-equal 3))
 (signals type-error
   (make-incremental-regex-stream (compile-regex "a") :start -1))
 (signals type-error
   (make-incremental-regex-stream (compile-regex "a") :start "0")))

(it
 "accepts Unicode byte streams and rejects empty programs"
 (expect (incremental-regex-stream-p
          (make-incremental-regex-stream (compile-byte-regex "\\p{L}")))
         :to-be-truthy)
 (signals regex-syntax-error
   (make-incremental-regex-stream
    (compile-byte-regex "a(?-u:b)")))
 (signals regex-syntax-error
   (make-incremental-regex-stream (compile-regex ""))))

(it
 "assembles UTF-8 scalars across byte chunks"
 (let* ((regex (compile-byte-regex ".."))
        (stream (make-incremental-regex-stream regex :start 10))
        (prefix (make-array 4
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#xc3 #xa9 #xf0 #x9f)))
        (suffix (make-array 2
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#x98 #x80))))
   (expect (incremental-regex-stream-feed stream prefix) :to-be-null)
   (expect (incremental-regex-stream-position stream) :to-equal 12)
   (let ((result (first (incremental-regex-stream-feed stream suffix))))
     (expect (list (match-start result) (match-end result))
             :to-equal
             '(10 16)))
   (expect (incremental-regex-stream-position stream) :to-equal 16)
   (expect (incremental-regex-stream-finish stream) :to-be-null)))

(it
 "drains an incomplete UTF-8 prefix at finish"
 (let* ((regex (compile-byte-regex "."))
        (stream (make-incremental-regex-stream regex))
        (prefix (make-array 1
                            :element-type '(unsigned-byte 8)
                            :initial-contents '(#xc3))))
   (expect (incremental-regex-stream-feed stream prefix) :to-be-null)
   (expect (incremental-regex-stream-position stream) :to-equal 0)
   (expect (incremental-regex-stream-finish stream) :to-be-null)
   (expect (incremental-regex-stream-position stream) :to-equal 1)))

(it
 "does not finalize a match while a longer alternative remains possible"
 (let ((stream (make-incremental-regex-stream
                (compile-regex "ab|a"))))
   (expect (incremental-regex-stream-feed stream "a") :to-be-null)
   (let ((result (first (incremental-regex-stream-feed stream "b"))))
     (expect (match-start result) :to-equal 0)
     (expect (match-end result) :to-equal 2))
   (expect (incremental-regex-stream-finish stream) :to-be-null)))

(it
 "finalizes a shorter higher-priority alternative immediately"
 (let ((stream (make-incremental-regex-stream
                (compile-regex "a|ab"))))
   (let ((result (first (incremental-regex-stream-feed stream "a"))))
     (expect (list (match-start result) (match-end result))
             :to-equal
             '(0 1)))
   (expect (incremental-regex-stream-feed stream "b") :to-be-null)
   (expect (incremental-regex-stream-finish stream) :to-be-null)))

(it
 "preserves capture locations when a match crosses chunks"
 (let ((stream (make-incremental-regex-stream
                (compile-regex "(a)(b)")
                :start 5)))
   (incremental-regex-stream-feed stream "a")
   (let ((results (incremental-regex-stream-feed stream "b")))
     (let ((result (first results)))
       (expect (match-start result) :to-equal 5)
       (expect (match-end result) :to-equal 7)
       (expect (match-group-start result 1) :to-equal 5)
       (expect (match-group-end result 1) :to-equal 6)
       (expect (match-group-start result 2) :to-equal 6)
       (expect (match-group-end result 2) :to-equal 7)))))

(it
 "supports ranged byte chunks with logical stream offsets"
 (let* ((regex (compile-byte-regex "(?-u:ab)"))
        (stream (make-incremental-regex-stream regex :start 10))
        (chunk (make-array 4
                           :element-type '(unsigned-byte 8)
                           :initial-contents '(120 97 98 121))))
   (let ((results (incremental-regex-stream-feed stream chunk
                                                  :start 1
                                                  :end 3)))
     (let ((result (first results)))
       (expect (match-start result) :to-equal 10)
       (expect (match-end result) :to-equal 12)))
   (expect (incremental-regex-stream-position stream) :to-equal 12)
   (expect (incremental-regex-stream-finish stream) :to-be-null)))

(it
 "supports reset, idempotent finish, and rejects post-finish input"
 (let ((stream (make-incremental-regex-stream (compile-regex "a"))))
   (incremental-regex-stream-feed stream "a")
   (let ((first-finish (incremental-regex-stream-finish stream)))
     (expect (incremental-regex-stream-finished-p stream) :to-be-truthy)
     (expect (incremental-regex-stream-finish stream)
             :to-equal
             first-finish)
     (signals error (incremental-regex-stream-feed stream "a")))
   (incremental-regex-stream-reset stream)
   (expect (incremental-regex-stream-position stream) :to-equal 0)
   (expect (incremental-regex-stream-finished-p stream) :to-be-null)
   (expect (length (incremental-regex-stream-feed stream "a")) :to-equal 1)))

(it
 "rejects expressions outside the exact incremental subset"
 (dolist (pattern '("a*" "(?=a)" "^a" "a\\R" "\\b"
                    "(?<x>a)\\k<x>"))
   (signals regex-syntax-error
     (make-incremental-regex-stream (compile-regex pattern)))))

(it
 "rejects invalid chunks and ranges"
 (let ((stream (make-incremental-regex-stream (compile-regex "a"))))
   (signals type-error
     (incremental-regex-stream-feed stream "a" :start -1))
   (signals type-error
     (incremental-regex-stream-feed stream "a" :end 2))
   (signals type-error
     (incremental-regex-stream-feed stream #(97)))))

(it
 "leaves an empty incremental stream open until finish"
 (let ((stream (make-incremental-regex-stream (compile-regex "a"))))
   (expect (incremental-regex-stream-finished-p stream) :to-be-null)
   (expect (incremental-regex-stream-finish stream) :to-be-null)
   (expect (incremental-regex-stream-finished-p stream) :to-be-truthy)))
