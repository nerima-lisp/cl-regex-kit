(in-package #:cl-regex-kit)

(defstruct (unicode-property-descriptor
    (:constructor %make-unicode-property-descriptor (kind payload))) (kind nil :read-only t)
  (payload nil :read-only t))

(defun packed-unicode-ranges-match-code-p (ranges code)
  (let ((count (ash (length ranges) -1)))
    (if (<= count 8) (loop for index below (length ranges) by 2
            thereis (<= (aref ranges index) code (aref ranges (1+ index))))
      (loop with low = 0
            with high = (1- count)
            while (<= low high)
            for middle = (ash (+ low high) -1)
            for index = (* middle 2)
            for lower = (aref ranges index)
            for upper = (aref ranges (1+ index))
            do (cond
          ((< code lower)
            (setf high (1- middle)))
          ((> code upper)
            (setf low (1+ middle)))
          (t (return t)))
            finally (return)))))

(defun unicode-property-descriptor-matches-p (descriptor character)
  (let ((kind (unicode-property-descriptor-kind descriptor))
        (payload (unicode-property-descriptor-payload descriptor)))
    (case kind
      (:any t)
      (:ascii (<= (char-code character) 127))
      (:assigned
       (not (eq payload (sb-unicode:general-category character))))
      (:ranges
       (packed-unicode-ranges-match-code-p payload (char-code character)))
      (:predicate
       (funcall payload character))
      (:category
       (eq payload (sb-unicode:general-category character)))
      (:major-category
       (member (sb-unicode:general-category character) payload :test (function eq)))
      (:cased-letter
       (member (sb-unicode:general-category character) payload :test (function eq)))
      (:script
       (eq payload (sb-unicode:script character)))
      (:script-extension
       (or (eq (car payload) (sb-unicode:script character))
           (packed-unicode-ranges-match-code-p (cdr payload) (char-code character))))
      (:block
       (eq payload (sb-unicode:char-block character)))
      (:grapheme-break
       (eq payload (sb-unicode:grapheme-break-class character)))
      (:word-break
       (eq payload (unicode-word-break-class character)))
      (:sentence-break
       (eq payload (sb-unicode:sentence-break-class character)))
      (otherwise
       (error "Unknown Unicode property descriptor kind: ~S" kind)))))
