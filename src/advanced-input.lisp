(in-package #:cl-regex-kit)

(defun %advanced-read-element (context position unicode-p)
  (let ((text (advanced-context-text context))
        (limit (advanced-context-limit context)))
    (cond
      ((>= position limit) (values nil nil nil))
      ((stringp text) (values (aref text position) (1+ position) t))
      (unicode-p
       (multiple-value-bind (character end valid-p) (utf8-character-at
                                                     text
                                                     position)
         (if (and valid-p (<= end limit)) (values character end t)
           (values nil nil nil))))
      (t (values (aref text position) (1+ position) t)))))

(defun %advanced-element-equal-p (left right case-insensitive-p unicode-p)
  (cond
    ((and (characterp left) (characterp right))
     (if case-insensitive-p (if unicode-p (unicode-case-insensitive-char=
                                           left
                                           right)
                              (ascii-case-insensitive-char= left right))
       (char= left right)))
    ((and (integerp left) (integerp right))
     (if case-insensitive-p (= (ascii-fold-octet left) (ascii-fold-octet right))
       (= left right)))
    (t nil)))
