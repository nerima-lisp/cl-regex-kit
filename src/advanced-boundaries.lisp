(in-package #:cl-regex-kit)

(defstruct advanced-sentence-unit start end class)

(defun %advanced-boundary-cache (context key producer)
  (let ((cache (advanced-context-boundary-cache context)))
    (unless (hash-table-p cache)
      (setf cache (make-hash-table :test #'eq)
            (advanced-context-boundary-cache context) cache))
    (multiple-value-bind (value present-p)
        (gethash key cache)
      (if present-p
          value
          (setf (gethash key cache) (funcall producer))))))

(defun %advanced-sentence-ignored-p (class)
  (member class '(:extend :format) :test #'eq))

(defun %advanced-sentence-paragraph-p (class)
  (member class '(:cr :lf :sep) :test #'eq))

(defun %advanced-sentence-terminal-p (class)
  (member class '(:aterm :sterm) :test #'eq))

(defun %advanced-sentence-unit-at (context position)
  (multiple-value-bind (character end valid-p)
      (%advanced-read-element context position t)
    (when (and valid-p (characterp character))
      (make-advanced-sentence-unit
       :start position
       :end end
       :class (sb-unicode:sentence-break-class character)))))

(defun %advanced-sentence-significant-before (units index)
  (loop for cursor = (min index (1- (length units)))
          then (1- cursor)
        while (>= cursor 0)
        unless (%advanced-sentence-ignored-p
                (advanced-sentence-unit-class (aref units cursor)))
          return cursor))

(defun %advanced-sentence-terminal-prefix-p
    (units left-index spaces-p &optional terminal-class)
  ;; The significant prefix is SATerm Close* Sp*.  Scan it backwards while
  ;; retaining the order of Close* and Sp*: a Close encountered after the
  ;; space run belongs to the following text, not to this terminal prefix.
  (let ((cursor left-index)
        (phase :spaces))
    (loop
      (when (< cursor 0)
        (return nil))
      (let ((class (advanced-sentence-unit-class (aref units cursor))))
        (cond
          ((%advanced-sentence-ignored-p class)
           (decf cursor))
          ((eq class :sp)
           (if (and spaces-p (eq phase :spaces))
               (decf cursor)
               (return nil)))
          ((eq class :close)
           (setf phase :closes)
           (decf cursor))
          ((if terminal-class
               (eq class terminal-class)
               (%advanced-sentence-terminal-p class))
           (return t))
           (t
           (return nil)))))))

(defun %advanced-sentence-sb8-before-lower-p
    (units left-index right-index)
  (and (%advanced-sentence-terminal-prefix-p units left-index t :aterm)
       (let ((cursor right-index)
             (limit (length units)))
         (loop
           (when (>= cursor limit)
             (return nil))
           (let ((class (advanced-sentence-unit-class
                         (aref units cursor))))
             (cond
               ((%advanced-sentence-ignored-p class)
                (incf cursor))
               ((eq class :lower)
                (return t))
               ((member class
                        '(:oletter :upper :lower :cr :lf :sep :aterm :sterm)
                        :test #'eq)
                (return nil))
               (t
                (incf cursor))))))))

(defun %advanced-sentence-aterm-before-lower-p (units left-index)
  (let ((cursor left-index))
    (loop
      (when (< cursor 0)
        (return nil))
      (let ((class (advanced-sentence-unit-class (aref units cursor))))
        (cond
          ((%advanced-sentence-ignored-p class)
           (decf cursor))
          ((eq class :aterm)
           (return t))
          ((member class '(:oletter :upper :lower :cr :lf :sep :sterm)
                   :test #'eq)
           (return nil))
          (t
           (decf cursor)))))))

(defun %advanced-sentence-upper-aterm-before-upper-p (units left-index)
  (let ((aterm-index (%advanced-sentence-significant-before units left-index)))
    (and aterm-index
         (eq (advanced-sentence-unit-class (aref units aterm-index)) :aterm)
         (let ((previous-index
                 (%advanced-sentence-significant-before units
                                                        (1- aterm-index))))
           (and previous-index
                (member
                 (advanced-sentence-unit-class (aref units previous-index))
                 '(:upper :lower)
                 :test #'eq))))))

(defun %advanced-sentence-boundary-rule (units left-index right-index)
  (let* ((right-class
           (advanced-sentence-unit-class (aref units right-index)))
         (left-significant-index
           (%advanced-sentence-significant-before units left-index))
         (left-class
           (and left-significant-index
                (advanced-sentence-unit-class
                 (aref units left-significant-index))))
         (terminal-p
           (%advanced-sentence-terminal-prefix-p units left-index nil))
         (terminal-with-spaces-p
           (%advanced-sentence-terminal-prefix-p units left-index t)))
    (cond
      ((and (eq left-class :cr) (eq right-class :lf))
       nil)
      ((%advanced-sentence-paragraph-p left-class)
       t)
      ((and (eq left-class :aterm) (eq right-class :numeric))
       nil)
      ((and (eq right-class :upper)
            (%advanced-sentence-upper-aterm-before-upper-p units left-index))
       nil)
      ((%advanced-sentence-sb8-before-lower-p
        units left-index right-index)
       nil)
      ((and (eq right-class :lower)
            (%advanced-sentence-aterm-before-lower-p units left-index))
       nil)
      ((and terminal-with-spaces-p
            (member right-class '(:scontinue :aterm :sterm) :test #'eq))
       nil)
      ((and terminal-p
            (member right-class '(:close :sp :cr :lf :sep) :test #'eq))
       nil)
      ((and terminal-with-spaces-p
            (member right-class '(:sp :cr :lf :sep) :test #'eq))
       nil)
      (terminal-with-spaces-p
       t)
      (terminal-p
       t)
      (t
       nil))))

(defun %advanced-sentence-boundary-between-p (units right-index)
  (let* ((left-index (1- right-index))
         (left-class
           (advanced-sentence-unit-class (aref units left-index)))
         (right-class
           (advanced-sentence-unit-class (aref units right-index))))
    (cond
      ((and (eq left-class :cr) (eq right-class :lf))
       nil)
      ;; SB5's paragraph break belongs before ignored characters.  Once an
      ;; Extend or Format follows that break, it must not be emitted again
      ;; after the ignored run.  Other terminal classes, such as ATerm, still
      ;; emit their own boundary after the ignored run.
      ((and (%advanced-sentence-ignored-p left-class)
            (let ((significant-index
                    (%advanced-sentence-significant-before units left-index)))
              (and significant-index
                   (%advanced-sentence-paragraph-p
                    (advanced-sentence-unit-class
                     (aref units significant-index))))))
       nil)
      ((%advanced-sentence-paragraph-p left-class)
       t)
      ((%advanced-sentence-ignored-p right-class)
       nil)
      (t
       (%advanced-sentence-boundary-rule units left-index right-index)))))

(defun %advanced-sentence-boundaries (context)
  (let* ((limit (advanced-context-limit context))
         (boundaries (make-array (1+ limit) :initial-element nil))
         (units nil)
         (position 0)
         (valid-p t))
    (setf (aref boundaries 0) t)
    (loop while (< position limit)
          do (let ((unit (%advanced-sentence-unit-at context position)))
               (unless unit
                 (setf valid-p nil)
                 (return))
               (push unit units)
               (setf position (advanced-sentence-unit-end unit))))
    (if (not valid-p)
        (make-array (1+ limit) :initial-element t)
        (let ((units (coerce (nreverse units) 'vector)))
          (setf (aref boundaries limit) t)
          (loop for index from 1 below (length units)
                for start = (advanced-sentence-unit-start (aref units index))
                do (setf (aref boundaries start)
                         (%advanced-sentence-boundary-between-p units index)))
          boundaries))))

(defun %advanced-sentence-boundary-p (context position unicode-p)
  (if (not unicode-p)
      t
      (let ((boundaries
              (%advanced-boundary-cache
               context
               :sentence
               (lambda ()
                 (%advanced-sentence-boundaries context)))))
        (and (<= 0 position)
             (< position (length boundaries))
             (aref boundaries position)))))

(defun %advanced-grapheme-boundary-unit-at (context position)
  (multiple-value-bind (character end valid-p)
      (%advanced-read-element context position t)
    (when (and valid-p (characterp character))
      (make-advanced-grapheme-unit
       :character character
       :start position
       :end end
       :class (%advanced-grapheme-class character)
       :indic-conjunct-break (%advanced-indic-conjunct-break-class character (%advanced-grapheme-class character))
       :extended-pictographic-p
       (%advanced-extended-pictographic-p character)))))

(defun %advanced-grapheme-boundary-p (context position unicode-p)
  (let ((limit (advanced-context-limit context)))
    (cond
      ((not unicode-p)
       t)
      ((or (zerop position) (= position limit))
       t)
      ((or (< position 0) (> position limit))
       nil)
      (t
       (let ((cursor 0)
             (units nil))
         (loop while (< cursor limit)
               do (let ((unit
                          (%advanced-grapheme-boundary-unit-at
                           context cursor)))
                    (unless unit
                      (return-from %advanced-grapheme-boundary-p t))
                    (let ((end (advanced-grapheme-unit-end unit)))
                      (cond
                        ((= cursor position)
                         (return-from
                             %advanced-grapheme-boundary-p
                           (%advanced-grapheme-break-p units unit t)))
                        ((> end position)
                         (return-from %advanced-grapheme-boundary-p nil))
                        (t
                         (push unit units)
                         (setf cursor end))))))
         t)))))

(defun %advanced-word-boundary-p (context position unicode-p)
  (let ((text (advanced-context-text context))
        (limit (advanced-context-limit context)))
    (if (stringp text)
        (word-boundary-p text position unicode-p)
        (if unicode-p
            (%byte-unicode-word-boundary-p text position)
            (let ((before
                    (and (plusp position)
                         (byte-word-character-p
                          (aref text (1- position)))))
                  (after
                    (and (< position limit)
                         (byte-word-character-p
                          (aref text position)))))
              (not (eq before after)))))))

(defun %advanced-special-boundary-p (node context position)
  (case (anchor-node-kind node)
    (:grapheme-boundary
     (%advanced-grapheme-boundary-p
      context position (anchor-node-unicode-p node)))
    (:word-boundary-unicode
     (%advanced-word-boundary-p
      context position (anchor-node-unicode-p node)))
    (:sentence-boundary
     (%advanced-sentence-boundary-p
      context position (anchor-node-unicode-p node)))
    (otherwise
     nil)))
