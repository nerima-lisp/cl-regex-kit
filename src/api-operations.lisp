(in-package #:cl-regex-kit)

(defmacro do-matches ((match regex text &key (start 0) end timeout) &body body)
  "Evaluate BODY for each non-overlapping match, from left to right."
  (let ((regex-var (gensym "REGEX-"))
        (text-var (gensym "TEXT-"))
        (position-var (gensym "POSITION-"))
        (end-var (gensym "END-"))
        (timeout-var (gensym "TIMEOUT-"))
        (length-var (gensym "LENGTH-"))
        (previous-non-empty-end-var (gensym "PREVIOUS-NON-EMPTY-END-")))
    `(let ((,regex-var ,regex)
          (,text-var ,text)
          (,position-var ,start)
          (,end-var ,end)
          (,timeout-var ,timeout))
      (check-type ,regex-var regex)
      (setf ,end-var (validate-text-range ,regex-var ,text-var ,position-var ,end-var))
      (call-with-timeout
        ,timeout-var
        (lambda ()
          (loop with ,length-var = ,end-var
                with ,previous-non-empty-end-var = nil
                while (<= ,position-var ,length-var)
                for ,match = (scan ,regex-var ,text-var :start ,position-var :end ,end-var)
                while ,match
                do (if (and ,previous-non-empty-end-var
                            (= (match-start ,match) (match-end ,match))
                            (= (match-start ,match) ,previous-non-empty-end-var))
                       ;; Rust's iterators suppress an empty match immediately
                       ;; following a non-empty match, then advance one position.
                       (setf ,position-var (1+ (match-end ,match))
                             ,previous-non-empty-end-var nil)
                       (progn
                         ,@body
                         (if (= (match-start ,match) (match-end ,match))
                             (setf ,position-var (1+ (match-end ,match))
                                   ,previous-non-empty-end-var nil)
                             (setf ,position-var (match-end ,match)
                                   ,previous-non-empty-end-var (match-end ,match)))))
                when (> ,position-var ,length-var)
                  do (loop-finish)))))))

(defmacro do-matches-overlapping ((match regex text &key (start 0) end timeout) &body body)
  "Evaluate BODY for every match of REGEX, including overlapping matches.

Unlike DO-MATCHES, the next search begins one input unit after the start of
the current match.  For strings an input unit is a character; for byte
vectors it is an octet.  Empty matches are therefore reported at every
eligible input position."
  (let ((regex-var (gensym "REGEX-"))
        (text-var (gensym "TEXT-"))
        (position-var (gensym "POSITION-"))
        (end-var (gensym "END-"))
        (timeout-var (gensym "TIMEOUT-")))
    `(let ((,regex-var ,regex)
           (,text-var ,text)
           (,position-var ,start)
           (,end-var ,end)
           (,timeout-var ,timeout))
       (check-type ,regex-var regex)
       (setf ,end-var (validate-text-range ,regex-var ,text-var ,position-var ,end-var))
       (call-with-timeout
        ,timeout-var
        (lambda ()
          (loop while (<= ,position-var ,end-var)
                for ,match = (scan ,regex-var ,text-var
                                   :start ,position-var
                                   :end ,end-var)
                while ,match
                do (progn
                     ,@body
                     (setf ,position-var (1+ (match-start ,match))))))))))

(defmacro do-captures ((locations regex text &key (start 0) end timeout) &body body)
  "Evaluate BODY for each non-overlapping match with reusable capture offsets.

LOCATIONS is bound once to a CAPTURE-LOCATIONS buffer and updated before every
BODY invocation. Its contents are valid only until the next iteration."
  (let ((regex-var (gensym "REGEX-"))
        (locations-var (gensym "LOCATIONS-")))
    `(let* ((,regex-var ,regex)
            (,locations-var (regex-capture-locations ,regex-var)))
       (do-matches (result ,regex-var ,text :start ,start :end ,end :timeout ,timeout)
         (copy-match-result-to-capture-locations result ,locations-var)
         (let ((,locations ,locations-var))
           ,@body)))))

(defmacro do-captures-overlapping ((locations regex text &key (start 0) end timeout) &body body)
  "Evaluate BODY for every overlapping match with reusable capture offsets.

LOCATIONS is bound once to a CAPTURE-LOCATIONS buffer and updated before every
BODY invocation. Its contents are valid only until the next iteration."
  (let ((regex-var (gensym "REGEX-"))
        (locations-var (gensym "LOCATIONS-")))
    `(let* ((,regex-var ,regex)
            (,locations-var (regex-capture-locations ,regex-var)))
       (do-matches-overlapping (result ,regex-var ,text
                                       :start ,start
                                       :end ,end
                                       :timeout ,timeout)
         (copy-match-result-to-capture-locations result ,locations-var)
         (let ((,locations ,locations-var))
           ,@body)))))

(defun all-matches (regex text &key (start 0) end timeout)
  "Return every non-overlapping MATCH-RESULT of REGEX in TEXT, left to right."
  (let ((matches nil))
    (do-matches
      (result regex text :start start :end end :timeout timeout)
      (push result matches))
    (nreverse matches)))

(defun all-matches-overlapping (regex text &key (start 0) end timeout)
  "Return every overlapping MATCH-RESULT of REGEX, left to right."
  (let ((matches nil))
    (do-matches-overlapping
      (result regex text :start start :end end :timeout timeout)
      (push result matches))
    (nreverse matches)))

(defun split-up-to (regex text count start end timeout &optional drop-terminal-empty-p)
  "Split TEXT into at most COUNT fields, or without a bound when COUNT is NIL."
  (unless (or (null count) (and (integerp count) (not (minusp count))))
    (error 'type-error :datum count :expected-type '(or null (integer 0 *))))
  (check-type regex regex)
  (validate-text-range regex text start end)
  (validate-timeout timeout)
  (when (zerop (or count 1))
    (return-from split-up-to))
  (let ((fields nil)
        ;; START limits delimiter discovery; it must not discard TEXT's prefix.
        (position 0)
        (split-count 0)
        (terminal-match-p nil))
    (do-matches (result regex text :start start :end end :timeout timeout)
      (when (and count (>= split-count (1- count)))
        (return))
      (push (subseq text position (match-start result)) fields)
      (setf position (match-end result)
            terminal-match-p (= position (length text)))
      (incf split-count))
    (unless (and drop-terminal-empty-p terminal-match-p)
      (push (subseq text position) fields))
    (nreverse fields)))

(defun split (regex text &key (start 0) end timeout)
  "Split TEXT on every non-overlapping match of REGEX."
  (split-up-to regex text nil start end timeout))

(defun split-terminator (regex text &key (start 0) end timeout)
  "Split TEXT, omitting an empty field caused by a terminal delimiter."
  (split-up-to regex text nil start end timeout t))

(defun split-inclusive (regex text &key (start 0) end timeout)
  "Split TEXT, retaining each delimiter at the end of its preceding field."
  (check-type regex regex)
  (validate-text-range regex text start end)
  (validate-timeout timeout)
  (let ((fields nil)
        ;; START limits delimiter discovery; it must not discard TEXT's prefix.
        (position 0)
        (matched-p nil))
    (do-matches (result regex text :start start :end end :timeout timeout)
      (push (subseq text position (match-end result)) fields)
      (setf position (match-end result)
            matched-p t))
    (when (or (not matched-p) (< position (length text)))
      (push (subseq text position) fields))
    (nreverse fields)))

(defun split-n (regex text count &key (start 0) end timeout)
  "Split TEXT into at most COUNT fields, like Rust Regex::splitn."
  (split-up-to regex text count start end timeout))
