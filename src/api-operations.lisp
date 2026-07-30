(in-package #:cl-regex-kit)

(defmacro do-matches ((match regex text &key (start 0) timeout) &body body)
  "Evaluate BODY for each non-overlapping match, from left to right."
  (let ((regex-var (gensym "REGEX-"))
        (text-var (gensym "TEXT-"))
        (position-var (gensym "POSITION-"))
        (timeout-var (gensym "TIMEOUT-"))
        (length-var (gensym "LENGTH-")))
    `(let ((,regex-var ,regex)
          (,text-var ,text)
          (,position-var ,start)
          (,timeout-var ,timeout))
      (check-type ,regex-var regex)
      (validate-text-and-start ,regex-var ,text-var ,position-var)
      (call-with-timeout
        ,timeout-var
        (lambda ()
          (loop with ,length-var = (length ,text-var)
                while (<= ,position-var ,length-var)
                for ,match = (scan ,regex-var ,text-var :start ,position-var)
                while ,match
                do (progn
              ,@body
              (setf ,position-var (if (= (match-start ,match) (match-end ,match)) (1+ (match-end ,match))
                  (match-end ,match))))
                when (> ,position-var ,length-var)
                  do (loop-finish)))))))

(defun all-matches (regex text &key (start 0) timeout)
  "Return every non-overlapping MATCH-RESULT of REGEX in TEXT, left to right."
  (let ((matches nil))
    (do-matches
      (result regex text :start start :timeout timeout)
      (push result matches))
    (nreverse matches)))

(defun split-up-to (regex text count start timeout)
    "Split TEXT into at most COUNT fields, or without a bound when COUNT is NIL."
    (unless (or (null count) (and (integerp count) (not (minusp count))))
      (error 'type-error :datum count :expected-type '(or null (integer 0 *))))
    (when (zerop (or count 1))
      (return-from split-up-to))
    (let ((fields nil)
          ;; START limits delimiter discovery; it must not discard TEXT's prefix.
          (position 0)
          (split-count 0))
      (do-matches (result regex text :start start :timeout timeout)
        (when (and count (>= split-count (1- count)))
          (return))
        (push (subseq text position (match-start result)) fields)
        (setf position (match-end result))
        (incf split-count))
      (push (subseq text position) fields)
      (nreverse fields)))

(defun split (regex text &key (start 0) timeout)
  "Split TEXT on every non-overlapping match of REGEX."
  (split-up-to regex text nil start timeout))

(defun split-n (regex text count &key (start 0) timeout)
  "Split TEXT into at most COUNT fields, like Rust Regex::splitn."
  (split-up-to regex text count start timeout))

(defun replacement-capture (match-result text designator empty-value)
  (let ((index
        (cond
          ((integerp designator) designator)
          ((stringp designator)
            (cdr (assoc designator (match-result-group-names match-result) :test #'string=)))
          (t nil))))
    (if (and
        (integerp index)
        (<= 0 index)
        (< index (length (match-result-groups match-result)))) (or (match-group-string match-result index text) empty-value)
      empty-value)))

(defun replacement-capture-string (match-result text designator)
  (replacement-capture match-result text designator ""))

(defun empty-octet-vector ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun ascii-octet-character (octet)
  (and (< octet 128) (code-char octet)))

(defun replacement-name-character-p (character)
  (capture-name-character-p character))

(defun replacement-designator (token)
  (if (every #'digit-char-p token) (parse-integer token)
    token))

(defun expand-replacement-template (template match-result text)
  "Expand Rust-style dollar captures in TEMPLATE for MATCH-RESULT."
  (with-output-to-string (output)
    (loop with length = (length template)
          for position from 0 below length
          for character = (char template position)
          do (cond
        ((char/= character #\$) (write-char character output))
        ((= (1+ position) length) (write-char #\$ output))
        ((char= (char template (1+ position)) #\$)
          (write-char #\$ output)
          (incf position))
        ((char= (char template (1+ position)) #\{)
          (let ((end (position #\} template :start (+ position 2))))
            (if end (let ((token (subseq template (+ position 2) end)))
                (when (plusp (length token))
                  (write-string
                    (replacement-capture-string match-result text (replacement-designator token))
                    output))
                (setf position end))
              (write-char #\$ output))))
        (t
          (let ((end
                (loop for
                      end from (1+ position) below length
                      while (replacement-name-character-p (char template end))
                      finally (return end))))
            (if (= end (1+ position)) (write-char #\$ output)
              (progn
                (write-string
                  (replacement-capture-string
                    match-result
                    text
                    (replacement-designator (subseq template (1+ position) end)))
                  output)
                (setf position (1- end))))))))))

(defun expand-byte-replacement-template (template match-result text)
  "Expand Rust-style ASCII dollar captures in byte TEMPLATE."
  (let ((output
        (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((append-octets (octets)
               (loop for octet across octets
                do (vector-push-extend octet output)))
             (append-capture (token)
               (when (every
              (lambda (octet)
                (< octet 128))
              token)
            (let ((name (map 'string #'ascii-octet-character token)))
              (append-octets
                (replacement-capture
                  match-result
                  text
                  (if (every #'digit-char-p name) (parse-integer name)
                    name)
                  (empty-octet-vector)))))))
      (loop with length = (length template)
            for position from 0 below length
            for octet = (aref template position)
            do (cond
          ((/= octet #x24) (vector-push-extend octet output))
          ((= (1+ position) length) (vector-push-extend #x24 output))
          ((= (aref template (1+ position)) #x24)
            (vector-push-extend #x24 output)
            (incf position))
          ((= (aref template (1+ position)) #x7b)
            (let ((end (position #x7d template :start (+ position 2))))
              (if end (progn
                  (when (> end (+ position 2))
                    (append-capture (subseq template (+ position 2) end)))
                  (setf position end))
                (vector-push-extend #x24 output))))
          (t
            (let ((end
                  (loop for
                        end from (1+ position) below length
                        for character = (ascii-octet-character (aref template end))
                        while (and character (replacement-name-character-p character))
                        finally (return end))))
              (if (= end (1+ position)) (vector-push-extend #x24 output)
                (progn
                  (append-capture (subseq template (1+ position) end))
                  (setf position (1- end))))))))
      output)))

(defun replacement-string (replacement match-result text)
  (let ((value
        (cond
          ((stringp replacement)
            (expand-replacement-template replacement match-result text))
          ((functionp replacement) (funcall replacement match-result text))
          (t (error 'type-error :datum replacement :expected-type '(or string function))))))
    (check-type value string)
    value))

(defun replacement-octets (replacement match-result text)
  (let ((value
        (cond
          ((typep replacement 'octet-vector)
            (expand-byte-replacement-template replacement match-result text))
          ((functionp replacement) (funcall replacement match-result text))
          (t
            (error
              'type-error
              :datum
              replacement
              :expected-type
              '(or octet-vector function))))))
    (check-type value octet-vector)
    value))

(defun replacement-value (regex replacement match-result text)
  (if (byte-regex-p regex) (replacement-octets replacement match-result text)
    (replacement-string replacement match-result text)))

(defun concatenate-replacement (regex prefix replacement suffix)
  (if (byte-regex-p regex) (concatenate 'octet-vector prefix replacement suffix)
    (concatenate 'string prefix replacement suffix)))

(defun replace-first (regex text replacement &key (start 0) timeout)
  "Replace the first match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a Rust-style template string or a function of result and text."
  (let ((result (scan regex text :start start :timeout timeout)))
    (if result (concatenate-replacement
        regex
        (subseq text 0 (match-start result))
        (replacement-value regex replacement result text)
        (subseq text (match-end result)))
      text)))

(defun valid-replacement-limit-p (limit)
  (or (null limit) (and (integerp limit) (not (minusp limit)))))

(defun replace-up-to (regex text replacement limit start timeout)
  (unless (valid-replacement-limit-p limit)
    (error 'type-error :datum limit :expected-type '(or null (integer 0 *))))
  (check-type regex regex)
  (validate-text-and-start regex text start)
  (when (zerop (or limit 1))
    (return-from replace-up-to text))
  (if (byte-regex-p regex) (let ((output
          (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
          (position 0)
          (replacement-count 0))
      (flet ((append-octets (octets)
                 (loop for octet across octets
                  do (vector-push-extend octet output))))
        (do-matches
          (result regex text :start start :timeout timeout)
          (when (and limit (>= replacement-count limit))
            (return))
          (append-octets (subseq text position (match-start result)))
          (append-octets (replacement-value regex replacement result text))
          (setf position (match-end result))
          (incf replacement-count))
        (append-octets (subseq text position))
        output))
    (with-output-to-string (output)
      (let ((position 0)
            (replacement-count 0))
        (do-matches
          (result regex text :start start :timeout timeout)
          (when (and limit (>= replacement-count limit))
            (return))
          (write-string (subseq text position (match-start result)) output)
          (write-string (replacement-value regex replacement result text) output)
          (setf position (match-end result))
          (incf replacement-count))
        (write-string (subseq text position) output)))))

(defun replace-n (regex text replacement count &key (start 0) timeout)
  "Replace at most COUNT non-overlapping matches of REGEX in TEXT.
This is the CL-REGEX-KIT equivalent of Rust Regex::replacen."
  (replace-up-to regex text replacement count start timeout))

(defun replace-all (regex text replacement &key (start 0) timeout)
  "Replace every non-overlapping match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a Rust-style template string or a function of result and text."
  (replace-up-to regex text replacement nil start timeout))
