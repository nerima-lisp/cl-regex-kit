(in-package #:cl-regex-kit)

(defun replacement-capture (match-result text designator empty-value)
  (let ((index
        (if (integerp designator)
            designator
            (cdr (assoc designator (match-result-group-names match-result) :test #'string=)))))
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
  (or (digit-char-p character)
      (not (null (find character
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
                       :test #'char=)))))

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
        (if (functionp replacement)
            (funcall replacement match-result text)
            (expand-replacement-template replacement match-result text))))
    (check-type value string)
    value))

(defun replacement-octets (replacement match-result text)
  (let ((value
        (if (functionp replacement)
            (funcall replacement match-result text)
            (expand-byte-replacement-template replacement match-result text))))
    (check-type value octet-vector)
    value))

(defun replacement-value (regex replacement match-result text)
  (if (byte-regex-p regex) (replacement-octets replacement match-result text)
    (replacement-string replacement match-result text)))

(defun validate-replacement (regex replacement)
  "Validate REPLACEMENT's static shape for REGEX and return it."
  (check-type regex regex)
  (unless (or (functionp replacement)
              (if (byte-regex-p regex)
                  (typep replacement 'octet-vector)
                  (stringp replacement)))
    (error 'type-error
           :datum replacement
           :expected-type (if (byte-regex-p regex)
                              '(or octet-vector function)
                              '(or string function))))
  replacement)

(defun concatenate-replacement (regex prefix replacement suffix)
  (if (byte-regex-p regex) (concatenate 'octet-vector prefix replacement suffix)
    (concatenate 'string prefix replacement suffix)))

(defun replace-first (regex text replacement &key (start 0) end timeout)
  "Replace the first match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a Rust-style template string or a function of result and text."
  (validate-replacement regex replacement)
  (let ((result (scan regex text :start start :end end :timeout timeout)))
    (if result (concatenate-replacement
        regex
        (subseq text 0 (match-start result))
        (replacement-value regex replacement result text)
        (subseq text (match-end result)))
      text)))

(defun valid-replacement-limit-p (limit)
  (or (null limit) (and (integerp limit) (not (minusp limit)))))

(defun replace-up-to (regex text replacement limit start end timeout)
  (unless (valid-replacement-limit-p limit)
    (error 'type-error :datum limit :expected-type '(or null (integer 0 *))))
  (check-type regex regex)
  (validate-text-range regex text start end)
  (validate-timeout timeout)
  (validate-replacement regex replacement)
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
          (result regex text :start start :end end :timeout timeout)
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
          (result regex text :start start :end end :timeout timeout)
          (when (and limit (>= replacement-count limit))
            (return))
          (write-string (subseq text position (match-start result)) output)
          (write-string (replacement-value regex replacement result text) output)
          (setf position (match-end result))
          (incf replacement-count))
        (write-string (subseq text position) output)))))

(defun replace-n (regex text replacement count &key (start 0) end timeout)
  "Replace at most COUNT non-overlapping matches of REGEX in TEXT.
This is the CL-REGEX-KIT equivalent of Rust Regex::replacen."
  (replace-up-to regex text replacement count start end timeout))

(defun replace-all (regex text replacement &key (start 0) end timeout)
  "Replace every non-overlapping match of REGEX in TEXT with REPLACEMENT.
REPLACEMENT is a Rust-style template string or a function of result and text."
  (replace-up-to regex text replacement nil start end timeout))
