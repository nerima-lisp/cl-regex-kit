(in-package #:cl-regex-kit)

(defun replacement-capture-index (match-result designator)
  (if (integerp designator)
      designator
      (and (find designator
                 (match-result-group-names match-result)
                 :key #'car
                 :test #'string=)
           (resolve-group-index match-result designator))))

(defun replacement-capture (match-result text designator empty-value)
  (let ((index (replacement-capture-index match-result designator)))
    (if (and
         (integerp index)
         (<= 0 index)
         (< index (length (match-result-groups match-result))))
        (or (match-group-string
             match-result
             index
             text)
            empty-value)
        empty-value)))

(defun replacement-capture-string (match-result text designator)
  (replacement-capture match-result text designator ""))

(defun empty-octet-vector ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun ascii-octet-character (octet)
  (and (< octet 128) (code-char octet)))

(defun replacement-name-character-p (character)
  (or
   (digit-char-p character)
   (not
    (null
     (find
      character
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
      :test
      #'char=)))))

(defun replacement-designator (token)
  (if (every #'digit-char-p token) (parse-integer token)
    token))

(defun byte-replacement-name-character-p (octet)
  (let ((character (ascii-octet-character octet)))
    (and character (replacement-name-character-p character))))

(defun byte-replacement-designator (token)
  (when (every (lambda (octet)
                 (< octet 128))
               token)
    (replacement-designator
     (map 'string #'ascii-octet-character token))))

(defun make-octet-output-buffer ()
  (make-array
   0
   :element-type '(unsigned-byte 8)
   :adjustable t
   :fill-pointer 0))

(defun append-octets (output octets)
  "Append OCTETS to the adjustable octet OUTPUT buffer and return OUTPUT."
  (loop for octet across octets
        do (vector-push-extend octet output))
  output)

(defun expand-replacement-template-generic (template
                                            sigil
                                            open-brace
                                            close-brace
                                            name-char-p
                                            emit-literal
                                            emit-capture
                                            output)
  "Shared dollar-template scanner for character and octet domains. SIGIL, OPEN-BRACE, and CLOSE-BRACE are domain elements; NAME-CHAR-P recognizes bare capture names; EMIT-LITERAL and EMIT-CAPTURE write to OUTPUT."
  (loop with length = (length template)
        for position from 0 below length
        for element = (elt template position)
        do (cond
             ((not (eql element sigil)) (funcall emit-literal element output))
             ((= (1+ position) length) (funcall emit-literal sigil output))
             ((eql (elt template (1+ position)) sigil)
               (funcall emit-literal sigil output)
               (incf position))
             ((eql (elt template (1+ position)) open-brace)
              (let ((end (position close-brace template :start (+ position 2))))
                (if end (progn
                          (when (> end (+ position 2))
                            (funcall
                             emit-capture
                             (subseq template (+ position 2) end)
                             output))
                          (setf position end))
                  (funcall emit-literal sigil output))))
             (t
              (let ((end
                     (loop for
                           end from (1+ position) below length
                           while (funcall name-char-p (elt template end))
                           finally (return end))))
                (if (= end (1+ position)) (funcall emit-literal sigil output)
                  (progn
                    (funcall
                     emit-capture
                     (subseq template (1+ position) end)
                     output)
                    (setf position (1- end)))))))
        finally (return output)))

(defun emit-byte-replacement-capture (token output match-result text)
  (let ((designator (byte-replacement-designator token)))
    (when designator
      (append-octets
       output
       (replacement-capture
        match-result
        text
        designator
        (empty-octet-vector))))))

(defmacro define-replacement-template-expander
    (name
     (&key
      documentation
      sigil
      open-brace
      close-brace
      name-character-p
      byte-domain-p
      capture-form))
  "Define one specialized dollar-template expander from declarative pieces.

The scanner stays in EXPAND-REPLACEMENT-TEMPLATE-GENERIC; this macro only
describes domain-specific literals, capture-token parsing, and output shape."
  `(defun ,name (template match-result text)
     ,documentation
     ,(if byte-domain-p
          `(let ((output (make-octet-output-buffer)))
             (expand-replacement-template-generic
              template
              ,sigil
              ,open-brace
              ,close-brace
              (function ,name-character-p)
              (lambda (element output)
                (vector-push-extend element output))
              (lambda (token output)
                ,capture-form)
              output))
          `(with-output-to-string (output)
             (expand-replacement-template-generic
              template
              ,sigil
              ,open-brace
              ,close-brace
              (function ,name-character-p)
              (lambda (element output)
                (write-char element output))
              (lambda (token output)
                ,capture-form)
              output)))))

(define-replacement-template-expander
    expand-replacement-template
    (:documentation "Expand dollar-style TEMPLATE capture references for MATCH-RESULT."
     :sigil #\$
     :open-brace #\{
     :close-brace #\}
     :name-character-p replacement-name-character-p
     :capture-form
     (write-string
      (replacement-capture-string
       match-result
       text
       (replacement-designator token))
      output)))

(define-replacement-template-expander
    expand-byte-replacement-template
    (:documentation "Expand ASCII dollar-style byte TEMPLATE capture references."
     :sigil #x24
     :open-brace #x7b
     :close-brace #x7d
     :name-character-p byte-replacement-name-character-p
     :byte-domain-p t
     :capture-form
     (emit-byte-replacement-capture token output match-result text)))

(defmacro define-replacement-value-reader
    (name replacement-type expander &optional documentation)
  "Define one replacement resolver that accepts functions or templates."
  `(defun ,name (replacement match-result text)
     ,documentation
     (let ((value
            (if (functionp replacement)
                (funcall replacement match-result text)
                (,expander replacement match-result text))))
       (check-type value ,replacement-type)
       value)))

(define-replacement-value-reader
    replacement-string
    string
    expand-replacement-template
  "Resolve REPLACEMENT in the character domain.")

(define-replacement-value-reader
    replacement-octets
    octet-vector
    expand-byte-replacement-template
  "Resolve REPLACEMENT in the octet domain.")

(defun replacement-value (regex replacement match-result text)
  (if (byte-regex-p regex) (replacement-octets replacement match-result text)
    (replacement-string replacement match-result text)))

(defun validate-replacement (regex replacement)
  "Validate REPLACEMENT's static shape for REGEX and return it."
  (check-type regex regex)
  (unless (or
           (functionp replacement)
           (if (byte-regex-p regex) (typep replacement 'octet-vector)
             (stringp replacement)))
    (error
     'type-error
     :datum
     replacement
     :expected-type
     (if (byte-regex-p regex) '(or octet-vector function)
       '(or string function))))
  replacement)

(defun concatenate-replacement (regex prefix replacement suffix)
  (if (byte-regex-p regex) (concatenate 'octet-vector prefix replacement suffix)
    (concatenate 'string prefix replacement suffix)))

(defun replace-first (regex text replacement &key (start 0) end timeout)
  "Replace the first match of REGEX in TEXT with REPLACEMENT. REPLACEMENT is a dollar-style template string or a function of result and text."
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

(defun accumulate-replacements (regex
                                text
                                replacement
                                limit
                                start
                                end
                                timeout
                                append)
  "Shared CPS accumulation loop for REPLACE-UP-TO. APPEND receives literal spans and expanded replacement values, avoiding separate string and octet traversals."
  (let ((position 0)
        (replacement-count 0))
    (do-matches
     (result regex text :start start :end end :timeout timeout)
     (when (and limit (>= replacement-count limit))
       (return))
     (funcall append (subseq text position (match-start result)))
     (funcall append (replacement-value regex replacement result text))
     (setf position (match-end result))
     (incf replacement-count))
    (funcall append (subseq text position))))

(defun replace-up-to (regex text replacement limit start end timeout)
  (unless (valid-replacement-limit-p limit)
    (error
     (quote type-error)
     :datum
     limit
     :expected-type
     (quote (or null (integer 0 *)))))
  (check-type regex regex)
  (validate-text-range regex text start end)
  (validate-timeout timeout)
  (validate-replacement regex replacement)
  (when (zerop (or limit 1))
    (return-from replace-up-to text))
  (if (byte-regex-p regex) (let ((output (make-octet-output-buffer)))
                             (accumulate-replacements
                              regex
                              text
                              replacement
                              limit
                              start
                              end
                              timeout
                              (lambda (octets)
                                (append-octets output octets)))
                             output)
    (with-output-to-string (output)
      (accumulate-replacements
       regex
       text
       replacement
       limit
       start
       end
       timeout
       (lambda (string)
         (write-string string output))))))

(defun replace-n (regex text replacement count &key (start 0) end timeout)
  "Replace at most COUNT non-overlapping matches of REGEX in TEXT. This is the CL-REGEX-KIT equivalent of Rust Regex::replacen."
  (replace-up-to regex text replacement count start end timeout))

(defun replace-all (regex text replacement &key (start 0) end timeout)
  "Replace every non-overlapping match of REGEX in TEXT with REPLACEMENT. REPLACEMENT is a dollar-style template string or a function of result and text."
  (replace-up-to regex text replacement nil start end timeout))
