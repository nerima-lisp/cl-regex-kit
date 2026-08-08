(in-package #:cl-regex-kit)

(defun instruction-matches-p (instruction element)
  "Return true when consuming INSTRUCTION accepts CHARACTER or an octet."
  (case (inst-op instruction)
    (:char
      (if (characterp element) (if (literal-node-case-insensitive-p (inst-a instruction)) (funcall
            (if (literal-node-unicode-p (inst-a instruction)) #'unicode-case-insensitive-char=
              #'ascii-case-insensitive-char=)
            element
            (literal-node-char (inst-a instruction)))
          (char= element (literal-node-char (inst-a instruction))))
        (let ((literal (char-code (literal-node-char (inst-a instruction)))))
          (if (literal-node-case-insensitive-p (inst-a instruction)) (= (ascii-fold-octet element) (ascii-fold-octet literal))
            (= element literal)))))
    (:class
      (if (characterp element) (class-matches-p (inst-a instruction) element)
        (class-matches-octet-p (inst-a instruction) element)))
    (:any
      (let ((node (inst-a instruction)))
        (or
          (any-char-node-dotall-p node)
          (if (characterp element) (if (any-char-node-crlf-p node) (not (or (char= element #\Return) (char= element #\Newline)))
              (not (char= element (any-char-node-line-terminator node))))
            (if (any-char-node-crlf-p node) (and (/= element #x0d) (/= element #x0a))
              (/= element (char-code (any-char-node-line-terminator node))))))))))

(defun instruction-unicode-p (instruction)
  "Return whether a consuming INSTRUCTION uses Unicode scalar semantics."
  (case (inst-op instruction)
    (:char (literal-node-unicode-p (inst-a instruction)))
    (:class (char-class-node-unicode-p (inst-a instruction)))
    (:any (any-char-node-unicode-p (inst-a instruction)))
    (:line-break (line-break-node-unicode-p (inst-a instruction)))))

(defun newline-element-p (element)
  "Return true when ELEMENT is a character or octet newline."
  (if (characterp element) (char= element #\Newline)
    (= element #x0a)))

(defun instruction-match-end (instruction text position limit byte-mode-p never-newline-p)
  "Return the exclusive end index and success flag for a consuming instruction."
  (cond
    ((>= position limit) (values nil nil))
    ((eq (inst-op instruction) :line-break)
     (let ((unicode-p (instruction-unicode-p instruction)))
       (labels
           ((element-code (element)
              (if (characterp element) (char-code element) element))
            (line-break-code-p (code)
              (or
               (= code #x0a)
               (= code #x0b)
               (= code #x0c)
               (= code #x0d)
               (= code #x85)
               (and unicode-p
                    (or (= code #x2028) (= code #x2029)))))
            (match-one (at)
              (if (and byte-mode-p unicode-p)
                  (multiple-value-bind (character end valid-p)
                      (utf8-character-at text at)
                    (if (and valid-p (<= end limit))
                        (values (char-code character) end t)
                      (values nil nil nil)))
                (let ((element (aref text at)))
                  (values (element-code element) (1+ at) t)))))
         (multiple-value-bind (code end valid-p) (match-one position)
           (if (and
                valid-p
                (line-break-code-p code)
                (not (and never-newline-p (= code #x0a))))
               (if (and (= code #x0d) (< end limit))
                   (multiple-value-bind (next-code next-end next-valid-p)
                       (match-one end)
                     (if (and next-valid-p (= next-code #x0a))
                         (if never-newline-p
                             (values nil nil)
                           (values next-end t))
                       (values end t)))
                 (values end t))
             (values nil nil))))))
    ((and byte-mode-p (instruction-unicode-p instruction))
     (multiple-value-bind (character end valid-p) (utf8-character-at text position)
       (if (and
            valid-p
            (<= end limit)
            (not (and never-newline-p (newline-element-p character)))
            (instruction-matches-p instruction character)) (values end t)
         (values nil nil))))
    (t
     (let ((element (aref text position)))
       (if (and
            (not (and never-newline-p (newline-element-p element)))
            (instruction-matches-p instruction element)) (values (1+ position) t)
         (values nil nil))))))

(defun vm-word-position-p (kind text position byte-mode-p unicode-p)
  "Evaluate a word-boundary KIND at POSITION for string or byte input."
  (if byte-mode-p (if unicode-p (ecase kind
        (:boundary (byte-unicode-word-boundary-p text position))
        (:start (byte-unicode-word-start-p text position))
        (:end (byte-unicode-word-end-p text position))
        (:start-half (byte-unicode-word-start-half-p text position))
        (:end-half (byte-unicode-word-end-half-p text position)))
      (ecase kind
        (:boundary (byte-word-boundary-p text position))
        (:start (byte-word-start-p text position))
        (:end (byte-word-end-p text position))
        (:start-half (byte-word-start-half-p text position))
        (:end-half (byte-word-end-half-p text position))))
    (ecase kind
      (:boundary (word-boundary-p text position unicode-p))
      (:start (word-start-p text position unicode-p))
      (:end (word-end-p text position unicode-p))
      (:start-half (word-start-half-p text position unicode-p))
      (:end-half (word-end-half-p text position unicode-p)))))

(defun vm-non-boundary-position-p (text position byte-mode-p unicode-p)
  "Return true when a non-boundary assertion may evaluate at POSITION."
  (or
    (not byte-mode-p)
    (not unicode-p)
    (byte-unicode-non-boundary-position-p text position)))

(defun zero-width-instruction-matches-p (instruction text position length byte-mode-p)
  "Return whether zero-width INSTRUCTION succeeds at POSITION."
  (case (inst-op instruction)
    (:bol
      (or
        (zerop position)
        (and
          (logbitp 0 (or (inst-b instruction) 0))
          (funcall
            (if byte-mode-p #'byte-line-start-p
              #'line-start-p)
            text
            position
            (logbitp 2 (or (inst-b instruction) 0))
            (or (inst-c instruction) #\Newline)))))
    (:eol
      (or
        (= position length)
        (and
          (logbitp 0 (or (inst-b instruction) 0))
          (funcall
            (if byte-mode-p #'byte-line-end-p
              #'line-end-p)
            text
            position
            (logbitp 2 (or (inst-b instruction) 0))
            (or (inst-c instruction) #\Newline)))))
    (:bos (zerop position))
    (:eos (= position length))
    (:boundary
      (vm-word-position-p
        :boundary
        text
        position
        byte-mode-p
        (logbitp 1 (or (inst-b instruction) 0))))
    (:non-boundary
      (and
        (vm-non-boundary-position-p
          text
          position
          byte-mode-p
          (logbitp 1 (or (inst-b instruction) 0)))
        (not
          (vm-word-position-p
            :boundary
            text
            position
            byte-mode-p
            (logbitp 1 (or (inst-b instruction) 0))))))
    (:word-start
      (vm-word-position-p
        :start
        text
        position
        byte-mode-p
        (logbitp 1 (or (inst-b instruction) 0))))
    (:word-end
      (vm-word-position-p
        :end
        text
        position
        byte-mode-p
        (logbitp 1 (or (inst-b instruction) 0))))
    (:word-start-half
      (vm-word-position-p
        :start-half
        text
        position
        byte-mode-p
        (logbitp 1 (or (inst-b instruction) 0))))
    (:word-end-half
      (vm-word-position-p
        :end-half
        text
        position
        byte-mode-p
        (logbitp 1 (or (inst-b instruction) 0))))))
