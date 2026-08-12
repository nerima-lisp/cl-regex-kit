(in-package #:cl-regex-kit)

(defun scan (regex text &key (start 0) end timeout)
  "Find the leftmost-first match of REGEX in TEXT within [START, END).
Returns a MATCH-RESULT, or NIL if there is no match. TIMEOUT is a positive
number of seconds, or NIL to impose no deadline."
  (with-pike-vm-match (result regex text start end timeout)
    (when result
      (setf (match-result-group-names result) (regex-group-names regex)))
    result))

(define-forwarding-wrapper scan-at
    (regex text start &key end timeout)
  "Find REGEX's leftmost-first match in TEXT at or after START.

This is the Rust Regex::find_at equivalent. END, when supplied, remains the
exclusive upper bound of the searched range."
  scan
  (regex text :start start :end end :timeout timeout))

(define-forwarding-wrapper captures
    (regex text &key (start 0) end timeout)
  "Return REGEX's leftmost-first capture result in TEXT, or NIL.

This is the Rust Regex::captures equivalent. The returned MATCH-RESULT is
read with MATCH-CAPTURES and MATCH-GROUP-* functions."
  scan
  (regex text :start start :end end :timeout timeout))

(define-forwarding-wrapper captures-at
    (regex text start &key end timeout)
  "Return REGEX's leftmost-first capture result in TEXT at or after START.

This is the Rust Regex::captures_at equivalent. END, when supplied, remains
the exclusive upper bound of the searched range."
  scan
  (regex text :start start :end end :timeout timeout))

(defun scan-captures-into (regex locations text &key (start 0) end timeout)
  "Scan TEXT and write REGEX's capture offsets into LOCATIONS.

LOCATIONS must have been created by REGEX-CAPTURE-LOCATIONS for a regex with
the same capture count. Return the whole match's start and exclusive end as
two values, or NIL and NIL when no match exists. Nonparticipating captures
and every location after an unsuccessful scan contain NIL."
  (check-type regex regex)
  (check-type locations capture-locations)
  (unless (= (regex-capture-count regex) (capture-locations-count locations))
    (error 'type-error
           :datum locations
           :expected-type `(capture-locations-for ,(regex-capture-count regex) captures)))
  (clear-capture-locations locations)
  (let ((result (scan regex text :start start :end end :timeout timeout)))
    (when result
      (copy-match-result-to-capture-locations result locations)
      (return-from scan-captures-into
        (values (match-result-start result) (match-result-end result))))
    (values nil nil)))

(define-forwarding-wrapper scan-captures-into-at
    (regex locations text start &key end timeout)
  "Scan TEXT at or after START and write REGEX's capture offsets into LOCATIONS.

This is the Rust Regex::captures_read_at equivalent. END, when supplied,
remains the exclusive upper bound of the searched range."
  scan-captures-into
  (regex locations text :start start :end end :timeout timeout))

(defun shortest-match (regex text &key (start 0) end timeout)
  "Return the end index of REGEX's shortest leftmost match in TEXT.

The selected match begins at the earliest position at or after START and has
the earliest possible end position there. Return NIL when REGEX does not
match. TIMEOUT is a positive number of seconds, or NIL to impose no deadline."
  (with-pike-vm-match (result regex text start end timeout :shortest-p t)
    (and result (match-result-end result))))

(define-forwarding-wrapper shortest-match-at
    (regex text start &key end timeout)
  "Return the end index of REGEX's shortest leftmost match at or after START.

This is the Rust Regex::shortest_match_at equivalent. END, when supplied,
remains the exclusive upper bound of the searched range."
  shortest-match
  (regex text :start start :end end :timeout timeout))

(defun longest-match (regex text &key (start 0) end timeout)
  "Find REGEX's leftmost-longest match in TEXT at or after START.

This provides RE2's longest-match selection without changing SCAN's Rust-style
leftmost-first semantics. Return a MATCH-RESULT, or NIL if no match exists."
  (with-pike-vm-match (result regex text start end timeout :longest-p t)
    (when result
      (setf (match-result-group-names result) (regex-group-names regex)))
    result))

(defun is-match-p (regex text &key (start 0) end timeout)
  "Return true when REGEX matches TEXT within [START, END)."
  (with-pike-vm-match (result regex text start end timeout :boolean-p t)
    (not (null result))))

(define-forwarding-wrapper is-match-at
    (regex text start &key end timeout)
  "Return true when REGEX matches TEXT at or after START.

This is the Rust Regex::is_match_at equivalent. END, when supplied, remains
the exclusive upper bound of the searched range."
  is-match-p
  (regex text :start start :end end :timeout timeout))

(define-forwarding-wrapper match
    (pattern text &key (start 0) end timeout)
  "Compile PATTERN and SCAN TEXT once. Prefer COMPILE-REGEX + SCAN when
matching the same pattern repeatedly."
  scan
  ((compile-regex pattern) text :start start :end end :timeout timeout))

(define-forwarding-wrapper byte-match
    (pattern text &key (start 0) end timeout)
  "Compile byte PATTERN and scan octet-vector TEXT once."
  scan
  ((compile-byte-regex pattern) text :start start :end end :timeout timeout))

(defun match-string (match-result text)
  "The substring of TEXT the whole match covers."
  (check-type match-result match-result)
  (check-type text (or string octet-vector))
  (subseq text (match-result-start match-result) (match-result-end match-result)))

(defun match-captures (match-result text)
  "Return a fresh vector of strings captured by MATCH-RESULT in TEXT.

Index zero is the whole match.  Each subsequent index corresponds to its
numeric capture group; a nonparticipating optional group is represented by
NIL.  The returned vector never exposes the VM's capture slots."
  (check-type match-result match-result)
  (check-type text (or string octet-vector))
  (map 'vector
       (lambda (range)
         (when range
           (subseq text (car range) (cdr range))))
       (match-result-groups match-result)))

(defun match-start (match-result)
  "The start offset of the whole match."
  (check-type match-result match-result)
  (match-result-start match-result))

(defun match-end (match-result)
  "The end offset of the whole match."
  (check-type match-result match-result)
  (match-result-end match-result))

(defun match-mark (match-result)
  "The last MARK control-verb tag reached by the match, or NIL."
  (check-type match-result match-result)
  (match-result-mark match-result))

(defun match-edit-distance (match-result)
  "The number of insertions, deletions, and substitutions in MATCH-RESULT.

Ordinary exact matching returns zero. Fuzzy matching returns the minimum
bounded distance selected for the match."
  (check-type match-result match-result)
  (match-result-edit-distance match-result))

(defun resolve-group-index (match-result index)
  (check-type match-result match-result)
  (let ((resolved
          (if (stringp index)
              (let ((indexes
                      (mapcar #'cdr
                              (remove-if-not
                               (lambda (entry)
                                 (string= index (car entry)))
                               (match-result-group-names match-result)))))
                (or (find-if
                     (lambda (candidate)
                       (let ((offsets
                               (aref (match-result-groups match-result)
                                     candidate)))
                         (and (car offsets) (cdr offsets))))
                     indexes)
                    (first indexes)
                    (error (quote type-error)
                           :datum index
                           :expected-type
                           (quote (or integer known-capture-name)))))
              index)))
    (unless (and (integerp resolved)
                 (<= 0 resolved)
                 (< resolved (length (match-result-groups match-result))))
      (error (quote type-error)
             :datum index
             :expected-type
             (list (quote integer) 0
                   (1- (length (match-result-groups match-result))))))
    resolved))

(defun match-group-start (match-result index)
  "The start offset of capture group INDEX, or NIL if it did not participate."
  (car (aref (match-result-groups match-result)
             (resolve-group-index match-result index))))

(defun match-group-end (match-result index)
  "The end offset of capture group INDEX, or NIL if it did not participate."
  (cdr (aref (match-result-groups match-result)
             (resolve-group-index match-result index))))

(defun match-group-string (match-result index text)
  "The substring TEXT captured by group INDEX, or NIL if it did not participate."
  (check-type text (or string octet-vector))
  (let ((start (match-group-start match-result index))
        (end (match-group-end match-result index)))
    (when (and start end)
      (subseq text start end))))

(defun full-match (regex text &key (start 0) end timeout)
  "Return a match result spanning exactly [START, END), or NIL.

The result is chosen with leftmost-longest semantics at START so a complete
match remains visible when an earlier alternative is shorter. Anchors continue
to refer to the original TEXT, not the selected range."
  (check-type regex regex)
  (let ((limit (validate-text-range regex text start end)))
    (let ((result (longest-match regex text
                                 :start start :end limit :timeout timeout)))
      (and result
           (= (match-result-start result) start)
           (= (match-result-end result) limit)
           result))))

(defun full-match-p (regex text &key (start 0) end timeout)
  "Return true when REGEX has a match spanning exactly [START, END) in TEXT."
  (not (null (full-match regex text :start start :end end :timeout timeout))))
