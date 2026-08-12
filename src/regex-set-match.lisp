;;;; src/regex-set-match.lisp
;;;;
;;;; Multi-pattern matching built directly from compiled REGEX instances.
(in-package #:cl-regex-kit)

(defun validate-regex-set-text-range (regex-set text start end)
  "Validate TEXT's half-open range against REGEX-SET's input representation."
  (if (byte-regex-set-p regex-set)
      (progn
        (check-type text octet-vector)
        (validate-input-range text start end))
    (validate-string-range text start end)))

(defun call-with-validated-regex-set-match (regex-set
                                            text
                                            start
                                            end
                                            timeout
                                            thunk)
  "Validate REGEX-SET/TEXT/START/END, then invoke THUNK with the validated
LIMIT under TIMEOUT, continuation-passing style: THUNK performs the actual
VM run and returns its result, which this function returns unchanged. Shared
by every REGEX-SET matching entry point the same way CALL-WITH-VALIDATED-MATCH
is shared by the single-pattern ones in api-match.lisp."
  (check-type regex-set regex-set)
  (let ((limit (validate-regex-set-text-range regex-set text start end)))
    (call-with-timeout
     timeout
     (lambda ()
       (funcall thunk limit)))))

(defmacro with-validated-regex-set-match ((limit regex-set text start end timeout)
                                          &body body)
  "Evaluate BODY with LIMIT bound after validating REGEX-SET input."
  `(call-with-validated-regex-set-match
    ,regex-set
    ,text
    ,start
    ,end
    ,timeout
    (lambda (,limit)
      ,@body)))

(defun regex-set-ordinary-match-buffer (regex-set matches)
  "Return the destination bit-vector for ordinary-member matches."
  (if (regex-set-direct-ordinary-indices-p regex-set)
      matches
    (make-array (length (regex-set-ordinary-indices regex-set))
                :element-type 'bit
                :initial-element 0)))

(defun project-regex-set-ordinary-matches (regex-set ordinary-matches matches)
  "Project merged-program ORDINARY-MATCHES into source-order MATCHES."
  (unless (regex-set-direct-ordinary-indices-p regex-set)
    (loop for merged-index below (length ordinary-matches)
          when (= 1 (aref ordinary-matches merged-index))
            do (setf (aref matches
                           (aref (regex-set-ordinary-indices regex-set)
                                 merged-index))
                     1)))
  matches)

(defun run-regex-set-ordinary-matches (regex-set ordinary-matches text start limit)
  "Run the merged NFA members of REGEX-SET into ORDINARY-MATCHES."
  (let ((ordinary-count (length (regex-set-ordinary-indices regex-set))))
    (when (plusp ordinary-count)
      (run-pike-vm-set
       (regex-set-program regex-set)
       ordinary-count
       text
       :start
       start
       :end
       limit
       :matches
       ordinary-matches
       :never-newline-p
       (regex-set-never-newline-p regex-set))))
  ordinary-matches)

(defun regex-set-advanced-match-p (regex-set text start limit)
  "Return true when any advanced member of REGEX-SET matches."
  (loop for index across (regex-set-advanced-indices regex-set)
        thereis
        (scan
         (aref (regex-set-members regex-set) index)
         text
         :start start
         :end limit)))

(defun run-regex-set-advanced-matches (regex-set matches text start limit)
  "Mark advanced-member matches into MATCHES."
  (loop for index across (regex-set-advanced-indices regex-set)
        when (scan
               (aref (regex-set-members regex-set) index)
               text
               :start start
               :end limit)
          do (setf (aref matches index) 1))
  matches)

(defun find-regex-set-search-result (regex-set text start limit)
  "Return the earliest source-order search result within [START, LIMIT)."
  (let ((best-index nil)
        (best-result nil))
    (loop for index below (regex-set-count regex-set)
          for member = (aref (regex-set-members regex-set) index)
          for candidate = (scan member text :start start :end limit)
          when candidate
            do (when (prefer-regex-set-search-candidate-p
                      candidate
                      index
                      best-result
                      best-index)
                 (setf best-index index
                       best-result candidate)))
    (values best-index best-result)))

(defun regex-set-ordinary-match-p (regex-set text start limit)
  "Return true when any merged ordinary member matches within [START, LIMIT)."
  (let ((ordinary-count (length (regex-set-ordinary-indices regex-set))))
    (and (plusp ordinary-count)
         (run-pike-vm-set
          (regex-set-program regex-set)
          ordinary-count
          text
          :start
          start
          :end
          limit
          :stop-at-first-match-p
          t
          :never-newline-p
          (regex-set-never-newline-p regex-set)))))

(defun prefer-regex-set-search-candidate-p (candidate index best-result best-index)
  "Return true when CANDIDATE should replace the current best search result."
  (or (null best-result)
      (< (match-start candidate) (match-start best-result))
      (and (= (match-start candidate)
              (match-start best-result))
           (< index best-index))))

(defun regex-set-matches-into (regex-set
                               matches
                               text
                               &key
                               (start 0)
                               end
                               timeout)
  "Record REGEX-SET matches in MATCHES and return MATCHES.

MATCHES must be a bit vector whose length exactly matches REGEX-SET pattern
count. It is cleared before matching, then each matching source-order pattern
index is marked with a one."
  (check-type regex-set regex-set)
  (unless (and
           (typep matches 'bit-vector)
           (= (length matches) (regex-set-count regex-set)))
    (error
     'type-error
     :datum
     matches
     :expected-type
     (list 'bit-vector (regex-set-count regex-set))))
  (with-validated-regex-set-match (limit regex-set text start end timeout)
    (fill matches 0)
    (let ((ordinary-matches
            (regex-set-ordinary-match-buffer regex-set matches)))
      (run-regex-set-ordinary-matches
       regex-set
       ordinary-matches
       text
       start
       limit)
      (project-regex-set-ordinary-matches
       regex-set
       ordinary-matches
       matches)
      (run-regex-set-advanced-matches regex-set matches text start limit)
      matches)))

(defun regex-set-matches (regex-set text &key (start 0) end timeout)
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

Indexes are returned in source order, including duplicate source patterns."
  (check-type regex-set regex-set)
  (let ((matches
         (make-array
          (regex-set-count regex-set)
          :element-type
          'bit
          :initial-element
          0)))
    (regex-set-matches-into
     regex-set
     matches
     text
     :start
     start
     :end
     end
     :timeout
     timeout)
    (loop for index below (length matches)
          when (= 1 (aref matches index))
            collect index)))

(defmacro define-regex-set-start-forwarder (name target docstring)
  "Define a REGEX-SET API wrapper that forwards START to TARGET as :START."
  `(defun ,name (regex-set text start &key end timeout)
     ,docstring
     (,target regex-set text :start start :end end :timeout timeout)))

(define-regex-set-start-forwarder
  regex-set-matches-at
  regex-set-matches
  "Return indexes of patterns in REGEX-SET that match TEXT at or after START.

This is the Rust RegexSet::matches_at equivalent. END, when supplied, remains
the exclusive upper bound of the searched range.")

(defun regex-set-search (regex-set text &key (start 0) end timeout)
  "Find the earliest REGEX-SET member match in TEXT.

Returns two values: the source-pattern index and that member's MATCH-RESULT.
When several members begin at the same position, the lowest source-pattern
index wins. Returns NIL, NIL when no member matches."
  (check-type regex-set regex-set)
  (with-validated-regex-set-match (limit regex-set text start end timeout)
    (find-regex-set-search-result regex-set text start limit)))

(define-regex-set-start-forwarder
  regex-set-search-at
  regex-set-search
  "Find the earliest REGEX-SET member match in TEXT at or after START.

Returns the same two values as REGEX-SET-SEARCH.")

(defun regex-set-match-p (regex-set text &key (start 0) end timeout)
  "Return true when any pattern in REGEX-SET matches within [START, END)."
  (with-validated-regex-set-match (limit regex-set text start end timeout)
    (or (regex-set-ordinary-match-p regex-set text start limit)
        (regex-set-advanced-match-p regex-set text start limit))))

(define-regex-set-start-forwarder
  regex-set-match-at-p
  regex-set-match-p
  "Return true when any pattern in REGEX-SET matches TEXT at or after START.

This is the Rust RegexSet::is_match_at equivalent. END, when supplied,
remains the exclusive upper bound of the searched range.")
