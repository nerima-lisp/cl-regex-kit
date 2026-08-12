;;;; t/matchers.lisp
;;;;
;;;; Project-specific cl-weave matchers, so specs read as regex assertions
;;;; instead of the standard-library boolean matchers layered onto them.
(in-package #:cl-regex-kit/test)

(defmatcher :to-match-regex (actual expected)
  "ACTUAL, a string or octet vector, is matched by EXPECTED, a compiled
REGEX or BYTE-REGEX."
  (is-match-p (expected-one expected :to-match-regex) actual))

(defun octets (&rest values)
  "Construct an octet vector from VALUES."
  (make-array
   (length values)
   :element-type '(unsigned-byte 8)
   :initial-contents values))

(defun ascii-octets (string)
  "Encode STRING as an octet vector by CHAR-CODE for ASCII-oriented tests."
  (map
   '(vector (unsigned-byte 8))
   #'char-code
   string))

(defmacro it-match-cases (description pattern &key matches non-matches)
  "Register one matcher specification for a pattern and its examples.

The pattern is compiled once per specification, while MATCHES and NON-MATCHES
remain declarative data at the call site."
  (let ((compiled (gensym "COMPILED-REGEX")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         ,@(mapcar (lambda (text)
                     `(expect ,text :to-match-regex ,compiled))
                   matches)
         ,@(mapcar (lambda (text)
                     `(expect-not ,text :to-match-regex ,compiled))
                   non-matches)))))

(defmacro it-parses-cases (description patterns)
  "Register one parser specification for a declarative pattern table."
  `(it ,description
     (dolist (pattern ',patterns)
       (expect (cl-regex-kit::parse-regex pattern) :to-be-truthy))))

(defmacro it-advanced-pattern-cases (description patterns)
  "Register one advanced-pattern specification from a declarative pattern table."
  `(it ,description
     (dolist (pattern ',patterns)
       (expect-advanced-regex-config-cases (compile-regex pattern)))))

(defmacro expect-signals-cases (condition &body forms)
  "Assert that every form signals CONDITION."
  `(progn
     ,@(mapcar (lambda (form)
                 `(signals ,condition ,form))
               forms)))

(defmacro expect-signals-for-functions (condition functions &body forms)
  "Assert that each FORM signals CONDITION for every function in FUNCTIONS.

Each form may reference FN, which is bound to one function designator from
FUNCTIONS."
  (let ((functions-name (gensym "FUNCTIONS"))
        (fn (gensym "FUNCTION-DESIGNATOR")))
    `(let ((,functions-name ,functions))
       (dolist (,fn ,functions-name)
         ,@(mapcar (lambda (form)
                     `(signals ,condition
                        (let ((fn ,fn))
                          ,form)))
                   forms)))))

(defmacro expect-signals-for-values ((variable values) condition &body forms)
  "Assert that each FORM signals CONDITION for every value in VALUES.

Each form may reference VARIABLE, which is bound to one value from VALUES."
  (let ((values-name (gensym "VALUES"))
        (value-name (gensym "VALUE")))
    `(let ((,values-name ,values))
       (dolist (,value-name ,values-name)
         ,@(mapcar (lambda (form)
                     `(signals ,condition
                        (let ((,variable ,value-name))
                          ,form)))
                   forms)))))

(defmacro expect-macroexpand-signals-cases (condition &body forms)
  "Assert that MACROEXPAND-1 of every quoted form signals CONDITION."
  `(expect-signals-cases
    ,condition
    ,@(mapcar (lambda (form)
                `(macroexpand-1 ',form))
              forms)))

(defmacro expect-truthy-cases (&body forms)
  "Assert that every form evaluates to a truthy value."
  `(progn
     ,@(mapcar (lambda (form)
                 `(expect ,form :to-be-truthy))
               forms)))

(defmacro expect-falsy-cases (&body forms)
  "Assert that every form evaluates to NIL."
  `(progn
     ,@(mapcar (lambda (form)
                 `(expect ,form :to-be-falsy))
               forms)))

(defmacro expect-equal-cases (&body clauses)
  "Assert that each clause's form evaluates to the expected value."
  `(progn
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (form expected) clause
                   `(expect ,form :to-equal ,expected)))
               clauses)))

(defmacro expect-match-span-cases (&body clauses)
  "Assert that each clause's match result spans the expected bounds.

Each clause is (FORM EXPECTED-START EXPECTED-END)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form expected-start expected-end) clause
            `(expect (and ,form
                          (list (match-start ,form) (match-end ,form)))
                     :to-equal
                     (and ,expected-start
                          (list ,expected-start ,expected-end)))))
        clauses)))

(defmacro expect-match-string-cases (&body clauses)
  "Assert that each clause's match result returns the expected string.

Each clause is (FORM TEXT EXPECTED-STRING)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form text expected-string) clause
            `(expect (match-string ,form ,text)
                     :to-equal
                     ,expected-string)))
        clauses)))

(defmacro it-pattern-match-string-cases (description cases)
  "Register one matched-string specification from per-pattern declarative cases.

Each case is (PATTERN TEXT EXPECTED-STRING). The pattern is compiled per case,
which is useful when the data table mixes unrelated advanced constructs."
  (let ((pattern (gensym "MATCH-PATTERN"))
        (text (gensym "MATCH-TEXT"))
        (expected-string (gensym "EXPECTED-STRING"))
        (compiled (gensym "COMPILED-REGEX"))
        (result (gensym "MATCH-RESULT")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,text ,expected-string) case
           (let* ((,compiled (compile-regex ,pattern))
                  (,result (scan ,compiled ,text)))
             (expect (and ,result
                          (match-string ,result ,text))
                     :to-equal
                     ,expected-string)))))))

(defmacro it-byte-ascii-match-string-cases (description &body cases)
  "Register one byte matched-string specification from declarative clauses.

Each case is (PATTERN SUBJECT EXPECTED). SUBJECT and EXPECTED are ASCII strings
converted to octets before matching and comparison."
  (let ((pattern (gensym "BYTE-MATCH-PATTERN"))
        (subject (gensym "BYTE-MATCH-SUBJECT"))
        (expected (gensym "BYTE-MATCH-EXPECTED"))
        (text (gensym "BYTE-MATCH-TEXT"))
        (result (gensym "BYTE-MATCH-RESULT")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,subject ,expected) case
           (let* ((,text (ascii-octets ,subject))
                  (,result (scan (compile-byte-regex ,pattern) ,text)))
             (expect
              (and ,result
                   (equalp
                    (ascii-octets ,expected)
                    (match-string ,result ,text)))
              :to-be-truthy)))))))

(defmacro it-byte-match-string-cases (description &body cases)
  "Register one byte matched-string specification from evaluated clauses.

Each case is (PATTERN SUBJECT-FORM EXPECTED-FORM). SUBJECT-FORM and
EXPECTED-FORM are evaluated, then encoded with ASCII-OCTETS before matching
and comparison."
  `(it ,description
     ,@(mapcar
        (lambda (case)
          (destructuring-bind (pattern subject-form expected-form) case
            `(let* ((text (ascii-octets ,subject-form))
                    (result (scan (compile-byte-regex ,pattern) text)))
               (expect
                (and result
                     (equalp
                      (ascii-octets ,expected-form)
                      (match-string result text)))
                :to-be-truthy))))
        cases)))

(defmacro it-regex-set-match-cases
    (description compiler &body clauses)
  "Register one REGEX-SET match specification from declarative clauses.

Each clause is (PATTERNS SUBJECT EXPECTED). PATTERNS is passed to COMPILER,
SUBJECT is matched as-is, and the resulting member indices are compared to
EXPECTED."
  (let ((patterns (gensym "REGEX-SET-PATTERNS"))
        (subject (gensym "REGEX-SET-SUBJECT"))
        (expected (gensym "REGEX-SET-EXPECTED"))
        (compiled (gensym "COMPILED-REGEX-SET")))
    `(it ,description
       (dolist (case ',clauses)
         (destructuring-bind (,patterns ,subject ,expected) case
           (let ((,compiled (funcall ,compiler ,patterns)))
             (expect (regex-set-matches ,compiled ,subject)
                     :to-equal
                     ,expected)))))))

(defmacro it-split-cases (description pattern &body cases)
  "Register one split API specification from declarative operation cases.

Each case is (FUNCTION INPUT EXPECTED &REST ARGUMENTS). The regex is compiled
once per specification and applied across split entry points."
  (let ((compiled (gensym "COMPILED-SPLIT-REGEX"))
        (function-name (gensym "SPLIT-FUNCTION"))
        (input (gensym "SPLIT-INPUT"))
        (expected (gensym "SPLIT-EXPECTED"))
        (arguments (gensym "SPLIT-ARGUMENTS")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         (dolist (case ',cases)
           (destructuring-bind
               (,function-name ,input ,expected &rest ,arguments)
               case
             (expect (apply (symbol-function ,function-name)
                            ,compiled
                            ,input
                            ,arguments)
                     :to-equal
                     ,expected)))))))

(defmacro it-grep-count-cases (description &body cases)
  "Register one grep count CLI specification from declarative cases.

Each case is (MODE CONTENTS ARGUMENTS INPUT EXPECTED-EXIT-CODE COUNTS)."
  (let ((mode (gensym "GREP-MODE"))
        (contents (gensym "GREP-CONTENTS"))
        (arguments (gensym "GREP-ARGUMENTS"))
        (input (gensym "GREP-INPUT"))
        (expected-exit-code (gensym "GREP-EXPECTED-EXIT-CODE"))
        (counts (gensym "GREP-COUNTS")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind
             (,mode ,contents ,arguments ,input ,expected-exit-code ,counts)
             case
           (if ,contents
               (call-with-temporary-grep-files
                   ,contents
                 (lambda (paths)
                   (let* ((files (mapcar (function namestring) paths))
                          (argv (if (eq ,mode 'labelled)
                                    (append ,arguments files)
                                    (append ,arguments (list (first files))))))
                     (multiple-value-bind (exit-code stdout stderr)
                         (run-grep-cli argv)
                       (expect exit-code :to-equal ,expected-exit-code)
                       (expect stdout
                               :to-equal
                               (render-grep-count-output ,mode files ,counts))
                       (expect stderr :to-equal "")))))
               (multiple-value-bind (exit-code stdout stderr)
                   (run-grep-cli ,arguments ,input)
                 (expect exit-code :to-equal ,expected-exit-code)
                 (expect stdout
                         :to-equal
                         (render-grep-count-output ,mode nil ,counts))
                 (expect stderr :to-equal ""))))))))

(defmacro it-grep-error-cases (description &body cases)
  "Register one grep error CLI specification from declarative cases.

Each case is (KIND ARGUMENTS EXPECTED-EXIT-CODE EXPECTED-STDOUT STDERR-FRAGMENT)."
  (let ((kind (gensym "GREP-ERROR-KIND"))
        (arguments (gensym "GREP-ERROR-ARGUMENTS"))
        (expected-exit-code (gensym "GREP-ERROR-EXIT-CODE"))
        (expected-stdout (gensym "GREP-ERROR-STDOUT"))
        (stderr-fragment (gensym "GREP-ERROR-STDERR-FRAGMENT")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind
             (,kind ,arguments ,expected-exit-code ,expected-stdout ,stderr-fragment)
             case
           (ecase ,kind
             (invalid-regex
              (multiple-value-bind (exit-code stdout stderr)
                  (run-grep-cli ,arguments)
                (expect exit-code :to-equal ,expected-exit-code)
                (expect stdout :to-equal ,expected-stdout)
                (expect (search ,stderr-fragment stderr) :to-be-truthy)))
             (standard-input
              (let ((input (make-string-input-stream "match\n")))
                (close input)
                (multiple-value-bind (exit-code stdout stderr)
                    (run-grep-cli ,arguments input)
                  (expect exit-code :to-equal ,expected-exit-code)
                  (expect stdout :to-equal ,expected-stdout)
                  (expect (search ,stderr-fragment stderr) :to-be-truthy))))))))))

(defmacro it-replace-cases (description compiler pattern &body clauses)
  "Register one string replacement specification from declarative clauses.

Each clause is (FUNCTION INPUT REPLACEMENT EXPECTED &REST ARGUMENTS). The regex
is compiled once per specification and shared across replacement entry points."
  `(it-replacement-cases
    ,description
    ,compiler
    ,pattern
    expect-equal-cases
    ,@clauses))

(defmacro it-replacement-cases
    (description compiler pattern expectation-macro &body clauses)
  "Register one replacement specification from declarative clauses.

EXPECTATION-MACRO decides how each result is compared, so string and octet
entry points can share the same expansion shape."
  (let ((compiled (gensym "COMPILED-REPLACE-REGEX")))
    `(it ,description
       (let ((,compiled (funcall ,compiler ,pattern)))
         (,expectation-macro
         ,@(mapcar
             (lambda (clause)
               (destructuring-bind
                   (function-name input replacement expected &rest arguments)
                   clause
                 `(,(if (symbolp function-name)
                        `(,function-name
                          ,compiled
                          ,input
                          ,replacement
                          ,@arguments)
                        `(funcall
                          ,function-name
                          ,compiled
                          ,input
                          ,replacement
                          ,@arguments))
                     ,expected)))
             clauses))))))

(defmacro it-byte-replace-cases (description compiler pattern &body clauses)
  "Register one octet replacement specification from declarative clauses.

Each clause is (FUNCTION INPUT REPLACEMENT EXPECTED &REST ARGUMENTS). The regex
is compiled once per specification and each octet result is coerced to a list
for stable comparison."
  `(it-replacement-cases
    ,description
    ,compiler
    ,pattern
    expect-octets-cases
    ,@clauses))

(defmacro expect-match-captures-cases (&body clauses)
  "Assert that each clause's match result returns the expected captures.

Each clause is (FORM TEXT EXPECTED-CAPTURES)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form text expected-captures) clause
            `(expect (and ,form
                          (coerce (match-captures ,form ,text) 'list))
                     :to-equal
                     ,expected-captures)))
        clauses)))

(defmacro expect-match-spans-cases (&body clauses)
  "Assert that each clause's match sequence yields the expected spans.

Each clause is (FORM EXPECTED-SPANS). FORM must evaluate to a sequence of match
objects."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form expected-spans) clause
            `(expect (mapcar (lambda (result)
                               (list (match-start result) (match-end result)))
                             ,form)
                     :to-equal
                     ,expected-spans)))
        clauses)))

(defmacro expect-match-metadata-cases (&body clauses)
  "Assert that each match result exposes the expected span and distance.

Each clause is (FORM EXPECTED-START EXPECTED-END EXPECTED-DISTANCE)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind
              (form expected-start expected-end expected-distance)
              clause
            `(expect (and ,form
                          (list (match-start ,form)
                                (match-end ,form)
                                (match-edit-distance ,form)))
                     :to-equal
                     (and ,expected-start
                          (list ,expected-start
                                ,expected-end
                                ,expected-distance)))))
        clauses)))

(defmacro expect-regex-set-cases (&body clauses)
  "Assert that each regex-set clause returns the expected indices and predicate.

Each clause is (SET TEXT START EXPECTED-INDICES)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (set text start expected-indices) clause
            `(progn
               (expect (regex-set-matches ,set ,text :start ,start)
                       :to-equal
                       ,expected-indices)
               (expect (regex-set-match-p ,set ,text :start ,start)
                       :to-equal
                       (not (null ,expected-indices))))))
        clauses)))

(defmacro expect-regex-set-equivalent-cases (&body clauses)
  "Assert that merged regex-set execution matches per-pattern scans.

Each clause is (PATTERNS TEXT START)."
  (let ((compiled-name (gensym "COMPILED-REGEX-SET"))
        (expected-name (gensym "EXPECTED-INDICES")))
    `(progn
       ,@(mapcar
          (lambda (clause)
            (destructuring-bind (patterns text start) clause
              `(let* ((,compiled-name (compile-regex-set ,patterns))
                      (,expected-name
                      (loop for pattern in ,patterns
                            for index from 0
                            when (scan (compile-regex pattern) ,text :start ,start)
                              collect index)))
                 (expect (regex-set-matches ,compiled-name ,text :start ,start)
                         :to-equal
                         ,expected-name)
                 (expect (regex-set-match-p ,compiled-name ,text :start ,start)
                         :to-equal
                         (not (null ,expected-name))))))
          clauses))))

(defmacro expect-regex-set-search-cases (&body clauses)
  "Assert that each regex-set search clause returns the expected first match.

Each clause is
(FORM TEXT EXPECTED-INDEX EXPECTED-START EXPECTED-MATCH-STRING)."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind
              (form text expected-index expected-start expected-match-string)
              clause
            `(multiple-value-bind (index result)
                 ,form
               (expect index :to-be ,expected-index)
               (expect (and result (match-start result))
                       :to-be
                       ,expected-start)
               (expect (and result (match-string result ,text))
                       :to-equal
                       ,expected-match-string))))
        clauses)))

(defmacro expect-octets-cases (&body clauses)
  "Assert that each octet vector form coerces to the expected list."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form expected) clause
            `(expect (coerce ,form 'list) :to-equal ,expected)))
        clauses)))

(defmacro expect-list-of-octets-cases (&body clauses)
  "Assert that each list of octet vectors coerces to the expected lists."
  `(progn
     ,@(mapcar
        (lambda (clause)
          (destructuring-bind (form expected) clause
            `(expect (mapcar (lambda (field)
                               (coerce field 'list))
                             ,form)
                     :to-equal
                     ,expected)))
        clauses)))

(defmacro expect-benchmark-json-report (output &key revision)
  "Assert that OUTPUT is a benchmark JSON report.

REVISION, when provided, is checked against the metadata revision field."
  (let ((rendered-output (gensym "BENCHMARK-OUTPUT"))
        (json (gensym "BENCHMARK-JSON"))
        (top-level (gensym "BENCHMARK-TOP-LEVEL"))
        (metadata (gensym "BENCHMARK-METADATA"))
        (correctness (gensym "BENCHMARK-CORRECTNESS")))
    `(let* ((,rendered-output ,output)
            (,json
             (json-kit:read-json (make-string-input-stream ,rendered-output)))
            (,top-level
             (json-kit:json-object->alist ,json))
            (,metadata
             (json-kit:json-object->alist
              (cdr (assoc "metadata" ,top-level :test #'string=))))
            (,correctness
             (cdr (assoc "correctness" ,top-level :test #'string=))))
       (expect ,rendered-output :to-contain "{")
       (expect (assoc "metadata" ,top-level :test #'string=) :to-be-truthy)
       (expect (assoc "results" ,top-level :test #'string=) :to-be-truthy)
       (expect (assoc "correctness" ,top-level :test #'string=) :to-be-truthy)
       ,@(when revision
           `((expect (cdr (assoc "revision" ,metadata :test #'string=))
                     :to-equal
                     ,revision)))
       (expect ,correctness :to-equal "verified"))))

;; Advanced and Unicode-specific test helpers live in
;; matchers-advanced.lisp, unicode-matchers.lisp, and
;; unicode-property-matchers.lisp.
