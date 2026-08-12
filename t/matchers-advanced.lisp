;;;; t/matchers-advanced.lisp
;;;;
;;;; Advanced-engine and higher-level cl-weave matchers split out of
;;;; matchers.lisp so the generic assertions stay small and readable.
(in-package #:cl-regex-kit/test)

(defmacro expect-advanced-regex-p-cases (&body forms)
  "Assert that every form yields an advanced regex."
  `(progn
     ,@(mapcar (lambda (form)
                 `(expect (regex-advanced-p ,form) :to-be-truthy))
               forms)))

(defmacro expect-advanced-regex-config-cases (&body forms)
  "Assert that every form yields an advanced regex with runner limits."
  `(progn
     ,@(mapcar
        (lambda (form)
          `(progn
             (expect (regex-advanced-p ,form) :to-be-truthy)
             (expect (regex-advanced-step-limit ,form) :to-be-truthy)
             (expect (regex-advanced-nest-limit ,form) :to-be-truthy)
             (expect (regex-never-newline-p ,form) :to-be nil)))
        forms)))

(defmacro it-runner-availability-cases (description cases)
  "Register one advanced-runner availability specification from declarative cases.

Each case is (FUNCTION-NAME PATTERN MATCHING-TEXT). The function must be fbound,
and the compiled pattern must route through the advanced engine and match the
given text."
  (let ((function-name (gensym "RUNNER-FUNCTION"))
        (pattern (gensym "RUNNER-PATTERN"))
        (matching-text (gensym "RUNNER-MATCHING-TEXT"))
        (compiled (gensym "COMPILED-ADVANCED-REGEX")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,function-name ,pattern ,matching-text) case
           (expect (fboundp ,function-name) :to-be-truthy)
           (let ((,compiled (compile-regex ,pattern)))
             (expect (regex-advanced-p ,compiled) :to-be-truthy)
             (expect ,matching-text :to-match-regex ,compiled)))))))

(defmacro it-pattern-scan-cases (description cases)
  "Register one scan specification from declarative per-pattern cases.

Each case is (PATTERN TEXT EXPECTED-START EXPECTED-END). The pattern is
compiled per case, which is useful when the data table mixes unrelated advanced
constructs."
  (let ((pattern (gensym "SCAN-PATTERN"))
        (text (gensym "SCAN-TEXT"))
        (expected-start (gensym "EXPECTED-START"))
        (expected-end (gensym "EXPECTED-END"))
        (compiled (gensym "COMPILED-REGEX"))
        (result (gensym "SCAN-RESULT")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,text ,expected-start ,expected-end) case
           (let* ((,compiled (compile-regex ,pattern))
                  (,result (scan ,compiled ,text)))
             (expect (and ,result
                          (list (match-start ,result) (match-end ,result)))
                     :to-equal
                     (and ,expected-start
                          (list ,expected-start ,expected-end)))))))))

(defmacro it-advanced-pattern-scan-cases (description cases)
  "Register one advanced scan specification from declarative per-pattern cases.

Each case is (PATTERN TEXT EXPECTED-START EXPECTED-END). The pattern is
compiled per case and must route through the advanced engine."
  (let ((pattern (gensym "ADVANCED-SCAN-PATTERN"))
        (text (gensym "ADVANCED-SCAN-TEXT"))
        (expected-start (gensym "EXPECTED-START"))
        (expected-end (gensym "EXPECTED-END"))
        (compiled (gensym "COMPILED-ADVANCED-REGEX"))
        (result (gensym "ADVANCED-SCAN-RESULT")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,text ,expected-start ,expected-end) case
           (let* ((,compiled (compile-regex ,pattern))
                  (,result (scan ,compiled ,text)))
             (expect-advanced-regex-config-cases ,compiled)
             (expect (and ,result
                          (list (match-start ,result) (match-end ,result)))
                     :to-equal
                     (and ,expected-start
                          (list ,expected-start ,expected-end)))))))))

(defmacro it-advanced-public-scan-cases (description cases)
  "Register one advanced public-scan specification from declarative cases.

Each case is (PATTERN TEXT EXPECTED-START EXPECTED-END EXPECTED-STRING
&optional SCAN-ARGUMENTS). CASES is evaluated at runtime so local fixtures can
feed the table."
  (let ((description-name (gensym "DESCRIPTION"))
        (cases-name (gensym "CASES")))
    (let ((pattern (gensym "ADVANCED-PUBLIC-PATTERN"))
          (text (gensym "ADVANCED-PUBLIC-TEXT"))
          (expected-start (gensym "EXPECTED-START"))
          (expected-end (gensym "EXPECTED-END"))
          (expected-string (gensym "EXPECTED-STRING"))
          (scan-arguments (gensym "SCAN-ARGUMENTS"))
          (compiled (gensym "COMPILED-ADVANCED-REGEX"))
          (result (gensym "ADVANCED-PUBLIC-RESULT")))
      `(let ((,description-name ,description)
             (,cases-name ,cases))
         (it ,description-name
           (dolist (case ,cases-name)
             (destructuring-bind
                 (,pattern ,text
                  ,expected-start ,expected-end ,expected-string
                  &optional (,scan-arguments '()))
                 case
               (let* ((,compiled (compile-regex ,pattern))
                      (,result (apply (function scan)
                                      ,compiled
                                      ,text
                                      ,scan-arguments)))
                 (expect-advanced-regex-config-cases ,compiled)
                 (expect (and ,result
                              (list (match-start ,result)
                                    (match-end ,result)))
                         :to-equal
                         (list ,expected-start ,expected-end))
                 (expect (and ,result
                              (match-string ,result ,text))
                         :to-equal
                         ,expected-string)))))))))

(defmacro it-advanced-element-equal-cases (description cases)
  "Register one advanced-element equality specification from declarative cases.

Each case is (LEFT RIGHT CASE-FOLD-P SIMPLE-FOLD-P EXPECTED)."
  (let ((left (gensym "LEFT"))
        (right (gensym "RIGHT"))
        (case-fold-p (gensym "CASE-FOLD-P"))
        (simple-fold-p (gensym "SIMPLE-FOLD-P"))
        (expected (gensym "EXPECTED")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind
             (,left ,right ,case-fold-p ,simple-fold-p ,expected)
             case
           (expect
            (cl-regex-kit::%advanced-element-equal-p
             ,left
             ,right
             ,case-fold-p
             ,simple-fold-p)
            :to-equal
            ,expected))))))

(defmacro it-advanced-limit-cases (description cases)
  "Register one advanced limit-error specification from declarative cases.

Each case is (PATTERN LIMIT-KEY LIMIT-VALUE EXPECTED-KIND EXPECTED-LIMIT
EXPECTED-USED)."
  (let ((pattern (gensym "LIMIT-PATTERN"))
        (limit-key (gensym "LIMIT-KEY"))
        (limit-value (gensym "LIMIT-VALUE"))
        (expected-kind (gensym "EXPECTED-KIND"))
        (expected-limit (gensym "EXPECTED-LIMIT"))
        (expected-used (gensym "EXPECTED-USED"))
        (condition (gensym "LIMIT-CONDITION")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind
             (,pattern ,limit-key ,limit-value
              ,expected-kind ,expected-limit ,expected-used)
             case
           (let ((,condition
                   (handler-case
                       (progn
                         (scan (apply (function compile-regex)
                                      ,pattern
                                      (list ,limit-key ,limit-value))
                               "a")
                         nil)
                     (advanced-regex-limit-error (condition)
                       condition))))
             (expect ,condition :to-be-truthy)
             (when ,condition
               (expect (advanced-regex-limit-kind ,condition)
                       :to-equal
                       ,expected-kind)
               (expect (advanced-regex-limit ,condition)
                       :to-equal
                       ,expected-limit)
               (expect (advanced-regex-limit-used ,condition)
                       :to-equal
                       ,expected-used))))))))

(defmacro it-advanced-runner-cases (description pattern text cases)
  "Register one advanced-runner specification from declarative range cases.

Each case is (ARGUMENT-PLIST EXPECTED-START EXPECTED-END). The advanced regex
is compiled once per specification."
  (let ((description-name (gensym "DESCRIPTION"))
        (pattern-name (gensym "PATTERN"))
        (text-name (gensym "TEXT"))
        (cases-name (gensym "CASES")))
    (let ((compiled (gensym "COMPILED-ADVANCED-REGEX"))
          (result (gensym "ADVANCED-RESULT"))
          (arguments (gensym "ADVANCED-ARGUMENTS"))
          (expected-start (gensym "EXPECTED-START"))
          (expected-end (gensym "EXPECTED-END")))
      `(let ((,description-name ,description)
             (,pattern-name ,pattern)
             (,text-name ,text)
             (,cases-name ',cases))
         (it ,description-name
           (let ((,compiled (compile-regex ,pattern-name)))
             (dolist (case ,cases-name)
               (destructuring-bind (,arguments ,expected-start ,expected-end) case
                 (let ((,result (apply (function cl-regex-kit:run-advanced-regex)
                                       ,compiled
                                       ,text-name
                                       ,arguments)))
                   (expect (and ,result
                                (list (match-start ,result) (match-end ,result)))
                           :to-equal
                           (and ,expected-start
                                (list ,expected-start ,expected-end))))))))))))

(defmacro it-advanced-conditional-match-cases (&body cases)
  "Register multiple advanced conditional match specifications at once.

Each case is (DESCRIPTION PATTERN &key MATCHES NON-MATCHES). This keeps
advanced-condition coverage declarative while preserving one cl-weave spec per
case."
  `(progn
     ,@(mapcar
        (lambda (case)
          (destructuring-bind
              (description pattern &key matches non-matches)
              case
            `(it-match-cases
              ,description
              ,pattern
              :matches ,matches
              :non-matches ,non-matches)))
        cases)))

(defmacro with-advanced-state-fixtures
    ((context-name state-name group-name) &body body)
  "Bind advanced context, state, and group builders for internal tests."
  (let ((actual-limit-name (gensym "ACTUAL-LIMIT")))
    `(flet ((,context-name (text &key root (group-names nil) (byte-mode-p nil)
                                  (callout nil) (limit nil))
              (let ((,actual-limit-name (or limit (length text))))
                (cl-regex-kit::make-advanced-context
                 :text text
                 :search-start 0
                 :limit ,actual-limit-name
                 :text-length (length text)
                 :byte-mode-p byte-mode-p
                 :never-newline-p nil
                 :root root
                 :group-count 4
                 :group-names group-names
                 :step-limit most-positive-fixnum
                 :steps 0
                 :nest-limit most-positive-fixnum
                 :callout callout)))
            (,state-name (position slots &key control skip-to mark
                                           (reported-start nil)
                                           (recursion-depth nil)
                                           (recursion-target nil)
                                           (committed-p nil))
              (cl-regex-kit::%make-advanced-state
               position
               slots
               control
               skip-to
               mark
               reported-start
               recursion-depth
               recursion-target
               committed-p))
            (,group-name (name index)
              (make-instance 'cl-regex-kit::group-node
                             :name name
                             :capture-index index
                             :child (make-instance 'cl-regex-kit::literal-node
                                                   :char #\a))))
       ,@body)))

(defmacro with-advanced-boundary-fixtures
    ((context-name sentence-units-name grapheme-unit-name) &body body)
  "Bind sentence, grapheme, and boundary builders for internal tests."
  `(flet ((,context-name (text &key (byte-mode-p nil))
            (cl-regex-kit::make-advanced-context
             :text text
             :search-start 0
             :limit (length text)
             :text-length (length text)
             :byte-mode-p byte-mode-p
             :never-newline-p nil
             :root nil
             :group-count 0
             :group-names nil
             :step-limit most-positive-fixnum
             :steps 0
             :nest-limit most-positive-fixnum
             :callout nil))
          (,sentence-units-name (&rest classes)
            (coerce
             (loop for class in classes
                   for index from 0
                   collect (cl-regex-kit::make-advanced-sentence-unit
                            :start index :end (1+ index) :class class))
             'vector))
          (,grapheme-unit-name (class &key (indic "NONE") (pictographic-p nil))
            (cl-regex-kit::make-advanced-grapheme-unit
             :character #\a
             :start 0
             :end 1
             :class class
             :indic-conjunct-break indic
             :extended-pictographic-p pictographic-p)))
     ,@body))

(defmacro with-advanced-evaluator-fixtures
    ((context-name state-name sentence-units-name) &body body)
  "Bind advanced evaluator helpers for internal tests."
  (let ((actual-limit-name (gensym "ACTUAL-LIMIT")))
    `(flet ((,context-name (text &key root (group-names nil) (byte-mode-p nil)
                                  (callout nil) (limit nil))
              (let ((,actual-limit-name (or limit (length text))))
                (cl-regex-kit::make-advanced-context
                 :text text
                 :search-start 0
                 :limit ,actual-limit-name
                 :text-length (length text)
                 :byte-mode-p byte-mode-p
                 :never-newline-p nil
                 :root root
                 :group-count 4
                 :group-names group-names
                 :step-limit most-positive-fixnum
                 :steps 0
                 :nest-limit most-positive-fixnum
                 :callout callout)))
            (,state-name (position slots &key control skip-to mark
                                           (reported-start nil)
                                           (recursion-depth nil)
                                           (recursion-target nil)
                                           (committed-p nil))
              (cl-regex-kit::%make-advanced-state
               position
               slots
               control
               skip-to
               mark
               reported-start
               recursion-depth
               recursion-target
               committed-p))
            (,sentence-units-name (&rest classes)
              (coerce
               (mapcar
                (lambda (class)
                  (cl-regex-kit::make-advanced-sentence-unit
                   :start 0 :end 1 :class class))
                classes)
               'vector)))
       ,@body)))

(defmacro it-scan-range-cases (description pattern cases)
  "Register one scan specification from declarative match-range cases.

Each case is (TEXT EXPECTED-START EXPECTED-END &REST SCAN-ARGUMENTS). The
pattern is compiled once per specification."
  (let ((compiled (gensym "COMPILED-REGEX"))
        (text (gensym "TEXT"))
        (expected-start (gensym "EXPECTED-START"))
        (expected-end (gensym "EXPECTED-END"))
        (arguments (gensym "SCAN-ARGUMENTS"))
        (result (gensym "SCAN-RESULT")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         (dolist (case ',cases)
           (destructuring-bind
               (,text ,expected-start ,expected-end &rest ,arguments)
               case
             (let ((,result (apply (function scan)
                                   ,compiled
                                   ,text
                                   ,arguments)))
               (expect (and ,result
                            (list (match-start ,result) (match-end ,result)))
                       :to-equal
                       (and ,expected-start
                            (list ,expected-start ,expected-end))))))))))

(defmacro it-evaluated-scan-range-cases (description pattern cases)
  "Register one scan specification from evaluated match-range cases.

Each case is (TEXT EXPECTED-START EXPECTED-END &REST SCAN-ARGUMENTS). CASES is
evaluated at runtime, so TEXT may be produced by helper forms."
  (let ((compiled (gensym "COMPILED-REGEX"))
        (text (gensym "TEXT"))
        (expected-start (gensym "EXPECTED-START"))
        (expected-end (gensym "EXPECTED-END"))
        (arguments (gensym "SCAN-ARGUMENTS"))
        (result (gensym "SCAN-RESULT")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         (dolist (case ,cases)
           (destructuring-bind
               (,text ,expected-start ,expected-end &rest ,arguments)
               case
             (let ((,result (apply (function scan)
                                   ,compiled
                                   ,text
                                   ,arguments)))
               (expect (and ,result
                            (list (match-start ,result) (match-end ,result)))
                       :to-equal
                       (and ,expected-start
                            (list ,expected-start ,expected-end))))))))))

(defmacro it-scan-string-cases (description pattern cases)
  "Register one scan specification from declarative matched-string cases.

Each case is (TEXT EXPECTED-MATCH-STRING &REST SCAN-ARGUMENTS). The pattern is
compiled once per specification."
  (let ((compiled (gensym "COMPILED-REGEX"))
        (text (gensym "TEXT"))
        (expected (gensym "EXPECTED-MATCH-STRING"))
        (arguments (gensym "SCAN-ARGUMENTS"))
        (result (gensym "SCAN-RESULT")))
    `(it ,description
       (let ((,compiled (compile-regex ,pattern)))
         (dolist (case ',cases)
           (destructuring-bind (,text ,expected &rest ,arguments) case
             (let ((,result (apply (function scan)
                                   ,compiled
                                   ,text
                                   ,arguments)))
               (expect (and ,result
                            (match-string ,result ,text))
                       :to-equal
                       ,expected))))))))

(defmacro it-byte-scan-range-cases (description pattern cases)
  "Register one byte-scan specification from declarative match-range cases.

Each case is (TEXT EXPECTED-START EXPECTED-END &REST SCAN-ARGUMENTS). The
byte pattern is compiled once per specification."
  (let ((compiled (gensym "COMPILED-BYTE-REGEX"))
        (text (gensym "TEXT"))
        (expected-start (gensym "EXPECTED-START"))
        (expected-end (gensym "EXPECTED-END"))
        (arguments (gensym "SCAN-ARGUMENTS"))
        (result (gensym "SCAN-RESULT")))
    `(it ,description
       (let ((,compiled (compile-byte-regex ,pattern)))
         (expect (regex-advanced-p ,compiled) :to-be-truthy)
         (dolist (case ,cases)
           (destructuring-bind
               (,text ,expected-start ,expected-end &rest ,arguments)
               case
             (let ((,result (apply (function scan)
                                   ,compiled
                                   ,text
                                   ,arguments)))
               (expect (and ,result
                            (list (match-start ,result) (match-end ,result)))
                       :to-equal
                       (and ,expected-start
                            (list ,expected-start ,expected-end))))))))))

(defmacro it-lookaround-cases (description cases)
  "Register one advanced lookaround specification from declarative cases.

Each case is (PATTERN MATCHING NON-MATCHING). The pattern is compiled once per
case and asserted through the public matcher surface."
  (let ((pattern (gensym "LOOKAROUND-PATTERN"))
        (matching (gensym "LOOKAROUND-MATCHING"))
        (non-matching (gensym "LOOKAROUND-NON-MATCHING"))
        (compiled (gensym "COMPILED-LOOKAROUND-REGEX")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,matching ,non-matching) case
           (let ((,compiled (compile-regex ,pattern)))
             (expect ,matching :to-match-regex ,compiled)
             (expect-not ,non-matching :to-match-regex ,compiled)))))))

(defmacro it-fuzzy-distance-cases (description cases)
  "Register one fuzzy API specification from declarative distance cases.

Each case is (PATTERN TEXT EXPECTED-DISTANCE). The pattern is compiled once
per case and checked through the public fuzzy entry points."
  (let ((compiled (gensym "COMPILED-FUZZY-REGEX"))
        (pattern (gensym "PATTERN"))
        (text (gensym "TEXT"))
        (expected-distance (gensym "EXPECTED-DISTANCE")))
    `(it ,description
       (dolist (case ',cases)
         (destructuring-bind (,pattern ,text ,expected-distance) case
           (let ((,compiled (compile-regex ,pattern)))
             (expect (match-edit-distance (fuzzy-match ,pattern ,text))
                     :to-be ,expected-distance)
             (expect (match-edit-distance (fuzzy-match ,compiled ,text))
                     :to-be ,expected-distance)
             (expect (match-edit-distance (fuzzy-scan-at ,compiled ,text 0))
                     :to-be ,expected-distance)))))))
