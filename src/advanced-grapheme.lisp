(in-package #:cl-regex-kit)

(progn
  (defparameter +advanced-indic-consonant-ranges+
    +unicode-indic-conjunct-break-consonant-ranges+)
  (defparameter +advanced-indic-linker-ranges+
    +unicode-indic-conjunct-break-linker-ranges+)
  (defun %advanced-extended-pictographic-p (character)
    (and
     (characterp character)
     (range-matches-p +extended-pictographic-ranges+ character)))
  (defun %advanced-indic-conjunct-break-class (character grapheme-class)
    (declare (ignore grapheme-class))
    (cond
      ((not (characterp character)) "NONE")
      ((range-matches-p +advanced-indic-linker-ranges+ character) "LINKER")
      ((range-matches-p +advanced-indic-consonant-ranges+ character) "CONSONANT")
      ((range-matches-p +unicode-indic-conjunct-break-extend-ranges+ character)
       "EXTEND")
      (t "NONE")))
  (defun %advanced-indic-conjunct-break-p (units next)
    (and
     (string= (advanced-grapheme-unit-indic-conjunct-break next) "CONSONANT")
     (loop
       with linker-p = nil
       for unit in units
       for class = (advanced-grapheme-unit-indic-conjunct-break unit)
       while (member class (quote ("EXTEND" "LINKER"))
                       :test (function string=))
       do (when (string= class "LINKER")
            (setf linker-p t))
       finally
         (return
           (and
            linker-p
            (string= class "CONSONANT")))))))

(defun %advanced-grapheme-class (character)
  (if (characterp character)
      (let ((name
              (string-upcase
               (string
                (sb-unicode:grapheme-break-class character)))))
        (setf name
              (coerce
               (remove-if
                (lambda (item)
                  (find item "-_ " :test (function char=)))
                name)
               (quote string)))
        (if (string= name "REGIONALINDICATOR") "RI" name))
      "OTHER"))

(defun %advanced-grapheme-unit-at (node position context)
  (multiple-value-bind (character end valid-p) (%advanced-read-element
                                                context
                                                position
                                                (grapheme-node-unicode-p node))
    (when valid-p
      (let ((class (%advanced-grapheme-class character)))
        (make-advanced-grapheme-unit
         :character
         character
         :start
         position
         :end
         end
         :class
         class
         :indic-conjunct-break
         (%advanced-indic-conjunct-break-class character class)
         :extended-pictographic-p
         (%advanced-extended-pictographic-p character))))))

(defun %advanced-grapheme-break-p (units next extended-p)
  (let* ((previous (car units))
         (previous-class (advanced-grapheme-unit-class previous))
         (next-class (advanced-grapheme-unit-class next)))
    (cond
      ((and (string= previous-class "CR") (string= next-class "LF")) nil)
      ((or
        (member previous-class (quote ("CR" "LF" "CONTROL"))
                :test (function string=))
        (member next-class (quote ("CR" "LF" "CONTROL"))
                :test (function string=)))
       t)
      ((and
        (string= previous-class "L")
        (member next-class (quote ("L" "V" "LV" "LVT"))
                :test (function string=)))
       nil)
      ((and
        (member previous-class (quote ("LV" "V"))
                :test (function string=))
        (member next-class (quote ("V" "T"))
                :test (function string=)))
       nil)
      ((and
        (member previous-class (quote ("LVT" "T"))
                :test (function string=))
        (string= next-class "T"))
       nil)
      ((member next-class (quote ("EXTEND" "ZWJ"))
               :test (function string=))
       nil)
      ((and extended-p (string= next-class "SPACINGMARK")) nil)
      ((and extended-p (string= previous-class "PREPEND")) nil)
      ((and extended-p (%advanced-indic-conjunct-break-p units next)) nil)
      ((and
        (advanced-grapheme-unit-extended-pictographic-p next)
        (string= previous-class "ZWJ")
        (loop for unit in (cdr units)
              while (string= (advanced-grapheme-unit-class unit) "EXTEND")
              finally (return
                       (and
                        unit
                        (advanced-grapheme-unit-extended-pictographic-p unit)))))
       nil)
      ((and
        (string= previous-class "RI")
        (string= next-class "RI")
        (oddp
         (loop for unit in units
               while (string= (advanced-grapheme-unit-class unit) "RI")
               count 1)))
       nil)
      ((not extended-p) (not (string= next-class "EXTEND")))
      (t t))))

(defun %advanced-grapheme-end (node state context)
  (let ((first
         (%advanced-grapheme-unit-at
          node
          (advanced-state-position state)
          context)))
    (when first
      (let ((units (list first))
            (position (advanced-grapheme-unit-end first)))
        (loop while (< position (advanced-context-limit context))
              for next = (%advanced-grapheme-unit-at node position context)
              while next
              do (if (%advanced-grapheme-break-p
                      units
                      next
                      (grapheme-node-extended-p node)) (return position)
                   (progn
                     (push next units)
                     (setf position (advanced-grapheme-unit-end next))))
              finally (return position))))))

(defun %advanced-grapheme-result (node state context)
  (let ((end (%advanced-grapheme-end node state context)))
    (when end
      (list
       (%make-advanced-state
        end
        (advanced-state-slots state)
        nil
        nil
        (advanced-state-mark state)
        (advanced-state-reported-start state)
        (advanced-state-recursion-depth state)
        (advanced-state-recursion-target state)
        (advanced-state-committed-p state))))))
