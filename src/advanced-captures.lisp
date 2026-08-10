(in-package #:cl-regex-kit)

(defun %advanced-name= (left right)
  (and left right (string-equal (string left) (string right))))

(defun %advanced-capture-indices-by-name (name context)
  (loop for candidate in (advanced-context-group-names context)
        when (%advanced-name= name (car candidate))
        collect (cdr candidate)))

(defun %advanced-capture-participated-p (index state)
  (let ((slots (advanced-state-slots state)))
    (and (integerp index)
         (<= 0 index)
         (< (1+ (* 2 index)) (length slots))
         (aref slots (* 2 index))
         (aref slots (1+ (* 2 index))))))

(defun %advanced-capture-index-by-name (name context &optional state)
  (let ((indices (%advanced-capture-indices-by-name name context)))
    (or
     (and state
          (find-if
           (lambda (index)
             (%advanced-capture-participated-p index state))
           indices))
     (car indices))))

(defun %advanced-capture-index-for-group (group)
  (and group (group-node-capture-index group)))

(defun %advanced-group-matches-p (group name index)
  (and
   (typep group 'group-node)
   (or
    (and index (= index (or (group-node-capture-index group) -1)))
    (and name (%advanced-name= name (group-node-name group))))))

(defun %advanced-find-group (node &key name index)
  (when (typep node 'regex-node)
    (typecase node
      (group-node
       (or
        (when (%advanced-group-matches-p node name index)
          node)
        (%advanced-find-group (group-node-child node) :name name :index index)))
      (concat-node
       (loop for child in (concat-node-children node)
             thereis (%advanced-find-group child :name name :index index)))
      (alternation-node
       (loop for branch in (alternation-node-branches node)
             thereis (%advanced-find-group branch :name name :index index)))
      (repetition-node
       (%advanced-find-group
        (repetition-node-child node)
        :name
        name
        :index
        index))
      (possessive-repetition-node
       (%advanced-find-group
        (possessive-repetition-node-child node)
        :name
        name
        :index
        index))
      (assertion-node
       (%advanced-find-group
        (assertion-node-child node)
        :name
        name
        :index
        index))
      (atomic-node
       (%advanced-find-group (atomic-node-child node) :name name :index index))
      (conditional-node
       (or
        (%advanced-find-group
         (conditional-node-yes-branch node)
         :name
         name
         :index
         index)
        (%advanced-find-group
         (conditional-node-no-branch node)
         :name
         name
         :index
         index)))
      (otherwise nil))))

(defun %advanced-capture-index (node context &optional state)
  (or
   (and
    (backreference-node-capture-index node)
    (backreference-node-capture-index node))
   (and
    (backreference-node-name node)
    (or
     (%advanced-capture-index-by-name
      (backreference-node-name node)
      context
      state)
     (let ((group
            (%advanced-find-group
             (advanced-context-root context)
             :name
             (backreference-node-name node))))
       (%advanced-capture-index-for-group group))))))
