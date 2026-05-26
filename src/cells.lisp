(in-package :orra)

(defstruct bounds
  (x 0 :type integer)
  (y 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer))

(defclass cell (model-object)
  ((parent
    :initarg :parent
    :accessor parent-of
    :initform nil)
   (children
    :initarg :children
    :accessor children-of
    :initform nil)
   (bounds
    :initarg :bounds
    :accessor cell-bounds
    :initform (make-bounds))
   (model
    :initarg :model
    :accessor cell-model
    :initform nil)
   (label
    :initarg :label
    :accessor cell-label
    :initform "")))

(defclass container-cell (cell)
  ((orientation
    :initarg :orientation
    :accessor cell-orientation
    :initform :vertical)
   (spacing
    :initarg :spacing
    :accessor cell-spacing
    :initform 1)))

(defclass text-cell (cell)
  ((text
    :initarg :text
    :accessor cell-text
    :initform "")
   (role
    :initarg :role
    :accessor cell-role
    :initform :content)
   (style-spans
    :initarg :style-spans
    :accessor cell-style-spans
    :initform nil)))

(defun make-container-cell (&key registry model (label "") (orientation :vertical))
  (%register-if-present
   registry
   (make-instance 'container-cell
                  :id (fresh-id "cell")
                  :kind :container-cell
                  :model model
                  :label label
                  :orientation orientation)))

(defun make-text-cell (&key registry model (text "") (role :content) style-spans)
  (%register-if-present
   registry
   (make-instance 'text-cell
                  :id (fresh-id "cell")
                  :kind :text-cell
                  :model model
                  :label text
                  :text text
                  :role role
                  :style-spans style-spans)))

(defmethod append-child ((parent cell) (child cell))
  (setf (slot-value parent 'children)
        (append (children-of parent) (list child)))
  (setf (parent-of child) parent)
  child)

(defun cell-children (cell)
  (children-of cell))

(defgeneric preferred-height (cell))

(defmethod preferred-height ((cell text-cell))
  (max 1
       (length (split-lines (cell-text cell)))))

(defmethod preferred-height ((cell container-cell))
  (max 1
       (+ (length (children-of cell))
          (reduce #'+
                  (mapcar #'preferred-height (children-of cell))
                  :initial-value 0))))

(defun append-text-lines (container registry model lines &key (role :content))
  (dolist (line lines container)
    (append-child container
                  (make-text-cell
                   :registry registry
                   :model model
                   :text line
                   :role role))))

(defun append-heading-cell (container registry model text)
  (append-child container
                (make-text-cell
                 :registry registry
                 :model model
                 :text text
                 :role :heading)))

(defun object-reference-summary-string (object)
  (cond
    ((typep object 'model-object)
     (format nil "~A ~A" (object-kind object) (object-id object)))
    ((null object)
     "-")
    (t
     (normalize-display-string object))))

(defun quote-block-lines (node)
  (let ((lines (mapcar (lambda (line)
                         (format nil "> ~A" line))
                       (split-lines (quote-block-text node)))))
    (if (string= "" (quote-block-attribution node))
        lines
        (append lines
                (list (format nil "-- ~A"
                              (quote-block-attribution node)))))))

(defun format-list-block-item (node item index)
  (if (list-block-ordered-p node)
      (format nil "~D. ~A" (1+ index) item)
      (format nil "- ~A" item)))

(defun format-table-row (values)
  (format nil "~{~A~^ | ~}" values))

(defun format-task-list-item (item)
  (format nil "[~A] ~A"
          (if (task-item-done-p item) "x" " ")
          (task-item-text item)))

(defun perform-layout (cell &key (x 0) (y 0) (width 80))
  (setf (cell-bounds cell)
        (make-bounds :x x :y y :width width :height (preferred-height cell)))
  (when (typep cell 'container-cell)
    (let ((cursor-y (+ y 1)))
      (dolist (child (children-of cell))
        (perform-layout child :x (+ x 2) :y cursor-y :width (- width 4))
        (incf cursor-y (+ (preferred-height child)
                          (cell-spacing cell))))))
  cell)

(defun build-node-cell (node registry)
  (typecase node
    (notebook
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label (format nil "Notebook: ~A" (notebook-title node)))))
       (append-heading-cell cell
                            registry
                            node
                            (format nil "Notebook: ~A" (notebook-title node)))
       (dolist (child (children-of node) cell)
         (append-child cell (build-node-cell child registry)))))
    (section
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label (format nil "Section: ~A" (section-title node)))))
       (append-heading-cell cell
                            registry
                            node
                            (format nil "Section: ~A" (section-title node)))
       (dolist (child (children-of node) cell)
         (append-child cell (build-node-cell child registry)))))
    (paragraph
     (make-text-cell
      :registry registry
      :model node
      :text (paragraph-text node)
      :role :editable-content))
    (code-block
     (let* ((cell (make-container-cell
                   :registry registry
                   :model node
                   :label (format nil "Code [~A]" (code-block-language node))))
            (info (code-block-parse-info node))
            (syntax-tokens (code-block-syntax-tokens node)))
       (append-heading-cell cell
                            registry
                            node
                            (format nil "Code [~A]" (code-block-language node)))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (code-block-source node)
                      :role :editable-content
                      :style-spans syntax-tokens))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (code-block-syntax-summary-line node syntax-tokens)
                      :role :metadata))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (code-block-parse-status-line node info)
                      :role :metadata))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (code-block-selection-status-line node info)
                      :role :metadata))
       (when (code-block-structure-visible-p node)
         (let ((structure-cell (make-container-cell
                                :registry registry
                                :model node
                                :label "Structure")))
           (append-heading-cell structure-cell
                                registry
                                node
                                "Structure")
           (dolist (line (code-block-structure-lines node :info info))
             (append-child structure-cell
                           (make-text-cell
                            :registry registry
                            :model node
                            :text line
                            :role :metadata)))
           (append-child cell structure-cell)))
       (when (code-block-result node)
         (append-child cell
                       (build-node-cell (code-block-result node) registry)))
       cell))
    (quote-block
     (make-text-cell
      :registry registry
      :model node
      :text (format nil "~{~A~^~%~}" (quote-block-lines node))
      :role :quote))
    (reference-block
     (make-text-cell
      :registry registry
      :model node
      :text (format nil "@ ~A -> ~A~@[  |  ~A~]"
                    (if (string= "" (reference-block-label node))
                        "reference"
                        (reference-block-label node))
                    (object-reference-summary-string
                     (reference-block-target node))
                    (and (not (string= "" (reference-block-note node)))
                         (reference-block-note node)))
      :role :reference))
    (list-block
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label "List")))
       (append-heading-cell cell registry node "List")
       (loop for item in (list-block-items node)
             for index from 0
             do (append-child cell
                              (make-text-cell
                               :registry registry
                               :model node
                               :text (format-list-block-item node
                                                             item
                                                             index))))
       cell))
    (table-block
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label "Table")))
       (append-heading-cell cell registry node "Table")
       (when (table-block-columns node)
         (append-child cell
                       (make-text-cell
                        :registry registry
                        :model node
                        :text (format-table-row (table-block-columns node))
                        :role :heading)))
       (append-text-lines cell
                          registry
                          node
                          (mapcar #'format-table-row
                                  (table-block-rows node)))
       cell))
    (task-list
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label "Tasks")))
       (append-heading-cell cell registry node "Tasks")
       (append-text-lines cell
                          registry
                          node
                          (mapcar #'format-task-list-item
                                  (task-list-items node)))
       cell))
    (result-block
     (make-text-cell
      :registry registry
      :model node
      :text (cond
              ((eq (result-block-status node) :error)
               (format nil "!! ~A" (result-block-presentation node)))
              ((eq (result-block-status node) :stale)
               (format nil ".. ~A" (result-block-presentation node)))
              (t
               (format nil "=> ~A" (result-block-presentation node))))
      :role :result))
    (t
     (make-text-cell
      :registry registry
      :text (format nil "~A" node)))))

(defun build-workspace-cell-tree (workspace &optional registry)
  (let ((root (make-container-cell
               :registry registry
               :model workspace
               :label (format nil "Workspace: ~A" (workspace-title workspace)))))
    (append-child root
                  (make-text-cell
                   :registry registry
                   :model workspace
                   :text (format nil "Workspace: ~A  |  Live image workspace"
                                 (workspace-title workspace))
                   :role :heading))
    (when (root-notebook workspace)
      (append-child root
                    (build-node-cell (root-notebook workspace) registry)))
    root))
