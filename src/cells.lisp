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
    :initform "")))

(defun make-container-cell (&key registry model (label "") (orientation :vertical))
  (%register-if-present
   registry
   (make-instance 'container-cell
                  :id (fresh-id "cell")
                  :kind :container-cell
                  :model model
                  :label label
                  :orientation orientation)))

(defun make-text-cell (&key registry model (text ""))
  (%register-if-present
   registry
   (make-instance 'text-cell
                  :id (fresh-id "cell")
                  :kind :text-cell
                  :model model
                  :label text
                  :text text)))

(defmethod append-child ((parent cell) (child cell))
  (setf (slot-value parent 'children)
        (append (children-of parent) (list child)))
  (setf (parent-of child) parent)
  child)

(defun cell-children (cell)
  (children-of cell))

(defgeneric preferred-height (cell))

(defmethod preferred-height ((cell text-cell))
  1)

(defmethod preferred-height ((cell container-cell))
  (max 1
       (+ (length (children-of cell))
          (reduce #'+
                  (mapcar #'preferred-height (children-of cell))
                  :initial-value 0))))

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
       (dolist (child (children-of node) cell)
         (append-child cell (build-node-cell child registry)))))
    (section
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label (format nil "Section: ~A" (section-title node)))))
       (dolist (child (children-of node) cell)
         (append-child cell (build-node-cell child registry)))))
    (paragraph
     (make-text-cell :registry registry :model node :text (paragraph-text node)))
    (code-block
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label (format nil "Code [~A]" (code-block-language node)))))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (code-block-source node)))
       (when (code-block-result node)
         (append-child cell
                       (build-node-cell (code-block-result node) registry)))
       cell))
    (result-block
     (make-text-cell
      :registry registry
      :model node
      :text (format nil "=> ~A" (result-block-presentation node))))
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
                   :text "Live image workspace"))
    (when (root-notebook workspace)
      (append-child root
                    (build-node-cell (root-notebook workspace) registry)))
    root))
