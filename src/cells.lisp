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

(defun object-summary-string (object)
  (if object
      (format nil "~A ~A"
              (object-kind object)
              (object-id object))
      "-"))

(defun object-parent-summary-string (object)
  (if (and object (parent-of object))
      (object-summary-string (parent-of object))
      "-"))

(defun object-prototype-summary-string (object)
  (if (and object (object-prototype object))
      (object-summary-string (object-prototype object))
      "-"))

(defun model-inspector-lines (model &key (subject-label "object"))
  (let ((lines (list (format nil "~A: ~A"
                             subject-label
                             (object-summary-string model))
                     (format nil "parent: ~A"
                             (object-parent-summary-string model))
                     (format nil "prototype: ~A"
                             (object-prototype-summary-string model))
                     (format nil "children: ~D"
                             (length (if model
                                         (children-of model)
                                         nil))))))
    (when model
      (setf lines
            (append
             lines
             (typecase model
               (workspace
                (list (format nil "title: ~A" (workspace-title model))
                      (format nil "notebooks: ~D"
                              (length (workspace-notebooks model)))))
               (notebook
                (list (format nil "title: ~A" (notebook-title model))))
               (section
                (list (format nil "title: ~A" (section-title model))))
               (paragraph
                (list (format nil "text-len: ~D"
                              (length (paragraph-text model)))
                      (format nil "text: ~A"
                              (preview-string (paragraph-text model)))))
               (code-block
                (list (format nil "language: ~A"
                              (code-block-language model))
                      (format nil "source-len: ~D"
                              (length (code-block-source model)))
                      (format nil "source: ~A"
                              (preview-string (code-block-source model)))
                      (format nil "syntax: ~A"
                              (code-block-syntax-summary-line model))
                      (format nil "parse: ~A"
                              (code-block-parse-status-line model))
                      (format nil "selection: ~A"
                              (code-block-selection-status-line model))
                      (format nil "selected-form-index: ~A"
                              (or (code-block-selected-form-index model)
                                  "-"))
                      (format nil "selected-form-path: ~A"
                              (or (code-block-selected-form-path model)
                                  "-"))
                      (format nil "selected-form: ~A"
                              (if (code-block-selected-form model)
                                  (simple-form-summary
                                   (code-block-selected-form model))
                                  "-"))
                      (format nil "structure: ~:[hidden~;visible~]"
                              (code-block-structure-visible-p model))
                      (format nil "result: ~A"
                              (if (code-block-result model)
                                  (object-summary-string
                                   (code-block-result model))
                                  "-"))
                      (format nil "result-status: ~A"
                              (if (code-block-result model)
                                  (result-block-status
                                   (code-block-result model))
                                  "-"))))
               (repl-block
                (list (format nil "title: ~A" (repl-block-title model))
                      (format nil "package: ~A" (repl-block-package model))
                      (format nil "entries: ~D"
                              (length (children-of model)))))
               (repl-entry
                (list (format nil "input-len: ~D"
                              (length (repl-entry-input-source model)))
                      (format nil "input: ~A"
                              (preview-string
                               (repl-entry-input-source model)))
                      (format nil "result: ~A"
                              (if (repl-entry-result model)
                                  (object-summary-string
                                   (repl-entry-result model))
                                  "-"))
                      (format nil "result-status: ~A"
                              (if (repl-entry-result model)
                                  (result-block-status
                                   (repl-entry-result model))
                                  "-"))))
               (quote-block
                (list (format nil "text-len: ~D"
                              (length (quote-block-text model)))
                      (format nil "attribution: ~A"
                              (if (string= "" (quote-block-attribution model))
                                  "-"
                                  (quote-block-attribution model)))))
               (reference-block
                (list (format nil "target: ~A"
                              (object-reference-summary-string
                               (reference-block-target model)))
                      (format nil "label: ~A"
                              (if (string= "" (reference-block-label model))
                                  "-"
                                  (reference-block-label model)))
                      (format nil "note: ~A"
                              (if (string= "" (reference-block-note model))
                                  "-"
                                  (reference-block-note model)))))
               (inspector-block
                (list (format nil "target: ~A"
                              (object-reference-summary-string
                               (inspector-block-target model)))
                      (format nil "label: ~A"
                              (if (string= "" (inspector-block-label model))
                                  "-"
                                  (inspector-block-label model)))))
               (list-block
                (list (format nil "items: ~D"
                              (length (list-block-items model)))
                      (format nil "ordered: ~:[no~;yes~]"
                              (list-block-ordered-p model))))
               (table-block
                (list (format nil "columns: ~D"
                              (length (table-block-columns model)))
                      (format nil "rows: ~D"
                              (length (table-block-rows model)))))
               (task-list
                (list (format nil "tasks: ~D"
                              (length (task-list-items model)))
                      (format nil "done: ~D"
                              (count-if #'task-item-done-p
                                        (task-list-items model)))))
               (result-block
                (list (format nil "status: ~A"
                              (result-block-status model))
                      (format nil "presentation: ~A"
                              (preview-string
                               (result-block-presentation model)))
                      (format nil "input: ~A"
                              (preview-string
                               (result-block-input-source model)))
                      (format nil "package: ~A"
                              (if (string= "" (result-block-package model))
                                  "-"
                                  (result-block-package model)))
                      (format nil "evaluated-at: ~A"
                              (or (result-block-evaluated-at model)
                                  "-"))))
               (t nil)))))
    lines))

(defun result-block-value-line (node)
  (cond
    ((eq (result-block-status node) :error)
     (format nil "!! ~A" (result-block-presentation node)))
    ((eq (result-block-status node) :stale)
     (format nil ".. ~A" (result-block-presentation node)))
    (t
     (format nil "=> ~A" (result-block-presentation node)))))

(defun result-block-metadata-line (node)
  (format nil "input: ~A  |  package: ~A  |  evaluated-at: ~A"
          (preview-string (result-block-input-source node))
          (if (string= "" (result-block-package node))
              "-"
              (result-block-package node))
          (or (result-block-evaluated-at node) "-")))

(defun repl-entry-package-name (entry)
  (let ((result (repl-entry-result entry)))
    (cond
      ((and result
            (not (string= "" (result-block-package result))))
       (result-block-package result))
      ((typep (parent-of entry) 'repl-block)
       (repl-block-package (parent-of entry)))
      (t
       "-"))))

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
    (repl-block
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label (format nil "REPL: ~A [~A]"
                                 (repl-block-title node)
                                 (repl-block-package node)))))
       (append-heading-cell cell
                            registry
                            node
                            (format nil "REPL: ~A [~A]"
                                    (repl-block-title node)
                                    (repl-block-package node)))
       (dolist (entry (children-of node) cell)
         (append-child cell (build-node-cell entry registry)))))
    (repl-entry
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label "REPL Entry")))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (format nil "~A> ~A"
                                    (repl-entry-package-name node)
                                    (repl-entry-input-source node))
                      :role :content))
       (when (repl-entry-result node)
         (append-child cell
                       (build-node-cell (repl-entry-result node) registry)))
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
    (inspector-block
     (let* ((target (inspector-block-target node))
            (label (if (string= "" (inspector-block-label node))
                       (object-reference-summary-string target)
                       (inspector-block-label node)))
            (cell (make-container-cell
                   :registry registry
                   :model node
                   :label (format nil "Inspector: ~A" label))))
       (append-heading-cell cell
                            registry
                            node
                            (format nil "Inspector: ~A" label))
       (append-text-lines cell
                          registry
                          node
                          (model-inspector-lines target
                                                 :subject-label "object")
                          :role :metadata)
       cell))
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
     (let ((cell (make-container-cell
                  :registry registry
                  :model node
                  :label "Result")))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (result-block-value-line node)
                      :role :result))
       (append-child cell
                     (make-text-cell
                      :registry registry
                      :model node
                      :text (result-block-metadata-line node)
                      :role :metadata))
       cell))
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
