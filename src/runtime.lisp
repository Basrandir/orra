(in-package :orra)

(defvar *application* nil)

(defclass application ()
  ((registry
    :initarg :registry
    :accessor application-registry)
   (commands
    :initarg :commands
    :accessor application-commands)
   (keymap
    :initarg :keymap
    :accessor application-keymap)
   (workspace
    :initarg :workspace
    :accessor application-workspace)
   (backend
    :initarg :backend
    :accessor application-backend)
   (root-cell
    :initarg :root-cell
    :accessor application-root-cell
    :initform nil)
   (dirty-p
    :initarg :dirty-p
    :accessor application-dirty-p
    :initform t)
   (damage-regions
    :initarg :damage-regions
    :accessor application-damage-regions
    :initform (list :full))
   (viewport-y
    :initarg :viewport-y
    :accessor application-viewport-y
    :initform 0)
   (focused-model-id
    :initarg :focused-model-id
    :accessor application-focused-model-id
    :initform nil)
   (active-editor-model-id
    :initarg :active-editor-model-id
    :accessor application-active-editor-model-id
    :initform nil)
   (active-text-buffer
    :initarg :active-text-buffer
    :accessor application-active-text-buffer
    :initform nil)
   (editor-state-table
    :initarg :editor-state-table
    :accessor application-editor-state-table
    :initform (make-hash-table :test #'equal))
   (event-log
    :initarg :event-log
    :accessor application-event-log
    :initform nil)
   (event-log-limit
    :initarg :event-log-limit
    :accessor application-event-log-limit
    :initform 64)
   (debug-visible-p
    :initarg :debug-visible-p
    :accessor application-debug-visible-p
    :initform nil)
   (save-path
    :initarg :save-path
    :accessor application-save-path
    :initform nil)))

(defun mark-application-dirty (application &key (region :full))
  (setf (application-dirty-p application) t)
  (when region
    (pushnew region
             (application-damage-regions application)
             :test #'equal))
  application)

(defun clear-application-dirty (application)
  (setf (application-dirty-p application) nil)
  (setf (application-damage-regions application) nil)
  application)

(defun render-application-if-needed (application)
  (when (application-dirty-p application)
    (render-application application)))

(defun record-application-event (application kind message &key details)
  (let ((entry (list :kind kind
                     :message (string message)
                     :details details)))
    (push entry (application-event-log application))
    (when (> (length (application-event-log application))
             (application-event-log-limit application))
      (setf (application-event-log application)
            (subseq (application-event-log application)
                    0
                    (application-event-log-limit application))))
    (when (application-debug-visible-p application)
      (mark-application-dirty application))
    entry))

(defun clear-application-event-log (application)
  (setf (application-event-log application) nil)
  (mark-application-dirty application)
  application)

(defun toggle-application-debug-panel (application)
  (setf (application-debug-visible-p application)
        (not (application-debug-visible-p application)))
  (record-application-event
   application
   :debug
   (format nil "debug panel ~:[hidden~;visible~]"
           (application-debug-visible-p application)))
  (mark-application-dirty application)
  application)

(defun register-tree (registry object &optional seen)
  (let ((seen (or seen (make-hash-table :test #'equal))))
    (when (and object
               (typep object 'model-object)
               (not (gethash (object-id object) seen)))
      (setf (gethash (object-id object) seen) t)
      (register-object registry object)
      (dolist (child (children-of object))
        (register-tree registry child seen))
      (when (typep object 'code-block)
        (register-tree registry (code-block-result object) seen))
      (when (and (typep object 'reference-block)
                 (typep (reference-block-target object) 'model-object))
        (register-tree registry (reference-block-target object) seen)))))

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

(defun code-block-status-controls (model)
  (if (typep model 'code-block)
      (format nil
              "  |  Enter/e eval  |  [/] sibling  |  Left/Right depth  |  x delete  |  w wrap  |  u splice  |  v toggle structure~@[  |  ~A~]"
              (let ((info (code-block-parse-info model)))
                (when (code-block-selected-form-path model info)
                  (code-block-selection-status-line model info))))
      ""))

(defun code-block-edit-status-controls (model)
  (if (typep model 'code-block)
      "  |  Ctrl+[/] sibling  |  Ctrl+Left/Right depth  |  Ctrl+X delete  |  Ctrl+W wrap  |  Ctrl+U splice"
      ""))

(defun inspector-lines-for-model (application model)
  (let ((lines (list "Inspector"
                     (format nil "focused: ~A" (object-summary-string model))
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
                               (result-block-presentation model)))))
               (t nil)))))
    (when (and model
               (editing-active-p application)
               (string= (application-active-editor-model-id application)
                        (object-id model)))
      (let ((buffer (application-active-text-buffer application)))
        (multiple-value-bind (line column)
            (buffer-cursor-line-column buffer)
          (setf lines
                (append lines
                        (list (format nil "edit-cursor: ~D"
                                      (text-buffer-cursor buffer))
                              (format nil "edit-line: ~D" (1+ line))
                              (format nil "edit-column: ~D" (1+ column))
                              (format nil "edit-selection: ~A"
                                      (or (and (text-buffer-selected-text buffer)
                                               (preview-string
                                                (printable-string
                                                 (text-buffer-selected-text
                                                  buffer))))
                                          "-"))
                              (format nil "edit-lines: ~D"
                                      (text-buffer-line-count buffer))
                              (format nil "edit-length: ~D"
                                      (length
                                       (text-buffer-content buffer)))))))))
    lines))

(defun build-inspector-cell (application)
  (let ((inspector (make-container-cell :label "Inspector"))
        (model (focused-model application)))
    (dolist (line (inspector-lines-for-model application model) inspector)
      (append-child inspector
                    (make-text-cell :text line)))))

(defun event-log-line (entry)
  (format nil "~A: ~A~@[ | ~A~]"
          (getf entry :kind)
          (getf entry :message)
          (getf entry :details)))

(defun debug-bounds-string (bounds)
  (format nil "x=~D y=~D w=~D h=~D"
          (bounds-x bounds)
          (bounds-y bounds)
          (bounds-width bounds)
          (bounds-height bounds)))

(defun debug-cell-summary-line (cell depth)
  (let ((indent (make-string (* 2 depth) :initial-element #\Space))
        (model (cell-model cell)))
    (format nil "~A~A label=~S model=~A bounds=(~A) children=~D"
            indent
            (object-kind cell)
            (preview-string (cell-label cell) :limit 32)
            (object-summary-string model)
            (debug-bounds-string (cell-bounds cell))
            (length (children-of cell)))))

(defun debug-cell-tree-lines (root &key (limit 16))
  (if root
      (let ((lines nil)
            (emitted 0)
            (visited 0))
        (labels ((visit (cell depth)
                   (incf visited)
                   (when (< emitted limit)
                     (push (debug-cell-summary-line cell depth) lines)
                     (incf emitted))
                   (dolist (child (children-of cell))
                     (visit child (1+ depth)))))
          (visit root 0)
          (when (> visited emitted)
            (push (format nil "... ~D more cells" (- visited emitted))
                  lines))
          (nreverse lines)))
      (list "-")))

(defun focused-cell-bounds-line (application)
  (let ((cell (cell-for-model-id (application-root-cell application)
                                 (application-focused-model-id application))))
    (if cell
        (format nil "focused-cell-bounds: ~A bounds=(~A)"
                (object-summary-string (cell-model cell))
                (debug-bounds-string (cell-bounds cell)))
        "focused-cell-bounds: -")))

(defun debug-lines-for-application (application)
  (append
   (list "Debug"
         (format nil "focus-owner: ~A"
                 (object-summary-string (focused-model application)))
         (format nil "edit-owner: ~A"
                 (or (application-active-editor-model-id application) "-"))
         (format nil "viewport: ~D/~D"
                 (application-viewport-y application)
                 (application-max-viewport-y application))
         (format nil "previous-root-height: ~D"
                 (application-content-height application))
         (format nil "dirty: ~:[no~;yes~]"
                 (application-dirty-p application))
         (format nil "damage: ~A"
                 (or (application-damage-regions application) "-"))
         (focused-cell-bounds-line application)
         "Cell Tree")
   (debug-cell-tree-lines (application-root-cell application))
   (list
    "Event Trace")
   (let ((entries (subseq (application-event-log application)
                          0
                          (min 10
                               (length (application-event-log application))))))
     (if entries
         (mapcar #'event-log-line entries)
         (list "-")))))

(defun build-debug-cell (application)
  (let ((debug (make-container-cell :label "Debug")))
    (dolist (line (debug-lines-for-application application) debug)
      (append-child debug
                    (make-text-cell :text line)))))

(defun build-application-shell-cell (application)
  (let ((root (make-container-cell :label "Orra")))
    (append-child root
                  (build-workspace-cell-tree
                   (application-workspace application)))
    (append-child root (build-inspector-cell application))
    (when (application-debug-visible-p application)
      (append-child root (build-debug-cell application)))
    (append-child root
                  (make-text-cell
                   :text (application-status-line application)))
    root))

(defun rebuild-root-cell (application)
  (setf (application-root-cell application)
        (build-application-shell-cell application)))

(defun application-status-line (application)
  (let ((model (focused-model application)))
    (if (editing-active-p application)
        (format nil
                "EDIT ~A ~A  |  type to edit  |  arrows move  |  Shift+arrows select  |  PgUp/PgDn scroll  |  Ctrl+Z undo  |  Ctrl+Y redo~A  |  F12 debug  |  Esc stop"
                (if model (object-kind model) :none)
                (or (application-active-editor-model-id application) "-")
                (code-block-edit-status-controls model))
        (format nil
                "FOCUS ~A ~A  |  type to edit focused paragraph/code  |  click or Up/Down to move  |  PgUp/PgDn scroll~A  |  F12 debug  |  q quit"
                (if model (object-kind model) :none)
                (if model (object-id model) "-")
                (code-block-status-controls model)))))

(defun application-viewport-height (application)
  (backend-layout-height (application-backend application)))

(defun application-content-height (application)
  (if (application-root-cell application)
      (bounds-height (cell-bounds (application-root-cell application)))
      0))

(defun application-max-viewport-y (application)
  (max 0
       (- (application-content-height application)
          (application-viewport-height application))))

(defun clamp-application-viewport (application)
  (setf (application-viewport-y application)
        (max 0
             (min (application-viewport-y application)
                  (application-max-viewport-y application))))
  application)

(defun scroll-application (application rows)
  (let ((old-viewport-y (application-viewport-y application)))
    (incf (application-viewport-y application) rows)
    (clamp-application-viewport application)
    (unless (= old-viewport-y (application-viewport-y application))
      (mark-application-dirty application)))
  application)

(defun scroll-application-page (application direction)
  (scroll-application
   application
   (* direction
      (max 1
           (- (application-viewport-height application) 2)))))

(defun cell-for-model-id (root model-id &key editable-only)
  (labels ((matching-cell-p (cell)
             (and (cell-model cell)
                  (string= (object-id (cell-model cell)) model-id)
                  (or (not editable-only)
                      (editable-text-cell-p cell))))
           (visit (cell)
             (or (when (matching-cell-p cell)
                   cell)
                 (dolist (child (children-of cell) nil)
                   (let ((found (visit child)))
                     (when found
                       (return found)))))))
    (and root model-id (visit root))))

(defun ensure-row-range-visible (application start end &key (margin 1))
  (let* ((viewport-height (application-viewport-height application))
         (viewport-start (application-viewport-y application))
         (viewport-end (+ viewport-start viewport-height)))
    (cond
      ((< start (+ viewport-start margin))
       (setf (application-viewport-y application)
             (max 0 (- start margin))))
      ((> end (- viewport-end margin))
       (setf (application-viewport-y application)
             (- end viewport-height (- margin))))))
  (clamp-application-viewport application))

(defun ensure-cell-visible (application cell)
  (when cell
    (let ((bounds (cell-bounds cell)))
      (ensure-row-range-visible
       application
       (bounds-y bounds)
       (+ (bounds-y bounds)
          (bounds-height bounds)))))
  application)

(defun ensure-focused-model-visible (application)
  (ensure-cell-visible
   application
   (cell-for-model-id (application-root-cell application)
                      (application-focused-model-id application))))

(defun ensure-active-caret-visible (application)
  (when (editing-active-p application)
    (let ((cell (cell-for-model-id
                 (application-root-cell application)
                 (application-active-editor-model-id application)
                 :editable-only t)))
      (when cell
        (multiple-value-bind (line column)
            (buffer-cursor-line-column
             (application-active-text-buffer application))
          (declare (ignore column))
          (let ((row (+ (bounds-y (cell-bounds cell)) 1 line)))
            (ensure-row-range-visible application row (1+ row)))))))
  application)

(defun draw-application-scrollbar (application)
  (let* ((backend (application-backend application))
         (content-height (application-content-height application))
         (viewport-height (application-viewport-height application))
         (max-scroll (application-max-viewport-y application)))
    (when (plusp max-scroll)
      (let* ((track-height viewport-height)
             (thumb-height (max 1
                                (floor (* viewport-height viewport-height)
                                       content-height)))
             (thumb-y (if (plusp (- track-height thumb-height))
                          (floor (* (application-viewport-y application)
                                    (- track-height thumb-height))
                                 max-scroll)
                          0)))
        (backend-draw-scrollbar backend
                                (max 0
                                     (1- (backend-layout-width backend)))
                                0
                                track-height
                                thumb-y
                                thumb-height)))))

(defun focusable-model-object-p (object)
  (typep object
         '(or notebook section paragraph code-block quote-block
           reference-block list-block table-block task-list result-block)))

(defun visible-focusable-models (application)
  (let ((seen (make-hash-table :test #'equal))
        models)
    (labels ((visit (cell)
               (let ((model (cell-model cell)))
                 (when (and model
                            (focusable-model-object-p model)
                            (not (gethash (object-id model) seen)))
                   (setf (gethash (object-id model) seen) t)
                   (push model models)))
               (dolist (child (children-of cell))
                 (visit child))))
      (when (application-root-cell application)
        (visit (application-root-cell application))))
    (nreverse models)))

(defun ensure-valid-focus (application)
  (let ((models (visible-focusable-models application)))
    (cond
      ((null models)
       (setf (application-focused-model-id application) nil))
      ((and (application-focused-model-id application)
            (find (application-focused-model-id application)
                  models
                  :key #'object-id
                  :test #'string=))
       (application-focused-model-id application))
      (t
       (setf (application-focused-model-id application)
             (object-id (or (find-if #'editable-model-p models)
                            (first models))))))))

(defun focused-model (application)
  (let ((focused-id (application-focused-model-id application)))
    (and focused-id
         (find-object (application-registry application) focused-id))))

(defun focus-step (application direction)
  (let* ((models (visible-focusable-models application))
         (current-id (application-focused-model-id application))
         (old-viewport-y (application-viewport-y application)))
    (when models
      (stop-editing application)
      (let* ((position (or (position current-id
                                     models
                                     :key #'object-id
                                     :test #'string=)
                           0))
             (next-index (mod (+ position direction) (length models))))
        (setf (application-focused-model-id application)
              (object-id (nth next-index models)))
        (ensure-focused-model-visible application)
        (unless (and (equal current-id
                            (application-focused-model-id application))
                     (= old-viewport-y
                        (application-viewport-y application)))
          (mark-application-dirty application)))))
  application)

(defun focus-next-model (application)
  (focus-step application 1))

(defun focus-previous-model (application)
  (focus-step application -1))

(defun application-key-context (application)
  (if (editing-active-p application) :edit :focus))

(defun dispatch-application-key (application event)
  (let* ((context (application-key-context application))
         (binding (find-key-binding application
                                    context
                                    (key-event-key event)
                                    (key-event-controlp event)
                                    (key-event-shiftp event))))
    (cond
      (binding
       (record-application-event
        application
        :key
        (format nil "~A ~A -> ~A"
                context
                (key-event-key event)
                (key-binding-documentation binding)))
       (funcall (key-binding-function binding) application event))
      (t
       (let ((handledp (handle-unbound-application-key application event)))
         (record-application-event
          application
          :key
          (format nil "~A ~A ~:[unhandled~;handled as text~]"
                  context
                  (or (key-event-key event)
                      (key-event-text event)
                      "-")
                  handledp))
         handledp)))))

(defun handle-unbound-application-key (application event)
  (let ((text (key-event-text event)))
    (cond
      ((and text
            (editing-active-p application))
       (insert-into-active-buffer application text)
       t)
      ((and text
            (not (key-event-controlp event))
            (editable-model-p (focused-model application)))
       (begin-editing-focused-model application)
       (insert-into-active-buffer application text)
       t)
      (t nil))))

(defun cell-contains-point-p (cell x y)
  (let ((bounds (cell-bounds cell)))
    (and (<= (bounds-x bounds) x)
         (< x (+ (bounds-x bounds) (bounds-width bounds)))
         (<= (bounds-y bounds) y)
         (< y (+ (bounds-y bounds) (bounds-height bounds))))))

(defun find-cell-at-point (cell x y)
  (when (cell-contains-point-p cell x y)
    (or (dolist (child (reverse (children-of cell)) nil)
          (let ((found (find-cell-at-point child x y)))
            (when found
              (return found))))
        cell)))

(defun editable-text-cell-p (cell)
  (and (typep cell 'text-cell)
       (eq (cell-role cell) :editable-content)
       (cell-model cell)
       (editable-model-p (cell-model cell))))

(defun place-active-buffer-cursor-from-cell-point (application cell grid-x grid-y)
  (when (and (editing-active-p application)
             (editable-text-cell-p cell))
    (let* ((bounds (cell-bounds cell))
           (line (max 0 (- grid-y (bounds-y bounds))))
           (column (+ (cell-visible-column-offset cell line)
                      (max 0 (- grid-x (1+ (bounds-x bounds)))))))
      (move-buffer-cursor-to (application-active-text-buffer application)
                             (buffer-index-for-line-column
                              (application-active-text-buffer application)
                              line
                              column))
      (sync-active-buffer-structure-selection application)
      (mark-application-dirty application)))
  application)

(defun focus-model-at-pixel (application pixel-x pixel-y)
  (multiple-value-bind (grid-x grid-y)
      (backend-grid-point (application-backend application) pixel-x pixel-y)
    (let* ((logical-y (+ grid-y (application-viewport-y application)))
           (old-focused-id (application-focused-model-id application))
           (cell (and (application-root-cell application)
                      (find-cell-at-point
                       (application-root-cell application)
                       grid-x
                       logical-y)))
           (model (and cell (cell-model cell))))
      (when model
        (when (and (editing-active-p application)
                   (not (string=
                         (application-active-editor-model-id application)
                         (object-id model))))
          (stop-editing application))
        (cond
          ((and (editable-text-cell-p cell)
                (application-focused-model-id application)
                (string= (application-focused-model-id application)
                         (object-id model)))
           (unless (editing-active-p application)
             (begin-editing-model application model))
           (place-active-buffer-cursor-from-cell-point
            application
            cell
            grid-x
            logical-y)
           (ensure-active-caret-visible application))
          ((focusable-model-object-p model)
           (setf (application-focused-model-id application)
                 (object-id model))
           (unless (equal old-focused-id
                          (application-focused-model-id application))
             (mark-application-dirty application))))))
    application))

(defun editable-model-p (model)
  (typep model '(or paragraph code-block)))

(defun editable-model-string (model)
  (typecase model
    (paragraph (paragraph-text model))
    (code-block (code-block-source model))
    (t (error "Model ~A is not editable." model))))

(defun (setf editable-model-string) (value model)
  (typecase model
    (paragraph (setf (paragraph-text model) value))
    (code-block (replace-code-block-source model value))
    (t (error "Model ~A is not editable." model))))

(defun editing-active-p (application)
  (not (null (application-active-text-buffer application))))

(defun active-editor-model (application)
  (and (application-active-editor-model-id application)
       (find-object (application-registry application)
                    (application-active-editor-model-id application))))

(defun active-editor-model-p (application model)
  (and model
       (editing-active-p application)
       (application-active-editor-model-id application)
       (string= (application-active-editor-model-id application)
                (object-id model))))

(defun sync-active-buffer-structure-selection (application)
  (when (editing-active-p application)
    (let ((model (active-editor-model application)))
      (when (typep model 'code-block)
        (select-code-block-form-at-source-offset
         model
         (text-buffer-cursor (application-active-text-buffer application))))))
  application)

(defun string-edit-diff (old-content new-content)
  (let* ((old-content (string old-content))
         (new-content (string new-content))
         (old-length (length old-content))
         (new-length (length new-content))
         (prefix 0)
         (suffix 0)
         (prefix-limit (min old-length new-length)))
    (loop while (and (< prefix prefix-limit)
                     (char= (char old-content prefix)
                            (char new-content prefix)))
          do (incf prefix))
    (loop while (and (< suffix (- old-length prefix))
                     (< suffix (- new-length prefix))
                     (char= (char old-content (- old-length suffix 1))
                            (char new-content (- new-length suffix 1))))
          do (incf suffix))
    (values prefix
            (- old-length suffix)
            (subseq new-content prefix (- new-length suffix))
            (not (string= old-content new-content)))))

(defun sync-code-buffer-content-to-model
    (block new-content &key previous-content edit-start edit-end replacement)
  (let ((old-source (code-block-source block)))
    (unless (string= old-source new-content)
      (multiple-value-bind (dirty-start dirty-end dirty-replacement changedp)
          (cond
            ((and edit-start edit-end replacement)
             (values edit-start edit-end replacement t))
            ((and previous-content
                  (string= (string previous-content) old-source))
             (string-edit-diff previous-content new-content))
            (t
             (values 0 (length old-source) new-content t)))
        (when changedp
          (replace-code-block-source-incrementally
           block
           new-content
           dirty-start
           dirty-end
           :replacement dirty-replacement
           :previous-info (code-block-parse-info block))))))
  block)

(defun sync-active-buffer-to-model
    (application &key previous-content edit-start edit-end replacement)
  (when (editing-active-p application)
    (let ((model (active-editor-model application)))
      (when (editable-model-p model)
        (let ((content (text-buffer-content
                        (application-active-text-buffer application))))
          (if (typep model 'code-block)
              (progn
                (sync-code-buffer-content-to-model
                 model
                 content
                 :previous-content previous-content
                 :edit-start edit-start
                 :edit-end edit-end
                 :replacement replacement)
                (sync-active-buffer-structure-selection application))
              (setf (editable-model-string model) content))))))
  application)

(defun begin-editing-model (application model)
  (unless (editable-model-p model)
    (return-from begin-editing-model nil))
  (let* ((model-id (object-id model))
         (content (editable-model-string model))
         (saved-state (gethash model-id
                               (application-editor-state-table application))))
    (setf (application-focused-model-id application) model-id)
    (setf (application-active-editor-model-id application) model-id)
    (let ((buffer
           (if (and saved-state
                    (string= (getf saved-state :content) content))
               (make-text-buffer-from-state saved-state)
               (progn
                 (remhash model-id (application-editor-state-table application))
                 (make-text-buffer :content content)))))
      (when (and (typep model 'code-block)
                 (not saved-state))
        (let ((offset (code-block-selected-form-start-offset model)))
          (when offset
            (move-buffer-cursor-to buffer offset))))
      (setf (application-active-text-buffer application) buffer)
      (when (typep model 'code-block)
        (sync-active-buffer-structure-selection application))
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun begin-editing-focused-model (application)
  (begin-editing-model application (focused-model application)))

(defun remember-active-editor-state (application)
  (when (editing-active-p application)
    (setf (gethash (application-active-editor-model-id application)
                   (application-editor-state-table application))
          (text-buffer-state (application-active-text-buffer application))))
  application)

(defun clamp-editor-cursor (content cursor)
  (max 0
       (min (or cursor 0)
            (length content))))

(defun editor-state-snapshot (content cursor)
  (list content
        (clamp-editor-cursor content cursor)
        nil
        nil
        nil))

(defun editor-state-from-content (content cursor undo-stack redo-stack)
  (list :content content
        :cursor (clamp-editor-cursor content cursor)
        :undo-stack undo-stack
        :redo-stack redo-stack))

(defun sync-active-buffer-cursor-to-code-block-selection (application block)
  (when (active-editor-model-p application block)
    (move-buffer-cursor-to
     (application-active-text-buffer application)
     (or (code-block-selected-form-start-offset block)
         (length (code-block-source block))))
    (ensure-active-caret-visible application)))

(defun sync-active-buffer-with-code-block-structural-edit (application block)
  (when (active-editor-model-p application block)
    (replace-buffer-content
     (application-active-text-buffer application)
     (code-block-source block)
     :cursor (or (code-block-selected-form-start-offset block)
                 (length (code-block-source block))))))

(defun remember-code-block-structural-edit (application block previous-source
                                            &key previous-cursor)
  (let ((new-source (code-block-source block)))
    (unless (string= previous-source new-source)
      (let* ((model-id (object-id block))
             (saved-state (gethash model-id
                                   (application-editor-state-table application)))
             (matching-state-p (and saved-state
                                    (string= (getf saved-state :content "")
                                             previous-source)))
             (undo-stack (if matching-state-p
                             (copy-tree (getf saved-state :undo-stack))
                             nil))
             (historical-cursor (if matching-state-p
                                    (getf saved-state :cursor 0)
                                    previous-cursor))
             (new-cursor (or (code-block-selected-form-start-offset block)
                             (length new-source))))
        (push (editor-state-snapshot previous-source historical-cursor)
              undo-stack)
        (setf (gethash model-id
                       (application-editor-state-table application))
              (editor-state-from-content new-source
                                         new-cursor
                                         undo-stack
                                         nil)))))
  block)

(defun stop-editing (application)
  (let ((was-editing-p (editing-active-p application)))
    (sync-active-buffer-to-model application)
    (remember-active-editor-state application)
    (setf (application-active-editor-model-id application) nil)
    (setf (application-active-text-buffer application) nil)
    (when was-editing-p
      (mark-application-dirty application)))
  application)

(defun insert-into-active-buffer (application text)
  (when (editing-active-p application)
    (let* ((buffer (application-active-text-buffer application))
           (old-content (text-buffer-content buffer))
           (cursor (text-buffer-cursor buffer))
           (text (string text)))
      (multiple-value-bind (selection-start selection-end)
          (text-buffer-selection-range buffer)
        (insert-buffer-text buffer text)
        (sync-active-buffer-to-model application
                                     :previous-content old-content
                                     :edit-start (or selection-start cursor)
                                     :edit-end (or selection-end cursor)
                                     :replacement text))
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun delete-active-buffer-backward (application)
  (when (editing-active-p application)
    (let* ((buffer (application-active-text-buffer application))
           (old-content (text-buffer-content buffer))
           (cursor (text-buffer-cursor buffer)))
      (multiple-value-bind (selection-start selection-end)
          (text-buffer-selection-range buffer)
        (delete-buffer-backward buffer)
        (sync-active-buffer-to-model application
                                     :previous-content old-content
                                     :edit-start (or selection-start
                                                     (max 0 (1- cursor)))
                                     :edit-end (or selection-end cursor)
                                     :replacement ""))
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun delete-active-buffer-forward (application)
  (when (editing-active-p application)
    (let* ((buffer (application-active-text-buffer application))
           (old-content (text-buffer-content buffer))
           (cursor (text-buffer-cursor buffer)))
      (multiple-value-bind (selection-start selection-end)
          (text-buffer-selection-range buffer)
        (delete-buffer-forward buffer)
        (sync-active-buffer-to-model application
                                     :previous-content old-content
                                     :edit-start (or selection-start cursor)
                                     :edit-end (or selection-end
                                                   (min (length old-content)
                                                        (1+ cursor)))
                                     :replacement ""))
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun move-active-buffer-cursor-left (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-left (application-active-text-buffer application)
                             :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun move-active-buffer-cursor-right (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-right (application-active-text-buffer application)
                              :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun move-active-buffer-cursor-up (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-up (application-active-text-buffer application)
                           :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun move-active-buffer-cursor-down (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-down (application-active-text-buffer application)
                             :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun move-active-buffer-cursor-home (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-home (application-active-text-buffer application)
                             :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun move-active-buffer-cursor-end (application &key extend-selection)
  (when (editing-active-p application)
    (move-buffer-cursor-end (application-active-text-buffer application)
                            :extend-selection extend-selection)
    (sync-active-buffer-structure-selection application)
    (ensure-active-caret-visible application)
    (mark-application-dirty application))
  application)

(defun undo-active-buffer-edit (application)
  (when (editing-active-p application)
    (let* ((buffer (application-active-text-buffer application))
           (old-content (text-buffer-content buffer)))
      (undo-buffer-edit buffer)
      (sync-active-buffer-to-model application
                                   :previous-content old-content)
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun redo-active-buffer-edit (application)
  (when (editing-active-p application)
    (let* ((buffer (application-active-text-buffer application))
           (old-content (text-buffer-content buffer)))
      (redo-buffer-edit buffer)
      (sync-active-buffer-to-model application
                                   :previous-content old-content)
      (ensure-active-caret-visible application)
      (mark-application-dirty application)))
  application)

(defun render-application (application)
  (backend-begin-frame (application-backend application))
  (rebuild-root-cell application)
  (ensure-valid-focus application)
  (rebuild-root-cell application)
  (perform-layout (application-root-cell application)
                  :width (backend-layout-width
                          (application-backend application)))
  (clamp-application-viewport application)
  (let ((*application* application))
    (draw-cell-tree (application-backend application)
                    (application-root-cell application)
                    :viewport-y (application-viewport-y application)
                    :viewport-height (application-viewport-height application))
    (draw-application-scrollbar application))
  (backend-present (application-backend application))
  (clear-application-dirty application)
  application)

(defun ensure-code-block-result (application block)
  (or (code-block-result block)
      (setf (code-block-result block)
            (make-result-block
             :registry (application-registry application)))))

(defun store-code-block-result (application block status presentation
                                &key value)
  (let ((result (ensure-code-block-result application block)))
    (setf (result-block-value result) value)
    (setf (result-block-presentation result) presentation)
    (set-result-block-status result status)
    result))

(defun evaluate-forms (forms)
  (loop for form in forms
        collect (eval form)))

(defun evaluate-code-block (application block)
  (let ((parse-info (code-block-parse-info block)))
    (cond
      ((getf parse-info :unsupported-language)
       (store-code-block-result
        application
        block
        :error
        (format nil "No evaluator for ~A."
                (getf parse-info :unsupported-language))))
      ((getf parse-info :error)
       (store-code-block-result
        application
        block
        :error
        (code-block-parse-status-line block parse-info)))
      (t
       (handler-case
           (let* ((values (evaluate-forms (getf parse-info :forms)))
                  (value (if values (car (last values)) nil))
                  (presentation (printable-string value)))
             (store-code-block-result
              application
              block
              :ok
              presentation
              :value value))
         (error (condition)
           (store-code-block-result
            application
            block
            :error
            (format nil "Evaluation error: ~A" condition))))))))

(defun make-application (&key backend workspace save-path)
  (let* ((registry (make-object-registry))
         (workspace (or workspace (make-scratch-workspace registry)))
         (application (make-instance 'application
                                     :registry registry
                                     :commands (make-hash-table :test #'equal)
                                     :keymap (make-hash-table :test #'equal)
                                     :editor-state-table
                                     (make-hash-table :test #'equal)
                                     :workspace workspace
                                     :backend (or backend (make-null-backend))
                                     :save-path save-path)))
    (register-tree registry workspace)
    (install-defined-commands application)
    (install-defined-key-bindings application)
    (rebuild-root-cell application)
    (ensure-valid-focus application)
    (rebuild-root-cell application)
    application))

(defun start-application (application)
  (let ((*application* application))
    (run-backend (application-backend application)
                 application))
  application)

(defun load-workspace-into-application (application path)
  (let ((registry (make-object-registry)))
    (setf (application-registry application) registry)
    (setf (application-editor-state-table application)
          (make-hash-table :test #'equal))
    (setf (application-workspace application)
          (load-workspace-from-file path :registry registry))
    (install-defined-commands application)
    (setf (application-save-path application) path)
    (setf (application-viewport-y application) 0)
    (rebuild-root-cell application)
    (ensure-valid-focus application)
    (rebuild-root-cell application)
    application))

(defun save-runtime-image (path)
  #+sbcl
  (sb-ext:save-lisp-and-die path :toplevel #'start-demo :executable t)
  #-sbcl
  (error "Saving executable images is currently only implemented for SBCL."))

(defun quit-application (application)
  (declare (ignore application))
  (sdl2:push-event :quit))

(defun evaluate-focused-code-block (application)
  (let ((model (focused-model application)))
    (when (typep model 'code-block)
      (when (editing-active-p application)
        (stop-editing application))
      (evaluate-code-block application model)
      (rebuild-root-cell application)
      model)))

(defun toggle-focused-code-structure (application)
  (let ((model (focused-model application)))
    (when (typep model 'code-block)
      (when (editing-active-p application)
        (stop-editing application))
      (toggle-code-block-structure model)
      (rebuild-root-cell application)
      model)))

(defun step-focused-code-form-selection (application direction)
  (let ((model (focused-model application)))
    (when (typep model 'code-block)
      (when (and (editing-active-p application)
                 (not (active-editor-model-p application model)))
        (stop-editing application))
      (let ((info (code-block-parse-info model)))
        (if (minusp direction)
            (select-previous-code-block-form model info)
            (select-next-code-block-form model info)))
      (sync-active-buffer-cursor-to-code-block-selection application model)
      (rebuild-root-cell application)
      model)))

(defun shift-focused-code-form-depth (application direction)
  (let ((model (focused-model application)))
    (when (typep model 'code-block)
      (when (and (editing-active-p application)
                 (not (active-editor-model-p application model)))
        (stop-editing application))
      (let ((info (code-block-parse-info model)))
        (if (minusp direction)
            (select-parent-code-block-form model info)
            (select-child-code-block-form model info)))
      (sync-active-buffer-cursor-to-code-block-selection application model)
      (rebuild-root-cell application)
      model)))

(defun edit-code-block-structurally (application block operator)
  (when (typep block 'code-block)
    (let ((editing-active-model-p (active-editor-model-p application block)))
      (when (and (editing-active-p application)
                 (not editing-active-model-p))
        (stop-editing application))
      (let ((previous-source (code-block-source block))
            (previous-cursor (if editing-active-model-p
				 (text-buffer-cursor
                                  (application-active-text-buffer application))
				 (or (code-block-selected-form-start-offset block)
                                     (length (code-block-source block))))))
	(funcall operator block)
	(if editing-active-model-p
            (sync-active-buffer-with-code-block-structural-edit
             application
             block)
            (remember-code-block-structural-edit application
						 block
						 previous-source
						 :previous-cursor previous-cursor))
	(rebuild-root-cell application)
	block))))

(defun edit-focused-code-form-structurally (application operator)
  (edit-code-block-structurally application
                                (focused-model application)
                                operator))

(define-key-binding (:edit :z :control t)
    (application event)
  "Undo the active text edit."
  (declare (ignore event))
  (undo-active-buffer-edit application)
  t)

(define-key-binding (:edit :z :control t :shift t)
    (application event)
  "Redo the active text edit."
  (declare (ignore event))
  (redo-active-buffer-edit application)
  t)

(define-key-binding (:edit :y :control t)
    (application event)
  "Redo the active text edit."
  (declare (ignore event))
  (redo-active-buffer-edit application)
  t)

(define-key-binding (:edit :leftbracket :control t)
    (application event)
  "Move to the previous structural code form while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (step-focused-code-form-selection application -1)
    t))

(define-key-binding (:edit :rightbracket :control t)
    (application event)
  "Move to the next structural code form while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (step-focused-code-form-selection application 1)
    t))

(define-key-binding (:edit :left :control t)
    (application event)
  "Move code structural selection to its parent while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (shift-focused-code-form-depth application -1)
    t))

(define-key-binding (:edit :right :control t)
    (application event)
  "Move code structural selection to its child while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (shift-focused-code-form-depth application 1)
    t))

(define-key-binding (:edit :x :control t)
    (application event)
  "Delete the selected structural code form while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'delete-selected-code-block-form)
    t))

(define-key-binding (:edit :w :control t)
    (application event)
  "Wrap the selected structural code form while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'wrap-selected-code-block-form)
    t))

(define-key-binding (:edit :u :control t)
    (application event)
  "Splice the selected structural code form while editing."
  (declare (ignore event))
  (when (typep (active-editor-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'splice-selected-code-block-form)
    t))

(define-key-binding (:edit :escape)
    (application event)
  "Stop editing."
  (declare (ignore event))
  (stop-editing application)
  t)

(define-key-binding (:edit :backspace)
    (application event)
  "Delete backward in the active text buffer."
  (declare (ignore event))
  (delete-active-buffer-backward application)
  t)

(define-key-binding (:edit :delete)
    (application event)
  "Delete forward in the active text buffer."
  (declare (ignore event))
  (delete-active-buffer-forward application)
  t)

(define-key-binding (:edit :home :shift :any)
    (application event)
  "Move to the beginning of the line."
  (move-active-buffer-cursor-home application
                                  :extend-selection
                                  (key-event-shiftp event))
  t)

(define-key-binding (:edit :end :shift :any)
    (application event)
  "Move to the end of the line."
  (move-active-buffer-cursor-end application
                                 :extend-selection
                                 (key-event-shiftp event))
  t)

(define-key-binding (:edit :pageup)
    (application event)
  "Scroll up one page while editing."
  (declare (ignore event))
  (scroll-application-page application -1)
  t)

(define-key-binding (:edit :pagedown)
    (application event)
  "Scroll down one page while editing."
  (declare (ignore event))
  (scroll-application-page application 1)
  t)

(define-key-binding (:edit :left :shift :any)
    (application event)
  "Move the active cursor left."
  (move-active-buffer-cursor-left application
                                  :extend-selection
                                  (key-event-shiftp event))
  t)

(define-key-binding (:edit :right :shift :any)
    (application event)
  "Move the active cursor right."
  (move-active-buffer-cursor-right application
                                   :extend-selection
                                   (key-event-shiftp event))
  t)

(define-key-binding (:edit :up :shift :any)
    (application event)
  "Move the active cursor up."
  (move-active-buffer-cursor-up application
                                :extend-selection
                                (key-event-shiftp event))
  t)

(define-key-binding (:edit :down :shift :any)
    (application event)
  "Move the active cursor down."
  (move-active-buffer-cursor-down application
                                  :extend-selection
                                  (key-event-shiftp event))
  t)

(define-key-binding (:edit :return)
    (application event)
  "Insert a newline."
  (declare (ignore event))
  (insert-into-active-buffer application (string #\Newline))
  t)

(define-key-binding (:edit :f12)
    (application event)
  "Toggle debug panel."
  (declare (ignore event))
  (toggle-application-debug-panel application)
  t)

(define-key-binding (:focus :leftbracket)
    (application event)
  "Move to the previous structural code form."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (step-focused-code-form-selection application -1)
    t))

(define-key-binding (:focus :rightbracket)
    (application event)
  "Move to the next structural code form."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (step-focused-code-form-selection application 1)
    t))

(define-key-binding (:focus :left)
    (application event)
  "Move code structural selection to its parent."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (shift-focused-code-form-depth application -1)
    t))

(define-key-binding (:focus :right)
    (application event)
  "Move code structural selection to its child."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (shift-focused-code-form-depth application 1)
    t))

(define-key-binding (:focus :x)
    (application event)
  "Delete the selected structural code form."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'delete-selected-code-block-form)
    t))

(define-key-binding (:focus :w)
    (application event)
  "Wrap the selected structural code form."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'wrap-selected-code-block-form)
    t))

(define-key-binding (:focus :u)
    (application event)
  "Splice the selected structural code form."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (edit-focused-code-form-structurally
     application
     #'splice-selected-code-block-form)
    t))

(define-key-binding (:focus :v)
    (application event)
  "Toggle structural code preview."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (toggle-focused-code-structure application)
    t))

(define-key-binding (:focus :e)
    (application event)
  "Evaluate the focused code block."
  (declare (ignore event))
  (when (typep (focused-model application) 'code-block)
    (evaluate-focused-code-block application)
    t))

(define-key-binding (:focus :q)
    (application event)
  "Quit the application."
  (declare (ignore event))
  (quit-application application)
  t)

(define-key-binding (:focus :escape)
    (application event)
  "Quit the application."
  (declare (ignore event))
  (quit-application application)
  t)

(define-key-binding (:focus :j)
    (application event)
  "Focus the next model."
  (declare (ignore event))
  (focus-next-model application)
  t)

(define-key-binding (:focus :down)
    (application event)
  "Focus the next model."
  (declare (ignore event))
  (focus-next-model application)
  t)

(define-key-binding (:focus :k)
    (application event)
  "Focus the previous model."
  (declare (ignore event))
  (focus-previous-model application)
  t)

(define-key-binding (:focus :up)
    (application event)
  "Focus the previous model."
  (declare (ignore event))
  (focus-previous-model application)
  t)

(define-key-binding (:focus :pageup)
    (application event)
  "Scroll up one page."
  (declare (ignore event))
  (scroll-application-page application -1)
  t)

(define-key-binding (:focus :pagedown)
    (application event)
  "Scroll down one page."
  (declare (ignore event))
  (scroll-application-page application 1)
  t)

(define-key-binding (:focus :i)
    (application event)
  "Begin editing the focused model."
  (declare (ignore event))
  (begin-editing-focused-model application)
  t)

(define-key-binding (:focus :return)
    (application event)
  "Begin editing a paragraph or evaluate code."
  (declare (ignore event))
  (cond
    ((typep (focused-model application) 'paragraph)
     (begin-editing-focused-model application)
     t)
    ((typep (focused-model application) 'code-block)
     (evaluate-focused-code-block application)
     t)))

(define-key-binding (:focus :s)
    (application event)
  "Save the current workspace."
  (declare (ignore event))
  (invoke-command application 'save-workspace)
  t)

(define-key-binding (:focus :r)
    (application event)
  "Render the application."
  (declare (ignore event))
  (render-application application)
  t)

(define-key-binding (:focus :f12)
    (application event)
  "Toggle debug panel."
  (declare (ignore event))
  (toggle-application-debug-panel application)
  t)

(define-command render (application)
  "Rebuild and render the current cell tree."
  (render-application application))

(define-command scroll (application rows)
  "Scroll the application viewport by ROWS logical rows."
  (scroll-application application rows))

(define-command scroll-page (application direction)
  "Scroll the application viewport by one page in DIRECTION."
  (scroll-application-page application direction))

(define-command list-commands (application)
  "Return the installed commands."
  (list-commands application))

(define-command list-key-bindings (application)
  "Return the installed key bindings."
  (list-key-bindings application))

(define-command toggle-debug-panel (application)
  "Toggle the in-application debug panel."
  (toggle-application-debug-panel application))

(define-command clear-event-log (application)
  "Clear the in-application event log."
  (clear-application-event-log application))

(define-command append-paragraph (application text)
  "Append a new paragraph to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (paragraph (make-paragraph :text text :registry registry)))
    (append-child section paragraph)
    (rebuild-root-cell application)
    paragraph))

(define-command append-code-block (application source &optional (language :common-lisp))
  "Append a new code block to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (block (make-code-block
                 :language language
                 :source source
                 :registry registry)))
    (append-child section block)
    (rebuild-root-cell application)
    block))

(define-command append-quote-block (application text &optional (attribution ""))
  "Append a quote block to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (block (make-quote-block
                 :text text
                 :attribution attribution
                 :registry registry)))
    (append-child section block)
    (rebuild-root-cell application)
    block))

(define-command append-reference-block
    (application target-id &optional (label "") (note ""))
  "Append an object reference block to the default section."
  (let* ((registry (application-registry application))
         (target (find-object registry target-id)))
    (unless target
      (error "Unknown reference target ~A." target-id))
    (let* ((section (ensure-default-section
                     (application-workspace application)
                     registry))
           (block (make-reference-block
                   :target target
                   :label label
                   :note note
                   :registry registry)))
      (append-child section block)
      (rebuild-root-cell application)
      block)))

(define-command append-list-block (application items &optional ordered-p)
  "Append a list block to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (block (make-list-block
                 :items items
                 :ordered-p ordered-p
                 :registry registry)))
    (append-child section block)
    (rebuild-root-cell application)
    block))

(define-command append-table-block (application columns rows)
  "Append a table block to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (block (make-table-block
                 :columns columns
                 :rows rows
                 :registry registry)))
    (append-child section block)
    (rebuild-root-cell application)
    block))

(define-command append-task-list (application items)
  "Append a task list to the default section."
  (let* ((registry (application-registry application))
         (section (ensure-default-section
                   (application-workspace application)
                   registry))
         (block (make-task-list
                 :items items
                 :registry registry)))
    (append-child section block)
    (rebuild-root-cell application)
    block))

(define-command evaluate-code-block (application block-id)
  "Evaluate a code block by object id."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (prog1 (evaluate-code-block application block)
      (rebuild-root-cell application))))

(define-command toggle-code-structure (application block-id)
  "Toggle the structural code preview for a code block."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (prog1 (toggle-code-block-structure block)
      (rebuild-root-cell application))))

(define-command select-previous-code-form (application block-id)
  "Move the structural selection to the previous sibling form."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (let ((info (code-block-parse-info block)))
      (select-previous-code-block-form block info))
    (sync-active-buffer-cursor-to-code-block-selection application block)
    (rebuild-root-cell application)
    block))

(define-command select-next-code-form (application block-id)
  "Move the structural selection to the next sibling form."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (let ((info (code-block-parse-info block)))
      (select-next-code-block-form block info))
    (sync-active-buffer-cursor-to-code-block-selection application block)
    (rebuild-root-cell application)
    block))

(define-command select-child-code-form (application block-id)
  "Move the structural selection to the first child of the current form."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (let ((info (code-block-parse-info block)))
      (select-child-code-block-form block info))
    (sync-active-buffer-cursor-to-code-block-selection application block)
    (rebuild-root-cell application)
    block))

(define-command select-parent-code-form (application block-id)
  "Move the structural selection to the parent of the current form."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (let ((info (code-block-parse-info block)))
      (select-parent-code-block-form block info))
    (sync-active-buffer-cursor-to-code-block-selection application block)
    (rebuild-root-cell application)
    block))

(define-command delete-code-form (application block-id)
  "Delete the selected structural form from a code block."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (edit-code-block-structurally application
                                  block
                                  #'delete-selected-code-block-form)))

(define-command wrap-code-form (application block-id)
  "Wrap the selected structural form in PROGN."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (edit-code-block-structurally application
                                  block
                                  #'wrap-selected-code-block-form)))

(define-command splice-code-form (application block-id)
  "Splice the selected PROGN/LOCALLY body into its surrounding sibling sequence."
  (let ((block (find-object (application-registry application) block-id)))
    (unless (typep block 'code-block)
      (error "Object ~A is not a code block." block-id))
    (edit-code-block-structurally application
                                  block
                                  #'splice-selected-code-block-form)))

(define-command save-workspace (application &optional path)
  "Persist the current workspace to disk."
  (let ((target (or path
                    (application-save-path application)
                    "workspace.sexp")))
    (setf (application-save-path application) target)
    (save-workspace-to-file (application-workspace application)
                            target
                            :registry (application-registry application))))

(define-command load-workspace (application path)
  "Load a workspace from disk and replace the current one."
  (load-workspace-into-application application path))

(defun start-demo (&key backend)
  (start-application
   (make-application
    :backend (or backend
                 (make-sdl2-backend)))))
