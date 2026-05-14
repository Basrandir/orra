(in-package :orra)

(defvar *application* nil)

(defclass application ()
  ((registry
    :initarg :registry
    :accessor application-registry)
   (commands
    :initarg :commands
    :accessor application-commands)
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
   (save-path
    :initarg :save-path
    :accessor application-save-path
    :initform nil)))

(defun register-tree (registry object)
  (when (and object (typep object 'model-object))
    (register-object registry object)
    (dolist (child (children-of object))
      (register-tree registry child))
    (when (typep object 'code-block)
      (register-tree registry (code-block-result object)))))

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
      "  |  Enter/e eval  |  v toggle structure"
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
                      (format nil "parse: ~A"
                              (code-block-parse-status-line model))
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

(defun build-application-shell-cell (application)
  (let ((root (make-container-cell :label "Orra")))
    (append-child root
                  (build-workspace-cell-tree
                   (application-workspace application)))
    (append-child root (build-inspector-cell application))
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
                "EDIT ~A ~A  |  type to edit  |  Left/Right/Up/Down move  |  Home/End line bounds  |  Ctrl+Z undo  |  Ctrl+Y redo  |  Esc stop"
                (if model (object-kind model) :none)
                (or (application-active-editor-model-id application) "-"))
        (format nil
                "FOCUS ~A ~A  |  type to edit focused paragraph/code  |  click or Up/Down to move~A  |  q quit"
                (if model (object-kind model) :none)
                (if model (object-id model) "-")
                (code-block-status-controls model)))))

(defun focusable-model-object-p (object)
  (typep object '(or notebook section paragraph code-block result-block)))

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
         (current-id (application-focused-model-id application)))
    (when models
      (stop-editing application)
      (let* ((position (or (position current-id
                                     models
                                     :key #'object-id
                                     :test #'string=)
                           0))
             (next-index (mod (+ position direction) (length models))))
        (setf (application-focused-model-id application)
              (object-id (nth next-index models))))))
  application)

(defun focus-next-model (application)
  (focus-step application 1))

(defun focus-previous-model (application)
  (focus-step application -1))

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
                              column))))
  application)

(defun focus-model-at-pixel (application pixel-x pixel-y)
  (multiple-value-bind (grid-x grid-y)
      (backend-grid-point (application-backend application) pixel-x pixel-y)
    (let* ((cell (and (application-root-cell application)
                      (find-cell-at-point
                       (application-root-cell application)
                       grid-x
                       grid-y)))
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
            grid-y))
          ((focusable-model-object-p model)
           (setf (application-focused-model-id application)
                 (object-id model)))))))
  application)

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
    (code-block (setf (code-block-source model) value))
    (t (error "Model ~A is not editable." model))))

(defun editing-active-p (application)
  (not (null (application-active-text-buffer application))))

(defun sync-active-buffer-to-model (application)
  (when (editing-active-p application)
    (let ((model (find-object (application-registry application)
                              (application-active-editor-model-id application))))
      (when (editable-model-p model)
        (setf (editable-model-string model)
              (text-buffer-content (application-active-text-buffer application))))))
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
    (setf (application-active-text-buffer application)
          (if (and saved-state
                   (string= (getf saved-state :content) content))
              (make-text-buffer-from-state saved-state)
              (progn
                (remhash model-id (application-editor-state-table application))
                (make-text-buffer :content content)))))
  application)

(defun begin-editing-focused-model (application)
  (begin-editing-model application (focused-model application)))

(defun remember-active-editor-state (application)
  (when (editing-active-p application)
    (setf (gethash (application-active-editor-model-id application)
                   (application-editor-state-table application))
          (text-buffer-state (application-active-text-buffer application))))
  application)

(defun stop-editing (application)
  (sync-active-buffer-to-model application)
  (remember-active-editor-state application)
  (setf (application-active-editor-model-id application) nil)
  (setf (application-active-text-buffer application) nil)
  application)

(defun insert-into-active-buffer (application text)
  (when (editing-active-p application)
    (insert-buffer-text (application-active-text-buffer application) text)
    (sync-active-buffer-to-model application))
  application)

(defun delete-active-buffer-backward (application)
  (when (editing-active-p application)
    (delete-buffer-backward (application-active-text-buffer application))
    (sync-active-buffer-to-model application))
  application)

(defun delete-active-buffer-forward (application)
  (when (editing-active-p application)
    (delete-buffer-forward (application-active-text-buffer application))
    (sync-active-buffer-to-model application))
  application)

(defun move-active-buffer-cursor-left (application)
  (when (editing-active-p application)
    (move-buffer-cursor-left (application-active-text-buffer application)))
  application)

(defun move-active-buffer-cursor-right (application)
  (when (editing-active-p application)
    (move-buffer-cursor-right (application-active-text-buffer application)))
  application)

(defun move-active-buffer-cursor-up (application)
  (when (editing-active-p application)
    (move-buffer-cursor-up (application-active-text-buffer application)))
  application)

(defun move-active-buffer-cursor-down (application)
  (when (editing-active-p application)
    (move-buffer-cursor-down (application-active-text-buffer application)))
  application)

(defun move-active-buffer-cursor-home (application)
  (when (editing-active-p application)
    (move-buffer-cursor-home (application-active-text-buffer application)))
  application)

(defun move-active-buffer-cursor-end (application)
  (when (editing-active-p application)
    (move-buffer-cursor-end (application-active-text-buffer application)))
  application)

(defun undo-active-buffer-edit (application)
  (when (editing-active-p application)
    (undo-buffer-edit (application-active-text-buffer application))
    (sync-active-buffer-to-model application))
  application)

(defun redo-active-buffer-edit (application)
  (when (editing-active-p application)
    (redo-buffer-edit (application-active-text-buffer application))
    (sync-active-buffer-to-model application))
  application)

(defun render-application (application)
  (backend-begin-frame (application-backend application))
  (rebuild-root-cell application)
  (ensure-valid-focus application)
  (rebuild-root-cell application)
  (perform-layout (application-root-cell application)
                  :width (backend-layout-width
                          (application-backend application)))
  (draw-cell-tree (application-backend application)
                  (application-root-cell application))
  (backend-present (application-backend application))
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
                                     :editor-state-table
                                     (make-hash-table :test #'equal)
                                     :workspace workspace
                                     :backend (or backend (make-null-backend))
                                     :save-path save-path)))
    (register-tree registry workspace)
    (install-defined-commands application)
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

(define-command render (application)
  "Rebuild and render the current cell tree."
  (render-application application))

(define-command list-commands (application)
  "Return the installed commands."
  (list-commands application))

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
