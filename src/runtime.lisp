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

(defun preview-string (string &key (limit 48))
  (let ((string (string string)))
    (if (<= (length string) limit)
        string
        (format nil "~A..." (subseq string 0 (max 0 (- limit 3)))))))

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
                      (format nil "result: ~A"
                              (if (code-block-result model)
                                  (object-summary-string
                                   (code-block-result model))
                                  "-"))))
               (result-block
                (list (format nil "presentation: ~A"
                              (preview-string
                               (result-block-presentation model)))))
               (t nil)))))
    (when (and model
               (editing-active-p application)
               (string= (application-active-editor-model-id application)
                        (object-id model)))
      (let ((buffer (application-active-text-buffer application)))
        (setf lines
              (append lines
                      (list (format nil "edit-cursor: ~D"
                                    (text-buffer-cursor buffer))
                            (format nil "edit-length: ~D"
                                    (length
                                     (text-buffer-content buffer))))))))
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
                "EDIT ~A ~A  |  type to edit  |  Left/Right move  |  Backspace/Delete remove  |  Enter newline  |  Esc stop"
                (if model (object-kind model) :none)
                (or (application-active-editor-model-id application) "-"))
        (format nil
                "FOCUS ~A ~A  |  type to edit focused paragraph/code  |  click or Up/Down to move  |  Enter or e to eval code  |  q quit"
                (if model (object-kind model) :none)
                (if model (object-id model) "-")))))

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

(defun find-focusable-cell-at-point (cell x y)
  (when (cell-contains-point-p cell x y)
    (or (dolist (child (reverse (children-of cell)) nil)
          (let ((found (find-focusable-cell-at-point child x y)))
            (when found
              (return found))))
        (when (and (cell-model cell)
                   (focusable-model-object-p (cell-model cell)))
          cell))))

(defun focus-model-at-pixel (application pixel-x pixel-y)
  (multiple-value-bind (grid-x grid-y)
      (backend-grid-point (application-backend application) pixel-x pixel-y)
    (let ((cell (and (application-root-cell application)
                     (find-focusable-cell-at-point
                      (application-root-cell application)
                      grid-x
                      grid-y))))
      (when cell
        (when (and (editing-active-p application)
                   (not (string=
                         (application-active-editor-model-id application)
                         (object-id (cell-model cell)))))
          (stop-editing application))
        (if (and (editable-model-p (cell-model cell))
                 (application-focused-model-id application)
                 (string= (application-focused-model-id application)
                          (object-id (cell-model cell)))
                 (not (editing-active-p application)))
            (begin-editing-model application (cell-model cell))
            (setf (application-focused-model-id application)
                  (object-id (cell-model cell)))))))
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
  (setf (application-focused-model-id application) (object-id model))
  (setf (application-active-editor-model-id application) (object-id model))
  (setf (application-active-text-buffer application)
        (make-text-buffer :content (editable-model-string model)))
  application)

(defun begin-editing-focused-model (application)
  (begin-editing-model application (focused-model application)))

(defun stop-editing (application)
  (sync-active-buffer-to-model application)
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

(defun evaluate-forms (source)
  (let (values)
    (with-input-from-string (stream source)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (push (eval form) values)))
    (nreverse values)))

(defun evaluate-code-block (application block)
  (let* ((values (evaluate-forms (code-block-source block)))
         (value (if values (car (last values)) nil))
         (presentation (printable-string value))
         (result (or (code-block-result block)
                     (make-result-block
                      :registry (application-registry application)))))
    (setf (result-block-value result) value)
    (setf (result-block-presentation result) presentation)
    (setf (code-block-result block) result)
    result))

(defun make-application (&key backend workspace save-path)
  (let* ((registry (make-object-registry))
         (workspace (or workspace (make-scratch-workspace registry)))
         (application (make-instance 'application
                                     :registry registry
                                     :commands (make-hash-table :test #'equal)
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
