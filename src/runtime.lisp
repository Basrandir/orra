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

(defun rebuild-root-cell (application)
  (setf (application-root-cell application)
        (build-workspace-cell-tree
         (application-workspace application))))

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
             (object-id (first models)))))))

(defun focused-model (application)
  (let ((focused-id (application-focused-model-id application)))
    (and focused-id
         (find-object (application-registry application) focused-id))))

(defun focus-step (application direction)
  (let* ((models (visible-focusable-models application))
         (current-id (application-focused-model-id application)))
    (when models
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
        (setf (application-focused-model-id application)
              (object-id (cell-model cell))))))
  application)

(defun render-application (application)
  (backend-begin-frame (application-backend application))
  (rebuild-root-cell application)
  (perform-layout (application-root-cell application)
                  :width (backend-layout-width
                          (application-backend application)))
  (ensure-valid-focus application)
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
