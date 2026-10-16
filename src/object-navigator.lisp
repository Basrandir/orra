(in-package :orra)

(defclass object-navigator ()
  ((query-buffer :accessor navigator-query-buffer :initform (make-text-buffer))
   (selected-object :accessor navigator-selected-object :initform nil)
   (root-cell :accessor navigator-root-cell :initform nil)
   (viewport-y :accessor navigator-viewport-y :initform 0)
   (reveal :accessor navigator-reveal :initform :input)))

(defgeneric navigation-object-title (object)
  (:method ((object model-object)) (string-downcase (string (object-kind object))))
  (:method ((object workspace)) (workspace-title object))
  (:method ((object notebook)) (notebook-title object))
  (:method ((object section)) (section-title object))
  (:method ((object knowledge-item)) (knowledge-item-title object))
  (:method ((object paragraph)) (paragraph-text object))
  (:method ((object code-block)) (code-block-source object))
  (:method ((object quote-block)) (quote-block-text object))
  (:method ((object repl-block)) (repl-block-title object))
  (:method ((object repl-entry)) (repl-entry-input-source object))
  (:method ((object result-block)) (result-block-presentation object))
  (:method ((object reference-block)) (reference-block-label object))
  (:method ((object link-browser-block)) (link-browser-block-label object))
  (:method ((object inspector-block)) (inspector-block-label object))
  (:method ((object source-browser-block)) (source-browser-block-label object))
  (:method ((object cross-reference-browser-block)) (cross-reference-browser-block-label object))
  (:method ((object stack-frame-browser-block)) (stack-frame-browser-block-label object))
  (:method ((object condition-browser-block)) (condition-browser-block-label object)))

(defun navigation-preview (text)
  (substitute #\Space #\Return
              (substitute #\Space #\Tab
                          (substitute #\Space #\Newline (preview-string text :limit 64)))))

(defun object-navigator-entries (application)
  (let ((entries nil))
    (map-workspace-content
     (lambda (object notebook ancestors)
       (when (and notebook (focusable-model-object-p object))
         (let* ((title (navigation-object-title object))
                (path-objects (remove-if-not (lambda (ancestor)
                                               (typep ancestor 'composite-node)) ancestors))
                (path (format nil "~{~A~^ / ~}"
                              (mapcar (lambda (ancestor)
                                        (navigation-preview (navigation-object-title ancestor)))
                                      path-objects)))
                (label (format nil "[~A] ~A~@[ | ~A~] (~A)"
                               (string-downcase (string (object-kind object)))
                               (navigation-preview title) (unless (string= "" path) path)
                               (object-id object))))
           (push (list :object object :notebook notebook :label label
                       :search-text (format nil "~A ~A ~A ~{~A~^ ~}" title label
                                            (if (typep object 'knowledge-item) (knowledge-item-summary object) "")
                                            (mapcar #'navigation-object-title path-objects))) entries))))
     (application-workspace application))
    (nreverse entries)))

(defun object-navigator-matches (application)
  (let* ((navigator (application-object-navigator application))
         (terms (remove "" (uiop:split-string
                            (text-buffer-content (navigator-query-buffer navigator))
                            :separator '(#\Space #\Tab #\Newline #\Return)) :test #'string=)))
    (remove-if-not (lambda (entry)
                     (every (lambda (term) (search term (getf entry :search-text) :test #'char-equal)) terms))
                   (object-navigator-entries application))))

(defun selected-navigator-entry (application &optional (entries (object-navigator-matches application)))
  (let* ((navigator (application-object-navigator application))
         (selected (navigator-selected-object navigator))
         (entry (or (find selected entries :key (lambda (entry) (getf entry :object))) (first entries))))
    (setf (navigator-selected-object navigator) (getf entry :object))
    entry))

(defun open-object-navigator (application)
  (unless (or (application-pending-action application)
              (application-quit-requested-p application)
              (application-command-palette application)
              (application-object-navigator application))
    (setf (application-object-navigator application) (make-instance 'object-navigator))
    (setf (navigator-selected-object (application-object-navigator application)) (focused-model application))
    (selected-navigator-entry application)
    (mark-application-dirty application))
  application)

(defun close-object-navigator (application)
  (setf (application-object-navigator application) nil)
  (mark-application-dirty application)
  application)

(defun edit-object-navigator (application function)
  (let* ((navigator (application-object-navigator application))
         (buffer (navigator-query-buffer navigator))
         (before (text-buffer-content buffer)))
    (funcall function buffer)
    (unless (string= before (text-buffer-content buffer))
      (selected-navigator-entry application))
    (setf (navigator-reveal navigator) :input)
    (mark-application-dirty application))
  application)

(defun step-object-navigator (application steps)
  (let* ((navigator (application-object-navigator application))
         (entries (object-navigator-matches application))
         (index (position (selected-navigator-entry application entries) entries)))
    (when index
      (setf (navigator-selected-object navigator)
            (getf (nth (max 0 (min (+ index steps) (1- (length entries)))) entries) :object)
            (navigator-reveal navigator) :selection)
      (mark-application-dirty application)))
  application)

(defun accept-object-navigator (application)
  (let* ((navigator (application-object-navigator application))
         (target (navigator-selected-object navigator)))
    (when target
      (handler-case
          (progn
            ;; Revalidate the displayed instance; never jump to an unrelated first match.
            (unless (find target (object-navigator-matches application)
                          :key (lambda (entry) (getf entry :object)))
              (error "The selected object changed or was removed; choose it again."))
            (multiple-value-bind (focused status) (focus-linked-object application target)
              (unless focused (error "Cannot navigate to this object: ~A." status)))
            (close-object-navigator application)
            (close-application-keyboard-help application))
        (error (condition)
          (setf (navigator-reveal navigator) :input)
          (report-application-io-error application "Navigation" condition)))))
  application)

(defun navigator-max-viewport-y (application)
  (let ((root (navigator-root-cell (application-object-navigator application))))
    (max 0 (- (if root (bounds-height (cell-bounds root)) 0)
              (application-viewport-height application)))))

(defun scroll-object-navigator (application rows)
  (let ((navigator (application-object-navigator application)))
    (unless (navigator-root-cell navigator) (refresh-object-navigator application))
    (setf (navigator-reveal navigator) nil
          (navigator-viewport-y navigator)
          (max 0 (min (+ rows (navigator-viewport-y navigator))
                      (navigator-max-viewport-y application))))
    (mark-application-dirty application))
  application)

(defun refresh-object-navigator (application)
  (let* ((navigator (application-object-navigator application))
         (entries (object-navigator-matches application))
         (old-selection (navigator-selected-object navigator))
         (selected (selected-navigator-entry application entries))
         (root (make-container-cell :label "Workspace navigator"))
         (input (make-text-cell :text (text-buffer-content (navigator-query-buffer navigator))
				:editor-buffer (navigator-query-buffer navigator) :role :navigator-input))
         (selection-cell nil))
    (unless (eq old-selection (getf selected :object))
      (setf (navigator-reveal navigator) :input))
    (labels ((line (text)
               (append-child root (make-text-cell :text text :role :metadata :wrap-p t))))
      (line "Workspace navigator")
      (line "Search titles, text, types, notebook paths, or ids. Enter focuses without evaluating.")
      (line "Up/Down selects; Esc/F3 cancels. Search includes all notebooks and existing results.")
      (append-child root input)
      (line (format nil "~D matching object~:P" (length entries)))
      (unless entries (line "No matches. Change the search or press Esc to return."))
      (dolist (entry entries)
        (let ((cell (line (format nil "~A ~A" (if (eq entry selected) ">" " ") (getf entry :label)))))
          (when (eq entry selected) (setf selection-cell cell))))
      (line "Navigator controls")
      (dolist (text (key-binding-help-lines application :navigate)) (line text)))
    (perform-layout root :width (backend-layout-width (application-backend application))
                    :backend (application-backend application))
    (setf (navigator-root-cell navigator) root)
    (let* ((target (if (eq :selection (navigator-reveal navigator)) selection-cell input))
           (row (when target
                  (+ 1 (bounds-y (cell-bounds target))
                     (if (eq target input)
                         (* (cell-line-step input)
                            (nth-value 0 (string-line-column
                                          (text-buffer-content (navigator-query-buffer navigator))
                                          (text-buffer-cursor (navigator-query-buffer navigator))))) 0))))
           (height (application-viewport-height application)))
      (when (and row (navigator-reveal navigator))
        (setf (navigator-viewport-y navigator) (max 0 (min (navigator-viewport-y navigator) (1- row))))
        (when (> (+ row (cell-line-step target)) (+ (navigator-viewport-y navigator) height))
          (setf (navigator-viewport-y navigator) (+ (cell-line-step target) (- row height)))))
      (setf (navigator-viewport-y navigator)
            (max 0 (min (navigator-viewport-y navigator) (navigator-max-viewport-y application)))
            (navigator-reveal navigator) nil))
    application))

(define-command open-object-navigator (application)
  "Search notebooks and their contained objects, then focus without evaluating."
  (open-object-navigator application))

(dolist (context '(:global :help))
  (register-key-binding
   (make-instance 'key-binding :context context :key :f3
                  :documentation "Search notebooks and objects."
                  :function (lambda (application event)
                              (declare (ignore event))
                              (open-object-navigator application)
                              t))))

(dolist (key '(:escape :f3))
  (register-key-binding
   (make-instance 'key-binding :context :navigate :key key
                  :documentation "Cancel navigation and return to your work."
                  :function (lambda (application event)
                              (declare (ignore event))
                              (close-object-navigator application)
                              t))))

(define-key-binding (:navigate :return) (application event)
  "Focus the selected object without editing or evaluating it."
  (declare (ignore event))
  (accept-object-navigator application)
  t)

(define-key-binding (:navigate :up) (application event)
  "Select the previous object."
  (declare (ignore event))
  (step-object-navigator application -1)
  t)

(define-key-binding (:navigate :down) (application event)
  "Select the next object."
  (declare (ignore event))
  (step-object-navigator application 1)
  t)

(define-key-binding (:navigate :pageup) (application event)
  "Scroll the navigator up one page."
  (declare (ignore event))
  (scroll-application-page application -1)
  t)

(define-key-binding (:navigate :pagedown) (application event)
  "Scroll the navigator down one page."
  (declare (ignore event))
  (scroll-application-page application 1)
  t)

(define-key-binding (:navigate :g :control t) (application event)
  "Dismiss feedback, or cancel navigation if no feedback is visible."
  (declare (ignore event))
  (if (application-notification application) (dismiss-application-notification application)
      (close-object-navigator application))
  t)

(register-transient-input-bindings :navigate #'edit-object-navigator "navigator")
