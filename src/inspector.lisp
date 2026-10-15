(in-package :orra)

(defclass object-inspector-view ()
  ((root-cell :accessor inspector-root-cell :initform nil)
   (viewport-y :accessor inspector-viewport-y :initform 0)
   (target :accessor inspector-view-target :initform nil)
   (active-p :accessor inspector-keyboard-active-p :initform nil)))

(defun ensure-application-inspector (application)
  (or (application-inspector application)
      (setf (application-inspector application) (make-instance 'object-inspector-view))))

(defun inspector-active-p (application)
  (and (application-inspector-visible-p application)
       (not (outline-active-p application))
       (or (eq :inspector (view-layout-foreground (application-view-layout application)))
           (and (application-inspector application)
                (inspector-keyboard-active-p (application-inspector application))))))

(defun inspector-input-blocked-p (application)
  (or (auxiliary-overlay-active-p application) (outline-active-p application)))

(defun inspector-max-viewport-y (application)
  (let ((root (inspector-root-cell (ensure-application-inspector application))))
    (max 0 (- (if root (bounds-height (cell-bounds root)) 0)
              (application-viewport-height application)))))

(defun build-inspector-cell (application width)
  (let ((root (make-container-cell :label "Inspector" :inset 1 :top-inset 0 :spacing 0)))
    (append-child root (make-text-cell :text "Inspector" :role :heading))
    (append-child root
                  (make-text-cell :text (wrap-keyboard-help-text
                                         (if (inspector-active-p application)
                                             "F4 close | Esc return | F1 help"
                                             "F4 close | Shift+F4 focus")
                                         (max 1 (- width 5)))
                                  :role :metadata))
    (dolist (line (rest (inspector-lines-for-model application (focused-model application))))
      (append-child root (make-text-cell :text (wrap-keyboard-help-text line (max 1 (- width 5)))
                                         :role :metadata)))
    root))

(defun refresh-application-inspector (application)
  (let* ((view (ensure-application-inspector application))
         (target (focused-model application))
         (bounds (view-layout-inspector (application-view-layout application))))
    (when bounds
      (unless (eq target (inspector-view-target view))
        (setf (inspector-view-target view) target
              (inspector-viewport-y view) 0))
      (setf (inspector-root-cell view) (build-inspector-cell application (bounds-width bounds)))
      (perform-layout (inspector-root-cell view) :x (bounds-x bounds)
                      :width (max 1 (1- (bounds-width bounds))))
      (setf (inspector-viewport-y view)
            (max 0 (min (inspector-viewport-y view) (inspector-max-viewport-y application)))))
    (inspector-root-cell view)))

(defun toggle-application-inspector (application)
  (when (inspector-input-blocked-p application)
    (return-from toggle-application-inspector nil))
  (setf (application-inspector-visible-p application) (not (application-inspector-visible-p application))
        (inspector-keyboard-active-p (ensure-application-inspector application)) nil)
  (layout-application-document application)
  (when (application-inspector-visible-p application) (refresh-application-inspector application))
  (mark-application-dirty application)
  application)

(defun focus-application-inspector (application)
  (when (inspector-input-blocked-p application)
    (return-from focus-application-inspector nil))
  (setf (application-inspector-visible-p application) t
        (inspector-keyboard-active-p (ensure-application-inspector application)) t)
  (layout-application-document application)
  (refresh-application-inspector application)
  (mark-application-dirty application)
  application)

(defun return-from-application-inspector (application)
  (setf (inspector-keyboard-active-p (ensure-application-inspector application)) nil)
  (when (eq :inspector (view-layout-foreground (application-view-layout application)))
    (setf (application-inspector-visible-p application) nil))
  (layout-application-document application)
  (mark-application-dirty application)
  application)

(defun scroll-application-inspector (application rows)
  (refresh-application-inspector application)
  (let ((view (ensure-application-inspector application)))
    (setf (inspector-viewport-y view)
          (max 0 (min (+ rows (inspector-viewport-y view)) (inspector-max-viewport-y application)))))
  (mark-application-dirty application)
  application)

(defun inspector-point-p (application x y)
  (let ((layout (application-view-layout application)))
    (and (not (auxiliary-overlay-active-p application))
         (not (eq :outline (view-layout-foreground layout)))
         (view-bounds-contain-point-p (view-layout-inspector layout) x y))))

(defun draw-application-inspector (application)
  (let ((bounds (view-layout-inspector (application-view-layout application))))
    (when bounds
      (draw-application-view-pane application (refresh-application-inspector application) bounds
                                  (inspector-viewport-y (application-inspector application)) :surface :inspector))))

(define-command focus-inspector (application)
  "Focus the read-only inspector without replacing the active editor."
  (focus-application-inspector application))

(define-key-binding (:global :f4 :shift t) (application event)
  "Give keyboard focus to the inspector."
  (declare (ignore event)) (invoke-command application 'focus-inspector) t)

(define-key-binding (:inspect :f4) (application event)
  "Close the inspector and return to the document."
  (declare (ignore event)) (toggle-application-inspector application) t)

(define-key-binding (:inspect :escape) (application event)
  "Return to the document without changing the editor."
  (declare (ignore event)) (return-from-application-inspector application) t)

(define-key-binding (:inspect :g :control t) (application event)
  "Return to the document without changing the editor."
  (declare (ignore event)) (return-from-application-inspector application) t)

(define-key-binding (:inspect :up) (application event)
  "Scroll the inspector up one row."
  (declare (ignore event)) (scroll-application-inspector application -1) t)

(define-key-binding (:inspect :down) (application event)
  "Scroll the inspector down one row."
  (declare (ignore event)) (scroll-application-inspector application 1) t)

(define-key-binding (:inspect :pageup) (application event)
  "Scroll the inspector up one page."
  (declare (ignore event)) (scroll-application-page application -1) t)

(define-key-binding (:inspect :pagedown) (application event)
  "Scroll the inspector down one page."
  (declare (ignore event)) (scroll-application-page application 1) t)

(define-key-binding (:inspect :home) (application event)
  "Scroll to the start of the inspector."
  (declare (ignore event))
  (scroll-application-inspector application (- (inspector-viewport-y (ensure-application-inspector application)))) t)

(define-key-binding (:inspect :end) (application event)
  "Scroll to the end of the inspector."
  (declare (ignore event))
  (refresh-application-inspector application)
  (scroll-application-inspector application (inspector-max-viewport-y application)) t)

(define-key-binding (:inspect :f1) (application event)
  "Show keyboard help for the inspector."
  (declare (ignore event)) (toggle-application-keyboard-help application) t)

(define-key-binding (:inspect :f2) (application event)
  "Open the live command palette."
  (declare (ignore event)) (open-command-palette application) t)

(define-key-binding (:inspect :f3) (application event)
  "Search all workspace objects."
  (declare (ignore event)) (open-object-navigator application) t)
