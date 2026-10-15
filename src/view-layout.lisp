(in-package :orra)

(defparameter *outline-sidebar-width* 26)
(defparameter *inspector-sidebar-width* 30)
(defparameter *auxiliary-min-document-width* 72)

(defstruct view-layout document outline inspector foreground)

(defun auxiliary-overlay-active-p (application)
  (or (application-help-visible-p application)
      (application-command-palette application)
      (application-object-navigator application)
      (application-pending-action application)
      (application-quit-requested-p application)))

(defun view-bounds-contain-point-p (bounds x y)
  (and bounds (<= (bounds-x bounds) x) (< x (+ (bounds-x bounds) (bounds-width bounds)))
       (<= (bounds-y bounds) y) (< y (+ (bounds-y bounds) (bounds-height bounds)))))

(defun application-view-layout (application)
  "Allocate auxiliary panes without squeezing the document below its minimum."
  (let* ((width (max 1 (backend-layout-width (application-backend application))))
         (height (application-viewport-height application))
         (outline (application-outline application))
         (outline-requested (or (null outline) (outline-visible-p outline)))
         (outline-active (outline-active-p application))
         (inspector-requested (application-inspector-visible-p application))
         (left 0) (right 0))
    ;; The requested inspector takes priority over a passive outline. An outline
    ;; with keyboard ownership retains priority until the user returns from it.
    (when (and outline-active outline-requested
               (>= width (+ *outline-sidebar-width* 1 *auxiliary-min-document-width*)))
      (setf left (1+ *outline-sidebar-width*)))
    (when (and inspector-requested
               (>= (- width left) (+ *inspector-sidebar-width* 1 *auxiliary-min-document-width*)))
      (setf right (1+ *inspector-sidebar-width*)))
    (when (and outline-requested (zerop left)
               (>= (- width right) (+ *outline-sidebar-width* 1 *auxiliary-min-document-width*)))
      (setf left (1+ *outline-sidebar-width*)))
    (let ((foreground (cond ((and outline-active (zerop left)) :outline)
                            ((and inspector-requested (zerop right) (not outline-active)) :inspector))))
      (make-view-layout
       :document (make-bounds :x left :width (max 1 (- width left right)) :height height)
       :outline (cond ((eq foreground :outline) (make-bounds :width width :height height))
                      ((plusp left) (make-bounds :width *outline-sidebar-width* :height height)))
       :inspector (cond ((eq foreground :inspector) (make-bounds :width width :height height))
                        ((plusp right) (make-bounds :x (- width *inspector-sidebar-width*)
                                                    :width *inspector-sidebar-width* :height height)))
       :foreground foreground))))

(defun layout-application-document (application)
  (let ((bounds (view-layout-document (application-view-layout application))))
    (perform-layout (application-root-cell application)
                    :x (bounds-x bounds) :width (bounds-width bounds))))

(defun draw-application-view-pane (application root bounds viewport-y &key surface)
  (let ((backend (application-backend application)))
    (backend-call-with-clip
     backend bounds
     (lambda ()
       (when surface
         (backend-draw-cell backend (make-container-cell :decoration surface)
                            (bounds-x bounds) 0 (bounds-width bounds) (bounds-height bounds) nil))
       ;; Keep the pane's scrollbar out of its text/surface clip.
       (backend-call-with-clip
        backend (make-bounds :x (bounds-x bounds)
                             :width (max 1 (1- (bounds-width bounds)))
                             :height (bounds-height bounds))
        (lambda ()
          (draw-cell-tree backend root :viewport-y viewport-y :viewport-height (bounds-height bounds))))
       (draw-application-scrollbar application
                                   :content-height (bounds-height (cell-bounds root))
                                   :viewport-y viewport-y
                                   :x (+ (bounds-x bounds) (1- (bounds-width bounds))))))))
