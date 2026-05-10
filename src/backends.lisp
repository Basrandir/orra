(in-package :orra)

(defvar *application* nil)

(defclass backend ()
  ((name
    :initarg :name
    :reader backend-name)))

(defclass null-backend (backend)
  ((stream
    :initarg :stream
    :reader backend-stream)
   (frame-operations
    :initform nil
    :accessor backend-frame-operations)))

(defclass sdl2-backend (backend)
  ((title
    :initarg :title
    :accessor backend-title
    :initform "Orra")
   (pixel-width
    :initarg :pixel-width
    :accessor backend-pixel-width
    :initform 1280)
   (pixel-height
    :initarg :pixel-height
    :accessor backend-pixel-height
    :initform 800)
   (cell-width
    :initarg :cell-width
    :accessor backend-cell-width
    :initform 12)
   (row-height
    :initarg :row-height
    :accessor backend-row-height
    :initform 24)
   (padding-x
    :initarg :padding-x
    :accessor backend-padding-x
    :initform 18)
   (padding-y
    :initarg :padding-y
    :accessor backend-padding-y
    :initform 18)
   (font-path
    :initarg :font-path
    :accessor backend-font-path
    :initform nil)
   (font-size
    :initarg :font-size
    :accessor backend-font-size
    :initform 16)
   (font
    :accessor backend-font
    :initform nil)
   (window
    :accessor backend-window
    :initform nil)
   (renderer
    :accessor backend-renderer
    :initform nil)
   (single-frame-p
    :initarg :single-frame-p
    :accessor backend-single-frame-p
    :initform nil)))

(defun make-null-backend (&key (stream *standard-output*))
  (make-instance 'null-backend
                 :name :null
                 :stream stream))

(defun make-sdl2-backend (&key
                            (title "Orra")
                            (pixel-width 1280)
                            (pixel-height 800)
                            (cell-width 12)
                            (row-height 24)
                            (padding-x 18)
                            (padding-y 18)
                            font-path
                            (font-size 16)
                            single-frame-p)
  (make-instance 'sdl2-backend
                 :name :sdl2
                 :title title
                 :pixel-width pixel-width
                 :pixel-height pixel-height
                 :cell-width cell-width
                 :row-height row-height
                 :padding-x padding-x
                 :padding-y padding-y
                 :font-path font-path
                 :font-size font-size
                 :single-frame-p single-frame-p))

(defgeneric backend-begin-frame (backend))
(defgeneric backend-draw-text (backend x y text))
(defgeneric backend-draw-box (backend x y width height label focusedp))
(defgeneric backend-layout-width (backend))
(defgeneric backend-grid-point (backend pixel-x pixel-y))
(defgeneric backend-present (backend))
(defgeneric run-backend (backend application))

(defmethod backend-layout-width ((backend null-backend))
  96)

(defmethod backend-grid-point ((backend null-backend) pixel-x pixel-y)
  (values pixel-x pixel-y))

(defmethod backend-begin-frame ((backend null-backend))
  (setf (backend-frame-operations backend) nil))

(defmethod backend-draw-text ((backend null-backend) x y text)
  (push (list :text x y text) (backend-frame-operations backend)))

(defmethod backend-draw-box ((backend null-backend) x y width height label focusedp)
  (push (list :box x y width height label focusedp)
        (backend-frame-operations backend)))

(defmethod backend-present ((backend null-backend))
  (format (backend-stream backend) "~&;; frame (~A)~%" (backend-name backend))
  (dolist (operation (nreverse (backend-frame-operations backend)))
    (case (first operation)
      (:box
       (destructuring-bind (kind x y width height label focusedp) operation
         (declare (ignore kind))
         (format (backend-stream backend)
                 "~&BOX  x=~D y=~D w=~D h=~D ~:[ ~;*~] ~A~%"
                 x y width height focusedp label)))
      (:text
       (destructuring-bind (kind x y text) operation
         (declare (ignore kind))
         (format (backend-stream backend)
                 "~&TEXT x=~D y=~D  ~A~%"
                 x y text)))))
  (finish-output (backend-stream backend)))

(defun find-default-font-path ()
  (or (find-if #'probe-file
               '("/run/current-system/profile/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                 "/run/current-system/profile/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
                 "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                 "/usr/share/fonts/TTF/DejaVuSans.ttf"
                 "/Library/Fonts/Menlo.ttc"
                 "/Library/Fonts/Arial Unicode.ttf"
                 "C:/Windows/Fonts/consola.ttf"
                 "C:/Windows/Fonts/segoeui.ttf"))
      nil))

(defmacro with-sdl-rect ((name x y width height) &body body)
  `(let ((,name (sdl2:make-rect ,x ,y ,width ,height)))
     (unwind-protect
          (progn ,@body)
       (sdl2:free-rect ,name))))

(defun logical-x->pixel (backend x)
  (+ (backend-padding-x backend)
     (* x (backend-cell-width backend))))

(defun logical-y->pixel (backend y)
  (+ (backend-padding-y backend)
     (* y (backend-row-height backend))))

(defun logical-width->pixel (backend width)
  (max (backend-cell-width backend)
       (* width (backend-cell-width backend))))

(defun logical-height->pixel (backend height)
  (max (backend-row-height backend)
       (* height (backend-row-height backend))))

(defun trim-text-for-cell (text width)
  (let* ((safe-width (max 1 width))
         (string (string text)))
    (if (<= (length string) safe-width)
        string
        (format nil "~A..." (subseq string 0 (max 0 (- safe-width 3)))))))

(defmethod backend-layout-width ((backend sdl2-backend))
  (max 20
       (floor (- (backend-pixel-width backend)
                 (* 2 (backend-padding-x backend)))
              (backend-cell-width backend))))

(defmethod backend-grid-point ((backend sdl2-backend) pixel-x pixel-y)
  (values (max 0
               (floor (- pixel-x (backend-padding-x backend))
                      (backend-cell-width backend)))
          (max 0
               (floor (- pixel-y (backend-padding-y backend))
                      (backend-row-height backend)))))

(defun open-sdl2-font (backend)
  (let ((font-path (or (backend-font-path backend)
                       (find-default-font-path))))
    (when font-path
      (setf (backend-font-path backend) font-path)
      (sdl2-ttf:open-font font-path (backend-font-size backend)))))

(defmethod backend-begin-frame ((backend sdl2-backend))
  (sdl2:set-render-draw-color (backend-renderer backend) 28 30 36 255)
  (sdl2:render-clear (backend-renderer backend)))

(defmethod backend-draw-box ((backend sdl2-backend) x y width height label focusedp)
  (declare (ignore label))
  (let ((pixel-x (logical-x->pixel backend x))
        (pixel-y (logical-y->pixel backend y))
        (pixel-width (logical-width->pixel backend width))
        (pixel-height (logical-height->pixel backend height)))
    (with-sdl-rect (rect pixel-x pixel-y pixel-width pixel-height)
      (if focusedp
          (sdl2:set-render-draw-color (backend-renderer backend) 72 120 202 255)
          (sdl2:set-render-draw-color (backend-renderer backend) 48 53 63 255))
      (sdl2:render-fill-rect (backend-renderer backend) rect)
      (if focusedp
          (sdl2:set-render-draw-color (backend-renderer backend) 222 231 255 255)
          (sdl2:set-render-draw-color (backend-renderer backend) 103 111 124 255))
      (sdl2:render-draw-rect (backend-renderer backend) rect))))

(defmethod backend-draw-text ((backend sdl2-backend) x y text)
  (when (backend-font backend)
    (let* ((renderer (backend-renderer backend))
           (surface (sdl2-ttf:render-text-blended
                     (backend-font backend)
                     text
                     239 241 245 255))
           (texture (sdl2:create-texture-from-surface renderer surface))
           (pixel-x (logical-x->pixel backend x))
           (pixel-y (logical-y->pixel backend y))
           (width (sdl2:surface-width surface))
           (height (sdl2:surface-height surface)))
      (unwind-protect
           (with-sdl-rect (rect pixel-x pixel-y width height)
             (sdl2:render-copy renderer texture :dest-rect rect))
        (sdl2:destroy-texture texture)
        (sdl2:free-surface surface)))))

(defmethod backend-present ((backend sdl2-backend))
  (sdl2:render-present (backend-renderer backend)))

(defun draw-cell-tree (backend cell)
  (let* ((bounds (cell-bounds cell))
         (x (bounds-x bounds))
         (y (bounds-y bounds))
         (width (bounds-width bounds))
         (height (bounds-height bounds))
         (label (slot-value cell 'label))
         (focusedp (and *application*
                        (cell-model cell)
                        (application-focused-model-id *application*)
                        (string=
                         (object-id (cell-model cell))
                         (application-focused-model-id *application*)))))
    (backend-draw-box backend x y width height label focusedp)
    (when (typep cell 'text-cell)
      (backend-draw-text backend
                         (+ x 1)
                         (+ y 1)
                         (trim-text-for-cell (cell-text cell) (max 1 (- width 2)))))
    (dolist (child (children-of cell))
      (draw-cell-tree backend child))))

(defmethod run-backend ((backend null-backend) application)
  (render-application application)
  backend)

(defun handle-sdl2-keydown (application keysym)
  (let ((scancode (sdl2:scancode-value keysym)))
    (cond
      ((or (sdl2:scancode= scancode :scancode-q)
           (sdl2:scancode= scancode :scancode-escape))
       (quit-application application))
      ((or (sdl2:scancode= scancode :scancode-j)
           (sdl2:scancode= scancode :scancode-down))
       (focus-next-model application))
      ((or (sdl2:scancode= scancode :scancode-k)
           (sdl2:scancode= scancode :scancode-up))
       (focus-previous-model application))
      ((or (sdl2:scancode= scancode :scancode-return)
           (sdl2:scancode= scancode :scancode-e))
       (evaluate-focused-code-block application))
      ((sdl2:scancode= scancode :scancode-s)
       (invoke-command application 'save-workspace))
      ((sdl2:scancode= scancode :scancode-r)
       (render-application application)))))

(defmethod run-backend ((backend sdl2-backend) application)
  (sdl2:with-init (:video)
    (sdl2-ttf:init)
    (unwind-protect
         (sdl2:with-window (window
                            :title (backend-title backend)
                            :w (backend-pixel-width backend)
                            :h (backend-pixel-height backend)
                            :flags '(:shown :resizable))
           (sdl2:with-renderer (renderer window)
             (setf (backend-window backend) window)
             (setf (backend-renderer backend) renderer)
             (setf (backend-font backend) (open-sdl2-font backend))
             (render-application application)
             (sdl2:with-event-loop (:method :poll)
               (:keydown (:keysym keysym)
                (handle-sdl2-keydown application keysym))
               (:mousebuttondown (:x x :y y)
                (focus-model-at-pixel application x y))
               (:idle ()
                (multiple-value-bind (width height)
                    (sdl2:get-window-size window)
                  (setf (backend-pixel-width backend) width)
                  (setf (backend-pixel-height backend) height))
                (render-application application)
                (when (backend-single-frame-p backend)
                  (sdl2:push-event :quit))
                (sdl2:delay 16))
               (:quit ()
                t))))
      (when (backend-font backend)
        (sdl2-ttf:close-font (backend-font backend))
        (setf (backend-font backend) nil))
      (sdl2-ttf:quit)))
  backend)
