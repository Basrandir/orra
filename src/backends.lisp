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
   (text-input-active-p
    :accessor backend-text-input-active-p
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

(defun normalize-font-path (path)
  (typecase path
    (pathname (namestring path))
    (string path)
    (t nil)))

(defun env-font-path ()
  (let ((configured-font (uiop:getenv "ORRA_FONT")))
    (when (and configured-font (plusp (length configured-font)))
      configured-font)))

(defun fontconfig-font-path ()
  (ignore-errors
    (let* ((output (uiop:run-program
                    '("fc-match" "-f" "%{file}\\n" "monospace")
                    :output :string
                    :ignore-error-status t))
           (path (string-trim '(#\Space #\Tab #\Newline #\Return)
                              output)))
      (when (plusp (length path))
        path))))

(defun resolve-font-path (backend)
  (or (and (backend-font-path backend)
           (normalize-font-path (backend-font-path backend)))
      (env-font-path)
      (fontconfig-font-path)))

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

(defun cursor-display-string (text cursor)
  (let ((cursor (max 0 (min cursor (length text)))))
    (concatenate 'string
                 (subseq text 0 cursor)
                 "|"
                 (subseq text cursor))))

(defun shifted-printable-char (character)
  (case character
    (#\1 #\!)
    (#\2 #\@)
    (#\3 #\#)
    (#\4 #\$)
    (#\5 #\%)
    (#\6 #\^)
    (#\7 #\&)
    (#\8 #\*)
    (#\9 #\()
    (#\0 #\))
    (#\- #\_)
    (#\= #\+)
    (#\[ #\{)
    (#\] #\})
    (#\\ #\|)
    (#\; #\:)
    (#\' #\")
    (#\, #\<)
    (#\. #\>)
    (#\/ #\?)
    (#\` #\~)
    (t character)))

(defun printable-key-text-from-sym (sym-value shiftp capslockp)
  (when (<= 32 sym-value 126)
    (let ((character (code-char sym-value)))
      (cond
        ((alpha-char-p character)
         (string (if (if capslockp
                         (not shiftp)
                         shiftp)
                     (char-upcase character)
                     (char-downcase character))))
        (shiftp
         (string (shifted-printable-char character)))
        (t
         (string character))))))

(defun printable-key-text (keysym)
  (let ((modifiers (sdl2:mod-value keysym)))
    (unless (or (sdl2:mod-value-p modifiers :lctrl :rctrl :lalt :ralt :lgui :rgui)
                (sdl2:scancode= keysym :scancode-tab))
      (printable-key-text-from-sym
       (sdl2:sym-value keysym)
       (sdl2:mod-value-p modifiers :lshift :rshift)
       (sdl2:mod-value-p modifiers :caps)))))

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
  (let ((font-path (resolve-font-path backend)))
    (cond
      ((null font-path)
       (format *error-output*
               "~&Orra: no font available. Set ORRA_FONT or ensure fc-match is installed and configured.~%")
       nil)
      (t
       (handler-case
           (let ((font (sdl2-ttf:open-font font-path (backend-font-size backend))))
             (setf (backend-font-path backend) font-path)
             font)
         (error (condition)
           (format *error-output*
                   "~&Orra: failed to open font ~A: ~A~%"
                   font-path
                   condition)
           nil))))))

(defmethod backend-begin-frame ((backend sdl2-backend))
  (sdl2:set-render-draw-color (backend-renderer backend) 28 30 36 255)
  (sdl2:render-clear (backend-renderer backend)))

(defmethod backend-draw-box ((backend sdl2-backend) x y width height label focusedp)
  (declare (ignore label))
  (let ((pixel-x (logical-x->pixel backend x))
        (pixel-y (logical-y->pixel backend y))
        (pixel-width (logical-width->pixel backend width))
        (pixel-height (logical-height->pixel backend height)))
    (sdl2:with-rects ((rect pixel-x pixel-y pixel-width pixel-height))
      (if focusedp
          (sdl2:set-render-draw-color (backend-renderer backend) 72 120 202 255)
          (sdl2:set-render-draw-color (backend-renderer backend) 48 53 63 255))
      (sdl2:render-fill-rect (backend-renderer backend) rect)
      (if focusedp
          (sdl2:set-render-draw-color (backend-renderer backend) 222 231 255 255)
          (sdl2:set-render-draw-color (backend-renderer backend) 103 111 124 255))
      (sdl2:render-draw-rect (backend-renderer backend) rect))))

(defun release-ttf-surface (surface)
  ;; sdl2-ttf returns autocollected surfaces; cancel the finalizer before
  ;; freeing explicitly so repeated redraws do not leave a stale free hook
  ;; behind for the GC to trigger later.
  (when surface
    (tg:cancel-finalization surface)
    (sdl2:free-surface surface)))

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
           (sdl2:with-rects ((rect pixel-x pixel-y width height))
             (sdl2:render-copy renderer texture :dest-rect rect))
        (sdl2:destroy-texture texture)
        (release-ttf-surface surface)))))

(defmethod backend-present ((backend sdl2-backend))
  (sdl2:render-present (backend-renderer backend)))

(defun displayed-cell-text (cell)
  (let ((text (cell-text cell)))
    (if (and *application*
             (editing-active-p *application*)
             (cell-model cell)
             (application-active-editor-model-id *application*)
             (string=
              (object-id (cell-model cell))
              (application-active-editor-model-id *application*)))
        (cursor-display-string
         (text-buffer-content (application-active-text-buffer *application*))
         (text-buffer-cursor (application-active-text-buffer *application*)))
        text)))

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
                         (trim-text-for-cell (displayed-cell-text cell)
                                             (max 1 (- width 2)))))
    (dolist (child (children-of cell))
      (draw-cell-tree backend child))))

(defmethod run-backend ((backend null-backend) application)
  (render-application application)
  backend)

(defun handle-sdl2-keydown (application keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        (printable-text (printable-key-text keysym)))
    (if (editing-active-p application)
        (cond
          ((sdl2:scancode= scancode :scancode-escape)
           (stop-editing application))
          ((sdl2:scancode= scancode :scancode-backspace)
           (delete-active-buffer-backward application))
          ((sdl2:scancode= scancode :scancode-delete)
           (delete-active-buffer-forward application))
          ((sdl2:scancode= scancode :scancode-left)
           (move-active-buffer-cursor-left application))
          ((sdl2:scancode= scancode :scancode-right)
           (move-active-buffer-cursor-right application))
          ((sdl2:scancode= scancode :scancode-return)
           (insert-into-active-buffer application (string #\Newline)))
          (printable-text
           (insert-into-active-buffer application printable-text)))
        (cond
          ((and printable-text
                (editable-model-p (focused-model application)))
           (begin-editing-focused-model application)
           (insert-into-active-buffer application printable-text))
          ((or (sdl2:scancode= scancode :scancode-q)
               (sdl2:scancode= scancode :scancode-escape))
           (quit-application application))
          ((or (sdl2:scancode= scancode :scancode-j)
               (sdl2:scancode= scancode :scancode-down))
           (focus-next-model application))
          ((or (sdl2:scancode= scancode :scancode-k)
               (sdl2:scancode= scancode :scancode-up))
           (focus-previous-model application))
          ((sdl2:scancode= scancode :scancode-i)
           (begin-editing-focused-model application))
          ((and (sdl2:scancode= scancode :scancode-return)
                (typep (focused-model application) 'paragraph))
           (begin-editing-focused-model application))
          ((or (sdl2:scancode= scancode :scancode-return)
               (sdl2:scancode= scancode :scancode-e))
           (evaluate-focused-code-block application))
          ((sdl2:scancode= scancode :scancode-s)
           (invoke-command application 'save-workspace))
          ((sdl2:scancode= scancode :scancode-r)
           (render-application application))))))

(defun sync-sdl2-text-input-state (backend application)
  (let ((editingp (editing-active-p application)))
    (cond
      ((and editingp (not (backend-text-input-active-p backend)))
       (sdl2:start-text-input)
       (setf (backend-text-input-active-p backend) t))
      ((and (not editingp) (backend-text-input-active-p backend))
       (sdl2:stop-text-input)
       (setf (backend-text-input-active-p backend) nil)))))

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
                (sync-sdl2-text-input-state backend application)
                (render-application application)
                (when (backend-single-frame-p backend)
                  (sdl2:push-event :quit))
                (sdl2:delay 16))
               (:quit ()
                t))))
      (when (backend-font backend)
        (sdl2-ttf:close-font (backend-font backend))
        (setf (backend-font backend) nil))
      (when (backend-text-input-active-p backend)
        (sdl2:stop-text-input)
        (setf (backend-text-input-active-p backend) nil))
      (sdl2-ttf:quit)))
  backend)
