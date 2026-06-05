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
   (layout-width
    :initarg :layout-width
    :accessor backend-null-layout-width
    :initform 96)
   (layout-height
    :initarg :layout-height
    :accessor backend-null-layout-height
    :initform 120)
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

(defun make-null-backend (&key (stream *standard-output*)
                            (layout-width 96)
                            (layout-height 120))
  (make-instance 'null-backend
                 :name :null
                 :stream stream
                 :layout-width layout-width
                 :layout-height layout-height))

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
(defgeneric backend-draw-styled-text (backend x y text style-spans))
(defgeneric backend-draw-box (backend x y width height label focusedp))
(defgeneric backend-draw-selection
    (backend x y &key text-prefix selected-text trailing-cell-p))
(defgeneric backend-draw-caret (backend x y activep &key text-prefix))
(defgeneric backend-draw-scrollbar (backend x y height thumb-y thumb-height))
(defgeneric backend-layout-width (backend))
(defgeneric backend-layout-height (backend))
(defgeneric backend-grid-point (backend pixel-x pixel-y))
(defgeneric backend-present (backend))
(defgeneric run-backend (backend application))

(defmethod backend-layout-width ((backend null-backend))
  (backend-null-layout-width backend))

(defmethod backend-layout-height ((backend null-backend))
  (backend-null-layout-height backend))

(defmethod backend-grid-point ((backend null-backend) pixel-x pixel-y)
  (values pixel-x pixel-y))

(defmethod backend-begin-frame ((backend null-backend))
  (setf (backend-frame-operations backend) nil))

(defmethod backend-draw-text ((backend null-backend) x y text)
  (push (list :text x y text) (backend-frame-operations backend)))

(defmethod backend-draw-styled-text ((backend null-backend) x y text style-spans)
  (push (list :styled-text x y text style-spans)
        (backend-frame-operations backend)))

(defmethod backend-draw-box ((backend null-backend) x y width height label focusedp)
  (push (list :box x y width height label focusedp)
        (backend-frame-operations backend)))

(defmethod backend-draw-selection ((backend null-backend) x y
                                   &key (text-prefix "") (selected-text "")
                                     trailing-cell-p)
  (push (list :selection
              (+ x (length text-prefix))
              y
              (max 1 (+ (length selected-text)
                        (if trailing-cell-p 1 0))))
        (backend-frame-operations backend)))

(defmethod backend-draw-caret ((backend null-backend) x y activep
                               &key (text-prefix ""))
  (push (list :caret (+ x (length text-prefix)) y activep)
        (backend-frame-operations backend)))

(defmethod backend-draw-scrollbar
    ((backend null-backend) x y height thumb-y thumb-height)
  (push (list :scrollbar x y height thumb-y thumb-height)
        (backend-frame-operations backend)))

(defmethod backend-present ((backend null-backend))
  (format (backend-stream backend) "~&;; frame (~A)~%" (backend-name backend))
  (dolist (operation (reverse (backend-frame-operations backend)))
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
                 x y text)))
      (:styled-text
       (destructuring-bind (kind x y text style-spans) operation
         (declare (ignore kind style-spans))
         (format (backend-stream backend)
                 "~&TEXT x=~D y=~D  ~A~%"
                 x y text)))
      (:selection
       (destructuring-bind (kind x y width) operation
         (declare (ignore kind))
         (format (backend-stream backend)
                 "~&SELECTION x=~D y=~D w=~D~%"
                 x y width)))
      (:caret
       (destructuring-bind (kind x y activep) operation
         (declare (ignore kind))
         (format (backend-stream backend)
                 "~&CARET x=~D y=~D ~:[ ~;*~]~%"
                 x y activep)))
      (:scrollbar
       (destructuring-bind (kind x y height thumb-y thumb-height) operation
         (declare (ignore kind))
         (format (backend-stream backend)
                 "~&SCROLLBAR x=~D y=~D h=~D thumb-y=~D thumb-h=~D~%"
                 x y height thumb-y thumb-height)))))
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

(defmethod backend-layout-height ((backend sdl2-backend))
  (max 8
       (floor (- (backend-pixel-height backend)
                 (* 2 (backend-padding-y backend)))
              (backend-row-height backend))))

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

(defmethod backend-draw-caret ((backend sdl2-backend) x y activep
                               &key (text-prefix ""))
  (let ((pixel-x (+ (logical-x->pixel backend x)
                    (if (and (backend-font backend)
                             (plusp (length text-prefix)))
                        (nth-value 0
                                   (sdl2-ttf:size-text (backend-font backend)
                                                       text-prefix))
                        0)))
        (pixel-y (logical-y->pixel backend y))
        (pixel-height (backend-row-height backend)))
    (if activep
        (sdl2:set-render-draw-color (backend-renderer backend) 245 194 102 255)
        (sdl2:set-render-draw-color (backend-renderer backend) 222 231 255 255))
    (sdl2:with-rects ((rect pixel-x pixel-y 2 pixel-height))
      (sdl2:render-fill-rect (backend-renderer backend) rect))))

(defmethod backend-draw-selection ((backend sdl2-backend) x y
                                   &key (text-prefix "") (selected-text "")
                                     trailing-cell-p)
  (let ((pixel-x (+ (logical-x->pixel backend x)
                    (if (and (backend-font backend)
                             (plusp (length text-prefix)))
                        (nth-value 0
                                   (sdl2-ttf:size-text (backend-font backend)
                                                       text-prefix))
                        0)))
        (pixel-y (logical-y->pixel backend y))
        (pixel-width (+ (if (and (backend-font backend)
                                 (plusp (length selected-text)))
                            (nth-value 0
                                       (sdl2-ttf:size-text
                                        (backend-font backend)
                                        selected-text))
                            0)
                        (if (or trailing-cell-p
                                (zerop (length selected-text)))
                            (backend-cell-width backend)
                            0)))
        (pixel-height (backend-row-height backend)))
    (sdl2:set-render-draw-color (backend-renderer backend) 67 92 135 255)
    (sdl2:with-rects ((rect pixel-x pixel-y pixel-width pixel-height))
      (sdl2:render-fill-rect (backend-renderer backend) rect))))

(defmethod backend-draw-scrollbar
    ((backend sdl2-backend) x y height thumb-y thumb-height)
  (let ((pixel-x (logical-x->pixel backend x))
        (pixel-y (logical-y->pixel backend y))
        (pixel-width (backend-cell-width backend))
        (pixel-height (logical-height->pixel backend height))
        (thumb-pixel-y (logical-y->pixel backend (+ y thumb-y)))
        (thumb-pixel-height (logical-height->pixel backend thumb-height)))
    (sdl2:with-rects ((track pixel-x pixel-y pixel-width pixel-height)
                      (thumb pixel-x thumb-pixel-y pixel-width thumb-pixel-height))
      (sdl2:set-render-draw-color (backend-renderer backend) 41 45 54 255)
      (sdl2:render-fill-rect (backend-renderer backend) track)
      (sdl2:set-render-draw-color (backend-renderer backend) 133 141 157 255)
      (sdl2:render-fill-rect (backend-renderer backend) thumb))))

(defun release-ttf-surface (surface)
  ;; sdl2-ttf returns autocollected surfaces; cancel the finalizer before
  ;; freeing explicitly so repeated redraws do not leave a stale free hook
  ;; behind for the GC to trigger later.
  (when surface
    (tg:cancel-finalization surface)
    (sdl2:free-surface surface)))

(defmethod backend-draw-text ((backend sdl2-backend) x y text)
  (draw-sdl2-text-at-pixel backend
                           (logical-x->pixel backend x)
                           (logical-y->pixel backend y)
                           text
                           239 241 245 255))

(defun draw-sdl2-text-at-pixel (backend pixel-x pixel-y text red green blue alpha)
  (when (backend-font backend)
    (if (zerop (length text))
        0
        (let* ((renderer (backend-renderer backend))
               (surface (sdl2-ttf:render-text-blended
                         (backend-font backend)
                         text
                         red green blue alpha))
               (texture (sdl2:create-texture-from-surface renderer surface))
               (width (sdl2:surface-width surface))
               (height (sdl2:surface-height surface)))
          (unwind-protect
               (sdl2:with-rects ((rect pixel-x pixel-y width height))
                 (sdl2:render-copy renderer texture :dest-rect rect)
                 width)
            (sdl2:destroy-texture texture)
            (release-ttf-surface surface))))))

(defun style-span-color (kind)
  (case kind
    (:comment (values 133 141 157 255))
    (:string (values 166 227 161 255))
    (:keyword (values 245 194 102 255))
    (:number (values 137 180 250 255))
    (:paren (values 147 153 178 255))
    (:quote (values 250 179 135 255))
    (t (values 239 241 245 255))))

(defmethod backend-draw-styled-text ((backend sdl2-backend) x y text style-spans)
  (if (and style-spans (backend-font backend))
      (let ((pixel-x (logical-x->pixel backend x))
            (pixel-y (logical-y->pixel backend y))
            (cursor 0))
        (dolist (span style-spans)
          (let ((start (max 0
                            (min (length text)
                                 (or (getf span :start) 0))))
                (end (max 0
                          (min (length text)
                               (or (getf span :end) 0)))))
            (when (< cursor start)
              (incf pixel-x
                    (or (draw-sdl2-text-at-pixel
                         backend
                         pixel-x
                         pixel-y
                         (subseq text cursor start)
                         239 241 245 255)
                        0)))
            (when (< start end)
              (multiple-value-bind (red green blue alpha)
                  (style-span-color (getf span :kind))
                (incf pixel-x
                      (or (draw-sdl2-text-at-pixel
                           backend
                           pixel-x
                           pixel-y
                           (subseq text start end)
                           red green blue alpha)
                          0))))
            (setf cursor (max cursor end))))
        (when (< cursor (length text))
          (draw-sdl2-text-at-pixel backend
                                   pixel-x
                                   pixel-y
                                   (subseq text cursor)
                                   239 241 245 255)))
      (backend-draw-text backend x y text)))

(defmethod backend-present ((backend sdl2-backend))
  (sdl2:render-present (backend-renderer backend)))

(defun active-editor-cell-p (cell)
  (and *application*
       (editing-active-p *application*)
       (eq (cell-role cell) :editable-content)
       (cell-model cell)
       (application-active-editor-model-id *application*)
       (string=
        (object-id (cell-model cell))
        (application-active-editor-model-id *application*))))

(defun editable-cell-text (cell)
  (let ((model (cell-model cell)))
    (typecase model
      (paragraph (paragraph-text model))
      (code-block (code-block-source model))
      (t (cell-text cell)))))

(defun focused-editable-cell-p (cell)
  (and *application*
       (not (editing-active-p *application*))
       (eq (cell-role cell) :editable-content)
       (cell-model cell)
       (application-focused-model-id *application*)
       (string= (object-id (cell-model cell))
                (application-focused-model-id *application*))))

(defun saved-editor-cursor-for-model (application model content)
  (let ((state (gethash (object-id model)
                        (application-editor-state-table application))))
    (and state
         (string= (getf state :content "") content)
         (getf state :cursor))))

(defun prospective-editor-cursor-for-model (application model content)
  (or (saved-editor-cursor-for-model application model content)
      (and (typep model 'code-block)
           (code-block-selected-form-start-offset model))
      (length content)))

(defun cell-caret-cursor (cell)
  (cond
    ((active-editor-cell-p cell)
     (text-buffer-cursor (application-active-text-buffer *application*)))
    ((focused-editable-cell-p cell)
     (let* ((model (cell-model cell))
            (content (editable-cell-text cell)))
       (prospective-editor-cursor-for-model *application* model content)))
    (t nil)))

(defun displayed-cell-text (cell)
  (let ((text (cell-text cell)))
    (if (active-editor-cell-p cell)
        (text-buffer-content (application-active-text-buffer *application*))
        text)))

(defun cell-visible-text-width (cell)
  (max 1 (- (bounds-width (cell-bounds cell)) 2)))

(defun cell-visible-column-offset (cell line-index)
  (let ((cursor (cell-caret-cursor cell)))
    (if cursor
        (multiple-value-bind (cursor-line cursor-column)
            (string-line-column (displayed-cell-text cell) cursor)
          (if (= line-index cursor-line)
              (max 0
                   (- cursor-column
                      (1- (cell-visible-text-width cell))))
              0))
        0)))

(defun visible-line-text-for-cell (cell line line-index)
  (let ((start (min (length line)
                    (cell-visible-column-offset cell line-index)))
        (width (cell-visible-text-width cell)))
    (if (zerop start)
        (trim-text-for-cell line width)
        (subseq line start
                (min (length line)
                     (+ start width))))))

(defun split-lines-with-starts (string)
  (let ((string (string string))
        (start 0)
        lines)
    (loop for newline = (position #\Newline string :start start)
          do (if newline
                 (progn
                   (push (list (subseq string start newline) start) lines)
                   (setf start (1+ newline)))
                 (return)))
    (push (list (subseq string start) start) lines)
    (nreverse lines)))

(defun visible-line-source-window (cell line line-index)
  (let* ((start (min (length line)
                     (cell-visible-column-offset cell line-index)))
         (width (cell-visible-text-width cell)))
    (if (zerop start)
        (let ((source-end (if (<= (length line) width)
                              (length line)
                              (max 0 (- width 3)))))
          (values (trim-text-for-cell line width)
                  0
                  source-end))
        (let ((source-end (min (length line)
                               (+ start width))))
          (values (subseq line start source-end)
                  start
                  source-end)))))

(defun visible-style-spans-for-range (style-spans start end)
  (loop for span in style-spans
        for span-start = (or (getf span :start) 0)
        for span-end = (or (getf span :end) 0)
        for visible-start = (max start span-start)
        for visible-end = (min end span-end)
        when (< visible-start visible-end)
        collect (list :kind (getf span :kind)
                      :start (- visible-start start)
                      :end (- visible-end start))))

(defun displayed-cell-style-spans (cell)
  (unless (active-editor-cell-p cell)
    (cell-style-spans cell)))

(defun visible-lines-with-style-spans-for-cell (cell)
  (let ((style-spans (displayed-cell-style-spans cell)))
    (loop for (line line-start) in (split-lines-with-starts
                                    (displayed-cell-text cell))
          for line-index from 0
          collect (multiple-value-bind (visible-text source-start source-end)
                      (visible-line-source-window cell line line-index)
                    (let ((global-start (+ line-start source-start))
                          (global-end (+ line-start source-end)))
                      (list visible-text
                            (and style-spans
                                 (visible-style-spans-for-range
                                  style-spans
                                  global-start
                                  global-end))))))))

(defun visible-lines-for-cell (cell)
  (mapcar #'first (visible-lines-with-style-spans-for-cell cell)))

(defun row-visible-p (row viewport-y viewport-height)
  (and (<= viewport-y row)
       (< row (+ viewport-y viewport-height))))

(defun cell-intersects-viewport-p (cell viewport-y viewport-height)
  (let ((bounds (cell-bounds cell)))
    (and (< (bounds-y bounds) (+ viewport-y viewport-height))
         (< viewport-y (+ (bounds-y bounds)
                          (bounds-height bounds))))))

(defun draw-cell-caret (backend cell viewport-y viewport-height)
  (let ((cursor (cell-caret-cursor cell)))
    (when cursor
      (multiple-value-bind (line column)
          (string-line-column (displayed-cell-text cell) cursor)
        (let* ((bounds (cell-bounds cell))
               (row (+ (bounds-y bounds) 1 line))
               (offset (cell-visible-column-offset cell line))
               (text-line (nth line
                               (split-lines (displayed-cell-text cell))))
               (visible-column (max 0
                                    (min (length text-line)
                                         (- column offset))))
               (text-prefix (subseq text-line
                                    offset
                                    (+ offset visible-column))))
          (when (row-visible-p row viewport-y viewport-height)
            (backend-draw-caret backend
                                (+ (bounds-x bounds) 1)
                                (- row viewport-y)
                                (active-editor-cell-p cell)
                                :text-prefix text-prefix)))))))

(defun draw-cell-selection (backend cell viewport-y viewport-height)
  (when (active-editor-cell-p cell)
    (let ((content (displayed-cell-text cell)))
      (multiple-value-bind (selection-start selection-end)
          (text-buffer-selection-range
           (application-active-text-buffer *application*))
        (when selection-start
          (loop for (line line-start) in (split-lines-with-starts content)
                for line-index from 0
                for row = (+ (bounds-y (cell-bounds cell)) 1 line-index)
                do (multiple-value-bind (visible-text source-start source-end)
                       (visible-line-source-window cell line line-index)
                     (declare (ignore visible-text))
                     (let* ((visible-start (+ line-start source-start))
                            (visible-end (+ line-start source-end))
                            (start (max selection-start visible-start))
                            (end (min selection-end visible-end))
                            (newline-offset (+ line-start (length line)))
                            (newline-selected-p
                             (and (= source-end (length line))
                                  (< newline-offset (length content))
                                  (<= selection-start newline-offset)
                                  (< newline-offset selection-end))))
                       (when (and (or (< start end)
                                      newline-selected-p)
                                  (row-visible-p row
                                                 viewport-y
                                                 viewport-height))
                         (backend-draw-selection
                          backend
                          (1+ (bounds-x (cell-bounds cell)))
                          (- row viewport-y)
                          :text-prefix (subseq line
                                               source-start
                                               (- start line-start))
                          :selected-text (subseq line
                                                 (- start line-start)
                                                 (- end line-start))
                          :trailing-cell-p newline-selected-p))))))))))

(defun draw-cell-tree (backend cell &key (viewport-y 0) viewport-height)
  (let* ((bounds (cell-bounds cell))
         (x (bounds-x bounds))
         (y (bounds-y bounds))
         (width (bounds-width bounds))
         (height (bounds-height bounds))
         (screen-y (- y viewport-y))
         (viewport-height (or viewport-height
                              most-positive-fixnum))
         (label (slot-value cell 'label))
         (focusedp (and *application*
                        (cell-model cell)
                        (application-focused-model-id *application*)
                        (string=
                         (object-id (cell-model cell))
                         (application-focused-model-id *application*)))))
    (when (cell-intersects-viewport-p cell viewport-y viewport-height)
      (backend-draw-box backend x screen-y width height label focusedp)
      (when (typep cell 'text-cell)
        (draw-cell-selection backend cell viewport-y viewport-height)
        (loop for (line style-spans) in (visible-lines-with-style-spans-for-cell cell)
              for row from 0
              for logical-row = (+ y 1 row)
              do (when (and (plusp (length line))
                            (row-visible-p logical-row
                                           viewport-y
                                           viewport-height))
                   (if style-spans
                       (backend-draw-styled-text backend
                                                 (+ x 1)
                                                 (- logical-row viewport-y)
                                                 line
                                                 style-spans)
                       (backend-draw-text backend
                                          (+ x 1)
                                          (- logical-row viewport-y)
                                          line)))))
      (when (typep cell 'text-cell)
        (draw-cell-caret backend cell viewport-y viewport-height))
      (dolist (child (children-of cell))
        (draw-cell-tree backend
                        child
                        :viewport-y viewport-y
                        :viewport-height viewport-height)))))

(defmethod run-backend ((backend null-backend) application)
  (render-application-if-needed application)
  backend)

(defun sdl2-scancode-key (keysym)
  (let ((scancode (sdl2:scancode-value keysym)))
    (cond
      ((sdl2:scancode= scancode :scancode-escape) :escape)
      ((sdl2:scancode= scancode :scancode-backspace) :backspace)
      ((sdl2:scancode= scancode :scancode-delete) :delete)
      ((sdl2:scancode= scancode :scancode-home) :home)
      ((sdl2:scancode= scancode :scancode-end) :end)
      ((sdl2:scancode= scancode :scancode-pageup) :pageup)
      ((sdl2:scancode= scancode :scancode-pagedown) :pagedown)
      ((sdl2:scancode= scancode :scancode-left) :left)
      ((sdl2:scancode= scancode :scancode-right) :right)
      ((sdl2:scancode= scancode :scancode-up) :up)
      ((sdl2:scancode= scancode :scancode-down) :down)
      ((sdl2:scancode= scancode :scancode-return) :return)
      ((sdl2:scancode= scancode :scancode-f12) :f12)
      ((sdl2:scancode= scancode :scancode-leftbracket) :leftbracket)
      ((sdl2:scancode= scancode :scancode-rightbracket) :rightbracket)
      ((sdl2:scancode= scancode :scancode-q) :q)
      ((sdl2:scancode= scancode :scancode-j) :j)
      ((sdl2:scancode= scancode :scancode-k) :k)
      ((sdl2:scancode= scancode :scancode-i) :i)
      ((sdl2:scancode= scancode :scancode-e) :e)
      ((sdl2:scancode= scancode :scancode-s) :s)
      ((sdl2:scancode= scancode :scancode-r) :r)
      ((sdl2:scancode= scancode :scancode-v) :v)
      ((sdl2:scancode= scancode :scancode-x) :x)
      ((sdl2:scancode= scancode :scancode-w) :w)
      ((sdl2:scancode= scancode :scancode-u) :u)
      ((sdl2:scancode= scancode :scancode-y) :y)
      ((sdl2:scancode= scancode :scancode-z) :z)
      (t nil))))

(defun handle-sdl2-keydown (application keysym)
  (let* ((modifiers (sdl2:mod-value keysym))
         (controlp (sdl2:mod-value-p modifiers :lctrl :rctrl))
         (shiftp (sdl2:mod-value-p modifiers :lshift :rshift))
         (altp (sdl2:mod-value-p modifiers :lalt :ralt))
         (metap (sdl2:mod-value-p modifiers :lgui :rgui)))
    (dispatch-application-key
     application
     (make-key-event :key (sdl2-scancode-key keysym)
                     :text (printable-key-text keysym)
                     :controlp controlp
                     :shiftp shiftp
                     :altp altp
                     :metap metap))))

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
             (render-application-if-needed application)
             (sdl2:with-event-loop (:method :poll)
               (:keydown (:keysym keysym)
			 (handle-sdl2-keydown application keysym))
               (:mousewheel (:y y)
                            (scroll-application application (* -3 y)))
               (:mousebuttondown (:x x :y y)
				 (focus-model-at-pixel application x y))
               (:idle ()
                      (multiple-value-bind (width height)
			  (sdl2:get-window-size window)
                        (unless (and (= width (backend-pixel-width backend))
                                     (= height (backend-pixel-height backend)))
                          (setf (backend-pixel-width backend) width)
                          (setf (backend-pixel-height backend) height)
                          (mark-application-dirty application)))
                      (sync-sdl2-text-input-state backend application)
                      (render-application-if-needed application)
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
