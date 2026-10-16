(in-package :orra)

(defun role-font-environment-path (role)
  (let ((value (uiop:getenv (format nil "ORRA_FONT_~A" (if (eq role :interface) "UI" (symbol-name role))))))
    (when (and value (plusp (length value))) value)))

(defun resolve-role-font-path (backend role &key (environment-font (env-font-path))
                                              (role-environment-font (role-font-environment-path role))
                                              (finder #'fontconfig-font-path))
  (let ((options (getf (backend-typography backend) role)))
    (or (normalize-font-path (getf options :font-path))
        role-environment-font
        (normalize-font-path (backend-font-path backend))
        environment-font
        (funcall finder (getf options :family)))))

(defun close-sdl2-role-fonts (backend)
  (let ((fonts (backend-owned-fonts backend)))
    (setf (backend-owned-fonts backend) nil (backend-font backend) nil)
    (clrhash (backend-role-fonts backend))
    (dolist (font fonts) (sdl2-ttf:close-font font))))

(defun open-sdl2-role-fonts (backend)
  (let ((cache (make-hash-table :test #'equal)) (complete nil))
    (close-sdl2-role-fonts backend)
    (unwind-protect
         (progn
           (dolist (role '(:interface :body :heading :code :metadata))
             (let* ((options (getf (backend-typography backend) role))
                    (path (resolve-role-font-path backend role))
                    (size (getf options :size))
                    (font
                     (labels ((open-path (candidate)
                                (when candidate
                                  (or (gethash (list candidate size) cache)
                                      (let ((opened (sdl2-ttf:open-font candidate size)))
                                        (push opened (backend-owned-fonts backend))
                                        (setf (gethash (list candidate size) cache) opened))))))
                       (handler-case (open-path path)
                         (error (condition)
                           (format *error-output* "~&Orra: cannot open ~A font ~A: ~A. Using fontconfig fallback.~%"
                                   role path condition)
                           (setf path (fontconfig-font-path (getf options :family)))
                           (ignore-errors (open-path path)))))))
               (unless font
                 (setf font (backend-font backend))
                 (format *error-output* "~&Orra: no ~A font available; using the UI font. Set ORRA_FONT or configure fc-match.~%" role))
               (unless font (error "Orra cannot open a UI font. Set ORRA_FONT or configure fc-match."))
               (setf (gethash role (backend-role-fonts backend))
                     (list :font font :path path :size size))
               (when (eq role :interface) (setf (backend-font backend) font))))
           (setf complete t))
      (unless complete (close-sdl2-role-fonts backend))))
  backend)

(defun current-text-font (backend &optional (cell *rendering-text-cell*))
  (or (getf (gethash (text-style-for-cell cell) (backend-role-fonts backend)) :font)
      (backend-font backend)))

(defmethod backend-measure-cell-text ((backend sdl2-backend) cell text)
  (if (zerop (length text)) 0
      (let ((font (current-text-font backend cell)))
        (if font (nth-value 0 (sdl2-ttf:size-utf8 font text))
            (* (length text) (backend-cell-width backend))))))

(defmethod backend-cell-text-width-limit ((backend sdl2-backend) cell)
  (* (max 1 (- (bounds-width (cell-bounds cell)) 2)) (backend-cell-width backend)))

(defmethod backend-cell-caret-width ((backend sdl2-backend) cell)
  (declare (ignore cell)) 2)

(defmethod backend-cell-line-step ((backend sdl2-backend) cell)
  (max 1 (ceiling (sdl2-cell-text-height backend cell) (backend-row-height backend))))

(defun sdl2-cell-text-height (backend &optional (cell *rendering-text-cell*))
  (let ((font (current-text-font backend cell)))
    (if font (nth-value 1 (sdl2-ttf:size-utf8 font "Mg")) (backend-row-height backend))))

(defmethod backend-cell-relative-text-x ((backend sdl2-backend) cell pixel-x)
  (- pixel-x (logical-x->pixel backend (1+ (bounds-x (cell-bounds cell))))))

(defun call-with-sdl2-text-texture (backend text red green blue alpha function)
  (let ((font (current-text-font backend)))
    (when (and font (plusp (length text)))
      (let ((surface (sdl2-ttf:render-utf8-blended font text red green blue alpha))
            (texture nil))
        (unwind-protect
             (progn
               (setf texture (sdl2:create-texture-from-surface (backend-renderer backend) surface))
               (funcall function texture (sdl2:surface-width surface) (sdl2:surface-height surface)))
          (when texture (sdl2:destroy-texture texture))
          (release-ttf-surface surface))))))

(defun call-with-sdl2-pixel-clip (backend x y width height function)
  (let ((renderer (backend-renderer backend)))
    (sdl2:with-rects ((previous 0 0 0 0))
      (let ((enabled (not (zerop (sdl2-ffi.functions::sdl-render-is-clip-enabled renderer)))))
        (sdl2-ffi.functions::sdl-render-get-clip-rect renderer previous)
        (when enabled
          (let ((right (min (+ x width) (+ (sdl2:rect-x previous) (sdl2:rect-width previous))))
                (bottom (min (+ y height) (+ (sdl2:rect-y previous) (sdl2:rect-height previous)))))
            (setf x (max x (sdl2:rect-x previous)) y (max y (sdl2:rect-y previous))
                  width (max 0 (- right x)) height (max 0 (- bottom y)))))
        (when (and (plusp width) (plusp height))
          (sdl2:with-rects ((clip x y width height))
            (sdl2::check-rc (sdl2-ffi.functions::sdl-render-set-clip-rect renderer clip))
            (unwind-protect (funcall function)
              (sdl2::check-rc (sdl2-ffi.functions::sdl-render-set-clip-rect renderer (and enabled previous))))))))))

(defun draw-sdl2-styled-line (backend x y text style-spans)
  ;; Color full-line textures through span clips: splitting into independently
  ;; rendered tokens would change kerning and disagree with caret measurements.
  (let ((pixel-x (logical-x->pixel backend x)) (pixel-y (logical-y->pixel backend y))
        (groups (make-hash-table :test #'eq)) (cursor 0))
    (dolist (span (stable-sort (copy-list style-spans) #'< :key (lambda (entry) (getf entry :start 0))))
      (let* ((start (max cursor (min (length text) (getf span :start 0))))
             (end (max start (min (length text) (getf span :end 0)))))
        (when (< cursor start)
          (push (list :start cursor :end start) (gethash nil groups)))
        (when (< start end)
          (push (list :start start :end end) (gethash (getf span :kind) groups)))
        (setf cursor end)))
    (when (< cursor (length text))
      (push (list :start cursor :end (length text)) (gethash nil groups)))
    (maphash
     (lambda (kind spans)
       (multiple-value-bind (red green blue alpha) (style-span-color backend kind)
         (call-with-sdl2-text-texture
          backend text red green blue alpha
          (lambda (texture width height)
            (dolist (span spans)
              (let* ((start (max 0 (min (length text) (getf span :start 0))))
                     (end (max start (min (length text) (getf span :end 0))))
                     (left (backend-measure-cell-text backend *rendering-text-cell* (subseq text 0 start)))
                     (right (backend-measure-cell-text backend *rendering-text-cell* (subseq text 0 end))))
                (call-with-sdl2-pixel-clip
                 backend (+ pixel-x left) pixel-y (- right left) height
                 (lambda ()
                   (sdl2:with-rects ((rect pixel-x pixel-y width height))
                     (sdl2:render-copy (backend-renderer backend) texture :dest-rect rect))))))))))
     groups)))
