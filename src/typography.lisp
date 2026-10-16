(in-package :orra)

(defparameter *default-interface-typography*
  '(:body (:family "serif" :size 18)
    :heading (:family "serif" :size 24)
    :code (:family "monospace" :size 16)
    :interface (:family "sans-serif" :size 16)
    :metadata (:family "sans-serif" :size 14)))

(defvar *rendering-text-cell* nil)
(defvar *text-measurement-backend* nil)

(defun normalize-interface-typography (specification &optional (base-size 16))
  "Validate configuration as data and own all strings/lists per backend."
  (check-type base-size (integer 1 256))
  (unless (and (listp specification) (ignore-errors (evenp (length specification))))
    (error "Typography must be a property list."))
  (let ((result (loop for (role options) on *default-interface-typography* by #'cddr
                      collect role
                      collect (loop for (key value) on options by #'cddr
                                    collect key collect (if (stringp value) (copy-seq value) value))))
        (seen nil))
    (loop for tail on result by #'cddr
          for options = (second tail)
          do (setf (getf options :size) (max 1 (min 256 (round (* (getf options :size) base-size) 16)))))
    (loop for (role options) on specification by #'cddr
          do (unless (and (member role '(:body :heading :code :interface :metadata))
                          (not (member role seen)))
               (error "Unknown or duplicate typography role: ~S" role))
          (push role seen)
          (unless (and (listp options) (ignore-errors (evenp (length options))))
            (error "Typography options for ~S must be a property list." role))
          (let ((keys nil))
            (loop for (key value) on options by #'cddr
                  do (unless (and (member key '(:family :size :font-path)) (not (member key keys)))
                       (error "Unknown or duplicate typography option: ~S" key))
                  (push key keys)
                  (unless (case key
                            (:family (and (stringp value) (plusp (length value))))
                            (:size (typep value '(integer 1 256)))
                            (:font-path (or (pathnamep value)
                                            (and (stringp value) (plusp (length value))))))
                    (error "Invalid typography option ~S: ~S" key value))
                  (setf (getf (getf result role) key)
                        (if (stringp value) (copy-seq value) value)))))
    result))

(defun text-style-for-cell (cell)
  (or (and cell (cell-text-style cell))
      (cond
        ((null cell) :interface)
        ((eq (cell-role cell) :metadata) :metadata)
        ((and (eq (cell-role cell) :heading) (typep (cell-model cell) '(or notebook section))) :heading)
        ((and (not (eq (cell-role cell) :heading))
              (typep (cell-model cell) '(or paragraph quote-block))) :body)
        ((and (not (eq (cell-role cell) :heading))
              (typep (cell-model cell) '(or code-block result-block repl-entry))) :code)
        (t :interface))))

(defgeneric backend-measure-cell-text (backend cell text)
  (:method (backend cell text) (declare (ignore backend cell)) (length text)))

(defgeneric backend-cell-text-width-limit (backend cell)
  (:method (backend cell) (declare (ignore backend)) (max 1 (- (bounds-width (cell-bounds cell)) 2))))

(defgeneric backend-cell-caret-width (backend cell)
  (:method (backend cell) (declare (ignore backend cell)) 1))

(defgeneric backend-cell-line-step (backend cell)
  (:method (backend cell) (declare (ignore backend cell)) 1))

(defgeneric backend-cell-relative-text-x (backend cell pixel-x)
  (:method (backend cell pixel-x)
    (declare (ignore backend)) (- pixel-x (1+ (bounds-x (cell-bounds cell))))))

(defun prepare-cell-text-metrics (backend cell &optional (width (bounds-width (cell-bounds cell))))
  (when (typep cell 'text-cell)
    (setf (bounds-width (cell-bounds cell)) (max 1 width)
          (cell-line-step cell) (max 1 (backend-cell-line-step backend cell)))
    (when (cell-wrap-p cell)
      (unless (and (cell-wrap-source cell) (equal (cell-text cell) (cell-wrapped-text cell)))
        (setf (cell-wrap-source cell) (cell-text cell)))
      (setf (cell-text cell) (wrap-measured-cell-text backend cell (cell-wrap-source cell))
            (cell-wrapped-text cell) (cell-text cell))))
  (let* ((inset (if (typep cell 'container-cell)
                    (min (cell-inset cell) (floor (1- (max 1 width)) 2)) 0))
         (child-width (max 1 (- width (* 2 inset)))))
    (dolist (child (children-of cell)) (prepare-cell-text-metrics backend child child-width)))
  cell)

(defun fitting-text-end (backend cell text start limit &optional (end (length text)))
  "Find the longest source prefix that fits, retaining character offsets."
  (let ((low start) (high end))
    (loop while (< low high)
          for middle = (ceiling (+ low high) 2)
          do (if (<= (backend-measure-cell-text backend cell (subseq text start middle)) limit)
                 (setf low middle) (setf high (1- middle))))
    low))

(defun wrap-measured-cell-text (backend cell text)
  (let ((limit (backend-cell-text-width-limit backend cell)) (lines nil))
    (dolist (line (split-lines text))
      (let ((start 0))
        (loop while (< start (length line))
              for fit = (fitting-text-end backend cell line start limit)
              for space = (and (< fit (length line))
                               (position #\Space line :start start :end (min (length line) (1+ fit)) :from-end t))
              for end = (max (1+ start) (if (and space (> space start)) space fit))
              do (push (subseq line start end) lines)
              (setf start end)
              (loop while (and (< start (length line)) (char= #\Space (char line start))) do (incf start)))
        (when (zerop (length line)) (push "" lines))))
    (format nil "~{~A~^~%~}" (nreverse lines))))
