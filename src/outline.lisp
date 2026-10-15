(in-package :orra)

(defparameter *outline-sidebar-width* 26)
(defparameter *outline-min-document-width* 72)

(defclass notebook-outline ()
  ((visible-p :accessor outline-visible-p :initform t)
   (active-p :accessor outline-keyboard-active-p :initform nil)
   (collapsed-ids :accessor outline-collapsed-ids :initform (make-hash-table :test #'equal))
   (selected-id :accessor outline-selected-id :initform nil)
   (selected-object :accessor outline-selected-object :initform nil)
   (viewport-y :accessor outline-viewport-y :initform 0)
   (root-cell :accessor outline-root-cell :initform nil)))

(defun ensure-application-outline (application)
  (or (application-outline application)
      (setf (application-outline application) (make-instance 'notebook-outline))))

(defun outline-active-p (application)
  (and (application-outline application)
       (outline-keyboard-active-p (application-outline application))))

(defun outline-input-blocked-p (application)
  (or (application-help-visible-p application)
      (application-command-palette application)
      (application-object-navigator application)
      (application-pending-action application)
      (application-quit-requested-p application)))

(defun outline-sidebar-p (application)
  (and (or (null (application-outline application))
           (outline-visible-p (application-outline application)))
       (>= (backend-layout-width (application-backend application))
           (+ *outline-sidebar-width* 1 *outline-min-document-width*))))

(defun layout-application-document (application)
  (let ((x (if (outline-sidebar-p application) (1+ *outline-sidebar-width*) 0)))
    (perform-layout (application-root-cell application) :x x
                    :width (max 1 (- (backend-layout-width (application-backend application)) x)))))

(defun outline-entries (application &key include-collapsed)
  "Derive navigation rows from live objects, never from document cells."
  (let ((outline (ensure-application-outline application))
        (entries nil))
    (map-workspace-content
     (lambda (object notebook ancestors)
       (declare (ignore notebook))
       (when (typep object '(or notebook section))
         (let ((parents (remove-if-not (lambda (parent) (typep parent '(or notebook section))) ancestors)))
           (unless (and (not include-collapsed)
                        (some (lambda (parent) (gethash (object-id parent) (outline-collapsed-ids outline)))
                              parents))
             (push (list :object object :id (object-id object) :parents parents
                         :depth (length parents)) entries)))))
     (application-workspace application))
    (nreverse entries)))

(defun outline-focus-anchor (application)
  (let ((seen (make-hash-table :test #'eq)))
    (loop for object = (focused-model application) then (parent-of object)
          while (and object (not (gethash object seen)))
          do (setf (gethash object seen) t)
          when (typep object '(or notebook section)) return (object-id object))))

(defun selected-outline-entry (application &optional (entries (outline-entries application)))
  (let* ((outline (ensure-application-outline application))
         (id (if (outline-active-p application)
                 (or (outline-selected-id outline) (outline-focus-anchor application))
                 (outline-focus-anchor application)))
         (all (outline-entries application :include-collapsed t))
         (previous (find id all :key (lambda (entry) (getf entry :id)) :test #'equal))
         (entry (or (find id entries :key (lambda (entry) (getf entry :id)) :test #'equal)
                    (loop for parent in (reverse (getf previous :parents))
                          for visible = (find (object-id parent) entries
                                              :key (lambda (entry) (getf entry :id)) :test #'equal)
                          when visible return visible)
                    (first entries))))
    (when (or (not (outline-active-p application))
              (not (equal (outline-selected-id outline) (getf entry :id)))
              (null (outline-selected-object outline)))
      (setf (outline-selected-object outline) (getf entry :object)))
    (setf (outline-selected-id outline) (getf entry :id))
    entry))

(defun outline-branch-p (entry entries)
  (some (lambda (candidate) (member (getf entry :object) (getf candidate :parents))) entries))

(defun outline-max-viewport-y (application)
  (let ((root (outline-root-cell (ensure-application-outline application))))
    (max 0 (- (if root (bounds-height (cell-bounds root)) 0)
              (application-viewport-height application)))))

(defun refresh-application-outline (application &key reveal-selection)
  (let* ((outline (ensure-application-outline application))
         (previous-selection (outline-selected-id outline))
         (entries (outline-entries application))
         (all (outline-entries application :include-collapsed t))
         (branches (make-hash-table :test #'equal))
         (selected (selected-outline-entry application entries))
         (width (if (outline-sidebar-p application) *outline-sidebar-width*
                    (max 1 (backend-layout-width (application-backend application)))))
         (root (make-container-cell :label "Outline" :inset 0 :top-inset 0 :spacing 0))
         (selected-cell nil))
    (dolist (entry all)
      (dolist (parent (getf entry :parents))
        (setf (gethash (object-id parent) branches) t)))
    (append-child root (make-text-cell :text "Notebooks / sections" :role :heading))
    (append-child root (make-text-cell :text (if (outline-active-p application)
                                                 "F5 hide | Esc return"
                                                 "F5 focus outline") :role :metadata))
    (dolist (entry entries)
      (let* ((id (getf entry :id))
             (cell (make-text-cell
                    :text (format nil "~A~A ~A"
                                  (make-string (* 2 (getf entry :depth)) :initial-element #\Space)
                                  (if (gethash id branches)
                                      (if (gethash id (outline-collapsed-ids outline)) "[+]" "[-]") "   ")
                                  (navigation-preview (navigation-object-title (getf entry :object))))
                    :role :metadata)))
        ;; Rows refer to IDs as view metadata, not focusable/editable model cells.
        (set-object-property cell :outline-id id)
        (set-object-property cell :outline-depth (getf entry :depth))
        (when (eq entry selected)
          (setf (cell-decoration cell) :outline-selected
                (cell-role cell) :heading
                selected-cell cell))
        (append-child root cell)))
    (unless entries
      (append-child root (make-text-cell :text "No notebooks" :role :metadata)))
    (perform-layout root :width (max 1 (1- width)))
    (setf (outline-root-cell outline) root)
    (when (and selected-cell
               (or reveal-selection
                   (and (not (outline-active-p application))
                        (not (equal previous-selection (outline-selected-id outline))))))
      (let* ((row (1+ (bounds-y (cell-bounds selected-cell))))
             (height (application-viewport-height application)))
        (when (< row (outline-viewport-y outline))
          (setf (outline-viewport-y outline) row))
        (when (>= row (+ (outline-viewport-y outline) height))
          (setf (outline-viewport-y outline) (1+ (- row height))))))
    (setf (outline-viewport-y outline)
          (max 0 (min (outline-viewport-y outline) (outline-max-viewport-y application))))
    root))

(defun close-application-outline (application &key hide)
  (let ((outline (ensure-application-outline application)))
    (setf (outline-keyboard-active-p outline) nil)
    (when hide (setf (outline-visible-p outline) nil)))
  (layout-application-document application)
  (mark-application-dirty application)
  application)

(defun toggle-application-outline (application)
  (unless (outline-input-blocked-p application)
    (let ((outline (ensure-application-outline application)))
      (if (outline-active-p application)
          (close-application-outline application :hide t)
          (progn
            (setf (outline-visible-p outline) t
                  (outline-keyboard-active-p outline) t
                  (outline-selected-id outline) (outline-focus-anchor application)
                  (outline-selected-object outline) nil)
            (layout-application-document application)
            (refresh-application-outline application :reveal-selection t)
            (mark-application-dirty application)))))
  application)

(defun step-outline-selection (application steps)
  (let* ((entries (outline-entries application))
         (index (position (selected-outline-entry application entries) entries)))
    (when index
      (setf (outline-selected-id (ensure-application-outline application))
            (getf (nth (max 0 (min (+ index steps) (1- (length entries)))) entries) :id)
            (outline-selected-object (ensure-application-outline application)) nil)
      (refresh-application-outline application :reveal-selection t)
      (mark-application-dirty application)))
  application)

(defun change-outline-expansion (application direction)
  (let* ((outline (ensure-application-outline application))
         (entry (selected-outline-entry application))
         (id (getf entry :id))
         (all (outline-entries application :include-collapsed t))
         (collapsed (gethash id (outline-collapsed-ids outline))))
    (when entry
      (cond
        ((and (plusp direction) collapsed) (remhash id (outline-collapsed-ids outline)))
        ((and (minusp direction) (not collapsed) (outline-branch-p entry all))
         (setf (gethash id (outline-collapsed-ids outline)) t))
        ((plusp direction)
         (when (outline-branch-p entry all) (step-outline-selection application 1)))
        (t (let ((parent (car (last (getf entry :parents)))))
             (when parent (setf (outline-selected-id outline) (object-id parent)
                                (outline-selected-object outline) nil)))))
      (refresh-application-outline application :reveal-selection t)
      (mark-application-dirty application)))
  application)

(defun accept-outline-selection (application)
  (unless (outline-input-blocked-p application)
    (let* ((outline (ensure-application-outline application))
           (id (outline-selected-id outline))
           (entry (find id (outline-entries application)
                        :key (lambda (entry) (getf entry :id)) :test #'equal))
           (target (getf entry :object)))
      (when id
        (handler-case
            (progn
              (unless (and target (eq target (outline-selected-object outline)))
                (error "The selected outline object changed or was removed; choose it again."))
              (multiple-value-bind (focused status) (focus-linked-object application target)
                (unless focused (error "Cannot navigate to this outline entry: ~A." status))
                (close-application-outline application)))
          (error (condition)
            (report-application-io-error application "Outline navigation" condition))))))
  application)

(defun scroll-application-outline (application rows)
  (refresh-application-outline application)
  (let ((outline (ensure-application-outline application)))
    (setf (outline-viewport-y outline)
          (max 0 (min (+ rows (outline-viewport-y outline)) (outline-max-viewport-y application)))))
  (mark-application-dirty application)
  application)

(defun outline-point-p (application x y)
  (and (not (outline-input-blocked-p application))
       (or (outline-sidebar-p application) (outline-active-p application))
       (<= 0 x) (< x (if (outline-sidebar-p application) *outline-sidebar-width*
                         (backend-layout-width (application-backend application))))
       (<= 0 y) (< y (application-viewport-height application))))

(defun handle-outline-click (application x y)
  (let* ((root (refresh-application-outline application))
         (outline (ensure-application-outline application))
         (cell (find-cell-at-point root x (+ y (outline-viewport-y outline))))
         (id (and cell (object-property cell :outline-id :inherit nil))))
    (when id
      (setf (outline-keyboard-active-p outline) t
            (outline-selected-id outline) id
            (outline-selected-object outline) nil)
      (let ((entry (selected-outline-entry application)))
        (if (and (< x (+ 4 (* 2 (object-property cell :outline-depth :inherit nil))))
                 (outline-branch-p entry (outline-entries application :include-collapsed t)))
            (let ((collapsed (gethash id (outline-collapsed-ids outline))))
              (change-outline-expansion application (if collapsed 1 -1)))
            (accept-outline-selection application)))))
  application)

(defun scroll-application-at-pixel (application rows pixel-x pixel-y)
  (multiple-value-bind (x y)
      (backend-grid-point (application-backend application) pixel-x pixel-y)
    (if (or (outline-point-p application x y)
            (and (outline-active-p application) (not (outline-sidebar-p application))
                 (not (outline-input-blocked-p application))))
        (scroll-application-outline application rows)
        ;; Pointer ownership takes priority over the keyboard-focused pane.
        (let* ((outline (application-outline application))
               (active (and outline (outline-keyboard-active-p outline))))
          (unwind-protect
               (progn
                 (when outline (setf (outline-keyboard-active-p outline) nil))
                 (scroll-application application rows))
            (when outline (setf (outline-keyboard-active-p outline) active))))))
  application)

(defun draw-application-outline (application)
  (let* ((root (refresh-application-outline application))
         (outline (ensure-application-outline application))
         (backend (application-backend application))
         (width (if (outline-sidebar-p application) *outline-sidebar-width* (backend-layout-width backend)))
         (height (application-viewport-height application)))
    (backend-call-with-clip
     backend (make-bounds :width (max 1 width) :height height)
     (lambda ()
       (backend-draw-cell backend (make-container-cell :decoration :inspector) 0 0 width height nil)
       (draw-cell-tree backend root :viewport-y (outline-viewport-y outline) :viewport-height height)
       (draw-application-scrollbar application :content-height (bounds-height (cell-bounds root))
                                   :viewport-y (outline-viewport-y outline) :x (1- width))))))

(define-command toggle-outline (application)
  "Focus the notebook outline, or hide it when already focused."
  (toggle-application-outline application))

(define-key-binding (:global :f5) (application event)
  "Focus or show the notebook outline."
  (declare (ignore event)) (invoke-command application 'toggle-outline) t)

(define-key-binding (:outline :f5) (application event)
  "Hide the outline and return to the document."
  (declare (ignore event)) (close-application-outline application :hide t) t)

(define-key-binding (:outline :escape) (application event)
  "Return to the document without changing the editor."
  (declare (ignore event)) (close-application-outline application) t)

(define-key-binding (:outline :g :control t) (application event)
  "Return to the document without changing the editor."
  (declare (ignore event)) (close-application-outline application) t)

(define-key-binding (:outline :up) (application event)
  "Select the previous outline entry."
  (declare (ignore event)) (step-outline-selection application -1) t)

(define-key-binding (:outline :down) (application event)
  "Select the next outline entry."
  (declare (ignore event)) (step-outline-selection application 1) t)

(define-key-binding (:outline :left) (application event)
  "Collapse this branch, or select its parent."
  (declare (ignore event)) (change-outline-expansion application -1) t)

(define-key-binding (:outline :right) (application event)
  "Expand this branch, or select its first child."
  (declare (ignore event)) (change-outline-expansion application 1) t)

(define-key-binding (:outline :return) (application event)
  "Navigate to the selected notebook or section without evaluating it."
  (declare (ignore event)) (accept-outline-selection application) t)

(define-key-binding (:outline :pageup) (application event)
  "Scroll the outline up one page."
  (declare (ignore event)) (scroll-application-page application -1) t)

(define-key-binding (:outline :pagedown) (application event)
  "Scroll the outline down one page."
  (declare (ignore event)) (scroll-application-page application 1) t)

(define-key-binding (:outline :f1) (application event)
  "Show keyboard help for the outline."
  (declare (ignore event)) (toggle-application-keyboard-help application) t)

(define-key-binding (:outline :f2) (application event)
  "Open the live command palette."
  (declare (ignore event)) (open-command-palette application) t)

(define-key-binding (:outline :f3) (application event)
  "Search all workspace objects."
  (declare (ignore event)) (open-object-navigator application) t)
