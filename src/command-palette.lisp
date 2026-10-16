(in-package :orra)

(defclass command-palette ()
  ((query-buffer :accessor palette-query-buffer :initform (make-text-buffer))
   (arguments-buffer :accessor palette-arguments-buffer :initform (make-text-buffer))
   (arguments-owner :accessor palette-arguments-owner :initform nil)
   (selected-name :accessor palette-selected-name :initform nil)
   (argument-command :accessor palette-argument-command :initform nil)
   (root-cell :accessor palette-root-cell :initform nil)
   (viewport-y :accessor palette-viewport-y :initform 0)
   (reveal :accessor palette-reveal :initform :input)))

(defun command-display-name (command)
  (let ((*package* (find-package :orra)) (*print-case* :downcase))
    (prin1-to-string (command-name command))))

(defun command-argument-hint (command)
  (if (eq :unknown (command-lambda-list command))
      "Argument signature unavailable; consult the command documentation."
      (let ((*package* (find-package :orra)) (*print-case* :downcase))
        (format nil "Arguments: ~S" (rest (command-lambda-list command))))))

(defun command-palette-matches (application)
  (let* ((palette (application-command-palette application))
         (query (text-buffer-content (palette-query-buffer palette)))
         (terms (remove "" (uiop:split-string (string-downcase query)
                                              :separator '(#\Space #\Tab #\Newline))
                        :test #'string=)))
    (sort
     (remove-if-not
      (lambda (command)
        (let ((description (string-downcase
                            (format nil "~A ~A" (command-display-name command)
                                    (command-documentation command)))))
          (every (lambda (term) (search term description)) terms)))
      (list-commands application))
     #'string< :key #'command-display-name)))

(defun selected-palette-command (application)
  (let* ((palette (application-command-palette application))
         (commands (command-palette-matches application))
         (selected (or (find (palette-selected-name palette) commands :key #'command-name)
                       (first commands))))
    (setf (palette-selected-name palette) (and selected (command-name selected)))
    selected))

(defun palette-input-buffer (palette)
  (if (palette-argument-command palette)
      (palette-arguments-buffer palette)
      (palette-query-buffer palette)))

(defun open-command-palette (application)
  (unless (or (application-pending-action application)
              (application-quit-requested-p application)
              (application-object-navigator application)
              (application-command-palette application))
    (setf (application-command-palette application) (make-instance 'command-palette))
    (mark-application-dirty application))
  application)

(defun close-command-palette (application)
  (setf (application-command-palette application) nil)
  (mark-application-dirty application)
  application)

(defun palette-max-viewport-y (application)
  (let ((root (palette-root-cell (application-command-palette application))))
    (max 0 (- (if root (bounds-height (cell-bounds root)) 0)
              (application-viewport-height application)))))

(defun scroll-command-palette (application rows)
  (let ((palette (application-command-palette application)))
    (unless (palette-root-cell palette) (refresh-command-palette application))
    (setf (palette-reveal palette) nil
          (palette-viewport-y palette)
          (max 0 (min (+ rows (palette-viewport-y palette))
                      (palette-max-viewport-y application))))
    (mark-application-dirty application))
  application)

(defun edit-command-palette (application function)
  (let* ((palette (application-command-palette application))
         (buffer (palette-input-buffer palette))
         (old-content (text-buffer-content buffer)))
    (funcall function buffer)
    (unless (or (palette-argument-command palette)
                (string= old-content (text-buffer-content buffer)))
      (selected-palette-command application))
    (setf (palette-reveal palette) :input)
    (mark-application-dirty application))
  application)

(defun step-command-palette (application steps)
  (let ((palette (application-command-palette application)))
    (if (palette-argument-command palette)
        (edit-command-palette application
                              (lambda (buffer)
                                (if (minusp steps) (move-buffer-cursor-up buffer)
                                    (move-buffer-cursor-down buffer))))
        (let* ((commands (command-palette-matches application))
               (selected (selected-palette-command application))
               (index (position selected commands)))
          (when index
            (setf (palette-selected-name palette)
                  (command-name (nth (max 0 (min (+ index steps) (1- (length commands)))) commands))
                  (palette-reveal palette) :selection)
            (mark-application-dirty application)))))
  application)

(defun read-command-arguments (text)
  "Read literal argument data using the standard reader, never EVAL."
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* (find-package :orra)) (eof (gensym)))
      (with-input-from-string (stream text)
        (loop for value = (read stream nil eof)
              until (eq value eof) collect value)))))

(defgeneric prepare-command-arguments-for-ui (application name arguments)
  (:method (application name arguments)
    (declare (ignore application name))
    arguments))

(defmethod prepare-command-arguments-for-ui (application (name (eql 'load-workspace)) arguments)
  (if (= 1 (length arguments))
      (list (uiop:ensure-pathname (first arguments) :ensure-absolute t
                                  :defaults *default-pathname-defaults*))
      arguments))

(defmethod prepare-command-arguments-for-ui (application (name (eql 'recover-workspace)) arguments)
  (if (<= (length arguments) 1)
      (list (resolve-workspace-checkpoint-path application (first arguments)))
      arguments))

(defun execute-palette-command (application command arguments)
  (handler-case
      (let* ((name (command-name command))
             (label (command-display-name command))
             (notification (application-notification application))
             (palette (application-command-palette application))
             (arguments (prepare-command-arguments-for-ui application name arguments)))
        (request-destructive-action
         application label
         (lambda ()
           (unless (eq command (gethash name (application-commands application)))
             (error "Command ~A changed or was removed; select it again before running." label))
           ;; Commands run against the underlying view, not the palette's input/scroll state.
           (setf (application-command-palette application) nil)
           (unwind-protect
                (apply #'invoke-command application name arguments)
             (unless (application-command-palette application)
               (setf (application-command-palette application) palette))))
         :on-success
         (lambda ()
           (when (eq palette (application-command-palette application))
             (close-command-palette application))
           (mark-application-dirty application)
           (when (or (null (application-notification application))
                     (eq notification (application-notification application)))
             (set-application-notification application :command (format nil "Executed ~A" label))))))
    (error (condition)
      (report-application-io-error application "Command" condition)))
  (when (and (application-command-palette application)
             (not (application-pending-action application)))
    (setf (palette-reveal (application-command-palette application)) :input))
  application)

(defun accept-command-palette (application)
  (let* ((palette (application-command-palette application))
         (command (palette-argument-command palette)))
    (if command
        (handler-case
            (execute-palette-command
             application command (read-command-arguments (text-buffer-content (palette-arguments-buffer palette))))
          (error (condition)
            (setf (palette-reveal palette) :input)
            (report-application-io-error application "Arguments" condition)))
        (let ((selected (selected-palette-command application)))
          (when selected
            (if (and (listp (command-lambda-list selected))
                     (= 1 (length (command-lambda-list selected))))
                (execute-palette-command application selected nil)
                (progn
                  (unless (eq selected (palette-arguments-owner palette))
                    (setf (palette-arguments-buffer palette) (make-text-buffer)))
                  (setf (palette-argument-command palette) selected
                        (palette-arguments-owner palette) selected
                        (palette-reveal palette) :input)))
            (mark-application-dirty application)))))
  application)

(defun back-to-palette-search (application)
  (let ((palette (application-command-palette application)))
    (setf (palette-argument-command palette) nil
          (palette-reveal palette) :input)
    (mark-application-dirty application))
  application)

(defun refresh-command-palette (application)
  (let* ((palette (application-command-palette application))
         (command (palette-argument-command palette))
         (selected (unless command (selected-palette-command application)))
         (root (make-container-cell :label "Command palette"))
         (input (make-text-cell :text (text-buffer-content (palette-input-buffer palette))
				:editor-buffer (palette-input-buffer palette) :role :palette-input))
         (selection-cell nil))
    (labels ((line (text)
               (append-child root (make-text-cell :text text :role :metadata :wrap-p t))))
      (line (if command (format nil "Command: ~A" (command-display-name command)) "Command palette"))
      (line "Enter chooses/runs; Esc/F2 cancels; Tab returns to search. Up/Down selects commands.")
      (line (if command "Literal arguments: quote strings, not lists. Blank uses command defaults."
                "Search command names and documentation:"))
      (append-child root input)
      (when command
        (line (command-argument-hint command))
        (line (command-documentation command))
        (line "Example arguments: \"Hello\" :common-lisp (1 2 3). Shift+Enter inserts a newline.")
        (line (format nil "Focused object id: ~A" (or (application-focused-model-id application) "none"))))
      (unless command
        (let ((commands (command-palette-matches application)))
          (line (format nil "~D matching command~:P" (length commands)))
          (unless commands (line "No matches. Change the search or press Esc to return."))
          (dolist (entry commands)
            (let ((cell (line (format nil "~A ~A - ~A" (if (eq entry selected) ">" " ")
                                      (command-display-name entry) (command-documentation entry)))))
              (when (eq entry selected) (setf selection-cell cell))))))
      (line "Palette controls")
      (dolist (text (key-binding-help-lines application :palette)) (line text)))
    (perform-layout root :width (backend-layout-width (application-backend application))
                    :backend (application-backend application))
    (setf (palette-root-cell palette) root)
    (let* ((target (if (eq :selection (palette-reveal palette)) selection-cell input))
           (row (when target
                  (+ 1 (bounds-y (cell-bounds target))
                     (if (eq target input)
                         (* (cell-line-step input)
                            (nth-value 0 (string-line-column (text-buffer-content (palette-input-buffer palette))
                                                             (text-buffer-cursor (palette-input-buffer palette)))))
                         0))))
           (height (application-viewport-height application)))
      (when (and row (palette-reveal palette))
        (setf (palette-viewport-y palette)
              (max 0 (min (palette-viewport-y palette) (1- row))))
        (when (> (+ row (cell-line-step target)) (+ (palette-viewport-y palette) height))
          (setf (palette-viewport-y palette) (+ (cell-line-step target) (- row height)))))
      (setf (palette-viewport-y palette)
            (max 0 (min (palette-viewport-y palette) (palette-max-viewport-y application)))
            (palette-reveal palette) nil))
    application))

(define-command open-command-palette (application)
  "Browse and run commands installed in the live Lisp image."
  (open-command-palette application))

(define-key-binding (:global :f2) (application event)
  "Browse and run live commands."
  (declare (ignore event))
  (open-command-palette application)
  t)

(define-key-binding (:help :f2) (application event)
  "Browse and run live commands."
  (declare (ignore event))
  (open-command-palette application)
  t)

(dolist (key '(:escape :f2))
  (register-key-binding
   (make-instance 'key-binding :context :palette :key key
                  :documentation "Cancel the palette and return to your work."
                  :function (lambda (application event)
                              (declare (ignore event))
                              (close-command-palette application) t))))

(define-key-binding (:palette :return) (application event)
  "Choose the selected command or run it with literal argument data."
  (declare (ignore event))
  (accept-command-palette application)
  t)

(define-key-binding (:palette :return :shift t) (application event)
  "Insert a newline in the palette input."
  (declare (ignore event))
  (edit-command-palette application (lambda (buffer) (insert-buffer-text buffer (string #\Newline))))
  t)

(define-key-binding (:palette :tab) (application event)
  "Return to command search without changing the notebook editor."
  (declare (ignore event))
  (back-to-palette-search application)
  t)

(define-key-binding (:palette :up) (application event)
  "Select the previous command, or move up in arguments."
  (declare (ignore event))
  (step-command-palette application -1)
  t)

(define-key-binding (:palette :down) (application event)
  "Select the next command, or move down in arguments."
  (declare (ignore event))
  (step-command-palette application 1)
  t)

(define-key-binding (:palette :g :control t) (application event)
  "Dismiss feedback, or cancel the palette if no feedback is visible."
  (declare (ignore event))
  (if (application-notification application) (dismiss-application-notification application)
      (close-command-palette application))
  t)

;; Transient inputs share ordinary live bindings, not backend-specific handlers.
(defun register-transient-input-bindings (context edit-function label)
  (dolist (spec (list (list :backspace nil nil #'delete-buffer-backward "Delete backward.")
                      (list :delete nil nil #'delete-buffer-forward "Delete forward.")
                      (list :left nil :any (lambda (buffer event)
                                             (move-buffer-cursor-left buffer :extend-selection (key-event-shiftp event)))
                            "Move left; Shift extends selection.")
                      (list :right nil :any (lambda (buffer event)
                                              (move-buffer-cursor-right buffer :extend-selection (key-event-shiftp event)))
                            "Move right; Shift extends selection.")
                      (list :home nil :any (lambda (buffer event)
                                             (move-buffer-cursor-home buffer :extend-selection (key-event-shiftp event)))
                            "Move to line start; Shift extends selection.")
                      (list :end nil :any (lambda (buffer event)
                                            (move-buffer-cursor-end buffer :extend-selection (key-event-shiftp event)))
                            "Move to line end; Shift extends selection.")
                      (list :a t nil (lambda (buffer event)
                                       (declare (ignore event))
                                       (move-buffer-cursor-to buffer 0)
                                       (move-buffer-cursor-to buffer (length (text-buffer-content buffer)) :extend-selection t))
                            (format nil "Select all ~A input." label))
                      (list :z t nil #'undo-buffer-edit (format nil "Undo ~A input." label))
                      (list :y t nil #'redo-buffer-edit (format nil "Redo ~A input." label))))
    (destructuring-bind (key control shift function documentation) spec
      (let ((function function) (eventp (member key '(:left :right :home :end :a))))
        (register-key-binding
         (make-instance 'key-binding :context context :key key :control control :shift shift
                        :documentation documentation
                        :function (lambda (application event)
                                    (funcall edit-function application
                                             (lambda (buffer)
                                               (if eventp (funcall function buffer event)
                                                   (funcall function buffer))))
                                    t)))))))

(register-transient-input-bindings :palette #'edit-command-palette "palette")

(define-key-binding (:palette :pageup) (application event)
  "Scroll the palette up one page."
  (declare (ignore event))
  (scroll-application-page application -1)
  t)

(define-key-binding (:palette :pagedown) (application event)
  "Scroll the palette down one page."
  (declare (ignore event))
  (scroll-application-page application 1)
  t)
