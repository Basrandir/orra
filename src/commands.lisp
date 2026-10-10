(in-package :orra)

(defclass command ()
  ((name
    :initarg :name
    :reader command-name)
   (documentation
    :initarg :documentation
    :reader command-documentation)
   (function
    :initarg :function
    :reader command-function)))

(defclass key-binding ()
  ((context
    :initarg :context
    :reader key-binding-context)
   (key
    :initarg :key
    :reader key-binding-key)
   (control
    :initarg :control
    :reader key-binding-control
    :initform nil)
   (shift
    :initarg :shift
    :reader key-binding-shift
    :initform nil)
   (alt
    :initarg :alt
    :reader key-binding-alt
    :initform nil)
   (meta
    :initarg :meta
    :reader key-binding-meta
    :initform nil)
   (documentation
    :initarg :documentation
    :reader key-binding-documentation)
   (function
    :initarg :function
    :reader key-binding-function)))

(defstruct key-event
  key
  text
  controlp
  shiftp
  altp
  metap)

(defvar *command-definitions* (make-hash-table :test #'equal))
(defvar *key-binding-definitions* nil)

(defun key-binding-descriptor (context key control shift &optional alt meta)
  ;; Keep existing descriptors valid for live, user-customized keymaps.
  (append (list context key (not (null control)) shift)
          (when (or alt meta)
            (list (not (null alt)) (not (null meta))))))

(defun binding-descriptor (binding)
  (key-binding-descriptor
   (key-binding-context binding)
   (key-binding-key binding)
   (key-binding-control binding)
   (key-binding-shift binding)
   (key-binding-alt binding)
   (key-binding-meta binding)))

(defun register-key-binding (binding)
  (let ((descriptor (binding-descriptor binding)))
    (setf *key-binding-definitions*
          (remove descriptor
                  *key-binding-definitions*
                  :key #'binding-descriptor
                  :test #'equal))
    (push binding *key-binding-definitions*))
  binding)

(defmacro define-command (name lambda-list documentation &body body)
  `(setf (gethash ',name *command-definitions*)
         (make-instance 'command
                        :name ',name
                        :documentation ,documentation
                        :function (lambda ,lambda-list ,@body))))

(defmacro define-key-binding ((context key &key control (shift nil) alt meta)
					     lambda-list
					     documentation
                              &body body)
  `(register-key-binding
    (make-instance 'key-binding
                   :context ,context
                   :key ,key
                   :control ,control
                   :shift ,shift
                   :alt ,alt
                   :meta ,meta
                   :documentation ,documentation
                   :function (lambda ,lambda-list ,@body))))

(defun list-commands (application)
  (let (commands)
    (maphash (lambda (name command)
               (declare (ignore name))
               (push command commands))
             (application-commands application))
    (sort commands #'string< :key (lambda (command)
                                    (symbol-name (command-name command))))))

(defun install-defined-commands (application)
  (maphash (lambda (name command)
             (setf (gethash name (application-commands application)) command))
           *command-definitions*)
  application)

(defun install-defined-key-bindings (application)
  (clrhash (application-keymap application))
  (dolist (binding *key-binding-definitions* application)
    (setf (gethash (binding-descriptor binding)
                   (application-keymap application))
          binding)))

(defun find-context-key-binding (application context key controlp shiftp
                                 &optional altp metap)
  (or (gethash (key-binding-descriptor context key controlp shiftp altp metap)
               (application-keymap application))
      (gethash (key-binding-descriptor context key controlp :any altp metap)
               (application-keymap application))))

(defun find-key-binding (application context key controlp shiftp &optional altp metap)
  (or (find-context-key-binding application context key controlp shiftp altp metap)
      (find-context-key-binding application :global key controlp shiftp altp metap)))

(defun key-binding-label (binding &key (shift (key-binding-shift binding)))
  (let* ((key (key-binding-key binding))
         (name (case key
                 (:return "Enter") (:escape "Esc")
                 (:leftbracket "[") (:rightbracket "]")
                 (:pageup "PgUp") (:pagedown "PgDn")
                 (otherwise
                  (let ((name (string key)))
                    (if (= 1 (length name))
                        (string-upcase name)
                        (string-capitalize name)))))))
    (format nil "~A~A~A~A~A"
            (if (key-binding-control binding) "Ctrl+" "")
            (if (key-binding-alt binding) "Alt+" "")
            (if (key-binding-meta binding) "Meta+" "")
            (case shift (:any "[Shift+]") ((nil) "") (otherwise "Shift+"))
            name)))

(defun key-binding-help-lines (application context)
  "Describe effective bindings, excluding shadowed global or Shift variants."
  (let* ((modalp (member context '(:help :confirm)))
         (finder (if modalp #'find-context-key-binding #'find-key-binding)))
    (loop for binding in (list-key-bindings application)
          when (or (eq (key-binding-context binding) context)
                   (and (not modalp)
                        (eq (key-binding-context binding) :global)))
          nconc (let* ((shifts (if (eq (key-binding-shift binding) :any)
                                   '(nil t)
                                   (list (key-binding-shift binding))))
                       (effective-shifts
                        (remove-if-not
                         (lambda (shift)
                           (eq binding
                               (funcall finder application context
                                        (key-binding-key binding)
                                        (key-binding-control binding) shift
                                        (key-binding-alt binding)
                                        (key-binding-meta binding))))
                         shifts)))
                  (when effective-shifts
                    (list (format nil "~A  -  ~A"
                                  (key-binding-label
                                   binding :shift (if (= 2 (length effective-shifts))
                                                      :any (first effective-shifts)))
                                  (or (key-binding-documentation binding) ""))))))))

(defun list-key-bindings (application)
  (let (bindings)
    (maphash (lambda (descriptor binding)
               (declare (ignore descriptor))
               (push binding bindings))
             (application-keymap application))
    (sort bindings
          #'string<
          :key (lambda (binding)
                 (format nil "~A ~A ~A ~A ~A ~A"
                         (key-binding-context binding)
                         (key-binding-control binding)
                         (key-binding-shift binding)
                         (key-binding-alt binding)
                         (key-binding-meta binding)
                         (key-binding-key binding))))))

(defun invoke-command (application name &rest arguments)
  (let ((command (gethash name (application-commands application))))
    (unless command
      (error "Unknown command ~S." name))
    (when (fboundp 'record-application-event)
      (funcall (symbol-function 'record-application-event)
               application
               :command
               (symbol-name name)))
    (apply (command-function command) application arguments)))
