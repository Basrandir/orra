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

(defun key-binding-descriptor (context key control shift)
  (list context key (not (null control)) shift))

(defun binding-descriptor (binding)
  (key-binding-descriptor
   (key-binding-context binding)
   (key-binding-key binding)
   (key-binding-control binding)
   (key-binding-shift binding)))

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

(defmacro define-key-binding ((context key &key control (shift nil))
					     lambda-list
					     documentation
                              &body body)
  `(register-key-binding
    (make-instance 'key-binding
                   :context ,context
                   :key ,key
                   :control ,control
                   :shift ,shift
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

(defun find-key-binding (application context key controlp shiftp)
  (or (gethash (key-binding-descriptor context key controlp shiftp)
               (application-keymap application))
      (gethash (key-binding-descriptor context key controlp :any)
               (application-keymap application))
      (gethash (key-binding-descriptor :global key controlp shiftp)
               (application-keymap application))
      (gethash (key-binding-descriptor :global key controlp :any)
               (application-keymap application))))

(defun list-key-bindings (application)
  (let (bindings)
    (maphash (lambda (descriptor binding)
               (declare (ignore descriptor))
               (push binding bindings))
             (application-keymap application))
    (sort bindings
          #'string<
          :key (lambda (binding)
                 (format nil "~A ~A ~A ~A"
                         (key-binding-context binding)
                         (key-binding-control binding)
                         (key-binding-shift binding)
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
