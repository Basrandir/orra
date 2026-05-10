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

(defvar *command-definitions* (make-hash-table :test #'equal))

(defmacro define-command (name lambda-list documentation &body body)
  `(setf (gethash ',name *command-definitions*)
         (make-instance 'command
                        :name ',name
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

(defun invoke-command (application name &rest arguments)
  (let ((command (gethash name (application-commands application))))
    (unless command
      (error "Unknown command ~S." name))
    (apply (command-function command) application arguments)))

