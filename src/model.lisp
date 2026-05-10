(in-package :orra)

(defclass model-object ()
  ((id
    :initarg :id
    :reader object-id)
   (kind
    :initarg :kind
    :reader object-kind)
   (prototype
    :initarg :prototype
    :accessor object-prototype
    :initform nil)
   (properties
    :initarg :properties
    :accessor object-properties
    :initform (make-hash-table :test #'equal))
   (metadata
    :initarg :metadata
    :accessor object-metadata
    :initform (make-hash-table :test #'equal))))

(defmethod print-object ((object model-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A ~A"
            (object-kind object)
            (object-id object))))

(defgeneric children-of (object)
  (:method ((object t))
    nil))

(defgeneric parent-of (object)
  (:method ((object t))
    nil))

(defun object-property (object key &key default (inherit t))
  (multiple-value-bind (value presentp)
      (gethash key (object-properties object))
    (cond
      (presentp value)
      ((and inherit (object-prototype object))
       (object-property (object-prototype object)
                        key
                        :default default
                        :inherit t))
      (t default))))

(defun set-object-property (object key value)
  (setf (gethash key (object-properties object)) value))

(defun set-object-metadata (object key value)
  (setf (gethash key (object-metadata object)) value))

(defclass object-registry ()
  ((objects
    :initform (make-hash-table :test #'equal)
    :reader registry-objects)))

(defun make-object-registry ()
  (make-instance 'object-registry))

(defun register-object (registry object)
  (setf (gethash (object-id object) (registry-objects registry)) object)
  object)

(defun find-object (registry id &optional default)
  (gethash id (registry-objects registry) default))

