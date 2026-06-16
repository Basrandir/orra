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
    :initform (make-hash-table :test #'equal))
   (computed-cache
    :accessor object-computed-cache
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

(defun object-slot-metadata-entries (object)
  (gethash :slots (object-metadata object)))

(defun find-object-slot-metadata-entry (object slot)
  (find slot
        (object-slot-metadata-entries object)
        :key #'first
        :test #'equal))

(defun object-slot-metadata-plist (object slot &key (inherit t))
  (or (second (find-object-slot-metadata-entry object slot))
      (and inherit
           (object-prototype object)
           (object-slot-metadata-plist (object-prototype object)
                                       slot
                                       :inherit t))))

(defun plist-value (plist key)
  (loop for (plist-key value) on plist by #'cddr
        when (eql plist-key key)
        return (values value t)
        finally (return (values nil nil))))

(defun object-slot-metadata (object slot metadata-key
                             &key default (inherit t))
  (multiple-value-bind (value presentp)
      (plist-value (object-slot-metadata-plist object
                                               slot
                                               :inherit nil)
                   metadata-key)
    (cond
      (presentp
       (values value t))
      ((and inherit (object-prototype object))
       (object-slot-metadata (object-prototype object)
                             slot
                             metadata-key
                             :default default
                             :inherit t))
      (t
       (values default nil)))))

(defun put-plist-value (plist key value)
  (let ((updated nil)
        result)
    (loop for (plist-key plist-value) on plist by #'cddr
          do (progn
               (push plist-key result)
               (if (eql plist-key key)
                   (progn
                     (push value result)
                     (setf updated t))
                   (push plist-value result))))
    (unless updated
      (push key result)
      (push value result))
    (nreverse result)))

(defun invalidate-object-computed-slot (object slot)
  (remhash slot (object-computed-cache object))
  object)

(defun set-object-slot-metadata (object slot metadata-key value)
  (let* ((entries (object-slot-metadata-entries object))
         (entry (find-object-slot-metadata-entry object slot))
         (metadata (if entry (second entry) nil))
         (updated-entry (list slot
                              (put-plist-value metadata
                                               metadata-key
                                               value))))
    (setf (gethash :slots (object-metadata object))
          (cons updated-entry
                (remove slot entries :key #'first :test #'equal))))
  (invalidate-object-computed-slot object slot)
  value)

(defun object-slot-names (object &key (inherit t))
  (let (slots)
    (labels ((collect (current)
               (dolist (entry (object-slot-metadata-entries current))
                 (pushnew (first entry) slots :test #'equal))
               (when (and inherit (object-prototype current))
                 (collect (object-prototype current)))))
      (collect object))
    (nreverse slots)))

(defun explicit-object-property (object key &key (inherit t))
  (multiple-value-bind (value presentp)
      (gethash key (object-properties object))
    (cond
      (presentp
       (values value t))
      ((and inherit (object-prototype object))
       (multiple-value-bind (prototype-value prototype-presentp)
           (explicit-object-property (object-prototype object)
                                     key
                                     :inherit t)
         (if prototype-presentp
             (values prototype-value t)
             (values nil nil))))
      (t (values nil nil)))))

(defun call-slot-function (function object key role)
  (cond
    ((functionp function)
     (funcall function object key))
    ((and (symbolp function) (fboundp function))
     (funcall (symbol-function function) object key))
    (t
     (error "Slot ~A function ~S is not callable." role function))))

(defun object-computed-slot-cached-p (object key &key (inherit t))
  (multiple-value-bind (value presentp)
      (object-slot-metadata object key :cached :inherit inherit)
    (and presentp value)))

(defun compute-object-slot-value (function object key)
  (call-slot-function function object key :computed))

(defun cached-object-computed-slot-value (object key function)
  (multiple-value-bind (entry presentp)
      (gethash key (object-computed-cache object))
    (if (and presentp (getf entry :valid-p))
        (values (getf entry :value) t)
        (let ((value (compute-object-slot-value function object key)))
          (setf (gethash key (object-computed-cache object))
                (list :valid-p t :value value))
          (values value t)))))

(defun object-computed-slot-value (object key &key default (inherit t))
  (multiple-value-bind (function presentp)
      (object-slot-metadata object
                            key
                            :computed-function
                            :inherit inherit)
    (if presentp
        (if (object-computed-slot-cached-p object key :inherit inherit)
            (cached-object-computed-slot-value object key function)
            (values (compute-object-slot-value function object key) t))
        (values default nil))))

(defun object-slot-default (object key &key default (inherit t))
  (multiple-value-bind (function presentp)
      (object-slot-metadata object
                            key
                            :default-function
                            :inherit inherit)
    (when presentp
      (return-from object-slot-default
        (values (call-slot-function function object key :default) t))))
  (multiple-value-bind (value presentp)
      (object-slot-metadata object
                            key
                            :default
                            :inherit inherit)
    (if presentp
        (values value t)
        (values default nil))))

(defun object-property (object key &key default (inherit t))
  (multiple-value-bind (value presentp)
      (explicit-object-property object key :inherit inherit)
    (if presentp
        value
        (multiple-value-bind (computed-value computed-presentp)
            (object-computed-slot-value object key :inherit inherit)
          (if computed-presentp
              computed-value
              (multiple-value-bind (slot-default slot-default-presentp)
                  (object-slot-default object key :inherit inherit)
                (if slot-default-presentp
                    slot-default
                    default)))))))

(defun computed-slot-depends-on-key-p (object slot key)
  (multiple-value-bind (function presentp)
      (object-slot-metadata object slot :computed-function)
    (declare (ignore function))
    (when presentp
      (multiple-value-bind (dependencies dependencies-presentp)
          (object-slot-metadata object slot :depends-on)
        (and dependencies-presentp
             (member key (ensure-list dependencies) :test #'equal))))))

(defun invalidate-dependent-computed-slots (object key)
  (dolist (slot (object-slot-names object))
    (when (computed-slot-depends-on-key-p object slot key)
      (invalidate-object-computed-slot object slot)))
  object)

(defun set-object-property (object key value)
  (setf (gethash key (object-properties object)) value)
  (invalidate-dependent-computed-slots object key)
  value)

(defun set-object-computed-slot (object slot function
                                 &key (depends-on nil depends-on-p)
                                   (cached nil cached-p))
  (set-object-slot-metadata object slot :computed-function function)
  (when depends-on-p
    (set-object-slot-metadata object slot :depends-on depends-on))
  (when cached-p
    (set-object-slot-metadata object slot :cached cached))
  object)

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
