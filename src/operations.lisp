(in-package :orra)

(defparameter *workspace-operation-types*
  '(:create-object
    :set-slot
    :reorder-children
    :insert-text-range
    :delete-text-range
    :attach-metadata
    :link-object
    :evaluate-cell))

(defun workspace-operation-type-p (type)
  (not (null (member type *workspace-operation-types*))))

(defun ensure-workspace-operation-type (type)
  (unless (workspace-operation-type-p type)
    (error "Unsupported workspace operation type ~S." type))
  type)

(defclass workspace-operation ()
  ((id
    :initarg :id
    :reader operation-id)
   (type
    :initarg :type
    :reader operation-type)
   (target-id
    :initarg :target-id
    :reader operation-target-id
    :initform nil)
   (payload
    :initarg :payload
    :reader operation-payload
    :initform nil)
   (actor-id
    :initarg :actor-id
    :reader operation-actor-id
    :initform nil)
   (session-id
    :initarg :session-id
    :reader operation-session-id
    :initform nil)
   (timestamp
    :initarg :timestamp
    :reader operation-timestamp
    :initform nil)
   (sequence
    :initarg :sequence
    :accessor operation-sequence
    :initform nil)))

(defmethod print-object ((operation workspace-operation) stream)
  (print-unreadable-object (operation stream :type t :identity nil)
    (format stream "~A ~A"
            (operation-type operation)
            (operation-id operation))))

(defun make-workspace-operation (&key (id (fresh-id "op"))
                                   type
                                   target-id
                                   payload
                                   actor-id
                                   session-id
                                   (timestamp (get-universal-time))
                                   sequence)
  (make-instance 'workspace-operation
                 :id id
                 :type (ensure-workspace-operation-type type)
                 :target-id target-id
                 :payload (copy-list payload)
                 :actor-id actor-id
                 :session-id session-id
                 :timestamp timestamp
                 :sequence sequence))

(defun workspace-operation-plist (operation)
  (append
   (list :id (operation-id operation)
         :type (operation-type operation)
         :target-id (operation-target-id operation)
         :payload (copy-list (operation-payload operation))
         :actor-id (operation-actor-id operation)
         :session-id (operation-session-id operation)
         :timestamp (operation-timestamp operation))
   (when (operation-sequence operation)
     (list :sequence (operation-sequence operation)))))

(defclass operation-journal ()
  ((workspace-id
    :initarg :workspace-id
    :reader journal-workspace-id
    :initform nil)
   (next-sequence
    :initarg :next-sequence
    :accessor journal-next-sequence
    :initform 1)
   (operations
    :accessor %journal-operations
    :initform nil)))

(defun make-operation-journal (&key workspace-id (next-sequence 1))
  (make-instance 'operation-journal
                 :workspace-id workspace-id
                 :next-sequence next-sequence))

(defun journal-operations (journal)
  (copy-list (%journal-operations journal)))

(defun record-operation (journal operation)
  (ensure-workspace-operation-type (operation-type operation))
  (unless (operation-sequence operation)
    (setf (operation-sequence operation)
          (journal-next-sequence journal))
    (incf (journal-next-sequence journal)))
  (setf (%journal-operations journal)
        (append (%journal-operations journal)
                (list operation)))
  operation)

(defun record-local-operation (journal type &key target-id payload actor-id
					      session-id timestamp)
  (record-operation
   journal
   (make-workspace-operation :type type
                             :target-id target-id
                             :payload payload
                             :actor-id actor-id
                             :session-id session-id
                             :timestamp timestamp)))
