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
    :initform nil)
   (operation-ids
    :accessor %journal-operation-ids
    :initform (make-hash-table :test #'equal))))

(defun make-operation-journal (&key workspace-id (next-sequence 1))
  (make-instance 'operation-journal
                 :workspace-id workspace-id
                 :next-sequence next-sequence))

(defun journal-operations (journal)
  (copy-list (%journal-operations journal)))

(defun find-journal-operation (journal operation-id &optional default)
  (or (gethash operation-id (%journal-operation-ids journal))
      default))

(defun journal-recorded-operation-p (journal operation-or-id)
  (let ((operation-id (if (typep operation-or-id 'workspace-operation)
                          (operation-id operation-or-id)
                          operation-or-id)))
    (not (null (find-journal-operation journal operation-id)))))

(defun record-operation (journal operation)
  (ensure-workspace-operation-type (operation-type operation))
  (or (find-journal-operation journal (operation-id operation))
      (progn
        (unless (operation-sequence operation)
          (setf (operation-sequence operation)
                (journal-next-sequence journal))
          (incf (journal-next-sequence journal)))
        (setf (%journal-operations journal)
              (append (%journal-operations journal)
                      (list operation)))
        (setf (gethash (operation-id operation)
                       (%journal-operation-ids journal))
              operation)
        operation)))

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

(defun operation-payload-value (operation key &optional default)
  (multiple-value-bind (value presentp)
      (plist-value (operation-payload operation) key)
    (if presentp value default)))

(defun target-object-for-operation (registry operation)
  (or (find-object registry (operation-target-id operation))
      (error "Operation ~A targets unknown object ~S."
             (operation-id operation)
             (operation-target-id operation))))

(defun set-workspace-slot (workspace slot value)
  (case slot
    (:title (setf (workspace-title workspace) (normalize-display-string value)))
    (:current-notebook (setf (workspace-current-notebook workspace) value))
    (otherwise (set-object-property workspace slot value)))
  workspace)

(defun set-notebook-slot (notebook slot value)
  (case slot
    (:title (setf (notebook-title notebook) (normalize-display-string value)))
    (otherwise (set-object-property notebook slot value)))
  notebook)

(defun set-section-slot (section slot value)
  (case slot
    (:title (setf (section-title section) (normalize-display-string value)))
    (otherwise (set-object-property section slot value)))
  section)

(defun set-paragraph-slot (paragraph slot value)
  (case slot
    (:text (setf (paragraph-text paragraph) (normalize-display-string value)))
    (otherwise (set-object-property paragraph slot value)))
  paragraph)

(defun set-code-block-slot (block slot value)
  (case slot
    (:source (replace-code-block-source block (normalize-display-string value)))
    (:language (setf (code-block-language block) value))
    (otherwise (set-object-property block slot value)))
  block)

(defun set-semantic-object-slot (object slot value)
  (typecase object
    (workspace (set-workspace-slot object slot value))
    (notebook (set-notebook-slot object slot value))
    (section (set-section-slot object slot value))
    (paragraph (set-paragraph-slot object slot value))
    (code-block (set-code-block-slot object slot value))
    (t
     (set-object-property object slot value)
     object)))

(defun apply-set-slot-operation (registry operation)
  (let ((slot (operation-payload-value operation :slot)))
    (unless slot
      (error "Set-slot operation ~A is missing :SLOT payload."
             (operation-id operation)))
    (set-semantic-object-slot
     (target-object-for-operation registry operation)
     slot
     (operation-payload-value operation :value))))

(defun apply-workspace-operation (registry operation)
  (case (operation-type operation)
    (:set-slot
     (apply-set-slot-operation registry operation))
    (otherwise
     (error "Applying workspace operation type ~S is not implemented yet."
            (operation-type operation)))))

(defun apply-remote-operation (registry journal operation)
  (unless (journal-recorded-operation-p journal operation)
    (let ((result (apply-workspace-operation registry operation)))
      (record-operation journal operation)
      result)))

(defun apply-operation-journal (registry journal)
  (mapcar (lambda (operation)
            (apply-workspace-operation registry operation))
          (journal-operations journal)))
