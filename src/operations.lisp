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

(defparameter *journal-operation-queue-statuses*
  '(:pending :acknowledged :failed))

(defun workspace-operation-type-p (type)
  (not (null (member type *workspace-operation-types*))))

(defun ensure-workspace-operation-type (type)
  (unless (workspace-operation-type-p type)
    (error "Unsupported workspace operation type ~S." type))
  type)

(defun ensure-journal-operation-queue-status (status)
  (unless (member status *journal-operation-queue-statuses*)
    (error "Unsupported operation queue status ~S." status))
  status)

(defun vector-clock-entry-position (entry)
  (let ((tail (cdr entry)))
    (if (consp tail)
        (first tail)
        tail)))

(defun ensure-vector-clock-position (position)
  (unless (and (integerp position)
               (not (minusp position)))
    (error "Vector clock positions must be non-negative integers, got ~S."
           position))
  position)

(defun map-vector-clock-entries (function clock)
  (cond
    ((null clock)
     nil)
    ((hash-table-p clock)
     (maphash (lambda (actor-id position)
                (funcall function
                         actor-id
                         (ensure-vector-clock-position position)))
              clock))
    ((listp clock)
     (dolist (entry clock)
       (unless (consp entry)
         (error "Vector clock entries must be conses, got ~S." entry))
       (funcall function
                (car entry)
                (ensure-vector-clock-position
                 (vector-clock-entry-position entry)))))
    (t
     (error "Vector clocks must be NIL, hash tables, or alists, got ~S."
            clock))))

(defun vector-clock-entry-sort-key (entry)
  (princ-to-string (car entry)))

(defun copy-vector-clock (clock)
  (let (entries)
    (map-vector-clock-entries
     (lambda (actor-id position)
       (when (and actor-id
                  (plusp position))
         (push (cons actor-id position) entries)))
     clock)
    (sort entries #'string< :key #'vector-clock-entry-sort-key)))

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
   (clock
    :initarg :clock
    :reader operation-clock
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
                                   clock
                                   sequence)
  (make-instance 'workspace-operation
                 :id id
                 :type (ensure-workspace-operation-type type)
                 :target-id target-id
                 :payload (copy-list payload)
                 :actor-id actor-id
                 :session-id session-id
                 :timestamp timestamp
                 :clock (copy-vector-clock clock)
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
   (when (operation-clock operation)
     (list :clock (copy-vector-clock (operation-clock operation))))
   (when (operation-sequence operation)
     (list :sequence (operation-sequence operation)))))

(defun required-operation-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Operation plist is missing required key ~S." key))
    value))

(defun operation-plist-initargs (plist)
  (let ((initargs (list (required-operation-plist-value plist :type)
                        :type
                        (required-operation-plist-value plist :id)
                        :id)))
    (dolist (key '(:target-id :payload :actor-id :session-id
                   :timestamp :clock :sequence)
             (nreverse initargs))
      (multiple-value-bind (value presentp)
          (plist-value plist key)
        (when presentp
          (push key initargs)
          (push value initargs))))))

(defun make-workspace-operation-from-plist (plist)
  (apply #'make-workspace-operation
         (operation-plist-initargs plist)))

(defun required-sync-payload-value (payload key)
  (multiple-value-bind (value presentp)
      (plist-value payload key)
    (unless presentp
      (error "Sync payload is missing required key ~S." key))
    value))

(defclass operation-journal ()
  ((workspace-id
    :initarg :workspace-id
    :reader journal-workspace-id
    :initform nil)
   (actor-id
    :initarg :actor-id
    :reader journal-actor-id
    :initform nil)
   (session-id
    :initarg :session-id
    :reader journal-session-id
    :initform nil)
   (next-sequence
    :initarg :next-sequence
    :accessor journal-next-sequence
    :initform 1)
   (clock
    :accessor %journal-clock
    :initform (make-hash-table :test #'equal))
   (operations
    :accessor %journal-operations
    :initform nil)
   (operation-ids
    :accessor %journal-operation-ids
    :initform (make-hash-table :test #'equal))
   (queue-statuses
    :accessor %journal-queue-statuses
    :initform (make-hash-table :test #'equal))))

(defun merge-journal-clock-position (journal actor-id position)
  (let ((position (ensure-vector-clock-position position)))
    (when (and actor-id
               (> position (gethash actor-id (%journal-clock journal) 0)))
      (setf (gethash actor-id (%journal-clock journal)) position))))

(defun merge-journal-clock (journal clock)
  (map-vector-clock-entries
   (lambda (actor-id position)
     (merge-journal-clock-position journal actor-id position))
   clock)
  journal)

(defun journal-clock-position (journal actor-id &optional (default 0))
  (gethash actor-id (%journal-clock journal) default))

(defun journal-vector-clock (journal)
  (copy-vector-clock (%journal-clock journal)))

(defun advance-journal-clock (journal actor-id)
  (let ((next-position (1+ (journal-clock-position journal actor-id))))
    (setf (gethash actor-id (%journal-clock journal)) next-position)
    next-position))

(defun make-operation-journal (&key workspace-id
                                 actor-id
                                 session-id
                                 (next-sequence 1)
                                 clock)
  (let ((journal (make-instance 'operation-journal
                                :workspace-id workspace-id
                                :actor-id actor-id
                                :session-id session-id
                                :next-sequence next-sequence)))
    (merge-journal-clock journal clock)
    journal))

(defun journal-operations (journal)
  (copy-list (%journal-operations journal)))

(defun find-journal-operation (journal operation-id &optional default)
  (or (gethash operation-id (%journal-operation-ids journal))
      default))

(defun journal-operation-id-for (operation-or-id)
  (if (typep operation-or-id 'workspace-operation)
      (operation-id operation-or-id)
      operation-or-id))

(defun journal-recorded-operation-p (journal operation-or-id)
  (let ((operation-id (journal-operation-id-for operation-or-id)))
    (not (null (find-journal-operation journal operation-id)))))

(defun journal-operation-queue-status (journal operation-or-id
                                       &optional default)
  (gethash (journal-operation-id-for operation-or-id)
           (%journal-queue-statuses journal)
           default))

(defun require-journal-operation (journal operation-or-id)
  (or (find-journal-operation journal
                              (journal-operation-id-for operation-or-id))
      (error "Journal does not contain operation ~S." operation-or-id)))

(defun set-journal-operation-queue-status (journal operation-or-id status)
  (let ((operation (require-journal-operation journal operation-or-id)))
    (setf (gethash (operation-id operation) (%journal-queue-statuses journal))
          (ensure-journal-operation-queue-status status))
    operation))

(defun journal-operations-with-queue-status (journal status)
  (let ((status (ensure-journal-operation-queue-status status)))
    (remove-if-not
     (lambda (operation)
       (eq status (journal-operation-queue-status journal operation)))
     (journal-operations journal))))

(defun journal-pending-operations (journal)
  (journal-operations-with-queue-status journal :pending))

(defun journal-pending-sync-payload (journal)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :clock (journal-vector-clock journal)
        :operations (mapcar #'workspace-operation-plist
                            (journal-pending-operations journal))))

(defun journal-failed-operations (journal)
  (journal-operations-with-queue-status journal :failed))

(defun acknowledge-journal-operation (journal operation-or-id)
  (set-journal-operation-queue-status journal operation-or-id :acknowledged))

(defun fail-journal-operation (journal operation-or-id)
  (set-journal-operation-queue-status journal operation-or-id :failed))

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
        (merge-journal-clock journal (operation-clock operation))
        operation)))

(defun record-local-operation (journal type &key target-id payload actor-id
                                              session-id timestamp)
  (let* ((effective-actor-id (or actor-id (journal-actor-id journal)))
         (effective-session-id (or session-id (journal-session-id journal)))
         (clock (when effective-actor-id
                  (advance-journal-clock journal effective-actor-id)
                  (journal-vector-clock journal))))
    (let ((operation (record-operation
                      journal
                      (make-workspace-operation :type type
                                                :target-id target-id
                                                :payload payload
                                                :actor-id effective-actor-id
                                                :session-id effective-session-id
                                                :timestamp timestamp
                                                :clock clock))))
      (set-journal-operation-queue-status journal operation :pending)
      operation)))

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

(defun ensure-sync-payload-workspace (journal payload)
  (let ((payload-workspace-id (required-sync-payload-value payload :workspace-id))
        (journal-workspace-id (journal-workspace-id journal)))
    (unless (equal payload-workspace-id journal-workspace-id)
      (error "Sync payload workspace ~S does not match journal workspace ~S."
             payload-workspace-id
             journal-workspace-id))
    payload-workspace-id))

(defun sync-payload-acknowledged-operation-ids (payload)
  (multiple-value-bind (operation-ids presentp)
      (plist-value payload :acknowledged-operation-ids)
    (if presentp operation-ids nil)))

(defun sync-payload-acknowledged-operations (journal payload)
  (mapcar (lambda (operation-id)
            (require-journal-operation journal operation-id))
          (sync-payload-acknowledged-operation-ids payload)))

(defun apply-sync-acknowledgement-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (let ((operations (sync-payload-acknowledged-operations journal payload)))
    (mapcar (lambda (operation)
              (acknowledge-journal-operation journal operation))
            operations)))

(defun sync-payload-operation-plists (payload)
  (multiple-value-bind (operation-plists presentp)
      (plist-value payload :operations)
    (if presentp operation-plists nil)))

(defun sync-payload-operations (payload)
  (mapcar #'make-workspace-operation-from-plist
          (sync-payload-operation-plists payload)))

(defun apply-remote-sync-payload (registry journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for operation in (sync-payload-operations payload)
        for result = (apply-remote-operation registry journal operation)
        when result
        collect result))

(defun apply-operation-journal (registry journal)
  (mapcar (lambda (operation)
            (apply-workspace-operation registry operation))
          (journal-operations journal)))
