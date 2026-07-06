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

(defparameter *collaboration-comment-statuses*
  '(:open :resolved :deleted))

(defparameter *workspace-member-roles*
  '(:owner :admin :editor :viewer))

(defparameter *workspace-member-statuses*
  '(:active :inactive :removed))

(defparameter *workspace-attachment-statuses*
  '(:pending :available :failed :deleted))

(defparameter *workspace-checkpoint-statuses*
  '(:pending :available :failed :deleted))

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

(defun ensure-collaboration-comment-status (status)
  (unless (member status *collaboration-comment-statuses*)
    (error "Unsupported collaboration comment status ~S." status))
  status)

(defun ensure-workspace-member-role (role)
  (unless (member role *workspace-member-roles*)
    (error "Unsupported workspace member role ~S." role))
  role)

(defun ensure-workspace-member-status (status)
  (unless (member status *workspace-member-statuses*)
    (error "Unsupported workspace member status ~S." status))
  status)

(defun ensure-workspace-attachment-status (status)
  (unless (member status *workspace-attachment-statuses*)
    (error "Unsupported workspace attachment status ~S." status))
  status)

(defun ensure-workspace-checkpoint-status (status)
  (unless (member status *workspace-checkpoint-statuses*)
    (error "Unsupported workspace checkpoint status ~S." status))
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

(defun required-presence-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Presence plist is missing required key ~S." key))
    value))

(defun ensure-presence-cursor-position (cursor-position)
  (unless (or (null cursor-position)
              (and (integerp cursor-position)
                   (not (minusp cursor-position))))
    (error "Presence cursor positions must be NIL or non-negative integers, got ~S."
           cursor-position))
  cursor-position)

(defun ensure-presence-updated-at (updated-at)
  (unless (and (integerp updated-at)
               (not (minusp updated-at)))
    (error "Presence update times must be non-negative integers, got ~S."
           updated-at))
  updated-at)

(defun ensure-presence-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Presence metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass collaborator-presence ()
  ((actor-id
    :initarg :actor-id
    :reader collaborator-presence-actor-id)
   (session-id
    :initarg :session-id
    :reader collaborator-presence-session-id)
   (focus-id
    :initarg :focus-id
    :reader collaborator-presence-focus-id
    :initform nil)
   (cursor-position
    :initarg :cursor-position
    :reader collaborator-presence-cursor-position
    :initform nil)
   (status
    :initarg :status
    :reader collaborator-presence-status
    :initform :active)
   (updated-at
    :initarg :updated-at
    :reader collaborator-presence-updated-at)
   (metadata
    :initarg :metadata
    :reader collaborator-presence-metadata
    :initform nil)))

(defun make-collaborator-presence (&key actor-id
                                     session-id
                                     focus-id
                                     cursor-position
                                     (status :active)
                                     (updated-at (get-universal-time))
                                     metadata)
  (unless actor-id
    (error "Collaborator presence requires an actor id."))
  (unless session-id
    (error "Collaborator presence requires a session id."))
  (make-instance 'collaborator-presence
                 :actor-id actor-id
                 :session-id session-id
                 :focus-id focus-id
                 :cursor-position
                 (ensure-presence-cursor-position cursor-position)
                 :status status
                 :updated-at (ensure-presence-updated-at updated-at)
                 :metadata (copy-list (ensure-presence-metadata metadata))))

(defun collaborator-presence-plist (presence)
  (list :actor-id (collaborator-presence-actor-id presence)
        :session-id (collaborator-presence-session-id presence)
        :focus-id (collaborator-presence-focus-id presence)
        :cursor-position (collaborator-presence-cursor-position presence)
        :status (collaborator-presence-status presence)
        :updated-at (collaborator-presence-updated-at presence)
        :metadata (copy-list (collaborator-presence-metadata presence))))

(defun make-collaborator-presence-from-plist (plist)
  (make-collaborator-presence
   :actor-id (required-presence-plist-value plist :actor-id)
   :session-id (required-presence-plist-value plist :session-id)
   :focus-id (getf plist :focus-id)
   :cursor-position (getf plist :cursor-position)
   :status (getf plist :status :active)
   :updated-at (getf plist :updated-at (get-universal-time))
   :metadata (getf plist :metadata)))

(defun required-comment-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Comment plist is missing required key ~S." key))
    value))

(defun ensure-comment-timestamp (timestamp)
  (unless (and (integerp timestamp)
               (not (minusp timestamp)))
    (error "Comment timestamps must be non-negative integers, got ~S."
           timestamp))
  timestamp)

(defun ensure-comment-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Comment metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass collaboration-comment ()
  ((id
    :initarg :id
    :reader collaboration-comment-id)
   (target-id
    :initarg :target-id
    :reader collaboration-comment-target-id)
   (body
    :initarg :body
    :reader collaboration-comment-body)
   (actor-id
    :initarg :actor-id
    :reader collaboration-comment-actor-id)
   (session-id
    :initarg :session-id
    :reader collaboration-comment-session-id)
   (status
    :initarg :status
    :reader collaboration-comment-status
    :initform :open)
   (created-at
    :initarg :created-at
    :reader collaboration-comment-created-at)
   (updated-at
    :initarg :updated-at
    :reader collaboration-comment-updated-at)
   (metadata
    :initarg :metadata
    :reader collaboration-comment-metadata
    :initform nil)))

(defun make-collaboration-comment (&key (id (fresh-id "comment"))
                                     target-id
                                     body
                                     actor-id
                                     session-id
                                     (status :open)
                                     created-at
                                     updated-at
                                     metadata)
  (unless id
    (error "Collaboration comments require an id."))
  (unless target-id
    (error "Collaboration comments require a target id."))
  (unless actor-id
    (error "Collaboration comments require an actor id."))
  (unless session-id
    (error "Collaboration comments require a session id."))
  (let* ((effective-created-at (or created-at (get-universal-time)))
         (effective-updated-at (or updated-at effective-created-at)))
    (make-instance 'collaboration-comment
                   :id id
                   :target-id target-id
                   :body (normalize-display-string body)
                   :actor-id actor-id
                   :session-id session-id
                   :status (ensure-collaboration-comment-status status)
                   :created-at (ensure-comment-timestamp effective-created-at)
                   :updated-at (ensure-comment-timestamp effective-updated-at)
                   :metadata (copy-list (ensure-comment-metadata metadata)))))

(defun collaboration-comment-plist (comment)
  (list :id (collaboration-comment-id comment)
        :target-id (collaboration-comment-target-id comment)
        :body (collaboration-comment-body comment)
        :actor-id (collaboration-comment-actor-id comment)
        :session-id (collaboration-comment-session-id comment)
        :status (collaboration-comment-status comment)
        :created-at (collaboration-comment-created-at comment)
        :updated-at (collaboration-comment-updated-at comment)
        :metadata (copy-list (collaboration-comment-metadata comment))))

(defun make-collaboration-comment-from-plist (plist)
  (make-collaboration-comment
   :id (required-comment-plist-value plist :id)
   :target-id (required-comment-plist-value plist :target-id)
   :body (required-comment-plist-value plist :body)
   :actor-id (required-comment-plist-value plist :actor-id)
   :session-id (required-comment-plist-value plist :session-id)
   :status (getf plist :status :open)
   :created-at (getf plist :created-at (get-universal-time))
   :updated-at (getf plist :updated-at)
   :metadata (getf plist :metadata)))

(defun required-member-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Workspace member plist is missing required key ~S." key))
    value))

(defun ensure-member-timestamp (timestamp)
  (unless (and (integerp timestamp)
               (not (minusp timestamp)))
    (error "Workspace member update times must be non-negative integers, got ~S."
           timestamp))
  timestamp)

(defun ensure-member-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Workspace member metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass workspace-member ()
  ((actor-id
    :initarg :actor-id
    :reader workspace-member-actor-id)
   (display-name
    :initarg :display-name
    :reader workspace-member-display-name)
   (role
    :initarg :role
    :reader workspace-member-role
    :initform :editor)
   (status
    :initarg :status
    :reader workspace-member-status
    :initform :active)
   (updated-at
    :initarg :updated-at
    :reader workspace-member-updated-at)
   (metadata
    :initarg :metadata
    :reader workspace-member-metadata
    :initform nil)))

(defun make-workspace-member (&key actor-id
                                display-name
                                (role :editor)
                                (status :active)
                                (updated-at (get-universal-time))
                                metadata)
  (unless actor-id
    (error "Workspace members require an actor id."))
  (make-instance 'workspace-member
                 :actor-id actor-id
                 :display-name (normalize-display-string display-name)
                 :role (ensure-workspace-member-role role)
                 :status (ensure-workspace-member-status status)
                 :updated-at (ensure-member-timestamp updated-at)
                 :metadata (copy-list (ensure-member-metadata metadata))))

(defun workspace-member-plist (member)
  (list :actor-id (workspace-member-actor-id member)
        :display-name (workspace-member-display-name member)
        :role (workspace-member-role member)
        :status (workspace-member-status member)
        :updated-at (workspace-member-updated-at member)
        :metadata (copy-list (workspace-member-metadata member))))

(defun make-workspace-member-from-plist (plist)
  (make-workspace-member
   :actor-id (required-member-plist-value plist :actor-id)
   :display-name (getf plist :display-name)
   :role (getf plist :role :editor)
   :status (getf plist :status :active)
   :updated-at (getf plist :updated-at (get-universal-time))
   :metadata (getf plist :metadata)))

(defun required-attachment-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Workspace attachment plist is missing required key ~S." key))
    value))

(defun ensure-attachment-byte-size (byte-size)
  (unless (and (integerp byte-size)
               (not (minusp byte-size)))
    (error "Workspace attachment byte sizes must be non-negative integers, got ~S."
           byte-size))
  byte-size)

(defun ensure-attachment-timestamp (timestamp)
  (unless (and (integerp timestamp)
               (not (minusp timestamp)))
    (error "Workspace attachment timestamps must be non-negative integers, got ~S."
           timestamp))
  timestamp)

(defun ensure-attachment-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Workspace attachment metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass workspace-attachment ()
  ((id
    :initarg :id
    :reader workspace-attachment-id)
   (target-id
    :initarg :target-id
    :reader workspace-attachment-target-id)
   (content-type
    :initarg :content-type
    :reader workspace-attachment-content-type)
   (byte-size
    :initarg :byte-size
    :reader workspace-attachment-byte-size)
   (digest
    :initarg :digest
    :reader workspace-attachment-digest
    :initform nil)
   (storage-ref
    :initarg :storage-ref
    :reader workspace-attachment-storage-ref
    :initform nil)
   (status
    :initarg :status
    :reader workspace-attachment-status
    :initform :pending)
   (actor-id
    :initarg :actor-id
    :reader workspace-attachment-actor-id)
   (session-id
    :initarg :session-id
    :reader workspace-attachment-session-id)
   (created-at
    :initarg :created-at
    :reader workspace-attachment-created-at)
   (updated-at
    :initarg :updated-at
    :reader workspace-attachment-updated-at)
   (metadata
    :initarg :metadata
    :reader workspace-attachment-metadata
    :initform nil)))

(defun make-workspace-attachment (&key (id (fresh-id "attachment"))
                                    target-id
                                    content-type
                                    byte-size
                                    digest
                                    storage-ref
                                    (status :pending)
                                    actor-id
                                    session-id
                                    created-at
                                    updated-at
                                    metadata)
  (unless id
    (error "Workspace attachments require an id."))
  (unless target-id
    (error "Workspace attachments require a target id."))
  (unless content-type
    (error "Workspace attachments require a content type."))
  (unless byte-size
    (error "Workspace attachments require a byte size."))
  (unless actor-id
    (error "Workspace attachments require an actor id."))
  (unless session-id
    (error "Workspace attachments require a session id."))
  (let* ((effective-created-at (or created-at (get-universal-time)))
         (effective-updated-at (or updated-at effective-created-at)))
    (make-instance 'workspace-attachment
                   :id id
                   :target-id target-id
                   :content-type (normalize-display-string content-type)
                   :byte-size
                   (ensure-attachment-byte-size byte-size)
                   :digest (and digest (normalize-display-string digest))
                   :storage-ref
                   (and storage-ref
                        (normalize-display-string storage-ref))
                   :status (ensure-workspace-attachment-status status)
                   :actor-id actor-id
                   :session-id session-id
                   :created-at
                   (ensure-attachment-timestamp effective-created-at)
                   :updated-at
                   (ensure-attachment-timestamp effective-updated-at)
                   :metadata
                   (copy-list (ensure-attachment-metadata metadata)))))

(defun workspace-attachment-plist (attachment)
  (list :id (workspace-attachment-id attachment)
        :target-id (workspace-attachment-target-id attachment)
        :content-type (workspace-attachment-content-type attachment)
        :byte-size (workspace-attachment-byte-size attachment)
        :digest (workspace-attachment-digest attachment)
        :storage-ref (workspace-attachment-storage-ref attachment)
        :status (workspace-attachment-status attachment)
        :actor-id (workspace-attachment-actor-id attachment)
        :session-id (workspace-attachment-session-id attachment)
        :created-at (workspace-attachment-created-at attachment)
        :updated-at (workspace-attachment-updated-at attachment)
        :metadata (copy-list (workspace-attachment-metadata attachment))))

(defun make-workspace-attachment-from-plist (plist)
  (make-workspace-attachment
   :id (required-attachment-plist-value plist :id)
   :target-id (required-attachment-plist-value plist :target-id)
   :content-type (required-attachment-plist-value plist :content-type)
   :byte-size (required-attachment-plist-value plist :byte-size)
   :digest (getf plist :digest)
   :storage-ref (getf plist :storage-ref)
   :status (getf plist :status :pending)
   :actor-id (required-attachment-plist-value plist :actor-id)
   :session-id (required-attachment-plist-value plist :session-id)
   :created-at (getf plist :created-at (get-universal-time))
   :updated-at (getf plist :updated-at)
   :metadata (getf plist :metadata)))

(defun required-checkpoint-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Workspace checkpoint plist is missing required key ~S." key))
    value))

(defun ensure-checkpoint-timestamp (timestamp)
  (unless (and (integerp timestamp)
               (not (minusp timestamp)))
    (error "Workspace checkpoint timestamps must be non-negative integers, got ~S."
           timestamp))
  timestamp)

(defun ensure-checkpoint-byte-size (byte-size)
  (unless (or (null byte-size)
              (and (integerp byte-size)
                   (not (minusp byte-size))))
    (error "Workspace checkpoint byte sizes must be NIL or non-negative integers, got ~S."
           byte-size))
  byte-size)

(defun ensure-checkpoint-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Workspace checkpoint metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass workspace-checkpoint ()
  ((id
    :initarg :id
    :reader workspace-checkpoint-id)
   (checkpoint-at
    :initarg :checkpoint-at
    :reader workspace-checkpoint-checkpoint-at)
   (storage-ref
    :initarg :storage-ref
    :reader workspace-checkpoint-storage-ref
    :initform nil)
   (byte-size
    :initarg :byte-size
    :reader workspace-checkpoint-byte-size
    :initform nil)
   (digest
    :initarg :digest
    :reader workspace-checkpoint-digest
    :initform nil)
   (status
    :initarg :status
    :reader workspace-checkpoint-status
    :initform :pending)
   (actor-id
    :initarg :actor-id
    :reader workspace-checkpoint-actor-id)
   (session-id
    :initarg :session-id
    :reader workspace-checkpoint-session-id)
   (clock
    :initarg :clock
    :reader workspace-checkpoint-clock
    :initform nil)
   (created-at
    :initarg :created-at
    :reader workspace-checkpoint-created-at)
   (updated-at
    :initarg :updated-at
    :reader workspace-checkpoint-updated-at)
   (metadata
    :initarg :metadata
    :reader workspace-checkpoint-metadata
    :initform nil)))

(defun make-workspace-checkpoint (&key (id (fresh-id "checkpoint"))
                                    checkpoint-at
                                    storage-ref
                                    byte-size
                                    digest
                                    (status :pending)
                                    actor-id
                                    session-id
                                    clock
                                    created-at
                                    updated-at
                                    metadata)
  (unless id
    (error "Workspace checkpoints require an id."))
  (unless checkpoint-at
    (error "Workspace checkpoints require a checkpoint timestamp."))
  (unless actor-id
    (error "Workspace checkpoints require an actor id."))
  (unless session-id
    (error "Workspace checkpoints require a session id."))
  (let* ((effective-checkpoint-at
          (ensure-checkpoint-timestamp checkpoint-at))
         (effective-created-at (or created-at effective-checkpoint-at))
         (effective-updated-at (or updated-at effective-created-at)))
    (make-instance 'workspace-checkpoint
                   :id id
                   :checkpoint-at effective-checkpoint-at
                   :storage-ref
                   (and storage-ref
                        (normalize-display-string storage-ref))
                   :byte-size (ensure-checkpoint-byte-size byte-size)
                   :digest (and digest (normalize-display-string digest))
                   :status (ensure-workspace-checkpoint-status status)
                   :actor-id actor-id
                   :session-id session-id
                   :clock (copy-vector-clock clock)
                   :created-at
                   (ensure-checkpoint-timestamp effective-created-at)
                   :updated-at
                   (ensure-checkpoint-timestamp effective-updated-at)
                   :metadata
                   (copy-list (ensure-checkpoint-metadata metadata)))))

(defun workspace-checkpoint-plist (checkpoint)
  (list :id (workspace-checkpoint-id checkpoint)
        :checkpoint-at (workspace-checkpoint-checkpoint-at checkpoint)
        :storage-ref (workspace-checkpoint-storage-ref checkpoint)
        :byte-size (workspace-checkpoint-byte-size checkpoint)
        :digest (workspace-checkpoint-digest checkpoint)
        :status (workspace-checkpoint-status checkpoint)
        :actor-id (workspace-checkpoint-actor-id checkpoint)
        :session-id (workspace-checkpoint-session-id checkpoint)
        :clock (copy-vector-clock (workspace-checkpoint-clock checkpoint))
        :created-at (workspace-checkpoint-created-at checkpoint)
        :updated-at (workspace-checkpoint-updated-at checkpoint)
        :metadata (copy-list (workspace-checkpoint-metadata checkpoint))))

(defun make-workspace-checkpoint-from-plist (plist)
  (make-workspace-checkpoint
   :id (required-checkpoint-plist-value plist :id)
   :checkpoint-at (required-checkpoint-plist-value plist :checkpoint-at)
   :storage-ref (getf plist :storage-ref)
   :byte-size (getf plist :byte-size)
   :digest (getf plist :digest)
   :status (getf plist :status :pending)
   :actor-id (required-checkpoint-plist-value plist :actor-id)
   :session-id (required-checkpoint-plist-value plist :session-id)
   :clock (getf plist :clock)
   :created-at (getf plist :created-at)
   :updated-at (getf plist :updated-at)
   :metadata (getf plist :metadata)))

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
    :initform (make-hash-table :test #'equal))
   (presences
    :accessor %journal-presences
    :initform (make-hash-table :test #'equal))
   (comments
    :accessor %journal-comments
    :initform (make-hash-table :test #'equal))
   (members
    :accessor %journal-members
    :initform (make-hash-table :test #'equal))
   (attachments
    :accessor %journal-attachments
    :initform (make-hash-table :test #'equal))
   (checkpoints
    :accessor %journal-checkpoints
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

(defun journal-presence-key (actor-id session-id)
  (list actor-id session-id))

(defun collaborator-presence-journal-key (presence)
  (journal-presence-key (collaborator-presence-actor-id presence)
                        (collaborator-presence-session-id presence)))

(defun collaborator-presence-sort-key (presence)
  (format nil "~A ~A"
          (collaborator-presence-actor-id presence)
          (collaborator-presence-session-id presence)))

(defun journal-presences (journal)
  (let (presences)
    (maphash (lambda (key presence)
               (declare (ignore key))
               (push presence presences))
             (%journal-presences journal))
    (sort presences #'string< :key #'collaborator-presence-sort-key)))

(defun find-journal-presence (journal actor-id session-id &optional default)
  (gethash (journal-presence-key actor-id session-id)
           (%journal-presences journal)
           default))

(defun collaborator-presence-newer-p (presence existing-presence)
  (or (null existing-presence)
      (> (collaborator-presence-updated-at presence)
         (collaborator-presence-updated-at existing-presence))))

(defun update-journal-presence (journal presence &key force)
  (let* ((key (collaborator-presence-journal-key presence))
         (existing-presence (gethash key (%journal-presences journal))))
    (when (or force
              (collaborator-presence-newer-p presence existing-presence))
      (setf (gethash key (%journal-presences journal)) presence)
      presence)))

(defun journal-local-presence (journal)
  (find-journal-presence journal
                         (journal-actor-id journal)
                         (journal-session-id journal)))

(defun record-local-presence (journal &key actor-id
                                        session-id
                                        focus-id
                                        cursor-position
                                        status
                                        updated-at
                                        metadata)
  (update-journal-presence
   journal
   (make-collaborator-presence
    :actor-id (or actor-id (journal-actor-id journal))
    :session-id (or session-id (journal-session-id journal))
    :focus-id focus-id
    :cursor-position cursor-position
    :status (or status :active)
    :updated-at (or updated-at (get-universal-time))
    :metadata metadata)
   :force t))

(defun journal-presence-sync-payload (journal &optional presence)
  (let ((presence (or presence (journal-local-presence journal))))
    (unless presence
      (error "Journal has no local presence to export."))
    (list :workspace-id (journal-workspace-id journal)
          :actor-id (journal-actor-id journal)
          :session-id (journal-session-id journal)
          :presence (collaborator-presence-plist presence))))

(defun collaboration-comment-sort-key (comment)
  (collaboration-comment-id comment))

(defun journal-comments (journal)
  (let (comments)
    (maphash (lambda (id comment)
               (declare (ignore id))
               (push comment comments))
             (%journal-comments journal))
    (sort comments #'string< :key #'collaboration-comment-sort-key)))

(defun find-journal-comment (journal comment-id &optional default)
  (gethash comment-id (%journal-comments journal) default))

(defun collaboration-comment-newer-p (comment existing-comment)
  (or (null existing-comment)
      (> (collaboration-comment-updated-at comment)
         (collaboration-comment-updated-at existing-comment))))

(defun update-journal-comment (journal comment &key force)
  (let* ((comment-id (collaboration-comment-id comment))
         (existing-comment (gethash comment-id (%journal-comments journal))))
    (when (or force
              (collaboration-comment-newer-p comment existing-comment))
      (setf (gethash comment-id (%journal-comments journal)) comment)
      comment)))

(defun record-local-comment (journal &key id
                                       target-id
                                       body
                                       actor-id
                                       session-id
                                       status
                                       created-at
                                       updated-at
                                       metadata)
  (update-journal-comment
   journal
   (make-collaboration-comment
    :id (or id (fresh-id "comment"))
    :target-id target-id
    :body body
    :actor-id (or actor-id (journal-actor-id journal))
    :session-id (or session-id (journal-session-id journal))
    :status (or status :open)
    :created-at created-at
    :updated-at updated-at
    :metadata metadata)
   :force t))

(defun journal-comment-sync-payload (journal comment)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :comment (collaboration-comment-plist comment)))

(defun workspace-member-sort-key (member)
  (workspace-member-actor-id member))

(defun journal-members (journal)
  (let (members)
    (maphash (lambda (actor-id member)
               (declare (ignore actor-id))
               (push member members))
             (%journal-members journal))
    (sort members #'string< :key #'workspace-member-sort-key)))

(defun find-journal-member (journal actor-id &optional default)
  (gethash actor-id (%journal-members journal) default))

(defun workspace-member-newer-p (member existing-member)
  (or (null existing-member)
      (> (workspace-member-updated-at member)
         (workspace-member-updated-at existing-member))))

(defun update-journal-member (journal member &key force)
  (let* ((actor-id (workspace-member-actor-id member))
         (existing-member (gethash actor-id (%journal-members journal))))
    (when (or force
              (workspace-member-newer-p member existing-member))
      (setf (gethash actor-id (%journal-members journal)) member)
      member)))

(defun record-workspace-member (journal &key actor-id
                                          display-name
                                          role
                                          status
                                          updated-at
                                          metadata)
  (update-journal-member
   journal
   (make-workspace-member
    :actor-id actor-id
    :display-name display-name
    :role (or role :editor)
    :status (or status :active)
    :updated-at (or updated-at (get-universal-time))
    :metadata metadata)
   :force t))

(defun journal-membership-sync-payload (journal member)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :member (workspace-member-plist member)))

(defun workspace-attachment-sort-key (attachment)
  (workspace-attachment-id attachment))

(defun journal-attachments (journal)
  (let (attachments)
    (maphash (lambda (id attachment)
               (declare (ignore id))
               (push attachment attachments))
             (%journal-attachments journal))
    (sort attachments #'string< :key #'workspace-attachment-sort-key)))

(defun find-journal-attachment (journal attachment-id &optional default)
  (gethash attachment-id (%journal-attachments journal) default))

(defun workspace-attachment-newer-p (attachment existing-attachment)
  (or (null existing-attachment)
      (> (workspace-attachment-updated-at attachment)
         (workspace-attachment-updated-at existing-attachment))))

(defun update-journal-attachment (journal attachment &key force)
  (let* ((attachment-id (workspace-attachment-id attachment))
         (existing-attachment
          (gethash attachment-id (%journal-attachments journal))))
    (when (or force
              (workspace-attachment-newer-p attachment existing-attachment))
      (setf (gethash attachment-id (%journal-attachments journal))
            attachment)
      attachment)))

(defun record-local-attachment (journal &key id
                                          target-id
                                          content-type
                                          byte-size
                                          digest
                                          storage-ref
                                          status
                                          actor-id
                                          session-id
                                          created-at
                                          updated-at
                                          metadata)
  (update-journal-attachment
   journal
   (make-workspace-attachment
    :id (or id (fresh-id "attachment"))
    :target-id target-id
    :content-type content-type
    :byte-size byte-size
    :digest digest
    :storage-ref storage-ref
    :status (or status :pending)
    :actor-id (or actor-id (journal-actor-id journal))
    :session-id (or session-id (journal-session-id journal))
    :created-at created-at
    :updated-at updated-at
    :metadata metadata)
   :force t))

(defun journal-attachment-sync-payload (journal attachment)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :attachment (workspace-attachment-plist attachment)))

(defun workspace-checkpoint-sort-key (checkpoint)
  (workspace-checkpoint-id checkpoint))

(defun journal-checkpoints (journal)
  (let (checkpoints)
    (maphash (lambda (id checkpoint)
               (declare (ignore id))
               (push checkpoint checkpoints))
             (%journal-checkpoints journal))
    (sort checkpoints #'string< :key #'workspace-checkpoint-sort-key)))

(defun find-journal-checkpoint (journal checkpoint-id &optional default)
  (gethash checkpoint-id (%journal-checkpoints journal) default))

(defun workspace-checkpoint-newer-p (checkpoint existing-checkpoint)
  (or (null existing-checkpoint)
      (> (workspace-checkpoint-updated-at checkpoint)
         (workspace-checkpoint-updated-at existing-checkpoint))))

(defun update-journal-checkpoint (journal checkpoint &key force)
  (let* ((checkpoint-id (workspace-checkpoint-id checkpoint))
         (existing-checkpoint
          (gethash checkpoint-id (%journal-checkpoints journal))))
    (when (or force
              (workspace-checkpoint-newer-p checkpoint existing-checkpoint))
      (setf (gethash checkpoint-id (%journal-checkpoints journal))
            checkpoint)
      checkpoint)))

(defun record-local-checkpoint (journal &key id
					  checkpoint-at
					  storage-ref
					  byte-size
					  digest
					  status
					  actor-id
					  session-id
					  clock
					  created-at
					  updated-at
					  metadata)
  (update-journal-checkpoint
   journal
   (make-workspace-checkpoint
    :id (or id (fresh-id "checkpoint"))
    :checkpoint-at checkpoint-at
    :storage-ref storage-ref
    :byte-size byte-size
    :digest digest
    :status (or status :pending)
    :actor-id (or actor-id (journal-actor-id journal))
    :session-id (or session-id (journal-session-id journal))
    :clock (or clock (journal-vector-clock journal))
    :created-at created-at
    :updated-at updated-at
    :metadata metadata)
   :force t))

(defun journal-checkpoint-sync-payload (journal checkpoint)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :checkpoint (workspace-checkpoint-plist checkpoint)))

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

(defun journal-sync-request-payload (journal &key presence comments members
                                               attachments checkpoints)
  (let ((presence (or presence (journal-local-presence journal))))
    (append
     (journal-pending-sync-payload journal)
     (when presence
       (list :presences
             (list (collaborator-presence-plist presence))))
     (when comments
       (list :comments
             (mapcar #'collaboration-comment-plist
                     (ensure-list comments))))
     (when members
       (list :members
             (mapcar #'workspace-member-plist
                     (ensure-list members))))
     (when attachments
       (list :attachments
             (mapcar #'workspace-attachment-plist
                     (ensure-list attachments))))
     (when checkpoints
       (list :checkpoints
             (mapcar #'workspace-checkpoint-plist
                     (ensure-list checkpoints)))))))

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

(defun sync-payload-entry-plists (payload singular-key plural-key)
  (let (entry-plists)
    (multiple-value-bind (entry presentp)
        (plist-value payload singular-key)
      (when presentp
        (push entry entry-plists)))
    (multiple-value-bind (entries presentp)
        (plist-value payload plural-key)
      (when presentp
        (setf entry-plists
              (append entry-plists
                      (ensure-list entries)))))
    entry-plists))

(defun sync-payload-presence-plists (payload)
  (sync-payload-entry-plists payload :presence :presences))

(defun sync-payload-presences (payload)
  (mapcar #'make-collaborator-presence-from-plist
          (sync-payload-presence-plists payload)))

(defun apply-presence-sync-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for presence in (sync-payload-presences payload)
        for accepted-presence = (update-journal-presence journal presence)
        when accepted-presence
        collect accepted-presence))

(defun sync-payload-comment-plists (payload)
  (sync-payload-entry-plists payload :comment :comments))

(defun sync-payload-comments (payload)
  (mapcar #'make-collaboration-comment-from-plist
          (sync-payload-comment-plists payload)))

(defun apply-comment-sync-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for comment in (sync-payload-comments payload)
        for accepted-comment = (update-journal-comment journal comment)
        when accepted-comment
        collect accepted-comment))

(defun sync-payload-member-plists (payload)
  (sync-payload-entry-plists payload :member :members))

(defun sync-payload-members (payload)
  (mapcar #'make-workspace-member-from-plist
          (sync-payload-member-plists payload)))

(defun apply-membership-sync-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for member in (sync-payload-members payload)
        for accepted-member = (update-journal-member journal member)
        when accepted-member
        collect accepted-member))

(defun sync-payload-attachment-plists (payload)
  (sync-payload-entry-plists payload :attachment :attachments))

(defun sync-payload-attachments (payload)
  (mapcar #'make-workspace-attachment-from-plist
          (sync-payload-attachment-plists payload)))

(defun apply-attachment-sync-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for attachment in (sync-payload-attachments payload)
        for accepted-attachment = (update-journal-attachment
                                   journal
                                   attachment)
        when accepted-attachment
        collect accepted-attachment))

(defun sync-payload-checkpoint-plists (payload)
  (sync-payload-entry-plists payload :checkpoint :checkpoints))

(defun sync-payload-checkpoints (payload)
  (mapcar #'make-workspace-checkpoint-from-plist
          (sync-payload-checkpoint-plists payload)))

(defun apply-checkpoint-sync-payload (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (loop for checkpoint in (sync-payload-checkpoints payload)
        for accepted-checkpoint = (update-journal-checkpoint
                                   journal
                                   checkpoint)
        when accepted-checkpoint
        collect accepted-checkpoint))

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

(defun apply-sync-response-payload (registry journal payload)
  (ensure-sync-payload-workspace journal payload)
  (merge-journal-clock journal (getf payload :clock))
  (list :acknowledged-operations
        (apply-sync-acknowledgement-payload journal payload)
        :applied-operations
        (apply-remote-sync-payload registry journal payload)
        :presences
        (apply-presence-sync-payload journal payload)
        :comments
        (apply-comment-sync-payload journal payload)
        :members
        (apply-membership-sync-payload journal payload)
        :attachments
        (apply-attachment-sync-payload journal payload)
        :checkpoints
        (apply-checkpoint-sync-payload journal payload)))

(defun apply-operation-journal (registry journal)
  (mapcar (lambda (operation)
            (apply-workspace-operation registry operation))
          (journal-operations journal)))
