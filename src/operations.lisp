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

(defparameter *sync-conflict-statuses*
  '(:open :resolved-local :resolved-remote))

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

(defparameter *sync-message-version* 1)

(defparameter *sync-message-types*
  '(:request :response :error))

(defun workspace-operation-type-p (type)
  (not (null (member type *workspace-operation-types*))))

(defun sync-message-version ()
  *sync-message-version*)

(defun ensure-sync-message-type (type)
  (unless (member type *sync-message-types*)
    (error "Unsupported sync message type ~S." type))
  type)

(defun ensure-sync-message-version (version)
  (unless (and (integerp version)
               (= version *sync-message-version*))
    (error "Unsupported sync message version ~S; this build supports ~S."
           version
           *sync-message-version*))
  version)

(defun make-sync-message (type payload &key
					 (version *sync-message-version*))
  (list :kind :orra-sync-message
        :version (ensure-sync-message-version version)
        :type (ensure-sync-message-type type)
        :payload payload))

(defun ensure-sync-message (message &key expected-type)
  (unless (listp message)
    (error "Sync messages must be plists, got ~S." message))
  (unless (eq :orra-sync-message (getf message :kind))
    (error "Invalid sync message kind ~S." (getf message :kind)))
  (ensure-sync-message-version (getf message :version))
  (let ((type (ensure-sync-message-type (getf message :type))))
    (when (and expected-type (not (eq expected-type type)))
      (error "Expected sync message type ~S, got ~S."
             expected-type
             type)))
  (multiple-value-bind (payload presentp)
      (plist-value message :payload)
    (declare (ignore payload))
    (unless presentp
      (error "Sync message is missing required key :PAYLOAD.")))
  message)

(defun sync-message-type (message)
  (getf (ensure-sync-message message) :type))

(defun sync-message-payload (message &key expected-type)
  (getf (ensure-sync-message message :expected-type expected-type)
        :payload))

(defun make-sync-error-payload (code condition
                                &key
                                  (retryable nil))
  (list :code code
        :message (princ-to-string condition)
        :retryable retryable))

(defun make-sync-error-message (code condition
                                &key
                                  (retryable nil))
  (make-sync-message :error
                     (make-sync-error-payload code
                                              condition
                                              :retryable retryable)))

(defun signal-sync-error-message (message)
  (let* ((payload (sync-message-payload message :expected-type :error))
         (code (getf payload :code))
         (diagnostic (getf payload :message)))
    (error "Sync transport error ~A: ~A" code diagnostic)))

(defun encode-sync-message (message)
  (with-output-to-string (stream)
    (with-standard-io-syntax
      (prin1 (ensure-sync-message message) stream))))

(defun sync-message-whitespace-char-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun sync-message-trailing-junk-p (string start)
  (loop for index from start below (length string)
        thereis (not (sync-message-whitespace-char-p
                      (char string index)))))

(defun decode-sync-message (string &key expected-type)
  (let ((*read-eval* nil)
        (string (string string)))
    (multiple-value-bind (message position)
        (read-from-string string)
      (when (sync-message-trailing-junk-p string position)
        (error "Sync message contains trailing data after position ~S."
               position))
      (ensure-sync-message message :expected-type expected-type))))

(defun ensure-workspace-operation-type (type)
  (unless (workspace-operation-type-p type)
    (error "Unsupported workspace operation type ~S." type))
  type)

(defun ensure-journal-operation-queue-status (status)
  (unless (member status *journal-operation-queue-statuses*)
    (error "Unsupported operation queue status ~S." status))
  status)

(defun ensure-sync-conflict-status (status)
  (unless (member status *sync-conflict-statuses*)
    (error "Unsupported sync conflict status ~S." status))
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

(defun vector-clock-actor-position (clock actor-id &optional (default 0))
  (let ((position default))
    (map-vector-clock-entries
     (lambda (entry-actor-id entry-position)
       (when (equal actor-id entry-actor-id)
         (setf position entry-position)))
     clock)
    position))

(defun vector-clock<=p (left-clock right-clock)
  (let ((dominatesp t))
    (map-vector-clock-entries
     (lambda (actor-id position)
       (when (> position
                (vector-clock-actor-position right-clock actor-id))
         (setf dominatesp nil)))
     left-clock)
    dominatesp))

(defun vector-clock-concurrent-p (left-clock right-clock)
  (and left-clock
       right-clock
       (not (vector-clock<=p left-clock right-clock))
       (not (vector-clock<=p right-clock left-clock))))

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

(defclass sync-conflict ()
  ((id
    :initarg :id
    :reader sync-conflict-id)
   (target-id
    :initarg :target-id
    :reader sync-conflict-target-id)
   (slot
    :initarg :slot
    :reader sync-conflict-slot)
   (local-operation-id
    :initarg :local-operation-id
    :reader sync-conflict-local-operation-id)
   (remote-operation-id
    :initarg :remote-operation-id
    :reader sync-conflict-remote-operation-id)
   (status
    :initarg :status
    :accessor %sync-conflict-status
    :initform :open)
   (created-at
    :initarg :created-at
    :reader sync-conflict-created-at
    :initform nil)
   (resolved-at
    :initarg :resolved-at
    :accessor %sync-conflict-resolved-at
    :initform nil)))

(defun sync-conflict-status (conflict)
  (%sync-conflict-status conflict))

(defun sync-conflict-resolved-at (conflict)
  (%sync-conflict-resolved-at conflict))

(defun make-sync-conflict (&key id
                             target-id
                             slot
                             local-operation-id
                             remote-operation-id
                             (status :open)
                             (created-at (get-universal-time))
                             resolved-at)
  (make-instance 'sync-conflict
                 :id (or id
                         (format nil "conflict:~A:~A"
                                 local-operation-id
                                 remote-operation-id))
                 :target-id target-id
                 :slot slot
                 :local-operation-id local-operation-id
                 :remote-operation-id remote-operation-id
                 :status (ensure-sync-conflict-status status)
                 :created-at created-at
                 :resolved-at resolved-at))

(defun sync-conflict-plist (conflict)
  (list :id (sync-conflict-id conflict)
        :target-id (sync-conflict-target-id conflict)
        :slot (sync-conflict-slot conflict)
        :local-operation-id (sync-conflict-local-operation-id conflict)
        :remote-operation-id (sync-conflict-remote-operation-id conflict)
        :status (sync-conflict-status conflict)
        :created-at (sync-conflict-created-at conflict)
        :resolved-at (sync-conflict-resolved-at conflict)))

(defun required-sync-conflict-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Sync conflict plist is missing required key ~S." key))
    value))

(defun make-sync-conflict-from-plist (plist)
  (make-sync-conflict
   :id (required-sync-conflict-plist-value plist :id)
   :target-id (required-sync-conflict-plist-value plist :target-id)
   :slot (required-sync-conflict-plist-value plist :slot)
   :local-operation-id
   (required-sync-conflict-plist-value plist :local-operation-id)
   :remote-operation-id
   (required-sync-conflict-plist-value plist :remote-operation-id)
   :status (getf plist :status :open)
   :created-at (required-sync-conflict-plist-value plist :created-at)
   :resolved-at (getf plist :resolved-at)))

(defun required-sync-payload-value (payload key)
  (multiple-value-bind (value presentp)
      (plist-value payload key)
    (unless presentp
      (error "Sync payload is missing required key ~S." key))
    value))

(defun required-sync-authentication-plist-value (plist key)
  (multiple-value-bind (value presentp)
      (plist-value plist key)
    (unless presentp
      (error "Sync authentication plist is missing required key ~S." key))
    value))

(defun ensure-sync-authentication-timestamp (timestamp)
  (unless (and (integerp timestamp)
               (not (minusp timestamp)))
    (error "Sync authentication timestamps must be non-negative integers, got ~S."
           timestamp))
  timestamp)

(defun ensure-sync-authentication-expiry (issued-at expires-at)
  (when expires-at
    (let ((expires-at (ensure-sync-authentication-timestamp expires-at)))
      (when (< expires-at issued-at)
        (error "Sync authentication expiry ~S precedes issue time ~S."
               expires-at
               issued-at))
      expires-at)))

(defun ensure-sync-authentication-scopes (scopes)
  (unless (or (null scopes)
              (listp scopes))
    (error "Sync authentication scopes must be NIL or a list, got ~S." scopes))
  (dolist (scope scopes)
    (unless (keywordp scope)
      (error "Sync authentication scopes must be keywords, got ~S." scope)))
  scopes)

(defun ensure-sync-authentication-metadata (metadata)
  (unless (or (null metadata)
              (listp metadata))
    (error "Sync authentication metadata must be NIL or a plist, got ~S."
           metadata))
  metadata)

(defclass sync-authentication ()
  ((token-id
    :initarg :token-id
    :reader sync-authentication-token-id)
   (workspace-id
    :initarg :workspace-id
    :reader sync-authentication-workspace-id)
   (actor-id
    :initarg :actor-id
    :reader sync-authentication-actor-id)
   (session-id
    :initarg :session-id
    :reader sync-authentication-session-id)
   (scopes
    :initarg :scopes
    :reader sync-authentication-scopes
    :initform nil)
   (issued-at
    :initarg :issued-at
    :reader sync-authentication-issued-at)
   (expires-at
    :initarg :expires-at
    :reader sync-authentication-expires-at
    :initform nil)
   (metadata
    :initarg :metadata
    :reader sync-authentication-metadata
    :initform nil)))

(defun make-sync-authentication (&key token-id
                                   workspace-id
                                   actor-id
                                   session-id
                                   scopes
                                   (issued-at (get-universal-time))
                                   expires-at
                                   metadata)
  (unless token-id
    (error "Sync authentication requires a token id."))
  (unless workspace-id
    (error "Sync authentication requires a workspace id."))
  (unless actor-id
    (error "Sync authentication requires an actor id."))
  (unless session-id
    (error "Sync authentication requires a session id."))
  (let ((issued-at (ensure-sync-authentication-timestamp issued-at)))
    (make-instance 'sync-authentication
                   :token-id token-id
                   :workspace-id workspace-id
                   :actor-id actor-id
                   :session-id session-id
                   :scopes (copy-list
                            (ensure-sync-authentication-scopes scopes))
                   :issued-at issued-at
                   :expires-at (ensure-sync-authentication-expiry issued-at
                                                                  expires-at)
                   :metadata (copy-list
                              (ensure-sync-authentication-metadata metadata)))))

(defun sync-authentication-plist (authentication)
  (list :token-id (sync-authentication-token-id authentication)
        :workspace-id (sync-authentication-workspace-id authentication)
        :actor-id (sync-authentication-actor-id authentication)
        :session-id (sync-authentication-session-id authentication)
        :scopes (copy-list (sync-authentication-scopes authentication))
        :issued-at (sync-authentication-issued-at authentication)
        :expires-at (sync-authentication-expires-at authentication)
        :metadata (copy-list (sync-authentication-metadata authentication))))

(defun make-sync-authentication-from-plist (plist)
  (make-sync-authentication
   :token-id (required-sync-authentication-plist-value plist :token-id)
   :workspace-id (required-sync-authentication-plist-value plist :workspace-id)
   :actor-id (required-sync-authentication-plist-value plist :actor-id)
   :session-id (required-sync-authentication-plist-value plist :session-id)
   :scopes (getf plist :scopes)
   :issued-at (required-sync-authentication-plist-value plist :issued-at)
   :expires-at (getf plist :expires-at)
   :metadata (getf plist :metadata)))

(defun sync-authentication-expired-p (authentication now)
  (let ((expires-at (sync-authentication-expires-at authentication)))
    (and expires-at
         (> now expires-at))))

(defun sync-authentication-has-scope-p (authentication scope)
  (or (null scope)
      (member scope (sync-authentication-scopes authentication))))

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
    :initform (make-hash-table :test #'equal))
   (conflicts
    :accessor %journal-conflicts
    :initform nil)
   (conflict-keys
    :accessor %journal-conflict-keys
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

(defun journal-conflicts (journal)
  (copy-list (%journal-conflicts journal)))

(defun sync-conflict-id-for (conflict-or-id)
  (typecase conflict-or-id
    (sync-conflict (sync-conflict-id conflict-or-id))
    (t conflict-or-id)))

(defun find-journal-conflict (journal conflict-or-id &optional default)
  (or (find (sync-conflict-id-for conflict-or-id)
            (journal-conflicts journal)
            :key #'sync-conflict-id
            :test #'equal)
      default))

(defun require-journal-conflict (journal conflict-or-id)
  (or (find-journal-conflict journal conflict-or-id)
      (error "Journal does not contain sync conflict ~S." conflict-or-id)))

(defun sync-conflict-key (target-id slot local-operation-id remote-operation-id)
  (list target-id slot local-operation-id remote-operation-id))

(defun sync-conflict-record-key (conflict)
  (sync-conflict-key (sync-conflict-target-id conflict)
                     (sync-conflict-slot conflict)
                     (sync-conflict-local-operation-id conflict)
                     (sync-conflict-remote-operation-id conflict)))

(defun sync-conflict-resolved-p (conflict)
  (not (sync-conflict-open-p conflict)))

(defun sync-conflict-resolution-newer-p (conflict existing-conflict)
  (and (sync-conflict-resolved-p conflict)
       (or (sync-conflict-open-p existing-conflict)
           (let ((resolved-at (sync-conflict-resolved-at conflict))
                 (existing-resolved-at
                  (sync-conflict-resolved-at existing-conflict)))
             (and resolved-at
                  (or (null existing-resolved-at)
                      (> resolved-at existing-resolved-at)))))))

(defun update-existing-sync-conflict (existing-conflict conflict)
  (setf (%sync-conflict-status existing-conflict)
        (sync-conflict-status conflict)
        (%sync-conflict-resolved-at existing-conflict)
        (sync-conflict-resolved-at conflict))
  existing-conflict)

(defun update-journal-conflict (journal conflict &key force)
  (let ((key (sync-conflict-record-key conflict)))
    (let ((existing-conflict (gethash key (%journal-conflict-keys journal))))
      (cond
        ((null existing-conflict)
         (setf (%journal-conflicts journal)
               (append (%journal-conflicts journal)
                       (list conflict)))
         (setf (gethash key (%journal-conflict-keys journal))
               conflict))
        ((or force
             (sync-conflict-resolution-newer-p conflict existing-conflict))
         (update-existing-sync-conflict existing-conflict conflict))))))

(defun record-sync-conflict (journal conflict)
  (update-journal-conflict journal conflict))

(defun journal-conflict-sync-payload (journal conflict)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :conflict (sync-conflict-plist conflict)))

(defun sync-conflict-open-p (conflict)
  (eq :open (sync-conflict-status conflict)))

(defun sync-conflict-suppresses-remote-operation-p (conflict)
  (member (sync-conflict-status conflict)
          '(:open :resolved-local)))

(defun operation-suppressed-by-conflict-p (journal operation)
  (some (lambda (conflict)
          (and (sync-conflict-suppresses-remote-operation-p conflict)
               (equal (operation-id operation)
                      (sync-conflict-remote-operation-id conflict))))
        (journal-conflicts journal)))

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

(defun sync-payload-authentication (payload)
  (make-sync-authentication-from-plist
   (required-sync-payload-value payload :auth)))

(defun ensure-sync-authentication-matches-payload (authentication payload)
  (let ((workspace-id (required-sync-payload-value payload :workspace-id))
        (actor-id (required-sync-payload-value payload :actor-id))
        (session-id (required-sync-payload-value payload :session-id)))
    (unless (equal workspace-id
                   (sync-authentication-workspace-id authentication))
      (error "Sync authentication workspace ~S does not match payload workspace ~S."
             (sync-authentication-workspace-id authentication)
             workspace-id))
    (unless (equal actor-id
                   (sync-authentication-actor-id authentication))
      (error "Sync authentication actor ~S does not match payload actor ~S."
             (sync-authentication-actor-id authentication)
             actor-id))
    (unless (equal session-id
                   (sync-authentication-session-id authentication))
      (error "Sync authentication session ~S does not match payload session ~S."
             (sync-authentication-session-id authentication)
             session-id)))
  authentication)

(defun ensure-sync-authentication-current (authentication now)
  (when (> (sync-authentication-issued-at authentication) now)
    (error "Sync authentication token ~S was issued in the future at ~S."
           (sync-authentication-token-id authentication)
           (sync-authentication-issued-at authentication)))
  (when (sync-authentication-expired-p authentication now)
    (error "Sync authentication token ~S expired at ~S."
           (sync-authentication-token-id authentication)
           (sync-authentication-expires-at authentication)))
  authentication)

(defun ensure-sync-authentication-scope (authentication required-scope)
  (unless (sync-authentication-has-scope-p authentication required-scope)
    (error "Sync authentication token ~S lacks required scope ~S."
           (sync-authentication-token-id authentication)
           required-scope))
  authentication)

(defun ensure-sync-authentication-member (journal authentication)
  (let* ((actor-id (sync-authentication-actor-id authentication))
         (member (find-journal-member journal actor-id)))
    (unless member
      (error "Sync authentication actor ~S is not a workspace member."
             actor-id))
    (unless (eq :active (workspace-member-status member))
      (error "Sync authentication actor ~S is not an active workspace member."
             actor-id)))
  authentication)

(defun authorize-sync-request-payload (journal payload &key
							 (now (get-universal-time))
							 required-scope)
  (ensure-sync-payload-workspace journal payload)
  (let ((authentication (sync-payload-authentication payload)))
    (ensure-sync-authentication-matches-payload authentication payload)
    (ensure-sync-authentication-current authentication now)
    (ensure-sync-authentication-scope authentication required-scope)
    (ensure-sync-authentication-member journal authentication)))

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

(defun journal-sync-request-payload (journal &key authentication presence
                                               comments members attachments
                                               checkpoints conflicts)
  (let ((presence (or presence (journal-local-presence journal))))
    (append
     (journal-pending-sync-payload journal)
     (when authentication
       (list :auth
             (sync-authentication-plist authentication)))
     (when presence
       (list :presences
             (list (collaborator-presence-plist presence))))
     (when comments
       (list :comments
             (mapcar #'collaboration-comment-plist
                     (ensure-list comments))))
     (when conflicts
       (list :conflicts
             (mapcar #'sync-conflict-plist
                     (ensure-list conflicts))))
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

(defun required-operation-payload-value (operation key)
  (multiple-value-bind (value presentp)
      (plist-value (operation-payload operation) key)
    (unless presentp
      (error "Operation ~A is missing required payload key ~S."
             (operation-id operation)
             key))
    value))

(defun set-slot-operation-p (operation)
  (eq :set-slot (operation-type operation)))

(defun operations-target-same-slot-p (left-operation right-operation)
  (and (set-slot-operation-p left-operation)
       (set-slot-operation-p right-operation)
       (equal (operation-target-id left-operation)
              (operation-target-id right-operation))
       (equal (operation-payload-value left-operation :slot)
              (operation-payload-value right-operation :slot))))

(defun operations-concurrent-p (left-operation right-operation)
  (vector-clock-concurrent-p (operation-clock left-operation)
                             (operation-clock right-operation)))

(defun conflicting-pending-local-operation (journal remote-operation)
  (find-if (lambda (local-operation)
             (and (operations-target-same-slot-p local-operation
                                                 remote-operation)
                  (operations-concurrent-p local-operation
                                           remote-operation)))
           (journal-pending-operations journal)))

(defun record-operation-conflict (journal local-operation remote-operation)
  (record-sync-conflict
   journal
   (make-sync-conflict
    :target-id (operation-target-id remote-operation)
    :slot (operation-payload-value remote-operation :slot)
    :local-operation-id (operation-id local-operation)
    :remote-operation-id (operation-id remote-operation)
    :created-at (operation-timestamp remote-operation))))

(defun target-object-for-operation (registry operation)
  (or (find-object registry (operation-target-id operation))
      (error "Operation ~A targets unknown object ~S."
             (operation-id operation)
             (operation-target-id operation))))

(defun ensure-operation-target-id (operation)
  (or (operation-target-id operation)
      (error "Operation ~A requires a target object id."
             (operation-id operation))))

(defun make-semantic-object-for-operation (kind id)
  (case kind
    (:workspace
     (make-instance 'workspace :id id :kind :workspace))
    (:notebook
     (make-instance 'notebook :id id :kind :notebook))
    (:section
     (make-instance 'section :id id :kind :section))
    (:paragraph
     (make-instance 'paragraph :id id :kind :paragraph))
    (:code-block
     (make-instance 'code-block :id id :kind :code-block))
    (:quote-block
     (make-instance 'quote-block :id id :kind :quote-block))
    (:reference-block
     (make-instance 'reference-block :id id :kind :reference-block))
    (:list-block
     (make-instance 'list-block :id id :kind :list-block))
    (:table-block
     (make-instance 'table-block :id id :kind :table-block))
    (:task-list
     (make-instance 'task-list :id id :kind :task-list))
    (:result-block
     (make-instance 'result-block :id id :kind :result-block))
    (:repl-block
     (make-instance 'repl-block :id id :kind :repl-block))
    (:repl-entry
     (make-instance 'repl-entry :id id :kind :repl-entry))
    (otherwise
     (error "Unsupported create-object kind ~S." kind))))

(defun ensure-plist-payload (operation key)
  (let ((value (operation-payload-value operation key)))
    (unless (or (null value) (listp value))
      (error "Operation ~A payload key ~S must be a plist, got ~S."
             (operation-id operation)
             key
             value))
    value))

(defun apply-semantic-slots (object slots)
  (loop for (slot value) on slots by #'cddr
        do (set-semantic-object-slot object slot value))
  object)

(defun apply-object-properties (object properties)
  (loop for (key value) on properties by #'cddr
        do (set-object-property object key value))
  object)

(defun apply-object-metadata (object metadata)
  (loop for (key value) on metadata by #'cddr
        do (set-object-metadata object key value))
  object)

(defun attach-child-once (parent child)
  (unless (member child (children-of parent))
    (append-child parent child))
  child)

(defun semantic-child-ids (parent)
  (mapcar #'object-id (children-of parent)))

(defun ensure-operation-child-ids (operation)
  (let ((child-ids (required-operation-payload-value operation :child-ids)))
    (unless (listp child-ids)
      (error "Operation ~A payload key :CHILD-IDS must be a list, got ~S."
             (operation-id operation)
             child-ids))
    (unless (= (length child-ids)
               (length (remove-duplicates child-ids :test #'equal)))
      (error "Operation ~A payload key :CHILD-IDS contains duplicates: ~S."
             (operation-id operation)
             child-ids))
    child-ids))

(defun ensure-child-id-permutation (parent operation child-ids)
  (let ((current-child-ids (semantic-child-ids parent)))
    (unless (and (= (length child-ids)
                    (length current-child-ids))
                 (null (set-difference child-ids
                                       current-child-ids
                                       :test #'equal))
                 (null (set-difference current-child-ids
                                       child-ids
                                       :test #'equal)))
      (error "Operation ~A child ids ~S are not a permutation of target children ~S."
             (operation-id operation)
             child-ids
             current-child-ids)))
  child-ids)

(defun reorderable-child-for-id (registry operation parent child-id)
  (let ((child (find-object registry child-id)))
    (unless child
      (error "Operation ~A references unknown child object ~S."
             (operation-id operation)
             child-id))
    (unless (eq parent (parent-of child))
      (error "Operation ~A child object ~S is not parented by target ~S."
             (operation-id operation)
             child-id
             (object-id parent)))
    child))

(defun set-semantic-children (parent children)
  (typecase parent
    (workspace
     (setf (workspace-notebooks parent) children)
     (unless (member (workspace-current-notebook parent) children)
       (setf (workspace-current-notebook parent) (first children))))
    (composite-node
     (setf (children-of parent) children))
    (otherwise
     (error "Object ~S cannot have reordered semantic children."
            (object-kind parent))))
  (dolist (child children)
    (setf (parent-of child) parent))
  parent)

(defun apply-reorder-children-operation (registry operation)
  (let* ((parent (target-object-for-operation registry operation))
         (child-ids (ensure-child-id-permutation
                     parent
                     operation
                     (ensure-operation-child-ids operation)))
         (children (mapcar (lambda (child-id)
                             (reorderable-child-for-id registry
                                                       operation
                                                       parent
                                                       child-id))
                           child-ids)))
    (set-semantic-children parent children)))

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

(defun default-text-range-slot (object operation)
  (typecase object
    (paragraph :text)
    (code-block :source)
    (otherwise
     (error "Operation ~A requires :SLOT for text range target ~S."
            (operation-id operation)
            (object-kind object)))))

(defun operation-text-range-slot (object operation)
  (or (operation-payload-value operation :slot)
      (default-text-range-slot object operation)))

(defun semantic-object-slot-value (object slot)
  (typecase object
    (paragraph
     (case slot
       (:text (paragraph-text object))
       (otherwise (object-property object slot))))
    (code-block
     (case slot
       (:source (code-block-source object))
       (:language (code-block-language object))
       (otherwise (object-property object slot))))
    (workspace
     (case slot
       (:title (workspace-title object))
       (:current-notebook (workspace-current-notebook object))
       (otherwise (object-property object slot))))
    (notebook
     (case slot
       (:title (notebook-title object))
       (otherwise (object-property object slot))))
    (section
     (case slot
       (:title (section-title object))
       (otherwise (object-property object slot))))
    (otherwise
     (object-property object slot))))

(defun ensure-text-range-position (operation key maximum)
  (let ((position (required-operation-payload-value operation key)))
    (unless (and (integerp position)
                 (<= 0 position maximum))
      (error "Operation ~A payload key ~S must be an integer in [0, ~A], got ~S."
             (operation-id operation)
             key
             maximum
             position))
    position))

(defun insert-string-range (string offset text)
  (concatenate 'string
               (subseq string 0 offset)
               text
               (subseq string offset)))

(defun delete-string-range (string offset range-length)
  (concatenate 'string
               (subseq string 0 offset)
               (subseq string (+ offset range-length))))

(defun apply-insert-text-range-operation (registry operation)
  (let* ((object (target-object-for-operation registry operation))
         (slot (operation-text-range-slot object operation))
         (current-text (normalize-display-string
                        (semantic-object-slot-value object slot)))
         (offset (ensure-text-range-position operation
                                             :offset
                                             (length current-text)))
         (text (normalize-display-string
                (required-operation-payload-value operation :text))))
    (set-semantic-object-slot object
                              slot
                              (insert-string-range current-text offset text))))

(defun apply-delete-text-range-operation (registry operation)
  (let* ((object (target-object-for-operation registry operation))
         (slot (operation-text-range-slot object operation))
         (current-text (normalize-display-string
                        (semantic-object-slot-value object slot)))
         (offset (ensure-text-range-position operation
                                             :offset
                                             (length current-text)))
         (range-length (required-operation-payload-value operation :length)))
    (unless (and (integerp range-length)
                 (<= 0 range-length (- (length current-text) offset)))
      (error "Operation ~A payload key :LENGTH must fit text from offset ~A, got ~S."
             (operation-id operation)
             offset
             range-length))
    (set-semantic-object-slot object
                              slot
                              (delete-string-range current-text offset range-length))))

(defun created-object-for-operation (registry operation)
  (let* ((target-id (ensure-operation-target-id operation))
         (kind (required-operation-payload-value operation :kind))
         (existing-object (find-object registry target-id)))
    (cond
      ((and existing-object
            (not (eq kind (object-kind existing-object))))
       (error "Operation ~A cannot create ~S object ~S over existing ~S."
              (operation-id operation)
              kind
              target-id
              (object-kind existing-object)))
      (existing-object existing-object)
      (t
       (register-object registry
                        (make-semantic-object-for-operation kind target-id))))))

(defun apply-create-object-operation (registry operation)
  (let ((object (created-object-for-operation registry operation)))
    (apply-semantic-slots object
                          (ensure-plist-payload operation :slots))
    (apply-object-properties object
                             (ensure-plist-payload operation :properties))
    (apply-object-metadata object
                           (ensure-plist-payload operation :metadata))
    (let ((parent-id (operation-payload-value operation :parent-id)))
      (when parent-id
        (attach-child-once
         (or (find-object registry parent-id)
             (error "Create-object operation ~A references unknown parent ~S."
                    (operation-id operation)
                    parent-id))
         object)))
    object))

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
    (:create-object
     (apply-create-object-operation registry operation))
    (:set-slot
     (apply-set-slot-operation registry operation))
    (:reorder-children
     (apply-reorder-children-operation registry operation))
    (:insert-text-range
     (apply-insert-text-range-operation registry operation))
    (:delete-text-range
     (apply-delete-text-range-operation registry operation))
    (otherwise
     (error "Applying workspace operation type ~S is not implemented yet."
            (operation-type operation)))))

(defun ensure-sync-conflict-resolution (resolution)
  (unless (member resolution '(:local :remote))
    (error "Unsupported sync conflict resolution ~S." resolution))
  resolution)

(defun resolve-sync-conflict (registry journal conflict-or-id resolution
                              &key
                                (resolved-at (get-universal-time)))
  (let* ((conflict (require-journal-conflict journal conflict-or-id))
         (local-operation
          (require-journal-operation
           journal
           (sync-conflict-local-operation-id conflict)))
         (remote-operation
          (require-journal-operation
           journal
           (sync-conflict-remote-operation-id conflict))))
    (unless (sync-conflict-open-p conflict)
      (error "Sync conflict ~A is already ~S."
             (sync-conflict-id conflict)
             (sync-conflict-status conflict)))
    (case (ensure-sync-conflict-resolution resolution)
      (:local
       (setf (%sync-conflict-status conflict) :resolved-local
             (%sync-conflict-resolved-at conflict) resolved-at)
       (apply-workspace-operation registry local-operation))
      (:remote
       (set-journal-operation-queue-status journal local-operation :failed)
       (setf (%sync-conflict-status conflict) :resolved-remote
             (%sync-conflict-resolved-at conflict) resolved-at)
       (apply-workspace-operation registry remote-operation)))))

(defun apply-remote-operation (registry journal operation)
  (unless (journal-recorded-operation-p journal operation)
    (let ((conflicting-operation
           (conflicting-pending-local-operation journal operation)))
      (if conflicting-operation
          (progn
            (record-operation journal operation)
            (record-operation-conflict journal
                                       conflicting-operation
                                       operation)
            nil)
          (let ((result (apply-workspace-operation registry operation)))
            (record-operation journal operation)
            result)))))

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

(defun sync-payload-conflict-plists (payload)
  (sync-payload-entry-plists payload :conflict :conflicts))

(defun sync-payload-conflicts (payload)
  (mapcar #'make-sync-conflict-from-plist
          (sync-payload-conflict-plists payload)))

(defun apply-conflict-sync-payload (journal payload &key force)
  (ensure-sync-payload-workspace journal payload)
  (loop for conflict in (sync-payload-conflicts payload)
        for accepted-conflict = (update-journal-conflict
                                 journal
                                 conflict
                                 :force force)
        when accepted-conflict
        collect accepted-conflict))

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

(defun apply-attachment-sync-payload (journal payload &key force)
  (ensure-sync-payload-workspace journal payload)
  (loop for attachment in (sync-payload-attachments payload)
        for accepted-attachment = (update-journal-attachment
                                   journal
                                   attachment
                                   :force force)
        when accepted-attachment
        collect accepted-attachment))

(defun sync-payload-checkpoint-plists (payload)
  (sync-payload-entry-plists payload :checkpoint :checkpoints))

(defun sync-payload-checkpoints (payload)
  (mapcar #'make-workspace-checkpoint-from-plist
          (sync-payload-checkpoint-plists payload)))

(defun apply-checkpoint-sync-payload (journal payload &key force)
  (ensure-sync-payload-workspace journal payload)
  (loop for checkpoint in (sync-payload-checkpoints payload)
        for accepted-checkpoint = (update-journal-checkpoint
                                   journal
                                   checkpoint
                                   :force force)
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
        :conflicts
        (apply-conflict-sync-payload journal payload)
        :members
        (apply-membership-sync-payload journal payload)
        :attachments
        (apply-attachment-sync-payload journal payload :force t)
        :checkpoints
        (apply-checkpoint-sync-payload journal payload :force t)))

(defclass sync-coordinator ()
  ((workspaces
    :accessor %sync-coordinator-workspaces
    :initform (make-hash-table :test #'equal))))

(defun make-sync-coordinator ()
  (make-instance 'sync-coordinator))

(defun sync-coordinator-workspace-journal (coordinator workspace-id
                                           &optional default)
  (gethash workspace-id
           (%sync-coordinator-workspaces coordinator)
           default))

(defun ensure-sync-coordinator-workspace (coordinator workspace-id
                                          &key
                                            (actor-id "sync-server")
                                            (session-id "sync-server"))
  (or (sync-coordinator-workspace-journal coordinator workspace-id)
      (setf (gethash workspace-id
                     (%sync-coordinator-workspaces coordinator))
            (make-operation-journal :workspace-id workspace-id
                                    :actor-id actor-id
                                    :session-id session-id))))

(defun sync-coordinator-register-member (coordinator workspace-id actor-id
                                         &key display-name role status
                                           updated-at metadata)
  (record-workspace-member
   (ensure-sync-coordinator-workspace coordinator workspace-id)
   :actor-id actor-id
   :display-name display-name
   :role role
   :status status
   :updated-at updated-at
   :metadata metadata))

(defun sync-coordinator-record-operations (journal payload)
  (ensure-sync-payload-workspace journal payload)
  (let ((operations (sync-payload-operations payload)))
    (dolist (operation operations)
      (record-operation journal operation))
    (mapcar #'operation-id operations)))

(defun sync-coordinator-storage-ref (journal kind id)
  (format nil "server://~A/~A/~A"
          (journal-workspace-id journal)
          kind
          id))

(defun sync-coordinator-server-storage-ref-p (journal kind id storage-ref)
  (equal storage-ref
         (sync-coordinator-storage-ref journal kind id)))

(defun sync-coordinator-coordinate-attachment (journal attachment)
  (let ((storage-ref (workspace-attachment-storage-ref attachment))
        (attachment-id (workspace-attachment-id attachment)))
    (unless (sync-coordinator-server-storage-ref-p journal
                                                   "attachments"
                                                   attachment-id
                                                   storage-ref)
      (update-journal-attachment
       journal
       (make-workspace-attachment
        :id attachment-id
        :target-id (workspace-attachment-target-id attachment)
        :content-type (workspace-attachment-content-type attachment)
        :byte-size (workspace-attachment-byte-size attachment)
        :digest (workspace-attachment-digest attachment)
        :storage-ref (sync-coordinator-storage-ref
                      journal
                      "attachments"
                      attachment-id)
        :status :pending
        :actor-id (workspace-attachment-actor-id attachment)
        :session-id (workspace-attachment-session-id attachment)
        :created-at (workspace-attachment-created-at attachment)
        :updated-at (workspace-attachment-updated-at attachment)
        :metadata (workspace-attachment-metadata attachment))
       :force t))))

(defun sync-coordinator-coordinate-attachments (journal attachments)
  (dolist (attachment attachments)
    (sync-coordinator-coordinate-attachment journal attachment)))

(defun sync-coordinator-coordinate-checkpoint (journal checkpoint)
  (let ((storage-ref (workspace-checkpoint-storage-ref checkpoint))
        (checkpoint-id (workspace-checkpoint-id checkpoint)))
    (unless (sync-coordinator-server-storage-ref-p journal
                                                   "checkpoints"
                                                   checkpoint-id
                                                   storage-ref)
      (update-journal-checkpoint
       journal
       (make-workspace-checkpoint
        :id checkpoint-id
        :checkpoint-at (workspace-checkpoint-checkpoint-at checkpoint)
        :storage-ref (sync-coordinator-storage-ref
                      journal
                      "checkpoints"
                      checkpoint-id)
        :byte-size (workspace-checkpoint-byte-size checkpoint)
        :digest (workspace-checkpoint-digest checkpoint)
        :status :pending
        :actor-id (workspace-checkpoint-actor-id checkpoint)
        :session-id (workspace-checkpoint-session-id checkpoint)
        :clock (workspace-checkpoint-clock checkpoint)
        :created-at (workspace-checkpoint-created-at checkpoint)
        :updated-at (workspace-checkpoint-updated-at checkpoint)
        :metadata (workspace-checkpoint-metadata checkpoint))
       :force t))))

(defun sync-coordinator-coordinate-checkpoints (journal checkpoints)
  (dolist (checkpoint checkpoints)
    (sync-coordinator-coordinate-checkpoint journal checkpoint)))

(defun sync-coordinator-apply-request-payload (journal payload)
  (merge-journal-clock journal (getf payload :clock))
  (let ((acknowledged-operation-ids
         (sync-coordinator-record-operations journal payload)))
    (apply-presence-sync-payload journal payload)
    (apply-comment-sync-payload journal payload)
    (apply-conflict-sync-payload journal payload)
    (apply-membership-sync-payload journal payload)
    (sync-coordinator-coordinate-attachments
     journal
     (apply-attachment-sync-payload journal payload))
    (sync-coordinator-coordinate-checkpoints
     journal
     (apply-checkpoint-sync-payload journal payload))
    acknowledged-operation-ids))

(defun sync-coordinator-peer-operation-p (operation actor-id)
  (not (equal actor-id (operation-actor-id operation))))

(defun sync-coordinator-peer-presence-p (presence actor-id session-id)
  (not (and (equal actor-id (collaborator-presence-actor-id presence))
            (equal session-id (collaborator-presence-session-id presence)))))

(defun sync-coordinator-response-payload (journal actor-id session-id
                                          acknowledged-operation-ids)
  (list :workspace-id (journal-workspace-id journal)
        :actor-id (journal-actor-id journal)
        :session-id (journal-session-id journal)
        :clock (journal-vector-clock journal)
        :acknowledged-operation-ids acknowledged-operation-ids
        :operations (mapcar #'workspace-operation-plist
                            (remove-if-not
                             (lambda (operation)
                               (sync-coordinator-peer-operation-p operation
                                                                  actor-id))
                             (journal-operations journal)))
        :presences (mapcar #'collaborator-presence-plist
                           (remove-if-not
                            (lambda (presence)
                              (sync-coordinator-peer-presence-p presence
								actor-id
								session-id))
                            (journal-presences journal)))
        :comments (mapcar #'collaboration-comment-plist
                          (journal-comments journal))
        :conflicts (mapcar #'sync-conflict-plist
                           (journal-conflicts journal))
        :members (mapcar #'workspace-member-plist
                         (journal-members journal))
        :attachments (mapcar #'workspace-attachment-plist
                             (journal-attachments journal))
        :checkpoints (mapcar #'workspace-checkpoint-plist
                             (journal-checkpoints journal))))

(defun handle-sync-request (coordinator payload &key
						  (now (get-universal-time)))
  (let* ((workspace-id (required-sync-payload-value payload :workspace-id))
         (actor-id (required-sync-payload-value payload :actor-id))
         (session-id (required-sync-payload-value payload :session-id))
         (journal (sync-coordinator-workspace-journal coordinator
                                                      workspace-id)))
    (unless journal
      (error "Sync coordinator has no workspace ~S." workspace-id))
    (authorize-sync-request-payload journal
                                    payload
                                    :now now
                                    :required-scope :sync)
    (sync-coordinator-response-payload
     journal
     actor-id
     session-id
     (sync-coordinator-apply-request-payload journal payload))))

(defun handle-sync-message (coordinator message
                            &key
                              (now (get-universal-time)))
  (make-sync-message
   :response
   (handle-sync-request coordinator
                        (sync-message-payload message
                                              :expected-type :request)
                        :now now)))

(defun handle-encoded-sync-message (coordinator encoded-message
                                    &key
                                      (now (get-universal-time)))
  (handler-case
      (let ((request-message (decode-sync-message encoded-message
                                                  :expected-type :request)))
        (handler-case
            (encode-sync-message
             (handle-sync-message coordinator request-message :now now))
          (error (condition)
            (encode-sync-message
             (make-sync-error-message :request-rejected condition)))))
    (error (condition)
      (encode-sync-message
       (make-sync-error-message :malformed-message condition)))))

(defclass sync-transport () ())

(defgeneric sync-transport-send-message (transport message &key now))

(defgeneric sync-transport-send-request (transport payload &key now))

(defclass local-sync-transport (sync-transport)
  ((coordinator
    :initarg :coordinator
    :reader local-sync-transport-coordinator)))

(defclass encoded-sync-transport (sync-transport)
  ((exchange-function
    :initarg :exchange-function
    :reader encoded-sync-transport-exchange-function)))

(defun make-local-sync-transport (coordinator)
  (make-instance 'local-sync-transport :coordinator coordinator))

(defun make-encoded-sync-transport (exchange-function)
  (unless (functionp exchange-function)
    (error "Encoded sync transports require a function, got ~S."
           exchange-function))
  (make-instance 'encoded-sync-transport
                 :exchange-function exchange-function))

(defmethod sync-transport-send-request ((transport sync-transport)
                                        payload
                                        &key
                                          (now (get-universal-time)))
  (sync-message-payload
   (sync-transport-send-message transport
                                (make-sync-message :request payload)
                                :now now)
   :expected-type :response))

(defmethod sync-transport-send-message ((transport local-sync-transport)
                                        message
                                        &key
                                          (now (get-universal-time)))
  (handle-sync-message (local-sync-transport-coordinator transport)
                       message
                       :now now))

(defmethod sync-transport-send-message ((transport encoded-sync-transport)
                                        message
                                        &key
                                          (now (get-universal-time)))
  (let ((encoded-response
         (funcall (encoded-sync-transport-exchange-function transport)
                  (encode-sync-message message)
                  :now now)))
    (unless (stringp encoded-response)
      (error "Encoded sync transport expected a response string, got ~S."
             encoded-response))
    (let ((response-message (decode-sync-message encoded-response)))
      (if (eq :error (sync-message-type response-message))
          (signal-sync-error-message response-message)
          (ensure-sync-message response-message :expected-type :response)))))

(defun sync-journal-with-transport (registry journal transport authentication
                                    &key
                                      (now (get-universal-time))
                                      presence
                                      comments
                                      members
                                      attachments
                                      checkpoints
                                      conflicts)
  (let* ((request
          (journal-sync-request-payload journal
                                        :authentication authentication
                                        :presence presence
                                        :comments comments
                                        :members members
                                        :attachments attachments
                                        :checkpoints checkpoints
                                        :conflicts conflicts))
         (response (sync-transport-send-request transport request :now now))
         (result (apply-sync-response-payload registry journal response)))
    (list :request request
          :response response
          :result result)))

(defun sync-journal-with-coordinator (registry journal coordinator authentication
                                      &key
                                        (now (get-universal-time))
                                        presence
                                        comments
                                        members
                                        attachments
                                        checkpoints
                                        conflicts)
  (sync-journal-with-transport registry
                               journal
                               (make-local-sync-transport coordinator)
                               authentication
                               :now now
                               :presence presence
                               :comments comments
                               :members members
                               :attachments attachments
                               :checkpoints checkpoints
                               :conflicts conflicts))

(defun apply-operation-journal (registry journal)
  (mapcar (lambda (operation)
            (unless (operation-suppressed-by-conflict-p journal operation)
              (apply-workspace-operation registry operation)))
          (journal-operations journal)))
