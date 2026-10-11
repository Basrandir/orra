(in-package :orra)

(defclass node (model-object)
  ((parent
    :initarg :parent
    :accessor parent-of
    :initform nil)))

(defclass composite-node (node)
  ((children
    :initarg :children
    :accessor children-of
    :initform nil)))

(defclass workspace (model-object)
  ((title
    :initarg :title
    :accessor workspace-title
    :initform "Untitled Workspace")
   (notebooks
    :initarg :notebooks
    :accessor workspace-notebooks
    :initform nil)
   (current-notebook
    :initarg :current-notebook
    :accessor workspace-current-notebook
    :initform nil)))

(defmethod children-of ((object workspace))
  (workspace-notebooks object))

(defclass notebook (composite-node)
  ((title
    :initarg :title
    :accessor notebook-title
    :initform "Notebook")))

(defclass section (composite-node)
  ((title
    :initarg :title
    :accessor section-title
    :initform "Section")))

(defclass knowledge-item (node)
  ((title
    :initarg :title
    :accessor knowledge-item-title
    :initform "")
   (summary
    :initarg :summary
    :accessor knowledge-item-summary
    :initform "")))

(defclass paragraph (node)
  ((text
    :initarg :text
    :accessor paragraph-text
    :initform "")))

(defclass result-block (node)
  ((value
    :initarg :value
    :accessor result-block-value
    :initform nil)
   (presentation
    :initarg :presentation
    :accessor result-block-presentation
    :initform "")
   (input-source
    :initarg :input-source
    :accessor result-block-input-source
    :initform "")
   (input-forms
    :initarg :input-forms
    :accessor result-block-input-forms
    :initform nil)
   (package-name
    :initarg :package-name
    :accessor result-block-package
    :initform "")
   (evaluated-at
    :initarg :evaluated-at
    :accessor result-block-evaluated-at
    :initform nil)
   (environment
    :initarg :environment
    :accessor result-block-environment
    :initform nil)))

(defun result-block-status (result-block)
  (object-property result-block :status :default :ok :inherit nil))

(defun set-result-block-status (result-block status)
  (set-object-property result-block :status status)
  result-block)

(defun invalidate-code-block-result (block)
  (let ((result (code-block-result block)))
    (when result
      (setf (result-block-value result) nil)
      (setf (result-block-presentation result)
            "Result invalidated by source changes.")
      (set-result-block-status result :stale)))
  block)

(defun replace-code-block-source (block source)
  (let ((source (string source)))
    (unless (string= (code-block-source block) source)
      (setf (code-block-source block) source)
      (setf (code-block-parse-cache block) nil)
      (invalidate-code-block-result block)))
  block)

(defclass code-block (node)
  ((language
    :initarg :language
    :accessor code-block-language
    :initform :common-lisp)
   (source
    :initarg :source
    :accessor code-block-source
    :initform "")
   (parse-cache
    :accessor code-block-parse-cache
    :initform nil)
   (result
    :initarg :result
    :accessor code-block-result
    :initform nil)))

(defclass repl-block (composite-node)
  ((title
    :initarg :title
    :accessor repl-block-title
    :initform "REPL")
   (package-name
    :initarg :package-name
    :accessor repl-block-package
    :initform "COMMON-LISP-USER")))

(defclass repl-entry (node)
  ((input-source
    :initarg :input-source
    :accessor repl-entry-input-source
    :initform "")
   (result
    :initarg :result
    :accessor repl-entry-result
    :initform nil)))

(defclass quote-block (node)
  ((text
    :initarg :text
    :accessor quote-block-text
    :initform "")
   (attribution
    :initarg :attribution
    :accessor quote-block-attribution
    :initform "")))

(defclass reference-block (node)
  ((target
    :initarg :target
    :accessor reference-block-target
    :initform nil)
   (label
    :initarg :label
    :accessor reference-block-label
    :initform "")
   (note
    :initarg :note
    :accessor reference-block-note
    :initform "")))

(defclass link-browser-block (node)
  ((target
    :initarg :target
    :accessor link-browser-block-target
    :initform nil)
   (direction
    :initarg :direction
    :accessor link-browser-block-direction
    :initform :both)
   (relation
    :initarg :relation
    :accessor link-browser-block-relation
    :initform nil)
   (label
    :initarg :label
    :accessor link-browser-block-label
    :initform "")))

(defclass inspector-block (node)
  ((target
    :initarg :target
    :accessor inspector-block-target
    :initform nil)
   (label
    :initarg :label
    :accessor inspector-block-label
    :initform "")))

(defclass source-browser-block (node)
  ((package-name
    :initarg :package-name
    :accessor source-browser-block-package
    :initform "COMMON-LISP-USER")
   (symbol-name
    :initarg :symbol-name
    :accessor source-browser-block-symbol
    :initform "")
   (label
    :initarg :label
    :accessor source-browser-block-label
    :initform "")))

(defclass cross-reference-browser-block (node)
  ((package-name
    :initarg :package-name
    :accessor cross-reference-browser-block-package
    :initform "COMMON-LISP-USER")
   (symbol-name
    :initarg :symbol-name
    :accessor cross-reference-browser-block-symbol
    :initform "")
   (label
    :initarg :label
    :accessor cross-reference-browser-block-label
    :initform "")))

(defclass stack-frame-browser-block (node)
  ((target
    :initarg :target
    :accessor stack-frame-browser-block-target
    :initform nil)
   (label
    :initarg :label
    :accessor stack-frame-browser-block-label
    :initform "")))

(defclass condition-browser-block (node)
  ((target
    :initarg :target
    :accessor condition-browser-block-target
    :initform nil)
   (label
    :initarg :label
    :accessor condition-browser-block-label
    :initform "")))

(defclass list-block (node)
  ((items
    :initarg :items
    :accessor list-block-items
    :initform nil)
   (ordered-p
    :initarg :ordered-p
    :accessor list-block-ordered-p
    :initform nil)))

(defclass table-block (node)
  ((columns
    :initarg :columns
    :accessor table-block-columns
    :initform nil)
   (rows
    :initarg :rows
    :accessor table-block-rows
    :initform nil)))

(defclass task-list (node)
  ((items
    :initarg :items
    :accessor task-list-items
    :initform nil)))

(defparameter *project-task-statuses*
  '(:todo :in-progress :blocked :done :cancelled))

(defparameter *project-task-priorities*
  '(:low :normal :high :urgent))

(defclass project-task (knowledge-item)
  ((status
    :initarg :status
    :accessor project-task-status
    :initform :todo)
   (priority
    :initarg :priority
    :accessor project-task-priority
    :initform :normal)
   (assignee-id
    :initarg :assignee-id
    :accessor project-task-assignee-id
    :initform nil)
   (due-at
    :initarg :due-at
    :accessor project-task-due-at
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor project-task-created-at
    :initform nil)))

(defparameter *project-issue-statuses*
  '(:open :triaged :in-progress :resolved :closed :wont-fix))

(defparameter *project-issue-severities*
  '(:low :medium :high :critical))

(defclass project-issue (knowledge-item)
  ((status
    :initarg :status
    :accessor project-issue-status
    :initform :open)
   (severity
    :initarg :severity
    :accessor project-issue-severity
    :initform :medium)
   (reporter-id
    :initarg :reporter-id
    :accessor project-issue-reporter-id
    :initform nil)
   (assignee-id
    :initarg :assignee-id
    :accessor project-issue-assignee-id
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor project-issue-created-at
    :initform nil)))

(defparameter *project-decision-statuses*
  '(:proposed :accepted :rejected :superseded))

(defclass project-decision (knowledge-item)
  ((status
    :initarg :status
    :accessor project-decision-status
    :initform :proposed)
   (outcome
    :initarg :outcome
    :accessor project-decision-outcome
    :initform "")
   (rationale
    :initarg :rationale
    :accessor project-decision-rationale
    :initform "")
   (decided-by-id
    :initarg :decided-by-id
    :accessor project-decision-decided-by-id
    :initform nil)
   (decided-at
    :initarg :decided-at
    :accessor project-decision-decided-at
    :initform nil)
   (created-at
    :initarg :created-at
    :accessor project-decision-created-at
    :initform nil)))

(defparameter *project-meeting-note-statuses*
  '(:planned :in-progress :completed :cancelled))

(defclass project-meeting-note (knowledge-item composite-node)
  ((status
    :initarg :status
    :accessor project-meeting-note-status
    :initform :planned)
   (occurred-at
    :initarg :occurred-at
    :accessor project-meeting-note-occurred-at
    :initform nil)
   (facilitator-id
    :initarg :facilitator-id
    :accessor project-meeting-note-facilitator-id
    :initform nil)
   (attendee-ids
    :initarg :attendee-ids
    :accessor project-meeting-note-attendee-ids
    :initform nil)
   (location
    :initarg :location
    :accessor project-meeting-note-location
    :initform "")
   (created-at
    :initarg :created-at
    :accessor project-meeting-note-created-at
    :initform nil)))

(defclass project-code-reference (knowledge-item)
  ((target
    :reader project-code-reference-target
    :initform nil)
   (form-path
    :reader project-code-reference-form-path
    :initform nil)
   (source-snapshot
    :reader project-code-reference-source-snapshot
    :initform "")
   (created-at
    :initarg :created-at
    :accessor project-code-reference-created-at
    :initform nil)))

(defun normalize-display-string (value)
  (cond
    ((null value) "")
    ((stringp value) value)
    (t (princ-to-string value))))

(defun normalize-display-strings (values)
  (mapcar #'normalize-display-string values))

(defun normalize-table-row (row)
  (normalize-display-strings (ensure-list row)))

(defun normalize-table-rows (rows)
  (mapcar #'normalize-table-row rows))

(defun make-task-item (&key (text "") done)
  (list :text (normalize-display-string text)
        :done (not (null done))))

(defun task-item-text (item)
  (if (listp item)
      (normalize-display-string (getf item :text ""))
      (normalize-display-string item)))

(defun task-item-done-p (item)
  (and (listp item)
       (not (null (getf item :done)))))

(defun normalize-task-item (item)
  (cond
    ((stringp item)
     (make-task-item :text item))
    ((listp item)
     (make-task-item :text (task-item-text item)
                     :done (task-item-done-p item)))
    (t
     (make-task-item :text item))))

(defun normalize-task-items (items)
  (mapcar #'normalize-task-item items))

(defun ensure-project-task-status (status)
  (unless (member status *project-task-statuses*)
    (error "Unsupported project task status ~S." status))
  status)

(defun ensure-project-task-priority (priority)
  (unless (member priority *project-task-priorities*)
    (error "Unsupported project task priority ~S." priority))
  priority)

(defun ensure-knowledge-item-time (value role)
  (unless (or (null value)
              (and (integerp value) (not (minusp value))))
    (error "Knowledge item ~A must be NIL or a non-negative universal time, got ~S."
           role
           value))
  value)

(defun ensure-knowledge-item-created-at (value)
  (or (ensure-knowledge-item-time value :created-at)
      (error "Knowledge item creation time cannot be NIL.")))

(defun normalize-knowledge-actor-id (actor-id)
  (and actor-id (normalize-display-string actor-id)))

(defun normalize-knowledge-actor-ids (actor-ids)
  (unless (listp actor-ids)
    (error "Knowledge item actor ids must be a list, got ~S." actor-ids))
  (mapcar #'normalize-knowledge-actor-id actor-ids))

(defun ensure-project-issue-status (status)
  (unless (member status *project-issue-statuses*)
    (error "Unsupported project issue status ~S." status))
  status)

(defun ensure-project-issue-severity (severity)
  (unless (member severity *project-issue-severities*)
    (error "Unsupported project issue severity ~S." severity))
  severity)

(defun ensure-project-decision-status (status)
  (unless (member status *project-decision-statuses*)
    (error "Unsupported project decision status ~S." status))
  status)

(defun ensure-project-meeting-note-status (status)
  (unless (member status *project-meeting-note-statuses*)
    (error "Unsupported project meeting note status ~S." status))
  status)

(defun ensure-link-browser-direction (direction)
  (unless (member direction '(:outgoing :incoming :both))
    (error "Unsupported link browser direction ~S." direction))
  direction)

(defun %register-if-present (registry object)
  (when registry
    (register-object registry object))
  object)

(defun make-workspace (&key (title "Untitled Workspace") registry)
  (%register-if-present
   registry
   (make-instance 'workspace
                  :id (fresh-id "workspace")
                  :kind :workspace
                  :title title)))

(defun make-notebook (&key (title "Notebook") registry)
  (%register-if-present
   registry
   (make-instance 'notebook
                  :id (fresh-id "notebook")
                  :kind :notebook
                  :title title)))

(defun make-section (&key (title "Section") registry)
  (%register-if-present
   registry
   (make-instance 'section
                  :id (fresh-id "section")
                  :kind :section
                  :title title)))

(defun make-paragraph (&key (text "") registry)
  (%register-if-present
   registry
   (make-instance 'paragraph
                  :id (fresh-id "paragraph")
                  :kind :paragraph
                  :text text)))

(defun make-code-block (&key (language :common-lisp) (source "") registry)
  (%register-if-present
   registry
   (make-instance 'code-block
                  :id (fresh-id "code")
                  :kind :code-block
                  :language language
                  :source source)))

(defun make-quote-block (&key (text "") (attribution "") registry)
  (%register-if-present
   registry
   (make-instance 'quote-block
                  :id (fresh-id "quote")
                  :kind :quote-block
                  :text (normalize-display-string text)
                  :attribution (normalize-display-string attribution))))

(defun make-reference-block (&key target (label "") (note "") registry)
  (%register-if-present
   registry
   (make-instance 'reference-block
                  :id (fresh-id "ref")
                  :kind :reference-block
                  :target target
                  :label (normalize-display-string label)
                  :note (normalize-display-string note))))

(defun make-link-browser-block
    (&key target (direction :both) relation (label "") registry)
  (unless (typep target 'model-object)
    (error "Link browsers require a semantic object target, got ~S." target))
  (%register-if-present
   registry
   (make-instance 'link-browser-block
                  :id (fresh-id "links")
                  :kind :link-browser-block
                  :target target
                  :direction (ensure-link-browser-direction direction)
                  :relation relation
                  :label (normalize-display-string label))))

(defun make-inspector-block (&key target (label "") registry)
  (%register-if-present
   registry
   (make-instance 'inspector-block
                  :id (fresh-id "inspect")
                  :kind :inspector-block
                  :target target
                  :label (normalize-display-string label))))

(defun make-source-browser-block (&key (package-name (package-name *package*))
                                    (symbol-name "")
                                    (label "")
                                    registry)
  (%register-if-present
   registry
   (make-instance 'source-browser-block
                  :id (fresh-id "source")
                  :kind :source-browser-block
                  :package-name (normalize-display-string package-name)
                  :symbol-name (normalize-display-string symbol-name)
                  :label (normalize-display-string label))))

(defun make-cross-reference-browser-block
    (&key (package-name (package-name *package*))
       (symbol-name "")
       (label "")
       registry)
  (%register-if-present
   registry
   (make-instance 'cross-reference-browser-block
                  :id (fresh-id "xref")
                  :kind :cross-reference-browser-block
                  :package-name (normalize-display-string package-name)
                  :symbol-name (normalize-display-string symbol-name)
                  :label (normalize-display-string label))))

(defun make-stack-frame-browser-block (&key target (label "") registry)
  (%register-if-present
   registry
   (make-instance 'stack-frame-browser-block
                  :id (fresh-id "stack")
                  :kind :stack-frame-browser-block
                  :target target
                  :label (normalize-display-string label))))

(defun make-condition-browser-block (&key target (label "") registry)
  (%register-if-present
   registry
   (make-instance 'condition-browser-block
                  :id (fresh-id "condition")
                  :kind :condition-browser-block
                  :target target
                  :label (normalize-display-string label))))

(defun make-list-block (&key items ordered-p registry)
  (%register-if-present
   registry
   (make-instance 'list-block
                  :id (fresh-id "list")
                  :kind :list-block
                  :items (normalize-display-strings items)
                  :ordered-p (not (null ordered-p)))))

(defun make-table-block (&key columns rows registry)
  (%register-if-present
   registry
   (make-instance 'table-block
                  :id (fresh-id "table")
                  :kind :table-block
                  :columns (normalize-display-strings columns)
                  :rows (normalize-table-rows rows))))

(defun make-task-list (&key items registry)
  (%register-if-present
   registry
   (make-instance 'task-list
                  :id (fresh-id "tasks")
                  :kind :task-list
                  :items (normalize-task-items items))))

(defun make-project-task (&key (title "") (summary "") (status :todo)
                            (priority :normal) assignee-id due-at
                            (created-at (get-universal-time)) registry)
  (%register-if-present
   registry
   (make-instance 'project-task
                  :id (fresh-id "task")
                  :kind :project-task
                  :title (normalize-display-string title)
                  :summary (normalize-display-string summary)
                  :status (ensure-project-task-status status)
                  :priority (ensure-project-task-priority priority)
                  :assignee-id
                  (normalize-knowledge-actor-id assignee-id)
                  :due-at (ensure-knowledge-item-time due-at :due-at)
                  :created-at
                  (ensure-knowledge-item-created-at created-at))))

(defun make-project-issue (&key (title "") (summary "") (status :open)
                             (severity :medium) reporter-id assignee-id
                             (created-at (get-universal-time)) registry)
  (%register-if-present
   registry
   (make-instance 'project-issue
                  :id (fresh-id "issue")
                  :kind :project-issue
                  :title (normalize-display-string title)
                  :summary (normalize-display-string summary)
                  :status (ensure-project-issue-status status)
                  :severity (ensure-project-issue-severity severity)
                  :reporter-id (normalize-knowledge-actor-id reporter-id)
                  :assignee-id (normalize-knowledge-actor-id assignee-id)
                  :created-at
                  (ensure-knowledge-item-created-at created-at))))

(defun make-project-decision
    (&key (title "") (summary "") (status :proposed) (outcome "")
       (rationale "") decided-by-id decided-at
       (created-at (get-universal-time)) registry)
  (%register-if-present
   registry
   (make-instance 'project-decision
                  :id (fresh-id "decision")
                  :kind :project-decision
                  :title (normalize-display-string title)
                  :summary (normalize-display-string summary)
                  :status (ensure-project-decision-status status)
                  :outcome (normalize-display-string outcome)
                  :rationale (normalize-display-string rationale)
                  :decided-by-id
                  (normalize-knowledge-actor-id decided-by-id)
                  :decided-at
                  (ensure-knowledge-item-time decided-at :decided-at)
                  :created-at
                  (ensure-knowledge-item-created-at created-at))))

(defun make-project-meeting-note
    (&key (title "") (summary "") (status :planned) occurred-at
       facilitator-id attendee-ids (location "")
       (created-at (get-universal-time)) registry)
  (%register-if-present
   registry
   (make-instance 'project-meeting-note
                  :id (fresh-id "meeting")
                  :kind :project-meeting-note
                  :title (normalize-display-string title)
                  :summary (normalize-display-string summary)
                  :status (ensure-project-meeting-note-status status)
                  :occurred-at
                  (ensure-knowledge-item-time occurred-at :occurred-at)
                  :facilitator-id
                  (normalize-knowledge-actor-id facilitator-id)
                  :attendee-ids
                  (normalize-knowledge-actor-ids attendee-ids)
                  :location (normalize-display-string location)
                  :created-at
                  (ensure-knowledge-item-created-at created-at))))

(defun make-project-code-reference
    (&key (title "") (summary "") target form-path
       (created-at (get-universal-time)) registry)
  (unless (typep target 'code-block)
    (error "Code references require a code block target, got ~S." target))
  (when (and form-path (not (code-block-common-lisp-p target)))
    (error "Structural code references require a Common Lisp code block."))
  (let ((reference (make-instance 'project-code-reference
                                  :id (fresh-id "code-reference")
                                  :kind :project-code-reference
                                  :title (normalize-display-string title)
                                  :summary (normalize-display-string summary)
                                  :created-at
                                  (ensure-knowledge-item-created-at created-at))))
    (set-project-code-reference-anchor
     reference target form-path (code-block-source target))
    (%register-if-present registry reference)))

(defun make-result-block (&key value (presentation "") (input-source "")
                            input-forms (package-name "") evaluated-at
                            environment registry)
  (%register-if-present
   registry
   (make-instance 'result-block
                  :id (fresh-id "result")
                  :kind :result-block
                  :value value
                  :presentation presentation
                  :input-source (normalize-display-string input-source)
                  :input-forms input-forms
                  :package-name (normalize-display-string package-name)
                  :evaluated-at evaluated-at
                  :environment environment)))

(defun make-repl-block (&key (title "REPL")
                          (package-name (package-name *package*))
                          registry)
  (%register-if-present
   registry
   (make-instance 'repl-block
                  :id (fresh-id "repl")
                  :kind :repl-block
                  :title (normalize-display-string title)
                  :package-name (normalize-display-string package-name))))

(defun make-repl-entry (&key (input-source "") result registry)
  (%register-if-present
   registry
   (make-instance 'repl-entry
                  :id (fresh-id "repl-entry")
                  :kind :repl-entry
                  :input-source (normalize-display-string input-source)
                  :result result)))

(defgeneric append-child (parent child))

(defmethod append-child ((parent workspace) (child notebook))
  (setf (workspace-notebooks parent)
        (append (workspace-notebooks parent) (list child)))
  (setf (parent-of child) parent)
  (unless (workspace-current-notebook parent)
    (setf (workspace-current-notebook parent) child))
  child)

(defmethod append-child ((parent composite-node) (child node))
  (setf (children-of parent)
        (append (children-of parent) (list child)))
  (setf (parent-of child) parent)
  child)

(defun root-notebook (workspace)
  (or (workspace-current-notebook workspace)
      (first (workspace-notebooks workspace))))

(defun map-workspace-content (function workspace)
  "Visit contained objects, including result slots, without following references."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (object notebook ancestors)
               (when (and (typep object 'model-object) (not (gethash object seen)))
                 (setf (gethash object seen) t)
                 (let ((notebook (if (typep object 'notebook) object notebook)))
                   (funcall function object notebook (reverse ancestors))
                   (dolist (child (children-of object))
                     (visit child notebook (cons object ancestors)))
                   (typecase object
                     (code-block (visit (code-block-result object) notebook (cons object ancestors)))
                     (repl-entry (visit (repl-entry-result object) notebook (cons object ancestors))))))))
      (visit workspace nil nil)))
  workspace)

(defun make-scratch-workspace (registry)
  (let* ((workspace (make-workspace
                     :title "Orra Scratch"
                     :registry registry))
         (notebook (make-notebook
                    :title "Structural Notebook"
                    :registry registry))
         (section (make-section
                   :title "Boot"
                   :registry registry))
         (paragraph (make-paragraph
                     :text "This image is live. Commands and objects are data."
                     :registry registry))
         (code (make-code-block
                :source "(list :hello :orra)"
                :registry registry)))
    (set-object-property code :show-structure t)
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (append-child section code)
    workspace))

(defun ensure-default-section (workspace registry)
  (let* ((notebook (or (root-notebook workspace)
                       (let ((new-notebook (make-notebook
                                            :title "Notebook"
                                            :registry registry)))
                         (append-child workspace new-notebook)
                         new-notebook)))
         (section (find-if (lambda (child)
                             (typep child 'section))
                           (children-of notebook))))
    (or section
        (let ((new-section (make-section
                            :title "Scratch"
                            :registry registry)))
          (append-child notebook new-section)
          new-section))))
