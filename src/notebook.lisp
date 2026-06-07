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

(defclass inspector-block (node)
  ((target
    :initarg :target
    :accessor inspector-block-target
    :initform nil)
   (label
    :initarg :label
    :accessor inspector-block-label
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

(defun make-inspector-block (&key target (label "") registry)
  (%register-if-present
   registry
   (make-instance 'inspector-block
                  :id (fresh-id "inspect")
                  :kind :inspector-block
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
