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
    :initform "")))

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
  (setf (code-block-source block) source)
  (invalidate-code-block-result block)
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
   (result
    :initarg :result
    :accessor code-block-result
    :initform nil)))

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

(defun make-result-block (&key value (presentation "") registry)
  (%register-if-present
   registry
   (make-instance 'result-block
                  :id (fresh-id "result")
                  :kind :result-block
                  :value value
                  :presentation presentation)))

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
