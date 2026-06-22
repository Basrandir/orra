(in-package :orra)

(defgeneric persistable-object-p (object)
  (:method ((object t))
    nil))

(defmethod persistable-object-p ((object workspace)) t)
(defmethod persistable-object-p ((object notebook)) t)
(defmethod persistable-object-p ((object section)) t)
(defmethod persistable-object-p ((object paragraph)) t)
(defmethod persistable-object-p ((object code-block)) t)
(defmethod persistable-object-p ((object repl-block)) t)
(defmethod persistable-object-p ((object repl-entry)) t)
(defmethod persistable-object-p ((object quote-block)) t)
(defmethod persistable-object-p ((object reference-block)) t)
(defmethod persistable-object-p ((object inspector-block)) t)
(defmethod persistable-object-p ((object source-browser-block)) t)
(defmethod persistable-object-p ((object cross-reference-browser-block)) t)
(defmethod persistable-object-p ((object stack-frame-browser-block)) t)
(defmethod persistable-object-p ((object condition-browser-block)) t)
(defmethod persistable-object-p ((object list-block)) t)
(defmethod persistable-object-p ((object table-block)) t)
(defmethod persistable-object-p ((object task-list)) t)
(defmethod persistable-object-p ((object result-block)) t)

(defparameter *workspace-file-version* 2)

(defun workspace-file-version ()
  *workspace-file-version*)

(defun encode-value (value)
  (cond
    ((typep value 'model-object)
     (list :ref (object-id value)))
    ((or (null value)
         (stringp value)
         (numberp value)
         (characterp value)
         (keywordp value))
     value)
    ((and (symbolp value) (member value '(t nil)))
     value)
    ((symbolp value)
     (list :symbol
           (package-name (symbol-package value))
           (symbol-name value)))
    ((listp value)
     (cons :list (mapcar #'encode-value value)))
    (t
     (list :printed (printable-string value)))))

(defun decode-value (value object-table)
  (cond
    ((atom value) value)
    ((eq (first value) :ref)
     (or (gethash (second value) object-table)
         (error "Unknown object reference ~S." (second value))))
    ((eq (first value) :symbol)
     (let ((package (or (find-package (second value))
                        (error "Unknown package ~S." (second value)))))
       (intern (third value) package)))
    ((eq (first value) :list)
     (mapcar (lambda (item)
               (decode-value item object-table))
             (rest value)))
    ((eq (first value) :printed)
     (read-one-form (second value)))
    (t value)))

(defun encode-hash-table (table)
  (mapcar (lambda (entry)
            (list (encode-value (car entry))
                  (encode-value (cdr entry))))
          (hash-table-alist table)))

(defun decode-hash-table (entries object-table)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (setf (gethash (decode-value (first entry) object-table) table)
            (decode-value (second entry) object-table)))))

(defun collect-workspace-objects (workspace)
  (let ((seen (make-hash-table :test #'equal))
        objects)
    (labels ((visit (object)
               (when (and object
                          (persistable-object-p object)
                          (not (gethash (object-id object) seen)))
                 (setf (gethash (object-id object) seen) t)
                 (push object objects)
                 (when (object-prototype object)
                   (visit (object-prototype object)))
                 (dolist (child (children-of object))
                   (visit child))
                 (when (typep object 'code-block)
                   (visit (code-block-result object)))
                 (when (typep object 'repl-entry)
                   (visit (repl-entry-result object)))
                 (when (and (typep object 'reference-block)
                            (typep (reference-block-target object)
                                   'model-object))
                   (visit (reference-block-target object)))
                 (when (and (typep object 'inspector-block)
                            (typep (inspector-block-target object)
                                   'model-object))
                   (visit (inspector-block-target object)))
                 (when (and (typep object 'stack-frame-browser-block)
                            (typep (stack-frame-browser-block-target object)
                                   'model-object))
                   (visit (stack-frame-browser-block-target object)))
                 (when (and (typep object 'condition-browser-block)
                            (typep (condition-browser-block-target object)
                                   'model-object))
                   (visit (condition-browser-block-target object))))))
      (visit workspace))
    (nreverse objects)))

(defgeneric serialize-object-record (object))

(defun base-record (object type)
  (list :id (object-id object)
        :type type
        :prototype (and (object-prototype object)
                        (object-id (object-prototype object)))
        :properties (encode-hash-table (object-properties object))
        :metadata (encode-hash-table (object-metadata object))))

(defmethod serialize-object-record ((object workspace))
  (append (base-record object :workspace)
          (list :title (workspace-title object)
                :notebooks (mapcar #'object-id (workspace-notebooks object))
                :current-notebook (and (root-notebook object)
                                       (object-id (root-notebook object))))))

(defmethod serialize-object-record ((object notebook))
  (append (base-record object :notebook)
          (list :title (notebook-title object)
                :children (mapcar #'object-id (children-of object)))))

(defmethod serialize-object-record ((object section))
  (append (base-record object :section)
          (list :title (section-title object)
                :children (mapcar #'object-id (children-of object)))))

(defmethod serialize-object-record ((object paragraph))
  (append (base-record object :paragraph)
          (list :text (paragraph-text object))))

(defmethod serialize-object-record ((object code-block))
  (append (base-record object :code-block)
          (list :language (code-block-language object)
                :source (code-block-source object)
                :result (and (code-block-result object)
                             (object-id (code-block-result object))))))

(defmethod serialize-object-record ((object repl-block))
  (append (base-record object :repl-block)
          (list :title (repl-block-title object)
                :package-name (repl-block-package object)
                :children (mapcar #'object-id (children-of object)))))

(defmethod serialize-object-record ((object repl-entry))
  (append (base-record object :repl-entry)
          (list :input-source (repl-entry-input-source object)
                :result (and (repl-entry-result object)
                             (object-id (repl-entry-result object))))))

(defmethod serialize-object-record ((object quote-block))
  (append (base-record object :quote-block)
          (list :text (quote-block-text object)
                :attribution (quote-block-attribution object))))

(defmethod serialize-object-record ((object reference-block))
  (append (base-record object :reference-block)
          (list :target (encode-value (reference-block-target object))
                :label (reference-block-label object)
                :note (reference-block-note object))))

(defmethod serialize-object-record ((object inspector-block))
  (append (base-record object :inspector-block)
          (list :target (encode-value (inspector-block-target object))
                :label (inspector-block-label object))))

(defmethod serialize-object-record ((object source-browser-block))
  (append (base-record object :source-browser-block)
          (list :package-name (source-browser-block-package object)
                :symbol-name (source-browser-block-symbol object)
                :label (source-browser-block-label object))))

(defmethod serialize-object-record ((object cross-reference-browser-block))
  (append (base-record object :cross-reference-browser-block)
          (list :package-name (cross-reference-browser-block-package object)
                :symbol-name (cross-reference-browser-block-symbol object)
                :label (cross-reference-browser-block-label object))))

(defmethod serialize-object-record ((object stack-frame-browser-block))
  (append (base-record object :stack-frame-browser-block)
          (list :target (encode-value (stack-frame-browser-block-target object))
                :label (stack-frame-browser-block-label object))))

(defmethod serialize-object-record ((object condition-browser-block))
  (append (base-record object :condition-browser-block)
          (list :target (encode-value (condition-browser-block-target object))
                :label (condition-browser-block-label object))))

(defmethod serialize-object-record ((object list-block))
  (append (base-record object :list-block)
          (list :items (list-block-items object)
                :ordered-p (list-block-ordered-p object))))

(defmethod serialize-object-record ((object table-block))
  (append (base-record object :table-block)
          (list :columns (table-block-columns object)
                :rows (table-block-rows object))))

(defmethod serialize-object-record ((object task-list))
  (append (base-record object :task-list)
          (list :items (task-list-items object))))

(defmethod serialize-object-record ((object result-block))
  (append (base-record object :result-block)
          (list :value (encode-value (result-block-value object))
                :presentation (result-block-presentation object)
                :input-source (result-block-input-source object)
                :input-forms (encode-value (result-block-input-forms object))
                :package-name (result-block-package object)
                :evaluated-at (result-block-evaluated-at object)
                :environment (encode-value
                              (result-block-environment object)))))

(defun make-object-from-record (record)
  (let ((id (getf record :id))
        (type (getf record :type)))
    (ecase type
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
      (:repl-block
       (make-instance 'repl-block :id id :kind :repl-block))
      (:repl-entry
       (make-instance 'repl-entry :id id :kind :repl-entry))
      (:quote-block
       (make-instance 'quote-block :id id :kind :quote-block))
      (:reference-block
       (make-instance 'reference-block :id id :kind :reference-block))
      (:inspector-block
       (make-instance 'inspector-block :id id :kind :inspector-block))
      (:source-browser-block
       (make-instance 'source-browser-block
                      :id id
                      :kind :source-browser-block))
      (:cross-reference-browser-block
       (make-instance 'cross-reference-browser-block
                      :id id
                      :kind :cross-reference-browser-block))
      (:stack-frame-browser-block
       (make-instance 'stack-frame-browser-block
                      :id id
                      :kind :stack-frame-browser-block))
      (:condition-browser-block
       (make-instance 'condition-browser-block
                      :id id
                      :kind :condition-browser-block))
      (:list-block
       (make-instance 'list-block :id id :kind :list-block))
      (:table-block
       (make-instance 'table-block :id id :kind :table-block))
      (:task-list
       (make-instance 'task-list :id id :kind :task-list))
      (:result-block
       (make-instance 'result-block :id id :kind :result-block)))))

(defun populate-object-from-record (object record object-table)
  (setf (object-prototype object)
        (and (getf record :prototype)
             (gethash (getf record :prototype) object-table)))
  (setf (object-properties object)
        (decode-hash-table (getf record :properties) object-table))
  (setf (object-metadata object)
        (decode-hash-table (getf record :metadata) object-table))
  (typecase object
    (workspace
     (setf (workspace-title object) (getf record :title))
     (setf (workspace-notebooks object)
           (mapcar (lambda (id)
                     (gethash id object-table))
                   (getf record :notebooks)))
     (dolist (notebook (workspace-notebooks object))
       (setf (parent-of notebook) object))
     (setf (workspace-current-notebook object)
           (and (getf record :current-notebook)
                (gethash (getf record :current-notebook) object-table))))
    ((or notebook section)
     (if (typep object 'notebook)
         (setf (notebook-title object) (getf record :title))
         (setf (section-title object) (getf record :title)))
     (setf (children-of object)
           (mapcar (lambda (id)
                     (gethash id object-table))
                   (getf record :children)))
     (dolist (child (children-of object))
       (setf (parent-of child) object)))
    (paragraph
     (setf (paragraph-text object) (getf record :text)))
    (code-block
     (setf (code-block-language object) (getf record :language))
     (setf (code-block-source object) (getf record :source))
     (setf (code-block-result object)
           (and (getf record :result)
                (gethash (getf record :result) object-table))))
    (repl-block
     (setf (repl-block-title object)
           (normalize-display-string (getf record :title)))
     (setf (repl-block-package object)
           (normalize-display-string (getf record :package-name)))
     (setf (children-of object)
           (mapcar (lambda (id)
                     (gethash id object-table))
                   (getf record :children)))
     (dolist (child (children-of object))
       (setf (parent-of child) object)))
    (repl-entry
     (setf (repl-entry-input-source object)
           (normalize-display-string (getf record :input-source)))
     (setf (repl-entry-result object)
           (and (getf record :result)
                (gethash (getf record :result) object-table)))
     (when (repl-entry-result object)
       (setf (parent-of (repl-entry-result object)) object)))
    (quote-block
     (setf (quote-block-text object)
           (normalize-display-string (getf record :text)))
     (setf (quote-block-attribution object)
           (normalize-display-string (getf record :attribution))))
    (reference-block
     (setf (reference-block-target object)
           (decode-value (getf record :target) object-table))
     (setf (reference-block-label object)
           (normalize-display-string (getf record :label)))
     (setf (reference-block-note object)
           (normalize-display-string (getf record :note))))
    (inspector-block
     (setf (inspector-block-target object)
           (decode-value (getf record :target) object-table))
     (setf (inspector-block-label object)
           (normalize-display-string (getf record :label))))
    (source-browser-block
     (setf (source-browser-block-package object)
           (normalize-display-string (getf record :package-name)))
     (setf (source-browser-block-symbol object)
           (normalize-display-string (getf record :symbol-name)))
     (setf (source-browser-block-label object)
           (normalize-display-string (getf record :label))))
    (cross-reference-browser-block
     (setf (cross-reference-browser-block-package object)
           (normalize-display-string (getf record :package-name)))
     (setf (cross-reference-browser-block-symbol object)
           (normalize-display-string (getf record :symbol-name)))
     (setf (cross-reference-browser-block-label object)
           (normalize-display-string (getf record :label))))
    (stack-frame-browser-block
     (setf (stack-frame-browser-block-target object)
           (decode-value (getf record :target) object-table))
     (setf (stack-frame-browser-block-label object)
           (normalize-display-string (getf record :label))))
    (condition-browser-block
     (setf (condition-browser-block-target object)
           (decode-value (getf record :target) object-table))
     (setf (condition-browser-block-label object)
           (normalize-display-string (getf record :label))))
    (list-block
     (setf (list-block-items object)
           (normalize-display-strings (getf record :items)))
     (setf (list-block-ordered-p object)
           (not (null (getf record :ordered-p)))))
    (table-block
     (setf (table-block-columns object)
           (normalize-display-strings (getf record :columns)))
     (setf (table-block-rows object)
           (normalize-table-rows (getf record :rows))))
    (task-list
     (setf (task-list-items object)
           (normalize-task-items (getf record :items))))
    (result-block
     (setf (result-block-value object)
           (decode-value (getf record :value) object-table))
     (setf (result-block-presentation object) (getf record :presentation))
     (setf (result-block-input-source object)
           (normalize-display-string (getf record :input-source)))
     (setf (result-block-input-forms object)
           (decode-value (getf record :input-forms) object-table))
     (setf (result-block-package object)
           (normalize-display-string (getf record :package-name)))
     (setf (result-block-evaluated-at object)
           (getf record :evaluated-at))
     (setf (result-block-environment object)
           (decode-value (getf record :environment) object-table))))
  object)

(defun workspace-file-payload (workspace &key registry (mode :save) timestamp)
  (let* ((objects (if registry
                      (remove-if-not #'persistable-object-p
                                     (registry-objects-list registry))
                      (collect-workspace-objects workspace)))
         (timestamp (or timestamp (get-universal-time)))
         (payload (list :version *workspace-file-version*
                        :mode mode
                        :saved-at timestamp
                        :workspace-id (object-id workspace)
                        :objects (mapcar #'serialize-object-record objects))))
    (case mode
      (:archive
       (append payload (list :archived-at timestamp)))
      (:checkpoint
       (append payload (list :checkpoint-at timestamp)))
      (t payload))))

(defun write-workspace-payload-to-file (payload path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      (print payload stream)))
  path)

(defun read-workspace-payload-from-file (path)
  (with-open-file (stream path :direction :input)
    (with-standard-io-syntax
      (read stream))))

(defun ensure-payload-value (payload key default)
  (multiple-value-bind (value presentp)
      (plist-value payload key)
    (declare (ignore value))
    (if presentp
        payload
        (put-plist-value payload key default))))

(defun migrate-workspace-payload-from-version-1 (payload)
  (let ((payload (put-plist-value payload
                                  :version
                                  *workspace-file-version*)))
    (setf payload (ensure-payload-value payload :mode :save))
    (setf payload (ensure-payload-value payload :saved-at 0))
    payload))

(defun migrate-workspace-payload (payload)
  (let ((version (or (getf payload :version) 1)))
    (cond
      ((> version *workspace-file-version*)
       (error "Unsupported workspace file version ~A; this build supports up to ~A."
              version
              *workspace-file-version*))
      ((= version *workspace-file-version*)
       payload)
      ((= version 1)
       (migrate-workspace-payload-from-version-1 payload))
      (t
       (error "Unsupported workspace file version ~A." version)))))

(defun save-workspace-to-file (workspace path &key registry (mode :save))
  (write-workspace-payload-to-file
   (workspace-file-payload workspace
                           :registry registry
                           :mode mode)
   path))

(defun clone-workspace-to-file (workspace path &key registry)
  (save-workspace-to-file workspace path :registry registry :mode :clone))

(defun archive-workspace-to-file (workspace path &key registry)
  (save-workspace-to-file workspace path :registry registry :mode :archive))

(defun checkpoint-workspace-to-file (workspace path &key registry timestamp)
  (write-workspace-payload-to-file
   (workspace-file-payload workspace
                           :registry registry
                           :mode :checkpoint
                           :timestamp timestamp)
   path))

(defun workspace-checkpoint-record (path)
  (handler-case
      (let ((payload (migrate-workspace-payload
                      (read-workspace-payload-from-file path))))
        (when (and (eq :checkpoint (getf payload :mode))
                   (integerp (getf payload :checkpoint-at)))
          (let ((registry (make-object-registry)))
            (load-workspace-from-file path :registry registry))
          (list :path path
                :checkpoint-at (getf payload :checkpoint-at))))
    (error ()
      nil)))

(defun workspace-checkpoint-records (directory)
  (loop for path in (directory
                     (merge-pathnames
                      "*.sexp"
                      (uiop:ensure-directory-pathname directory)))
        for record = (workspace-checkpoint-record path)
        when record
        collect record))

(defun latest-workspace-checkpoint (directory)
  (let ((records (workspace-checkpoint-records directory)))
    (getf (first (sort records #'>
                       :key (lambda (record)
                              (getf record :checkpoint-at))))
          :path)))

(defun load-workspace-from-file (path &key registry)
  (let* ((payload (read-workspace-payload-from-file path))
         (payload (migrate-workspace-payload payload))
         (records (getf payload :objects))
         (object-table (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((object (make-object-from-record record)))
        (setf (gethash (getf record :id) object-table) object)
        (when registry
          (register-object registry object))))
    (dolist (record records)
      (populate-object-from-record
       (gethash (getf record :id) object-table)
       record
       object-table))
    (let ((workspace (gethash (getf payload :workspace-id) object-table)))
      (unless (typep workspace 'workspace)
        (error "The file ~A does not describe a workspace." path))
      workspace)))
