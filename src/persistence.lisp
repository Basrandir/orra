(in-package :orra)

(defgeneric persistable-object-p (object)
  (:method ((object t))
    nil))

(defmethod persistable-object-p ((object workspace)) t)
(defmethod persistable-object-p ((object notebook)) t)
(defmethod persistable-object-p ((object section)) t)
(defmethod persistable-object-p ((object paragraph)) t)
(defmethod persistable-object-p ((object code-block)) t)
(defmethod persistable-object-p ((object result-block)) t)

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
                   (visit (code-block-result object))))))
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

(defmethod serialize-object-record ((object result-block))
  (append (base-record object :result-block)
          (list :value (encode-value (result-block-value object))
                :presentation (result-block-presentation object))))

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
    (result-block
     (setf (result-block-value object)
           (decode-value (getf record :value) object-table))
     (setf (result-block-presentation object) (getf record :presentation))))
  object)

(defun save-workspace-to-file (workspace path &key registry)
  (let* ((objects (if registry
                      (remove-if-not #'persistable-object-p
                                     (registry-objects-list registry))
                      (collect-workspace-objects workspace)))
         (payload (list :version 1
                        :workspace-id (object-id workspace)
                        :objects (mapcar #'serialize-object-record objects))))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (print payload stream))))
  path)

(defun load-workspace-from-file (path &key registry)
  (let* ((payload (with-open-file (stream path :direction :input)
                    (with-standard-io-syntax
                      (read stream))))
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
