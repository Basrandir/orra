(in-package :orra)

(defun language-name-string (language)
  (typecase language
    (symbol (symbol-name language))
    (string language)
    (t (princ-to-string language))))

(defun code-block-common-lisp-p (block)
  (let ((language (code-block-language block)))
    (and language
         (member (string-upcase (language-name-string language))
                 '("COMMON-LISP" "CL")
                 :test #'string=))))

(defun parse-common-lisp-source (source)
  (let ((offset 0)
        (forms nil))
    (handler-case
        (with-input-from-string (stream source)
          (loop for form = (progn
                             (setf offset
                                   (or (ignore-errors (file-position stream))
                                       offset))
                             (read stream nil :eof))
		until (eq form :eof)
		do (push form forms))
          (list :forms (nreverse forms)
                :error nil
                :offset nil))
      (error (condition)
        (list :forms (nreverse forms)
              :error (princ-to-string condition)
              :offset offset)))))

(defun code-block-parse-info (block)
  (if (code-block-common-lisp-p block)
      (parse-common-lisp-source (code-block-source block))
      (list :forms nil
            :error nil
            :offset nil
            :unsupported-language (code-block-language block))))

(defun code-block-structure-visible-p (block)
  (object-property block :show-structure :default nil :inherit nil))

(defun toggle-code-block-structure (block)
  (set-object-property block
                       :show-structure
                       (not (code-block-structure-visible-p block)))
  block)

(defun code-block-top-level-forms (block &optional info)
  (getf (or info (code-block-parse-info block)) :forms))

(defun serialize-common-lisp-forms (forms)
  (with-output-to-string (stream)
    (loop for form in forms
          for firstp = t then nil
          do (unless firstp
               (write-char #\Newline stream))
          (write-string (printable-string form) stream))))

(defun clamp-code-block-selected-form-index (index form-count)
  (and (plusp form-count)
       (max 0
            (min (1- form-count)
                 (if (integerp index) index 0)))))

(defun form-child-items-and-tail (form)
  (let ((items nil)
        (tail form))
    (loop while (consp tail)
          do (push (car tail) items)
          (setf tail (cdr tail)))
    (values (nreverse items) tail)))

(defun rebuild-cons-form (items tail)
  (reduce (lambda (rest item)
            (cons item rest))
          (reverse items)
          :initial-value tail))

(defun normalize-code-form-path (forms path)
  (labels ((normalize-sequence-path (sequence indices)
             (when (and indices
                        (every #'integerp indices))
               (let ((index (first indices)))
                 (when (and (<= 0 index)
                            (< index (length sequence)))
                   (if (null (rest indices))
                       (list index)
                       (let ((child-path
                              (normalize-form-path
                               (nth index sequence)
                               (rest indices))))
                         (and child-path
                              (cons index child-path))))))))
           (normalize-form-path (form indices)
             (multiple-value-bind (items tail)
                 (form-child-items-and-tail form)
               (declare (ignore tail))
               (and items
                    (normalize-sequence-path items indices)))))
    (normalize-sequence-path forms path)))

(defun code-block-selected-form-path (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (form-count (length forms))
         (stored-path (object-property block
                                       :selected-form-path
                                       :default nil
                                       :inherit nil))
         (stored-index (object-property block
                                        :selected-form-index
                                        :default 0
                                        :inherit nil)))
    (unless (or (getf info :error)
                (getf info :unsupported-language))
      (or (normalize-code-form-path forms stored-path)
          (let ((selected-index
                 (clamp-code-block-selected-form-index
                  stored-index
                  form-count)))
            (and selected-index
                 (list selected-index)))))))

(defun set-code-block-selected-form-path (block path &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (selected-path (normalize-code-form-path forms path)))
    (if selected-path
        (progn
          (set-object-property block :selected-form-path selected-path)
          (set-object-property block
                               :selected-form-index
                               (first selected-path)))
        (progn
          (remhash :selected-form-path (object-properties block))
          (remhash :selected-form-index (object-properties block))))
    selected-path))

(defun code-block-selected-form-index (block &optional info)
  (first (code-block-selected-form-path block info)))

(defun set-code-block-selected-form-index (block index &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (selected-index (clamp-code-block-selected-form-index
                          index
                          (length forms))))
    (set-code-block-selected-form-path
     block
     (and selected-index (list selected-index))
     info)
    selected-index))

(defun code-form-at-path (forms path)
  (labels ((walk-sequence (sequence indices)
             (let ((selected (nth (first indices) sequence)))
               (if (null (rest indices))
                   selected
                   (walk-form selected (rest indices)))))
           (walk-form (form indices)
             (multiple-value-bind (items tail)
                 (form-child-items-and-tail form)
               (declare (ignore tail))
               (and items
                    (walk-sequence items indices)))))
    (and path
         (walk-sequence forms path))))

(defun code-block-selected-form (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (selected-path (code-block-selected-form-path block info))
         (forms (code-block-top-level-forms block info)))
    (code-form-at-path forms selected-path)))

(defun code-form-child-items (form)
  (multiple-value-bind (items tail)
      (form-child-items-and-tail form)
    (declare (ignore tail))
    items))

(defun selected-code-form-path-display-string (path)
  (format nil "~{~D~^.~}" (mapcar #'1+ path)))

(defun code-form-sibling-sequence (forms path)
  (if (null (rest path))
      forms
      (code-form-child-items
       (code-form-at-path forms (butlast path)))))

(defun select-next-code-block-form (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (current-path (code-block-selected-form-path block info))
         (siblings (and current-path
                        (code-form-sibling-sequence forms current-path))))
    (when (and current-path siblings)
      (set-code-block-selected-form-path
       block
       (append (butlast current-path)
               (list (clamp-code-block-selected-form-index
                      (1+ (car (last current-path)))
                      (length siblings))))
       info))))

(defun select-previous-code-block-form (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (current-path (code-block-selected-form-path block info))
         (siblings (and current-path
                        (code-form-sibling-sequence forms current-path))))
    (when (and current-path siblings)
      (set-code-block-selected-form-path
       block
       (append (butlast current-path)
               (list (clamp-code-block-selected-form-index
                      (1- (car (last current-path)))
                      (length siblings))))
       info))))

(defun select-child-code-block-form (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (current-path (code-block-selected-form-path block info))
         (selected-form (code-block-selected-form block info))
         (children (and selected-form
                        (code-form-child-items selected-form))))
    (when (and current-path children)
      (set-code-block-selected-form-path
       block
       (append current-path '(0))
       info))))

(defun select-parent-code-block-form (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (current-path (code-block-selected-form-path block info)))
    (when (> (length current-path) 1)
      (set-code-block-selected-form-path
       block
       (butlast current-path)
       info))))

(defun replace-code-block-top-level-forms (block forms &key selected-index selected-path)
  (replace-code-block-source block (serialize-common-lisp-forms forms))
  (set-code-block-selected-form-path
   block
   (or selected-path
       (and selected-index
            (list selected-index))
       '(0))
   (code-block-parse-info block))
  block)

(defun replace-sequence-item (sequence index value)
  (loop for item in sequence
        for current-index from 0
        collect (if (= current-index index)
                    value
                    item)))

(defun rewrite-form-at-path (form path rewriter)
  (multiple-value-bind (items tail)
      (form-child-items-and-tail form)
    (multiple-value-bind (new-items new-path)
        (rewrite-sequence-at-path items path rewriter)
      (values (rebuild-cons-form new-items tail)
              new-path))))

(defun rewrite-sequence-at-path (sequence path rewriter)
  (let ((selected-index (first path)))
    (if (null (rest path))
        (funcall rewriter sequence selected-index)
        (multiple-value-bind (new-child child-path)
            (rewrite-form-at-path (nth selected-index sequence)
                                  (rest path)
                                  rewriter)
          (values (replace-sequence-item sequence
                                         selected-index
                                         new-child)
                  (cons selected-index child-path))))))

(defun rewrite-code-block-selected-form (block rewriter &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (forms (code-block-top-level-forms block info))
         (selected-path (code-block-selected-form-path block info)))
    (when selected-path
      (multiple-value-bind (new-forms new-path)
          (rewrite-sequence-at-path forms selected-path rewriter)
        (replace-code-block-top-level-forms
         block
         new-forms
         :selected-path new-path)))))

(defun wrap-selected-code-block-form (block &optional info)
  (rewrite-code-block-selected-form
   block
   (lambda (sequence selected-index)
     (values (replace-sequence-item
              sequence
              selected-index
              (list 'progn (nth selected-index sequence)))
             (list selected-index)))
   info))

(defun delete-selected-code-block-form (block &optional info)
  (rewrite-code-block-selected-form
   block
   (lambda (sequence selected-index)
     (let ((remaining-forms
            (loop for form in sequence
                  for index from 0
                  unless (= index selected-index)
                  collect form)))
       (values remaining-forms
               (and remaining-forms
                    (list (min selected-index
                               (1- (length remaining-forms))))))))
   info))

(defun splicable-wrapper-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (member (first form) '(progn locally) :test #'eq)))

(defun splice-selected-code-block-form (block &optional info)
  (rewrite-code-block-selected-form
   block
   (lambda (sequence selected-index)
     (let ((selected-form (nth selected-index sequence)))
       (if (splicable-wrapper-form-p selected-form)
           (let* ((replacement-forms (rest selected-form))
                  (rewritten-forms
                   (append (subseq sequence 0 selected-index)
                           replacement-forms
                           (nthcdr (1+ selected-index) sequence))))
             (values rewritten-forms
                     (and rewritten-forms
                          (list (min selected-index
                                     (1- (length rewritten-forms)))))))
           (values sequence (list selected-index)))))
   info))

(defun code-block-parse-status-line (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (unsupported-language (getf info :unsupported-language))
         (error-string (getf info :error))
         (forms (getf info :forms))
         (offset (getf info :offset)))
    (cond
      (unsupported-language
       (format nil "Parse skipped for ~A" unsupported-language))
      (error-string
       (format nil "Parse error@~D: ~A"
               (or offset 0)
               error-string))
      (t
       (format nil "Parse OK  |  ~D top-level form~:P"
               (length forms))))))

(defun code-block-selection-status-line (block &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (unsupported-language (getf info :unsupported-language))
         (error-string (getf info :error))
         (forms (code-block-top-level-forms block info))
         (selected-path (code-block-selected-form-path block info))
         (selected-index (and selected-path (first selected-path)))
         (selected-form (code-block-selected-form block info)))
    (cond
      (unsupported-language
       (format nil "Selection unavailable for ~A" unsupported-language))
      (error-string
       "Selection unavailable until parse succeeds")
      ((null forms)
       "No forms to select")
      ((> (length selected-path) 1)
       (format nil "Selected path ~A  |  ~A"
               (selected-code-form-path-display-string selected-path)
               (simple-form-summary selected-form)))
      (t
       (format nil "Selected form ~D/~D  |  ~A"
               (1+ selected-index)
               (length forms)
               (simple-form-summary selected-form))))))

(defun simple-form-summary (form)
  (cond
    ((keywordp form)
     (format nil "keyword ~A" form))
    ((symbolp form)
     (format nil "symbol ~A" form))
    ((stringp form)
     (format nil "string ~S" (preview-string form :limit 24)))
    ((characterp form)
     (format nil "character ~S" form))
    ((numberp form)
     (format nil "number ~A" form))
    ((consp form)
     (multiple-value-bind (items count properp tail)
         (list-structure-info form)
       (declare (ignore items))
       (if properp
           (format nil "list (~D item~:P)" count)
           (format nil "pair (~D item~:P + tail ~A)"
                   count
                   (preview-string (printable-string tail) :limit 20)))))
    (t
     (format nil "~A ~A"
             (type-of form)
             (preview-string (printable-string form) :limit 24)))))

(defun list-structure-info (list &key (preview-limit 8))
  (let ((items nil)
        (count 0)
        (tail list))
    (loop while (consp tail)
          do (incf count)
          (when (< (length items) preview-limit)
            (push (car tail) items))
          (setf tail (cdr tail)))
    (values (nreverse items)
            count
            (null tail)
            tail)))

(defun form-child-selected-path (selected-path index)
  (cond
    ((eq selected-path :not-selected)
     :not-selected)
    ((and (consp selected-path)
          (= (first selected-path) index))
     (rest selected-path))
    (t
     :not-selected)))

(defun form-structure-lines (form &key label (depth 0) (max-depth 3) (max-items 6)
                                    (selected-path :not-selected))
  (let* ((indent (make-string (* depth 2) :initial-element #\Space))
         (summary (simple-form-summary form))
         (lines (list (format nil "~A~:[ ~;>~] ~@[~A: ~]~A"
                              indent
                              (null selected-path)
                              label
                              summary))))
    (when (and (consp form)
               (< depth max-depth))
      (multiple-value-bind (items count properp tail)
          (list-structure-info form :preview-limit max-items)
        (loop for item in items
              for index from 0
              do (setf lines
                       (append lines
                               (form-structure-lines
                                item
                                :label (format nil "[~D]" index)
                                :depth (1+ depth)
                                :max-depth max-depth
                                :max-items max-items
                                :selected-path
                                (form-child-selected-path
                                 selected-path
                                 index)))))
        (when (> count max-items)
          (setf lines
                (append lines
                        (list (format nil "~A  ... ~D more item~:P"
                                      indent
                                      (- count max-items))))))
        (when (not properp)
          (setf lines
                (append lines
                        (form-structure-lines
                         tail
                         :label "tail"
                         :depth (1+ depth)
                         :max-depth max-depth
                         :max-items max-items))))))
    lines))

(defun code-block-structure-lines (block &key (max-depth 3) (max-items 6) info)
  (let* ((info (or info (code-block-parse-info block)))
         (error-string (getf info :error))
         (forms (getf info :forms))
         (offset (getf info :offset))
         (selected-path (code-block-selected-form-path block info)))
    (cond
      ((getf info :unsupported-language)
       (list (format nil "No structural lens for ~A"
                     (getf info :unsupported-language))))
      (error-string
       (list (format nil "Parse error @~D" (or offset 0))
             error-string))
      ((null forms)
       (list "No forms"))
      (t
       (loop for form in forms
             for index from 1
             for zero-index from 0
             append (form-structure-lines
                     form
                     :label (format nil "form ~D" index)
                     :depth 0
                     :max-depth max-depth
                     :max-items max-items
                     :selected-path
                     (form-child-selected-path
                      selected-path
                      zero-index)))))))
