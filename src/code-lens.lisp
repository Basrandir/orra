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

(defun form-structure-lines (form &key label (depth 0) (max-depth 3) (max-items 6))
  (let* ((indent (make-string (* depth 2) :initial-element #\Space))
         (summary (simple-form-summary form))
         (lines (list (format nil "~A~@[~A: ~]~A"
                              indent
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
                                :max-items max-items))))
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
         (offset (getf info :offset)))
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
             append (form-structure-lines
                     form
                     :label (format nil "form ~D" index)
                     :depth 0
                     :max-depth max-depth
                     :max-items max-items))))))
