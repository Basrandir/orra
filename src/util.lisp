(in-package :orra)

(defvar *id-counter* 0)

(defun fresh-id (&optional (prefix "obj"))
  "Generate a readable process-local identifier."
  (format nil "~A-~8,'0X-~6,'0X"
          prefix
          (get-universal-time)
          (incf *id-counter*)))

(defun hash-table-alist (table)
  (let (pairs)
    (maphash (lambda (key value)
               (push (cons key value) pairs))
             table)
    (nreverse pairs)))

(defun alist-hash-table (alist &key (test #'equal))
  (let ((table (make-hash-table :test test)))
    (dolist (entry alist table)
      (setf (gethash (car entry) table) (cdr entry)))))

(defun registry-objects-list (registry)
  (let (objects)
    (maphash (lambda (id object)
               (declare (ignore id))
               (push object objects))
             (slot-value registry 'objects))
    (nreverse objects)))

(defun read-one-form (string)
  (with-standard-io-syntax
    (car (read-from-string string))))

(defun printable-string (object)
  (with-standard-io-syntax
    (prin1-to-string object)))

(defun preview-string (string &key (limit 48))
  (let ((string (string string)))
    (if (<= (length string) limit)
        string
        (format nil "~A..." (subseq string 0 (max 0 (- limit 3)))))))

(defun ensure-list (value)
  (if (listp value)
      value
      (list value)))

(defun split-lines (string)
  (let ((string (string string))
        (start 0)
        lines)
    (loop for newline = (position #\Newline string :start start)
          do (if newline
                 (progn
                   (push (subseq string start newline) lines)
                   (setf start (1+ newline)))
                 (return)))
    (push (subseq string start) lines)
    (nreverse lines)))
