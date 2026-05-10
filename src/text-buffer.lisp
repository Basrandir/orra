(in-package :orra)

(defclass text-buffer ()
  ((id
    :initarg :id
    :reader object-id)
   (content
    :initarg :content
    :accessor text-buffer-content
    :initform "")
   (cursor
    :initarg :cursor
    :accessor text-buffer-cursor
    :initform 0)))

(defmethod print-object ((buffer text-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity nil)
    (format stream "~A @~D"
            (object-id buffer)
            (text-buffer-cursor buffer))))

(defun %clamp-buffer-cursor (buffer)
  (setf (text-buffer-cursor buffer)
        (max 0
             (min (text-buffer-cursor buffer)
                  (length (text-buffer-content buffer)))))
  buffer)

(defun make-text-buffer (&key (content "") cursor)
  (let ((buffer (make-instance 'text-buffer
                               :id (fresh-id "buffer")
                               :content content
                               :cursor (or cursor (length content)))))
    (%clamp-buffer-cursor buffer)))

(defun insert-buffer-text (buffer text)
  (let* ((content (text-buffer-content buffer))
         (cursor (text-buffer-cursor buffer))
         (text (string text)))
    (setf (text-buffer-content buffer)
          (concatenate 'string
                       (subseq content 0 cursor)
                       text
                       (subseq content cursor)))
    (incf (text-buffer-cursor buffer) (length text))
    (%clamp-buffer-cursor buffer)))

(defun delete-buffer-backward (buffer)
  (let ((cursor (text-buffer-cursor buffer)))
    (when (plusp cursor)
      (let ((content (text-buffer-content buffer)))
        (setf (text-buffer-content buffer)
              (concatenate 'string
                           (subseq content 0 (1- cursor))
                           (subseq content cursor)))
        (decf (text-buffer-cursor buffer)))))
  (%clamp-buffer-cursor buffer))

(defun delete-buffer-forward (buffer)
  (let* ((content (text-buffer-content buffer))
         (cursor (text-buffer-cursor buffer)))
    (when (< cursor (length content))
      (setf (text-buffer-content buffer)
            (concatenate 'string
                         (subseq content 0 cursor)
                         (subseq content (1+ cursor))))))
  (%clamp-buffer-cursor buffer))

(defun move-buffer-cursor-left (buffer)
  (decf (text-buffer-cursor buffer))
  (%clamp-buffer-cursor buffer))

(defun move-buffer-cursor-right (buffer)
  (incf (text-buffer-cursor buffer))
  (%clamp-buffer-cursor buffer))
