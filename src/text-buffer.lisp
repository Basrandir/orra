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
    :initform 0)
   (undo-stack
    :accessor text-buffer-undo-stack
    :initform nil)
   (redo-stack
    :accessor text-buffer-redo-stack
    :initform nil)))

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

(defun %buffer-snapshot (buffer)
  (list (text-buffer-content buffer)
        (text-buffer-cursor buffer)))

(defun %restore-buffer-snapshot (buffer snapshot)
  (destructuring-bind (content cursor) snapshot
    (setf (text-buffer-content buffer) content)
    (setf (text-buffer-cursor buffer) cursor))
  (%clamp-buffer-cursor buffer))

(defun %record-buffer-change (buffer)
  (push (%buffer-snapshot buffer) (text-buffer-undo-stack buffer))
  (setf (text-buffer-redo-stack buffer) nil)
  buffer)

(defun move-buffer-cursor-to (buffer position)
  (setf (text-buffer-cursor buffer) position)
  (%clamp-buffer-cursor buffer))

(defun text-buffer-line-count (buffer)
  (length (split-lines (text-buffer-content buffer))))

(defun %line-start-index-for-line (content requested-line)
  (let ((start 0)
        (line (max 0 requested-line)))
    (loop repeat line
          do
	  (let ((newline (position #\Newline content :start start)))
            (unless newline
              (return-from %line-start-index-for-line (length content)))
            (setf start (1+ newline))))
    start))

(defun %line-end-index-for-start (content start)
  (or (position #\Newline content :start start)
      (length content)))

(defun buffer-index-for-line-column (buffer line column)
  (let* ((content (text-buffer-content buffer))
         (target-line (min (max 0 line)
                           (max 0 (1- (text-buffer-line-count buffer)))))
         (start (%line-start-index-for-line content target-line))
         (end (%line-end-index-for-start content start)))
    (min end
         (+ start (max 0 column)))))

(defun buffer-cursor-line-column (buffer)
  (let* ((content (text-buffer-content buffer))
         (cursor (text-buffer-cursor buffer))
         (line-start (let ((newline (position #\Newline
                                              content
                                              :end cursor
                                              :from-end t)))
                       (if newline
                           (1+ newline)
                           0))))
    (values (count #\Newline content :end cursor)
            (- cursor line-start))))

(defun move-buffer-cursor-home (buffer)
  (multiple-value-bind (line column)
      (buffer-cursor-line-column buffer)
    (declare (ignore column))
    (move-buffer-cursor-to buffer
                           (buffer-index-for-line-column buffer line 0))))

(defun move-buffer-cursor-end (buffer)
  (multiple-value-bind (line column)
      (buffer-cursor-line-column buffer)
    (declare (ignore column))
    (let* ((content (text-buffer-content buffer))
           (start (%line-start-index-for-line content line))
           (end (%line-end-index-for-start content start)))
      (move-buffer-cursor-to buffer end))))

(defun move-buffer-cursor-up (buffer)
  (multiple-value-bind (line column)
      (buffer-cursor-line-column buffer)
    (move-buffer-cursor-to buffer
                           (buffer-index-for-line-column buffer
                                                         (max 0 (1- line))
                                                         column))))

(defun move-buffer-cursor-down (buffer)
  (multiple-value-bind (line column)
      (buffer-cursor-line-column buffer)
    (move-buffer-cursor-to buffer
                           (buffer-index-for-line-column buffer
                                                         (1+ line)
                                                         column))))

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
    (when (plusp (length text))
      (%record-buffer-change buffer)
      (setf (text-buffer-content buffer)
            (concatenate 'string
                         (subseq content 0 cursor)
                         text
                         (subseq content cursor)))
      (incf (text-buffer-cursor buffer) (length text)))
    (%clamp-buffer-cursor buffer)))

(defun delete-buffer-backward (buffer)
  (let ((cursor (text-buffer-cursor buffer)))
    (when (plusp cursor)
      (let ((content (text-buffer-content buffer)))
        (%record-buffer-change buffer)
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
      (%record-buffer-change buffer)
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

(defun replace-buffer-content (buffer content &key cursor)
  (let* ((content (string content))
         (new-cursor (if cursor
                         (max 0 (min cursor (length content)))
                         (length content))))
    (unless (and (string= content (text-buffer-content buffer))
                 (= new-cursor (text-buffer-cursor buffer)))
      (%record-buffer-change buffer)
      (setf (text-buffer-content buffer) content)
      (setf (text-buffer-cursor buffer) new-cursor)))
  (%clamp-buffer-cursor buffer))

(defun undo-buffer-edit (buffer)
  (when (text-buffer-undo-stack buffer)
    (push (%buffer-snapshot buffer) (text-buffer-redo-stack buffer))
    (%restore-buffer-snapshot buffer
                              (pop (text-buffer-undo-stack buffer))))
  buffer)

(defun redo-buffer-edit (buffer)
  (when (text-buffer-redo-stack buffer)
    (push (%buffer-snapshot buffer) (text-buffer-undo-stack buffer))
    (%restore-buffer-snapshot buffer
                              (pop (text-buffer-redo-stack buffer))))
  buffer)

(defun text-buffer-state (buffer)
  (list :content (text-buffer-content buffer)
        :cursor (text-buffer-cursor buffer)
        :undo-stack (copy-tree (text-buffer-undo-stack buffer))
        :redo-stack (copy-tree (text-buffer-redo-stack buffer))))

(defun make-text-buffer-from-state (state)
  (let ((buffer (make-text-buffer
                 :content (getf state :content "")
                 :cursor (getf state :cursor 0))))
    (setf (text-buffer-undo-stack buffer)
          (copy-tree (getf state :undo-stack)))
    (setf (text-buffer-redo-stack buffer)
          (copy-tree (getf state :redo-stack)))
    (%clamp-buffer-cursor buffer)))
