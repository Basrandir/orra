(in-package :orra)

(defclass gap-buffer ()
  ((data
    :initarg :data
    :accessor gap-buffer-data)
   (gap-start
    :initarg :gap-start
    :accessor gap-buffer-gap-start
    :initform 0)
   (gap-end
    :initarg :gap-end
    :accessor gap-buffer-gap-end
    :initform 0)))

(defun gap-buffer-gap-size (buffer)
  (- (gap-buffer-gap-end buffer)
     (gap-buffer-gap-start buffer)))

(defun gap-buffer-length (buffer)
  (- (length (gap-buffer-data buffer))
     (gap-buffer-gap-size buffer)))

(defun make-gap-buffer (&key (content "") capacity)
  (let* ((content (string content))
         (content-length (length content))
         (gap-size (max 16
                        (- (or capacity 0) content-length)))
         (data (make-string (+ content-length gap-size)
                            :initial-element #\Null)))
    (replace data content :start1 0)
    (make-instance 'gap-buffer
                   :data data
                   :gap-start content-length
                   :gap-end (length data))))

(defun gap-buffer-content (buffer)
  (let* ((data (gap-buffer-data buffer))
         (gap-start (gap-buffer-gap-start buffer))
         (gap-end (gap-buffer-gap-end buffer))
         (content (make-string (gap-buffer-length buffer))))
    (replace content data :start1 0 :start2 0 :end2 gap-start)
    (replace content
             data
             :start1 gap-start
             :start2 gap-end
             :end2 (length data))
    content))

(defun reset-gap-buffer-content (buffer content)
  (let ((replacement (make-gap-buffer :content content)))
    (setf (gap-buffer-data buffer) (gap-buffer-data replacement))
    (setf (gap-buffer-gap-start buffer)
          (gap-buffer-gap-start replacement))
    (setf (gap-buffer-gap-end buffer)
          (gap-buffer-gap-end replacement)))
  buffer)

(defun gap-buffer-move-gap-to (buffer position)
  (let* ((position (max 0 (min (or position 0)
                               (gap-buffer-length buffer))))
         (data (gap-buffer-data buffer)))
    (loop while (> (gap-buffer-gap-start buffer) position)
          do (decf (gap-buffer-gap-start buffer))
          (decf (gap-buffer-gap-end buffer))
          (setf (aref data (gap-buffer-gap-end buffer))
                (aref data (gap-buffer-gap-start buffer))))
    (loop while (< (gap-buffer-gap-start buffer) position)
          do (setf (aref data (gap-buffer-gap-start buffer))
                   (aref data (gap-buffer-gap-end buffer)))
          (incf (gap-buffer-gap-start buffer))
          (incf (gap-buffer-gap-end buffer))))
  buffer)

(defun gap-buffer-ensure-gap (buffer required-size)
  (when (< (gap-buffer-gap-size buffer) required-size)
    (let* ((data (gap-buffer-data buffer))
           (old-length (length data))
           (content-length (gap-buffer-length buffer))
           (new-gap-size (max required-size
                              16
                              (gap-buffer-gap-size buffer)
                              (ceiling old-length 2)))
           (new-data (make-string (+ content-length new-gap-size)
                                  :initial-element #\Null))
           (prefix-length (gap-buffer-gap-start buffer))
           (suffix-length (- old-length (gap-buffer-gap-end buffer)))
           (new-gap-end (- (length new-data) suffix-length)))
      (replace new-data data :start1 0 :start2 0 :end2 prefix-length)
      (replace new-data
               data
               :start1 new-gap-end
               :start2 (gap-buffer-gap-end buffer)
               :end2 old-length)
      (setf (gap-buffer-data buffer) new-data)
      (setf (gap-buffer-gap-end buffer) new-gap-end)))
  buffer)

(defun gap-buffer-replace-range (buffer start end replacement)
  (let* ((replacement (string replacement))
         (range-start (max 0 (min start (gap-buffer-length buffer))))
         (range-end (max 0 (min end (gap-buffer-length buffer))))
         (start (min range-start range-end))
         (end (max range-start range-end)))
    (gap-buffer-move-gap-to buffer start)
    (incf (gap-buffer-gap-end buffer) (- end start))
    (gap-buffer-ensure-gap buffer (length replacement))
    (loop for character across replacement
          do (setf (aref (gap-buffer-data buffer)
                         (gap-buffer-gap-start buffer))
                   character)
          (incf (gap-buffer-gap-start buffer))))
  buffer)

(defclass text-buffer ()
  ((id
    :initarg :id
    :reader object-id)
   (content
    :initarg :content
    :accessor %text-buffer-gap
    :initform (make-gap-buffer))
   (cursor
    :initarg :cursor
    :accessor text-buffer-cursor
    :initform 0)
   (undo-stack
    :accessor text-buffer-undo-stack
    :initform nil)
   (redo-stack
    :accessor text-buffer-redo-stack
    :initform nil)
   (markers
    :accessor text-buffer-markers
    :initform nil)))

(defmethod initialize-instance :after ((buffer text-buffer) &key)
  (unless (typep (%text-buffer-gap buffer) 'gap-buffer)
    (setf (%text-buffer-gap buffer)
          (make-gap-buffer :content (%text-buffer-gap buffer)))))

(defgeneric text-buffer-content (buffer))

(defmethod text-buffer-content ((buffer text-buffer))
  (gap-buffer-content (%text-buffer-gap buffer)))

(defgeneric (setf text-buffer-content) (value buffer))

(defmethod (setf text-buffer-content) (value (buffer text-buffer))
  (reset-gap-buffer-content (%text-buffer-gap buffer) value)
  (string value))

(defun text-buffer-gap-size (buffer)
  (gap-buffer-gap-size (%text-buffer-gap buffer)))

(defclass text-marker ()
  ((position
    :initarg :position
    :accessor text-marker-position
    :initform 0)
   (gravity
    :initarg :gravity
    :accessor text-marker-gravity
    :initform :right)))

(defun valid-text-marker-gravity-p (gravity)
  (member gravity '(:left :right)))

(defun clamp-text-position (content position)
  (max 0
       (min (or position 0)
            (length content))))

(defmethod print-object ((buffer text-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity nil)
    (format stream "~A @~D"
            (object-id buffer)
            (text-buffer-cursor buffer))))

(defmethod print-object ((marker text-marker) stream)
  (print-unreadable-object (marker stream :type t :identity nil)
    (format stream "~D ~A"
            (text-marker-position marker)
            (text-marker-gravity marker))))

(defun %clamp-buffer-cursor (buffer)
  (setf (text-buffer-cursor buffer)
        (clamp-text-position (text-buffer-content buffer)
                             (text-buffer-cursor buffer)))
  buffer)

(defun %clamp-text-marker (buffer marker)
  (setf (text-marker-position marker)
        (clamp-text-position (text-buffer-content buffer)
                             (text-marker-position marker)))
  marker)

(defun %clamp-buffer-markers (buffer)
  (dolist (marker (text-buffer-markers buffer) buffer)
    (%clamp-text-marker buffer marker)))

(defun make-text-marker (buffer &key position (gravity :right))
  (unless (valid-text-marker-gravity-p gravity)
    (error "Invalid text marker gravity ~S." gravity))
  (let ((marker (make-instance 'text-marker
                               :position (clamp-text-position
                                          (text-buffer-content buffer)
                                          (or position
                                              (text-buffer-cursor buffer)))
                               :gravity gravity)))
    (push marker (text-buffer-markers buffer))
    marker))

(defun delete-text-marker (buffer marker)
  (setf (text-buffer-markers buffer)
        (remove marker (text-buffer-markers buffer)))
  marker)

(defun %buffer-marker-snapshot (buffer)
  (mapcar (lambda (marker)
            (list marker (text-marker-position marker)))
          (text-buffer-markers buffer)))

(defun %buffer-snapshot (buffer &key (include-markers t))
  (list (text-buffer-content buffer)
        (text-buffer-cursor buffer)
        (and include-markers
             (%buffer-marker-snapshot buffer))))

(defun %restore-buffer-marker-snapshot (buffer marker-snapshot)
  (dolist (entry marker-snapshot)
    (destructuring-bind (marker position) entry
      (when (find marker (text-buffer-markers buffer))
        (setf (text-marker-position marker) position))))
  (%clamp-buffer-markers buffer))

(defun %restore-buffer-snapshot (buffer snapshot)
  (let ((content (first snapshot))
        (cursor (second snapshot))
        (marker-snapshot (third snapshot)))
    (setf (text-buffer-content buffer) content)
    (setf (text-buffer-cursor buffer) cursor)
    (when marker-snapshot
      (%restore-buffer-marker-snapshot buffer marker-snapshot)))
  (%clamp-buffer-cursor buffer)
  (%clamp-buffer-markers buffer))

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

(defun adjusted-position-for-replacement (position start end replacement-length gravity)
  (let ((delta (- replacement-length (- end start))))
    (cond
      ((< position start)
       position)
      ((> position end)
       (+ position delta))
      ((= start end)
       (if (eq gravity :right)
           (+ start replacement-length)
           start))
      ((= position end)
       (+ start replacement-length))
      ((eq gravity :right)
       (+ start replacement-length))
      (t
       start))))

(defun adjust-buffer-markers-for-replacement (buffer start end replacement-length)
  (dolist (marker (text-buffer-markers buffer) buffer)
    (setf (text-marker-position marker)
          (adjusted-position-for-replacement
           (text-marker-position marker)
           start
           end
           replacement-length
           (text-marker-gravity marker))))
  (%clamp-buffer-markers buffer))

(defun replace-buffer-range (buffer start end replacement &key cursor)
  (let* ((content (text-buffer-content buffer))
         (range-start (clamp-text-position content start))
         (range-end (clamp-text-position content end))
         (start (min range-start range-end))
         (end (max range-start range-end))
         (replacement (string replacement)))
    (unless (and (= start end)
                 (zerop (length replacement)))
      (%record-buffer-change buffer)
      (gap-buffer-replace-range (%text-buffer-gap buffer)
                                start
                                end
                                replacement)
      (setf (text-buffer-cursor buffer)
            (if cursor
                cursor
                (adjusted-position-for-replacement
                 (text-buffer-cursor buffer)
                 start
                 end
                 (length replacement)
                 :right)))
      (adjust-buffer-markers-for-replacement buffer
                                             start
                                             end
                                             (length replacement))))
  (%clamp-buffer-cursor buffer))

(defun insert-buffer-text (buffer text)
  (let ((cursor (text-buffer-cursor buffer)))
    (replace-buffer-range buffer
                          cursor
                          cursor
                          text
                          :cursor (+ cursor (length (string text))))))

(defun delete-buffer-backward (buffer)
  (let ((cursor (text-buffer-cursor buffer)))
    (when (plusp cursor)
      (replace-buffer-range buffer
                            (1- cursor)
                            cursor
                            ""
                            :cursor (1- cursor))))
  buffer)

(defun delete-buffer-forward (buffer)
  (let* ((content (text-buffer-content buffer))
         (cursor (text-buffer-cursor buffer)))
    (when (< cursor (length content))
      (replace-buffer-range buffer
                            cursor
                            (1+ cursor)
                            ""
                            :cursor cursor)))
  buffer)

(defun move-buffer-cursor-left (buffer)
  (decf (text-buffer-cursor buffer))
  (%clamp-buffer-cursor buffer))

(defun move-buffer-cursor-right (buffer)
  (incf (text-buffer-cursor buffer))
  (%clamp-buffer-cursor buffer))

(defun replace-buffer-content (buffer content &key cursor)
  (let* ((content (string content))
         (new-cursor (if cursor
                         (clamp-text-position content cursor)
                         (length content))))
    (unless (and (string= content (text-buffer-content buffer))
                 (= new-cursor (text-buffer-cursor buffer)))
      (replace-buffer-range buffer
                            0
                            (length (text-buffer-content buffer))
                            content
                            :cursor new-cursor)))
  buffer)

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

(defun %buffer-state-snapshot (snapshot)
  (list (first snapshot)
        (second snapshot)))

(defun text-buffer-state (buffer)
  (list :content (text-buffer-content buffer)
        :cursor (text-buffer-cursor buffer)
        :undo-stack (mapcar (lambda (snapshot)
                              (%buffer-state-snapshot snapshot))
                            (text-buffer-undo-stack buffer))
        :redo-stack (mapcar (lambda (snapshot)
                              (%buffer-state-snapshot snapshot))
                            (text-buffer-redo-stack buffer))))

(defun make-text-buffer-from-state (state)
  (let ((buffer (make-text-buffer
                 :content (getf state :content "")
                 :cursor (getf state :cursor 0))))
    (setf (text-buffer-undo-stack buffer)
          (copy-tree (getf state :undo-stack)))
    (setf (text-buffer-redo-stack buffer)
          (copy-tree (getf state :redo-stack)))
    (%clamp-buffer-cursor buffer)))
