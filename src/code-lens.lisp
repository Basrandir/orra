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
  (parse-common-lisp-source-region source 0 (length source)))

(defun code-block-parse-cache-valid-p (block cache)
  (and cache
       (equal (getf cache :language) (code-block-language block))
       (string= (getf cache :source "") (code-block-source block))))

(defun cache-code-block-parse-info (block info)
  (setf (code-block-parse-cache block)
        (list :language (code-block-language block)
              :source (code-block-source block)
              :info info))
  info)

(defun code-block-cached-parse-info (block)
  (let ((cache (code-block-parse-cache block)))
    (and (code-block-parse-cache-valid-p block cache)
         (getf cache :info))))

(defun code-block-parse-info (block)
  (or (code-block-cached-parse-info block)
      (cache-code-block-parse-info
       block
       (if (code-block-common-lisp-p block)
           (parse-common-lisp-source (code-block-source block))
           (list :forms nil
                 :spans nil
                 :records nil
                 :error nil
                 :offset nil
                 :unsupported-language (code-block-language block))))))

(defun code-block-structure-visible-p (block)
  (object-property block :show-structure :default nil :inherit nil))

(defun toggle-code-block-structure (block)
  (set-object-property block
                       :show-structure
                       (not (code-block-structure-visible-p block)))
  block)

(defun code-block-top-level-forms (block &optional info)
  (getf (or info (code-block-parse-info block)) :forms))

(defun whitespace-character-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun source-comment-end (source start end)
  (or (position #\Newline source :start start :end end)
      end))

(defun source-block-comment-end (source start end)
  (let ((depth 1)
        (position (+ start 2)))
    (loop while (< position end)
          do (cond
               ((and (< (1+ position) end)
                     (char= (char source position) #\#)
                     (char= (char source (1+ position)) #\|))
                (incf depth)
                (incf position 2))
               ((and (< (1+ position) end)
                     (char= (char source position) #\|)
                     (char= (char source (1+ position)) #\#))
                (decf depth)
                (incf position 2)
                (when (zerop depth)
                  (return-from source-block-comment-end position)))
               (t
                (incf position))))
    end))

(defun skip-source-whitespace-and-comments (source start end)
  (let ((position start))
    (loop while (< position end)
          for character = (char source position)
          do (cond
               ((whitespace-character-p character)
                (incf position))
               ((char= character #\;)
                (setf position (source-comment-end source position end)))
               ((and (< (1+ position) end)
                     (char= character #\#)
                     (char= (char source (1+ position)) #\|))
                (setf position
                      (source-block-comment-end source position end)))
               (t
                (return position))))
    position))

(defun source-string-end (source start end)
  (let ((position (1+ start)))
    (loop while (< position end)
          for character = (char source position)
          do (cond
               ((char= character #\\)
                (incf position 2))
               ((char= character #\")
                (return-from source-string-end (1+ position)))
               (t
                (incf position))))
    end))

(defun source-symbol-escape-end (source start end)
  (let ((position (1+ start)))
    (loop while (< position end)
          for character = (char source position)
          do (cond
               ((char= character #\\)
                (incf position 2))
               ((char= character #\|)
                (return-from source-symbol-escape-end (1+ position)))
               (t
                (incf position))))
    end))

(defun source-atom-end (source start end)
  (let ((position start))
    (loop while (< position end)
          for character = (char source position)
          do (cond
               ((or (whitespace-character-p character)
                    (find character '(#\( #\) #\" #\;)))
                (return position))
               ((char= character #\|)
                (setf position (source-symbol-escape-end source position end)))
               ((char= character #\\)
                (incf position 2))
               (t
                (incf position))))
    position))

(defun source-prefix-length (source start end)
  (cond
    ((>= start end) 0)
    ((and (< (1+ start) end)
          (char= (char source start) #\#)
          (char= (char source (1+ start)) #\'))
     2)
    ((find (char source start) '(#\' #\`))
     1)
    ((char= (char source start) #\,)
     (if (and (< (1+ start) end)
              (char= (char source (1+ start)) #\@))
         2
         1))
    (t
     0)))

(defparameter +common-lisp-syntax-summary-order+
  '(:comment :string :keyword :number :symbol :paren :quote))

(defun make-source-syntax-token (kind source start end)
  (list :kind kind
        :start start
        :end end
        :text (subseq source start end)))

(defun common-lisp-number-token-p (text)
  (handler-case
      (let ((*read-eval* nil))
        (multiple-value-bind (value end)
            (read-from-string text nil :syntax-token-eof)
          (and (not (eq value :syntax-token-eof))
               (= end (length text))
               (numberp value))))
    (error ()
      nil)))

(defun common-lisp-atom-syntax-kind (text)
  (cond
    ((and (plusp (length text))
          (char= (char text 0) #\:))
     :keyword)
    ((common-lisp-number-token-p text)
     :number)
    (t
     :symbol)))

(defun common-lisp-source-syntax-tokens (source)
  (let ((position 0)
        (end (length source))
        (tokens nil))
    (labels ((emit (kind start token-end)
               (push (make-source-syntax-token kind source start token-end)
                     tokens)
               (setf position token-end)))
      (loop while (< position end)
            for character = (char source position)
            do (cond
                 ((whitespace-character-p character)
                  (incf position))
                 ((char= character #\;)
                  (emit :comment
                        position
                        (source-comment-end source position end)))
                 ((and (< (1+ position) end)
                       (char= character #\#)
                       (char= (char source (1+ position)) #\|))
                  (emit :comment
                        position
                        (source-block-comment-end source position end)))
                 ((find character '(#\( #\)))
                  (emit :paren position (1+ position)))
                 ((char= character #\")
                  (emit :string
                        position
                        (source-string-end source position end)))
                 ((plusp (source-prefix-length source position end))
                  (emit :quote
                        position
                        (+ position
                           (source-prefix-length source position end))))
                 (t
                  (let ((token-end (source-atom-end source position end)))
                    (if (> token-end position)
                        (emit (common-lisp-atom-syntax-kind
                               (subseq source position token-end))
                              position
                              token-end)
                        (incf position)))))))
    (nreverse tokens)))

(defun syntax-kind-label (kind)
  (string-downcase (symbol-name kind)))

(defun common-lisp-syntax-summary-line (tokens)
  (let ((parts nil))
    (dolist (kind +common-lisp-syntax-summary-order+)
      (let ((count (count kind
                          tokens
                          :key (lambda (token) (getf token :kind)))))
        (when (plusp count)
          (push (format nil "~A ~D"
                        (syntax-kind-label kind)
                        count)
                parts))))
    (if parts
        (format nil "Syntax  |  ~{~A~^  |  ~}" (nreverse parts))
        "Syntax  |  no tokens")))

(defun code-block-syntax-tokens (block)
  (if (code-block-common-lisp-p block)
      (common-lisp-source-syntax-tokens (code-block-source block))
      nil))

(defun code-block-syntax-summary-line (block &optional tokens)
  (if (code-block-common-lisp-p block)
      (common-lisp-syntax-summary-line
       (or tokens
           (code-block-syntax-tokens block)))
      (format nil "Syntax skipped for ~A" (code-block-language block))))

(defun source-form-span (source start end)
  (let ((position (skip-source-whitespace-and-comments source start end)))
    (when (< position end)
      (let* ((prefix-length (source-prefix-length source position end))
             (form-start position)
             (core-start (+ position prefix-length)))
        (when (< core-start end)
          (let ((character (char source core-start)))
            (cond
              ((char= character #\()
               (let ((list-end (source-list-end source core-start end)))
                 (and list-end
                      (list form-start list-end))))
              ((char= character #\")
               (list form-start
                     (source-string-end source core-start end)))
              (t
               (let ((atom-end (source-atom-end source core-start end)))
                 (and (< core-start atom-end)
                      (list form-start atom-end)))))))))))

(defun source-list-end (source start end)
  (let ((position (1+ start)))
    (loop
     (setf position (skip-source-whitespace-and-comments source position end))
     (when (>= position end)
       (return nil))
     (when (char= (char source position) #\))
       (return (1+ position)))
     (let ((child-span (source-form-span source position end)))
       (unless child-span
         (return nil))
       (setf position (second child-span))))))

(defun source-list-core-start (source span)
  (destructuring-bind (start end) span
    (let ((core-start (+ start (source-prefix-length source start end))))
      (and (< core-start end)
           (char= (char source core-start) #\()
           core-start))))

(defun source-child-form-spans (source span)
  (destructuring-bind (start end) span
    (declare (ignore start))
    (let ((list-start (source-list-core-start source span)))
      (when list-start
        (let ((position (1+ list-start))
              (child-spans nil))
          (loop
           (setf position
                 (skip-source-whitespace-and-comments
                  source
                  position
                  (1- end)))
           (when (>= position (1- end))
             (return (nreverse child-spans)))
           (let ((child-span (source-form-span source position (1- end))))
             (unless child-span
               (return (nreverse child-spans)))
             (push child-span child-spans)
             (setf position (second child-span)))))))))

(defun source-top-level-form-spans (source)
  (let ((position 0)
        (end (length source))
        (spans nil))
    (loop
     (setf position (skip-source-whitespace-and-comments source position end))
     (when (>= position end)
       (return (nreverse spans)))
     (let ((span (source-form-span source position end)))
       (unless span
         (return (nreverse spans)))
       (push span spans)
       (setf position (second span))))))

(defun make-common-lisp-source-form-record (source form span)
  (destructuring-bind (start end) span
    (list :form form
          :span (list start end)
          :start start
          :end end
          :text (subseq source start end))))

(defun parse-common-lisp-source-region (source start end)
  (let* ((source-length (length source))
         (start (clamp-text-position source start))
         (end (clamp-text-position source end))
         (region-start (min start end))
         (region-end (max start end))
         (region (subseq source region-start region-end))
         (offset region-start)
         (forms nil)
         (spans nil)
         (records nil))
    (handler-case
        (with-input-from-string (stream region)
          (loop for form = (progn
                             (setf offset
                                   (+ region-start
                                      (or (ignore-errors
                                            (file-position stream))
                                          0)))
                             (read stream nil :eof))
                until (eq form :eof)
                do (let* ((read-end (+ region-start
                                       (or (ignore-errors
                                             (file-position stream))
                                           (- region-end region-start))))
                          (span (or (source-form-span source
                                                      offset
                                                      region-end)
                                    (list offset
                                          (min source-length read-end))))
                          (record (make-common-lisp-source-form-record
                                   source
                                   form
                                   span)))
                     (push form forms)
                     (push (getf record :span) spans)
                     (push record records)))
          (list :forms (nreverse forms)
                :spans (nreverse spans)
                :records (nreverse records)
                :error nil
                :offset nil))
      (error (condition)
        (list :forms (nreverse forms)
              :spans (nreverse spans)
              :records (nreverse records)
              :error (princ-to-string condition)
              :offset offset)))))

(defun common-lisp-parse-info-records (source info)
  (or (copy-list (getf info :records))
      (loop for form in (getf info :forms)
            for span in (or (getf info :spans)
                            (source-top-level-form-spans source))
            collect (make-common-lisp-source-form-record
                     source
                     form
                     span))))

(defun source-ranges-intersect-p (first-start first-end second-start second-end)
  (and (< first-start second-end)
       (< second-start first-end)))

(defun source-span-touches-position-p (span position)
  (and (< (first span) position)
       (< position (second span))))

(defun source-span-dirty-for-edit-p (span start end)
  (if (= start end)
      (source-span-touches-position-p span start)
      (source-ranges-intersect-p (first span)
                                 (second span)
                                 start
                                 end)))

(defun source-record-dirty-for-edit-p (record start end)
  (source-span-dirty-for-edit-p (getf record :span) start end))

(defun source-dirty-span-range (spans fallback-start fallback-end)
  (if spans
      (list (reduce #'min spans :key #'first)
            (reduce #'max spans :key #'second))
      (list fallback-start fallback-end)))

(defun shift-common-lisp-source-form-record (source record delta)
  (make-common-lisp-source-form-record
   source
   (getf record :form)
   (list (+ (getf record :start) delta)
         (+ (getf record :end) delta))))

(defun rebase-common-lisp-source-form-record (source record)
  (make-common-lisp-source-form-record
   source
   (getf record :form)
   (getf record :span)))

(defun common-lisp-parse-info-from-records
    (records &key error offset incremental-p dirty-range reused-prefix-count
               reused-suffix-count dirty-form-count fallback-reason)
  (let ((info (list :forms (mapcar (lambda (record)
                                     (getf record :form))
                                   records)
                    :spans (mapcar (lambda (record)
                                     (getf record :span))
                                   records)
                    :records records
                    :error error
                    :offset offset
                    :incremental-p incremental-p
                    :dirty-range dirty-range
                    :reused-prefix-count (or reused-prefix-count 0)
                    :reused-suffix-count (or reused-suffix-count 0)
                    :dirty-form-count (or dirty-form-count 0))))
    (when fallback-reason
      (setf (getf info :fallback-reason) fallback-reason))
    info))

(defun mark-common-lisp-parse-info-fallback (info reason)
  (setf (getf info :incremental-p) nil)
  (setf (getf info :fallback-reason) reason)
  (setf (getf info :reused-prefix-count) 0)
  (setf (getf info :reused-suffix-count) 0)
  (setf (getf info :dirty-form-count)
        (length (getf info :forms)))
  info)

(defun common-lisp-incremental-parse-info
    (old-source new-source edit-start edit-end
     &key replacement previous-info)
  (let* ((old-source (string old-source))
         (new-source (string new-source))
         (previous-info (or previous-info
                            (parse-common-lisp-source old-source)))
         (old-length (length old-source))
         (start (clamp-text-position old-source edit-start))
         (end (clamp-text-position old-source edit-end))
         (edit-start (min start end))
         (edit-end (max start end))
         (replacement-length (if replacement
                                 (length (string replacement))
                                 (- (length new-source)
                                    (- old-length (- edit-end edit-start)))))
         (delta (- replacement-length (- edit-end edit-start))))
    (if (getf previous-info :error)
        (mark-common-lisp-parse-info-fallback
         (parse-common-lisp-source new-source)
         :previous-error)
        (let* ((old-records
                (common-lisp-parse-info-records old-source previous-info))
               (dirty-old-records
                (remove-if-not
                 (lambda (record)
                   (source-record-dirty-for-edit-p record
                                                   edit-start
                                                   edit-end))
                 old-records))
               (old-dirty-range
                (source-dirty-span-range
                 (mapcar (lambda (record)
                           (getf record :span))
                         dirty-old-records)
                 edit-start
                 edit-end))
               (old-dirty-start (first old-dirty-range))
               (old-dirty-end (second old-dirty-range))
               (new-dirty-start old-dirty-start)
               (new-dirty-end (max new-dirty-start
                                   (+ old-dirty-end delta)))
               (new-top-level-spans (source-top-level-form-spans new-source))
               (dirty-new-spans
                (remove-if-not
                 (lambda (span)
                   (source-ranges-intersect-p (first span)
                                              (second span)
                                              new-dirty-start
                                              new-dirty-end))
                 new-top-level-spans))
               (new-dirty-range
                (source-dirty-span-range dirty-new-spans
                                         new-dirty-start
                                         new-dirty-end))
               (prefix-records
                (loop for record in old-records
                      while (<= (getf record :end) old-dirty-start)
                      collect (rebase-common-lisp-source-form-record
                               new-source
                               record)))
               (suffix-records
                (loop for record in old-records
                      when (>= (getf record :start) old-dirty-end)
                      collect (shift-common-lisp-source-form-record
                               new-source
                               record
                               delta)))
               (dirty-info
                (if (or dirty-new-spans
                        dirty-old-records
                        (plusp replacement-length))
                    (parse-common-lisp-source-region new-source
                                                     (first new-dirty-range)
                                                     (second new-dirty-range))
                    (common-lisp-parse-info-from-records nil)))
               (dirty-records (getf dirty-info :records)))
          (if (getf dirty-info :error)
              (common-lisp-parse-info-from-records
               prefix-records
               :error (getf dirty-info :error)
               :offset (getf dirty-info :offset)
               :incremental-p t
               :dirty-range new-dirty-range
               :reused-prefix-count (length prefix-records))
              (common-lisp-parse-info-from-records
               (append prefix-records
                       dirty-records
                       suffix-records)
               :incremental-p t
               :dirty-range new-dirty-range
               :reused-prefix-count (length prefix-records)
               :reused-suffix-count (length suffix-records)
               :dirty-form-count (length dirty-records)))))))

(defun code-block-incremental-parse-info
    (block new-source edit-start edit-end &key replacement previous-info)
  (if (code-block-common-lisp-p block)
      (common-lisp-incremental-parse-info
       (code-block-source block)
       new-source
       edit-start
       edit-end
       :replacement replacement
       :previous-info previous-info)
      (list :forms nil
            :spans nil
            :records nil
            :error nil
            :offset nil
            :unsupported-language (code-block-language block))))

(defun replace-code-block-source-incrementally
    (block new-source edit-start edit-end &key replacement previous-info)
  (let ((new-source (string new-source)))
    (if (string= (code-block-source block) new-source)
        block
        (let ((info (code-block-incremental-parse-info
                     block
                     new-source
                     edit-start
                     edit-end
                     :replacement replacement
                     :previous-info (or previous-info
                                        (code-block-parse-info block)))))
          (replace-code-block-source block new-source)
          (cache-code-block-parse-info block info)
          block))))

(defun source-span-for-path (source path)
  (labels ((walk (spans indices)
             (let ((span (nth (first indices) spans)))
               (when span
                 (if (null (rest indices))
                     span
                     (walk (source-child-form-spans source span)
                           (rest indices)))))))
    (and path
         (walk (source-top-level-form-spans source) path))))

(defun source-path-at-offset (source offset)
  (let ((bounded-offset (max 0 (min offset (length source)))))
    (labels ((find-containing-index (spans)
               (position-if (lambda (span)
                              (destructuring-bind (start end) span
                                (and (<= start bounded-offset)
                                     (<= bounded-offset end))))
                            spans))
             (walk (spans prefix)
               (let ((index (find-containing-index spans)))
                 (when index
                   (let* ((path (append prefix (list index)))
                          (span (nth index spans))
                          (children (source-child-form-spans source span))
                          (child-path (walk children path)))
                     (or child-path path))))))
      (walk (source-top-level-form-spans source) nil))))

(defun make-source-map-entry (source path span form)
  (destructuring-bind (start end) span
    (list :path path
          :start start
          :end end
          :text (subseq source start end)
          :form form
          :summary (simple-form-summary form))))

(defun source-map-entries-for-form (source path span form)
  (let ((entry (make-source-map-entry source path span form)))
    (if (consp form)
        (append
         (list entry)
         (loop for child in (code-form-child-items form)
               for child-span in (source-child-form-spans source span)
               for index from 0
               append (source-map-entries-for-form
                       source
                       (append path (list index))
                       child-span
                       child)))
        (list entry))))

(defun common-lisp-source-map (source &optional info)
  (let ((info (or info (parse-common-lisp-source source))))
    (unless (getf info :error)
      (loop for record in (common-lisp-parse-info-records source info)
            for index from 0
            append (source-map-entries-for-form
                    source
                    (list index)
                    (getf record :span)
                    (getf record :form))))))

(defun code-block-source-map (block &optional info)
  (let ((info (or info (code-block-parse-info block))))
    (when (and (code-block-common-lisp-p block)
               (not (getf info :error))
               (not (getf info :unsupported-language)))
      (common-lisp-source-map (code-block-source block) info))))

(defun source-map-entry-contains-offset-p (entry offset)
  (and (<= (getf entry :start) offset)
       (<= offset (getf entry :end))))

(defun source-map-entry-more-specific-p (entry other-entry)
  (or (null other-entry)
      (> (length (getf entry :path))
         (length (getf other-entry :path)))
      (and (= (length (getf entry :path))
              (length (getf other-entry :path)))
           (< (- (getf entry :end)
                 (getf entry :start))
              (- (getf other-entry :end)
                 (getf other-entry :start))))))

(defun source-map-entry-at-offset (source-map offset)
  (let ((bounded-offset (max 0 (or offset 0)))
        (best-entry nil))
    (dolist (entry source-map best-entry)
      (when (and (source-map-entry-contains-offset-p entry bounded-offset)
                 (source-map-entry-more-specific-p entry best-entry))
        (setf best-entry entry)))))

(defun code-block-source-map-entry-at-offset (block offset &optional info)
  (source-map-entry-at-offset (code-block-source-map block info) offset))

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

(defun code-block-selected-form-span (block &optional info)
  (let ((info (or info (code-block-parse-info block))))
    (when (and (not (getf info :error))
               (not (getf info :unsupported-language)))
      (source-span-for-path
       (code-block-source block)
       (code-block-selected-form-path block info)))))

(defun code-block-selected-form-start-offset (block &optional info)
  (let ((span (code-block-selected-form-span block info)))
    (and span
         (first span))))

(defun select-code-block-form-at-source-offset (block offset &optional info)
  (let ((info (or info (code-block-parse-info block))))
    (when (and (not (getf info :error))
               (not (getf info :unsupported-language)))
      (let ((path (source-path-at-offset (code-block-source block) offset)))
        (when path
          (set-code-block-selected-form-path block path info))))))

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

(defun replace-selected-code-block-form-source
    (block replacement-source &optional info)
  (let* ((info (or info (code-block-parse-info block)))
         (selected-path (code-block-selected-form-path block info))
         (span (and selected-path
                    (code-block-selected-form-span block info))))
    (when span
      (destructuring-bind (start end) span
        (let* ((source (code-block-source block))
               (replacement-source (string replacement-source))
               (new-source (concatenate 'string
                                        (subseq source 0 start)
                                        replacement-source
                                        (subseq source end))))
          (replace-code-block-source-incrementally
           block
           new-source
           start
           end
           :replacement replacement-source
           :previous-info info)
          (set-code-block-selected-form-path
           block
           selected-path
           (code-block-parse-info block)))))
    block))

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
