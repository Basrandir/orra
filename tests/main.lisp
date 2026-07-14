(in-package :orra.tests)

(defvar *tests* nil)

(defmacro deftest (name () &body body)
  `(progn
     (defun ,name ()
       ,@body)
     (pushnew ',name *tests*)
     ',name))

(defun is (condition &optional (message "Assertion failed."))
  (unless condition
    (error "~A" message)))

(defun collect-text-cells (cell)
  (let (texts)
    (labels ((visit (node)
               (when (typep node 'text-cell)
                 (push (orra::cell-text node) texts))
               (dolist (child (children-of node))
                 (visit child))))
      (visit cell))
    (nreverse texts)))

(defun find-text-cell-for-model (cell model-id)
  (labels ((visit (node)
             (or (when (and (typep node 'text-cell)
                            (cell-model node)
                            (string= (object-id (cell-model node))
                                     model-id)
                            (not (eq (orra::cell-role node) :heading)))
                   node)
                 (dolist (child (children-of node) nil)
                   (let ((found (visit child)))
                     (when found
                       (return found)))))))
    (visit cell)))

(defun find-styled-text-operation (backend text)
  (find-if (lambda (operation)
             (and (eq (first operation) :styled-text)
                  (string= (fourth operation) text)))
           (orra::backend-frame-operations backend)))

(defun find-frame-operation (backend kind &optional predicate)
  (find-if (lambda (operation)
             (and (eq (first operation) kind)
                  (or (null predicate)
                      (funcall predicate operation))))
           (orra::backend-frame-operations backend)))

(defun find-first-node-of-type (root type)
  (labels ((visit (node)
             (or (when (typep node type)
                   node)
                 (dolist (child (typecase node
                                  (workspace (children-of node))
                                  (orra::composite-node (children-of node))
                                  (t nil))
                          nil)
                   (let ((found (visit child)))
                     (when found
                       (return found)))))))
    (visit root)))

(defun read-persisted-payload (path)
  (with-open-file (stream path :direction :input)
    (with-standard-io-syntax
      (read stream))))

(defun make-test-directory (prefix)
  (let ((directory (merge-pathnames
                    (format nil "~A-~A/" prefix (fresh-id "tmp"))
                    (uiop:temporary-directory))))
    (ensure-directories-exist directory)
    directory))

(defun delete-test-directory (directory)
  (when (probe-file directory)
    (uiop:delete-directory-tree directory
                                :validate t
                                :if-does-not-exist :ignore)))

(defun workspace-has-paragraph-text-p (workspace text)
  (let ((notebook (root-notebook workspace)))
    (and notebook
         (some (lambda (section)
                 (some (lambda (object)
                         (and (typep object 'paragraph)
                              (string= text (paragraph-text object))))
                       (children-of section)))
               (children-of notebook)))))

(deftest text-buffer-editing ()
  (let ((buffer (make-text-buffer :content "abc" :cursor 1)))
    (insert-buffer-text buffer "Z")
    (is (string= "aZbc" (text-buffer-content buffer))
        "Insert should splice text at cursor.")
    (move-buffer-cursor-left buffer)
    (delete-buffer-forward buffer)
    (is (string= "abc" (text-buffer-content buffer))
        "Delete forward should remove the character at cursor.")
    (move-buffer-cursor-right buffer)
    (delete-buffer-backward buffer)
    (is (string= "ac" (text-buffer-content buffer))
        "Delete backward should remove the preceding character.")
    (is (= 1 (text-buffer-cursor buffer))
        "Cursor should stay within bounds.")))

(deftest text-buffer-uses-gap-backing ()
  (let ((buffer (make-text-buffer :content "abc" :cursor 1)))
    (is (typep (orra::%text-buffer-gap buffer) 'orra::gap-buffer)
        "Text buffers should use an internal gap-buffer backing store.")
    (let ((initial-gap-size (text-buffer-gap-size buffer)))
      (insert-buffer-text buffer "XYZ")
      (is (string= "aXYZbc" (text-buffer-content buffer))
          "Gap-backed insertion should preserve visible content.")
      (is (< (text-buffer-gap-size buffer) initial-gap-size)
          "Inserting into the buffer should consume available gap space.")
      (replace-buffer-range buffer 1 4 "")
      (is (string= "abc" (text-buffer-content buffer))
          "Gap-backed range deletion should preserve visible content.")
      (is (> (text-buffer-gap-size buffer) 0)
          "Deleting text should leave reusable gap capacity."))))

(deftest text-buffer-multiline-navigation-and-history ()
  (let ((buffer (make-text-buffer
                 :content (format nil "alpha~%beta~%gamma")
                 :cursor 8)))
    (multiple-value-bind (line column)
        (buffer-cursor-line-column buffer)
      (is (= 1 line)
          "Cursor should report the expected line.")
      (is (= 2 column)
          "Cursor should report the expected column."))
    (move-buffer-cursor-home buffer)
    (is (= 6 (text-buffer-cursor buffer))
        "Home should move to the start of the current line.")
    (move-buffer-cursor-end buffer)
    (is (= 10 (text-buffer-cursor buffer))
        "End should move to the end of the current line.")
    (move-buffer-cursor-down buffer)
    (multiple-value-bind (line column)
        (buffer-cursor-line-column buffer)
      (is (= 2 line)
          "Down should move to the next line.")
      (is (= 4 column)
          "Down should preserve the current column when possible."))
    (move-buffer-cursor-up buffer)
    (multiple-value-bind (line column)
        (buffer-cursor-line-column buffer)
      (is (= 1 line)
          "Up should move back to the previous line.")
      (is (= 4 column)
          "Up should preserve the current column when possible."))
    (insert-buffer-text buffer "!")
    (is (string= (format nil "alpha~%beta!~%gamma")
                 (text-buffer-content buffer))
        "Insert should update multiline buffers.")
    (undo-buffer-edit buffer)
    (is (string= (format nil "alpha~%beta~%gamma")
                 (text-buffer-content buffer))
        "Undo should restore the previous snapshot.")
    (redo-buffer-edit buffer)
    (is (string= (format nil "alpha~%beta!~%gamma")
                 (text-buffer-content buffer))
        "Redo should reapply the undone snapshot.")))

(deftest text-buffer-selection-replaces-selected-range ()
  (let ((buffer (make-text-buffer :content "abcd" :cursor 1)))
    (move-buffer-cursor-right buffer :extend-selection t)
    (move-buffer-cursor-right buffer :extend-selection t)
    (is (string= "bc" (text-buffer-selected-text buffer))
        "Shift movement should extend a text selection from the original cursor.")
    (insert-buffer-text buffer "Z")
    (is (string= "aZd" (text-buffer-content buffer))
        "Typing should replace the selected range.")
    (is (= 2 (text-buffer-cursor buffer))
        "Replacement should place the cursor after the inserted text.")
    (is (null (text-buffer-selected-text buffer))
        "Editing should clear the replaced selection.")
    (undo-buffer-edit buffer)
    (is (string= "abcd" (text-buffer-content buffer))
        "Undo should restore content replaced through a selection.")
    (is (string= "bc" (text-buffer-selected-text buffer))
        "Undo should restore the selection that preceded an edit.")
    (move-buffer-cursor-left buffer)
    (is (null (text-buffer-selected-text buffer))
        "Ordinary cursor movement should clear the selection.")))

(deftest text-buffer-delete-removes-selected-range ()
  (let ((buffer (make-text-buffer :content "abcd" :cursor 1)))
    (move-buffer-cursor-right buffer :extend-selection t)
    (move-buffer-cursor-right buffer :extend-selection t)
    (delete-buffer-backward buffer)
    (is (string= "ad" (text-buffer-content buffer))
        "Backward deletion should remove the selected range.")
    (undo-buffer-edit buffer)
    (delete-buffer-forward buffer)
    (is (string= "ad" (text-buffer-content buffer))
        "Forward deletion should remove the selected range.")))

(deftest text-buffer-markers-track-edits ()
  (let* ((buffer (make-text-buffer :content "abcdef" :cursor 2))
         (left-marker (make-text-marker buffer :position 2 :gravity :left))
         (right-marker (make-text-marker buffer :position 2 :gravity :right))
         (tail-marker (make-text-marker buffer :position 5 :gravity :right)))
    (insert-buffer-text buffer "XX")
    (is (string= "abXXcdef" (text-buffer-content buffer))
        "Insertion should update buffer content.")
    (is (= 2 (text-marker-position left-marker))
        "Left-gravity markers should stay before same-position insertions.")
    (is (= 4 (text-marker-position right-marker))
        "Right-gravity markers should move after same-position insertions.")
    (is (= 7 (text-marker-position tail-marker))
        "Markers after an insertion should shift right.")
    (replace-buffer-range buffer 1 5 "")
    (is (string= "adef" (text-buffer-content buffer))
        "Range replacement should update buffer content.")
    (is (= 1 (text-marker-position left-marker))
        "Markers inside a deleted range should collapse to the replacement start.")
    (is (= 1 (text-marker-position right-marker))
        "Right-gravity markers should collapse to the replacement end.")
    (is (= 3 (text-marker-position tail-marker))
        "Markers after a deleted range should shift left.")))

(deftest text-buffer-style-spans-track-edits ()
  (let ((buffer (make-text-buffer :content "abcdef" :cursor 2)))
    (add-text-buffer-style-span buffer 2 5 :keyword)
    (insert-buffer-text buffer "XX")
    (let ((span (first (text-buffer-style-spans buffer))))
      (is (eq :keyword (getf span :kind))
          "Style spans should retain their style kind.")
      (is (equal '(2 7)
                 (list (getf span :start)
                       (getf span :end)))
          "Style spans should expand around same-position insertions."))
    (replace-buffer-range buffer 3 6 "")
    (let ((span (first (text-buffer-style-spans buffer))))
      (is (equal '(2 4)
                 (list (getf span :start)
                       (getf span :end)))
          "Style spans should shrink around deleted ranges."))))

(deftest text-buffer-style-spans-follow-undo-redo ()
  (let ((buffer (make-text-buffer :content "abcd" :cursor 2)))
    (add-text-buffer-style-span buffer 1 3 :symbol)
    (insert-buffer-text buffer "XX")
    (is (equal '(1 5)
               (list (getf (first (text-buffer-style-spans buffer)) :start)
                     (getf (first (text-buffer-style-spans buffer)) :end)))
        "Style spans should update with ordinary edits.")
    (undo-buffer-edit buffer)
    (is (equal '(1 3)
               (list (getf (first (text-buffer-style-spans buffer)) :start)
                     (getf (first (text-buffer-style-spans buffer)) :end)))
        "Undo should restore prior style span ranges.")
    (redo-buffer-edit buffer)
    (is (equal '(1 5)
               (list (getf (first (text-buffer-style-spans buffer)) :start)
                     (getf (first (text-buffer-style-spans buffer)) :end)))
        "Redo should restore edited style span ranges.")))

(deftest text-buffer-style-spans-for-range-are-relative ()
  (let ((buffer (make-text-buffer :content "abcdef")))
    (add-text-buffer-style-span buffer 2 5 :number)
    (let ((span (first (text-buffer-style-spans-for-range buffer 3 6))))
      (is (eq :number (getf span :kind))
          "Range-filtered style spans should retain their style kind.")
      (is (equal '(0 2)
                 (list (getf span :start)
                       (getf span :end)))
          "Range-filtered style spans should be clipped relative to the requested range."))))

(deftest text-buffer-markers-follow-undo-redo ()
  (let* ((buffer (make-text-buffer :content "abcd" :cursor 2))
         (marker (make-text-marker buffer :position 2 :gravity :right)))
    (insert-buffer-text buffer "XX")
    (is (= 4 (text-marker-position marker))
        "Markers should move with ordinary edits.")
    (undo-buffer-edit buffer)
    (is (string= "abcd" (text-buffer-content buffer))
        "Undo should restore prior content.")
    (is (= 2 (text-marker-position marker))
        "Undo should restore prior marker positions.")
    (redo-buffer-edit buffer)
    (is (string= "abXXcd" (text-buffer-content buffer))
        "Redo should restore edited content.")
    (is (= 4 (text-marker-position marker))
        "Redo should restore edited marker positions.")))

(deftest text-buffer-state-omits-live-marker-objects ()
  (let* ((buffer (make-text-buffer :content "abcd" :cursor 2))
         (marker (make-text-marker buffer :position 2 :gravity :right)))
    (declare (ignore marker))
    (insert-buffer-text buffer "XX")
    (let ((snapshot (first (getf (orra::text-buffer-state buffer)
                                 :undo-stack))))
      (is (null (third snapshot))
          "Serialized editor state should not retain live marker objects."))))

(deftest text-buffer-state-retains-selection-anchor ()
  (let ((buffer (make-text-buffer :content "abcd" :cursor 1)))
    (move-buffer-cursor-right buffer :extend-selection t)
    (let ((restored (orra::make-text-buffer-from-state
                     (orra::text-buffer-state buffer))))
      (is (string= "b" (text-buffer-selected-text restored))
          "Saved editor state should preserve its active selection."))))

(deftest printable-key-text-from-sym ()
  (is (string= "a" (orra::printable-key-text-from-sym 97 nil nil))
      "Lowercase letters should round-trip.")
  (is (string= "A" (orra::printable-key-text-from-sym 97 t nil))
      "Shift should uppercase letters.")
  (is (string= "A" (orra::printable-key-text-from-sym 97 nil t))
      "Caps lock should uppercase letters.")
  (is (string= "!" (orra::printable-key-text-from-sym 49 t nil))
      "Shifted digits should map to punctuation.")
  (is (string= "/" (orra::printable-key-text-from-sym 47 nil nil))
      "Printable punctuation should round-trip."))

(deftest application-keymap-dispatches-focus-navigation ()
  (let* ((application (make-application :backend (make-null-backend)))
         (initial-id (object-id (focused-model application))))
    (dispatch-application-key application
                              (make-key-event :key :down))
    (is (not (string= initial-id
                      (object-id (focused-model application))))
        "Focus navigation should be dispatched through the application keymap.")))

(deftest application-keymap-dispatches-edit-selection ()
  (let ((application (make-application :backend (make-null-backend))))
    (begin-editing-focused-model application)
    (dispatch-application-key application
                              (make-key-event :key :home))
    (dispatch-application-key application
                              (make-key-event :key :right :shiftp t))
    (dispatch-application-key application
                              (make-key-event :key :right :shiftp t))
    (is (string= "Th"
                 (text-buffer-selected-text
                  (orra::application-active-text-buffer application)))
        "Selection movement should be driven by keymap dispatch.")))

(deftest application-keymap-can-be-overridden-per-application ()
  (let* ((application (make-application :backend (make-null-backend)))
         (initial-id (object-id (focused-model application))))
    (setf (gethash (orra::key-binding-descriptor :focus :j nil nil)
                   (application-keymap application))
          (make-instance 'orra::key-binding
                         :context :focus
                         :key :j
                         :documentation "Test override."
                         :function (lambda (application event)
                                     (declare (ignore event))
                                     (setf (application-viewport-y application)
                                           3)
                                     t)))
    (dispatch-application-key application
                              (make-key-event :key :j))
    (is (= 3 (application-viewport-y application))
        "Application keymaps should be overridable as runtime data.")
    (is (string= initial-id
                 (object-id (focused-model application)))
        "Overriding a binding should replace the installed behavior.")))

(deftest debug-panel-renders-event-trace ()
  (let ((application (make-application :backend (make-null-backend))))
    (dispatch-application-key application
                              (make-key-event :key :f12))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Debug" texts :test #'string=)
          "Toggling the debug panel should render a debug cell.")
      (is (find "Event Trace" texts :test #'string=)
          "The debug panel should include an event trace.")
      (is (find-if (lambda (text)
                     (search "KEY: FOCUS F12 -> Toggle debug panel."
                             text))
                   texts)
          "The event trace should include dispatched key bindings."))))

(deftest debug-panel-renders-cell-tree-and-focused-bounds ()
  (let ((application (make-application :backend (make-null-backend))))
    (dispatch-application-key application
                              (make-key-event :key :f12))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application)))
          (focused-id (object-id (focused-model application))))
      (is (find "Cell Tree" texts :test #'string=)
          "The debug panel should include a cell tree section.")
      (is (find-if (lambda (text)
                     (and (search "focused-cell-bounds:" text)
                          (search focused-id text)))
                   texts)
          "The debug panel should include bounds for the focused model cell.")
      (is (find-if (lambda (text)
                     (and (search "PARAGRAPH" text)
                          (search focused-id text)))
                   texts)
          "The cell tree should include model-backed cells."))))

(deftest application-dirty-state-tracks-render-needed ()
  (is (fboundp 'application-dirty-p)
      "Application should expose dirty state.")
  (let* ((stream (make-string-output-stream))
         (backend (make-null-backend :stream stream))
         (application (make-application :backend backend)))
    (is (application-dirty-p application)
        "New applications should start dirty because they have not rendered.")
    (is (find :full (application-damage-regions application))
        "New applications should request an initial full-frame damage region.")
    (render-application application)
    (is (not (application-dirty-p application))
        "Rendering should clear dirty state.")
    (is (null (application-damage-regions application))
        "Rendering should clear accumulated damage regions.")
    (let ((output-before (get-output-stream-string stream)))
      (is (null (render-application-if-needed application))
          "Clean applications should skip scheduled rendering.")
      (is (string= "" (get-output-stream-string stream))
          "Skipping a clean render should not present another frame.")
      (is (plusp (length output-before))
          "The initial render should have presented a frame."))
    (dispatch-application-key application
                              (make-key-event :key :down))
    (is (application-dirty-p application)
        "Handled input should mark the application dirty.")
    (is (find :full (application-damage-regions application))
        "Handled input should request a full-frame damage region until partial damage is available.")
    (is (eq application (render-application-if-needed application))
        "Dirty applications should render through the scheduled render path.")
    (is (not (application-dirty-p application))
        "Scheduled rendering should clear dirty state.")))

(deftest command-invocation-records-debug-event ()
  (let ((application (make-application :backend (make-null-backend))))
    (invoke-command application 'append-paragraph "event log marker")
    (let ((entry (first (application-event-log application))))
      (is (eq :command (getf entry :kind))
          "Command invocation should record a command event.")
      (is (string= "APPEND-PARAGRAPH" (getf entry :message))
          "Command events should include the command name."))
    (invoke-command application 'clear-event-log)
    (is (null (application-event-log application))
        "The event log should be clearable from a command.")))

(deftest common-lisp-source-parse-info ()
  (let ((info (parse-common-lisp-source
               (format nil "(list :hello :orra)~%(+ 20 22)"))))
    (is (null (getf info :error))
        "Valid Common Lisp source should parse without an error.")
    (is (= 2 (length (getf info :forms)))
        "Expected two top-level forms in the parse result.")))

(deftest common-lisp-source-parse-error-info ()
  (let ((info (parse-common-lisp-source "(list :hello :orra")))
    (is (getf info :error)
        "Malformed Common Lisp source should report a parse error.")
    (is (integerp (or (getf info :offset) 0))
        "Parse errors should report an offset when available.")))

(deftest common-lisp-source-parse-info-records-spans ()
  (let* ((source (format nil "(list :hello)~%(+ 1 2)"))
         (info (parse-common-lisp-source source))
         (records (getf info :records)))
    (is (equal '((0 13) (14 21))
               (mapcar (lambda (record)
                         (getf record :span))
                       records))
        "Parse records should keep absolute source spans for top-level forms.")
    (is (string= "(+ 1 2)" (getf (second records) :text))
        "Parse records should retain the source text for each form.")))

(deftest common-lisp-source-syntax-tokenizes-visible-forms ()
  (let* ((source (format nil "; greeting~%(list :hello \"world\" 42)"))
         (tokens (common-lisp-source-syntax-tokens source))
         (kinds (mapcar (lambda (token) (getf token :kind)) tokens)))
    (is (equal '(:comment :paren :symbol :keyword :string :number :paren)
               kinds)
        "Common Lisp syntax tokenization should classify visible source forms.")
    (is (string= "; greeting" (getf (first tokens) :text))
        "Syntax tokens should retain the original source text.")
    (is (= 24
           (getf (find :string tokens
                       :key (lambda (token) (getf token :kind)))
                 :start))
        "Syntax tokens should retain source offsets for later highlighting.")))

(deftest common-lisp-source-map-maps-nested-forms ()
  (let* ((source "(list (+ 1 2) (* 3 4))")
         (source-map (common-lisp-source-map source))
         (nested-list (find '(0 1) source-map
                            :test #'equal
                            :key (lambda (entry) (getf entry :path))))
         (nested-symbol (find '(0 1 0) source-map
                              :test #'equal
                              :key (lambda (entry) (getf entry :path)))))
    (is (string= "(+ 1 2)" (getf nested-list :text))
        "Source maps should retain text for nested forms.")
    (is (equal '(6 13)
               (list (getf nested-list :start)
                     (getf nested-list :end)))
        "Source maps should retain source spans for nested forms.")
    (is (equal '(+ 1 2) (getf nested-list :form))
        "Source maps should retain the parsed form at each path.")
    (is (string= "+" (getf nested-symbol :text))
        "Source maps should include atomic child forms.")))

(deftest source-map-entry-at-offset-returns-deepest-form ()
  (let* ((source "(list (+ 1 2) (* 3 4))")
         (source-map (common-lisp-source-map source)))
    (is (equal '(0 1 0)
               (getf (source-map-entry-at-offset source-map 7) :path))
        "Offset lookup should prefer the deepest containing source-mapped form.")
    (is (equal '(0 1 1)
               (getf (source-map-entry-at-offset source-map 9) :path))
        "Offset lookup should return atom-level mappings inside nested lists.")
    (is (equal '(0 1)
               (getf (source-map-entry-at-offset source-map 6) :path))
        "Offset lookup should return the nested list at its opening delimiter.")))

(deftest code-block-source-map-respects-language-and-parse-state ()
  (let ((block (make-code-block :source "(list :hello)")))
    (is (equal '(0 1)
               (getf (source-map-entry-at-offset
                      (code-block-source-map block)
                      6)
                     :path))
        "Code blocks should expose Common Lisp source mappings.")
    (setf (code-block-source block) "(list :hello")
    (is (null (code-block-source-map block))
        "Malformed code blocks should not expose stale source mappings.")
    (setf (code-block-source block) "(list :hello)")
    (setf (code-block-language block) :text)
    (is (null (code-block-source-map block))
        "Unsupported code-block languages should not expose source mappings.")))

(deftest code-block-parse-info-caches-current-source ()
  (let* ((block (make-code-block :source "(list :hello)"))
         (first-info (code-block-parse-info block))
         (second-info (code-block-parse-info block)))
    (is (eq first-info second-info)
        "Code-block parse info should be cached while the source is unchanged.")
    (replace-code-block-source block "(list :goodbye)")
    (let ((third-info (code-block-parse-info block)))
      (is (not (eq first-info third-info))
          "Replacing source should invalidate the old parse cache.")
      (is (equal '((list :goodbye)) (getf third-info :forms))
          "Reparsed cache should describe the current source."))))

(deftest common-lisp-incremental-parse-reuses-clean-top-level-forms ()
  (let* ((old-source (format nil "(list :a)~%(+ 1 2)~%(list :c)"))
         (old-info (parse-common-lisp-source old-source))
         (replacement "(* 10 2)")
         (start (search "(+ 1 2)" old-source :test #'char=))
         (end (+ start (length "(+ 1 2)")))
         (new-source (concatenate 'string
                                  (subseq old-source 0 start)
                                  replacement
                                  (subseq old-source end)))
         (info (common-lisp-incremental-parse-info
                old-source
                new-source
                start
                end
                :replacement replacement
                :previous-info old-info))
         (old-forms (getf old-info :forms))
         (forms (getf info :forms)))
    (is (null (getf info :error))
        "Incremental parse should succeed when the edited form is valid.")
    (is (equal `((list :a) (* 10 2) (list :c)) forms)
        "Incremental parse should return the updated top-level forms.")
    (is (eq (first forms) (first old-forms))
        "Incremental parse should reuse unchanged prefix form objects.")
    (is (eq (third forms) (third old-forms))
        "Incremental parse should reuse unchanged suffix form objects.")
    (is (= 1 (getf info :reused-prefix-count))
        "Incremental parse should report reused prefix forms.")
    (is (= 1 (getf info :reused-suffix-count))
        "Incremental parse should report reused suffix forms.")
    (is (= 1 (getf info :dirty-form-count))
        "Incremental parse should report the reparsed dirty form count.")
    (is (string= "(* 10 2)"
                 (getf (find '(1) (common-lisp-source-map new-source info)
                             :test #'equal
                             :key (lambda (entry) (getf entry :path)))
                       :text))
        "Incremental parse records should feed source maps for changed forms.")))

(deftest common-lisp-incremental-parse-inserts-top-level-form ()
  (let* ((old-source (format nil "(list :a)~%(list :c)"))
         (old-info (parse-common-lisp-source old-source))
         (replacement (format nil "(list :b)~%"))
         (start (search "(list :c)" old-source :test #'char=))
         (new-source (concatenate 'string
                                  (subseq old-source 0 start)
                                  replacement
                                  (subseq old-source start)))
         (info (common-lisp-incremental-parse-info
                old-source
                new-source
                start
                start
                :replacement replacement
                :previous-info old-info))
         (old-forms (getf old-info :forms))
         (forms (getf info :forms)))
    (is (equal '((list :a) (list :b) (list :c)) forms)
        "Incremental parse should insert a new top-level form between reused forms.")
    (is (eq (first forms) (first old-forms))
        "Top-level insertion should preserve the prefix form object.")
    (is (eq (third forms) (second old-forms))
        "Top-level insertion should preserve and shift the suffix form object.")
    (is (= 1 (getf info :reused-prefix-count))
        "Top-level insertion should report reused prefix forms.")
    (is (= 1 (getf info :reused-suffix-count))
        "Top-level insertion should report reused suffix forms.")
    (is (= 1 (getf info :dirty-form-count))
        "Top-level insertion should report one dirty parsed form.")))

(deftest common-lisp-incremental-parse-reports-dirty-errors ()
  (let* ((old-source (format nil "(list :a)~%(list :c)"))
         (old-info (parse-common-lisp-source old-source))
         (replacement "(list :c")
         (start (search "(list :c)" old-source :test #'char=))
         (end (+ start (length "(list :c)")))
         (new-source (concatenate 'string
                                  (subseq old-source 0 start)
                                  replacement
                                  (subseq old-source end)))
         (info (common-lisp-incremental-parse-info
                old-source
                new-source
                start
                end
                :replacement replacement
                :previous-info old-info)))
    (is (getf info :error)
        "Incremental parse should surface dirty-region parse errors.")
    (is (equal '((list :a)) (getf info :forms))
        "Incremental dirty errors should keep only the valid reused prefix forms.")
    (is (null (common-lisp-source-map new-source info))
        "Source maps should stay unavailable while the incremental parse has an error.")))

(deftest code-block-syntax-summary-renders ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                (format nil "; greeting~%(list :hello \"world\" 42)"))))
    (declare (ignore block))
    (render-application application)
    (is (find "Syntax  |  comment 1  |  string 1  |  keyword 1  |  number 1  |  symbol 1  |  paren 2"
              (collect-text-cells (application-root-cell application))
              :test #'string=)
        "Code blocks should render a syntax summary for backend highlighting work.")))

(deftest code-block-source-cell-carries-syntax-style-spans ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list :hello \"world\" 42)")))
    (render-application application)
    (let* ((cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id block)))
           (spans (cell-style-spans cell))
           (kinds (mapcar (lambda (span) (getf span :kind)) spans)))
      (is (eq :editable-content (orra::cell-role cell))
          "The code source cell should stay editable.")
      (is (equal '(:paren :symbol :keyword :string :number :paren) kinds)
          "Code source cells should carry syntax style spans for highlighting.")
      (is (= 6 (getf (find :keyword spans
                           :key (lambda (span) (getf span :kind)))
                     :start))
          "Syntax style spans should preserve source offsets."))))

(deftest null-backend-records-styled-code-source ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend)))
    (invoke-command application
                    'append-code-block
                    "(list :hello \"world\" 42)")
    (render-application application)
    (let* ((operation
            (find-styled-text-operation backend "(list :hello \"world\" 42)"))
           (spans (fifth operation)))
      (is operation
          "Rendering a styled code source cell should record a styled text operation.")
      (is (find :string spans :key (lambda (span) (getf span :kind)))
          "Styled text operations should include visible syntax spans."))))

(deftest inspector-describes-code-block-syntax ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                (format nil "; greeting~%(list :hello \"world\" 42)"))))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (render-application application)
    (is (find "syntax: Syntax  |  comment 1  |  string 1  |  keyword 1  |  number 1  |  symbol 1  |  paren 2"
              (collect-text-cells (application-root-cell application))
              :test #'string=)
        "Inspector should expose syntax metadata for focused code blocks.")))

(deftest property-inheritance ()
  (let* ((registry (make-object-registry))
         (prototype (make-paragraph :text "proto" :registry registry))
         (child (make-paragraph :text "child" :registry registry)))
    (setf (object-prototype child) prototype)
    (set-object-property prototype :font :serif)
    (is (eql :serif (object-property child :font))
        "Prototype property lookup failed.")))

(deftest workspace-operation-records-semantic-payload ()
  (let* ((registry (make-object-registry))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (operation
          (make-workspace-operation
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "updated")
           :actor-id "user-1"
           :session-id "session-1"
           :timestamp 100)))
    (is (stringp (operation-id operation))
        "Operations should have stable ids.")
    (is (eq :set-slot (operation-type operation))
        "Operation type should describe the semantic change.")
    (is (string= (object-id paragraph)
                 (operation-target-id operation))
        "Operation target should reference object identity, not object memory.")
    (is (equal (list :slot :text :value "updated")
               (operation-payload operation))
        "Operation payload should preserve serializer-friendly semantic data.")
    (is (equal (list :id (operation-id operation)
                     :type :set-slot
                     :target-id (object-id paragraph)
                     :payload (list :slot :text :value "updated")
                     :actor-id "user-1"
                     :session-id "session-1"
                     :timestamp 100)
               (workspace-operation-plist operation))
        "Operations should expose a persistence-ready plist representation.")))

(deftest operation-journal-assigns-local-sequence ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"))
         (first-operation
          (record-local-operation journal
                                  :create-object
                                  :target-id "paragraph-1"
                                  :payload (list :kind :paragraph)
                                  :actor-id "user-1"
                                  :session-id "session-1"
                                  :timestamp 101))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "hello")
                                  :actor-id "user-1"
                                  :session-id "session-1"
                                  :timestamp 102)))
    (is (= 1 (operation-sequence first-operation))
        "The first local operation should receive sequence 1.")
    (is (= 2 (operation-sequence second-operation))
        "The second local operation should receive sequence 2.")
    (is (= 3 (journal-next-sequence journal))
        "The journal should track the next available local sequence.")
    (is (equal (list first-operation second-operation)
               (journal-operations journal))
        "Journal reads should preserve append order.")
    (let ((snapshot (journal-operations journal)))
      (setf (cdr snapshot) nil)
      (is (= 2 (length (journal-operations journal)))
          "Journal reads should not expose mutable journal storage."))))

(deftest apply-workspace-operation-sets-target-slot ()
  (let* ((registry (make-object-registry))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-op-1"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "remote text")
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 200)))
    (is (eq paragraph
            (apply-workspace-operation registry operation))
        "Applying a set-slot operation should return the target object.")
    (is (string= "remote text" (paragraph-text paragraph))
        "Applying a set-slot operation should mutate the target model slot.")))

(deftest apply-remote-operation-records-and-dedupes ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-op-2"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "first remote value")
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 201
           :sequence 7)))
    (is (eq paragraph
            (apply-remote-operation registry journal operation))
        "Remote apply should apply new operations to the model.")
    (is (string= "first remote value" (paragraph-text paragraph))
        "Remote apply should perform the semantic operation.")
    (is (= 1 (length (journal-operations journal)))
        "Remote apply should record the applied operation once.")
    (is (journal-recorded-operation-p journal "remote-op-2")
        "The journal should remember applied operation ids for dedupe.")
    (setf (paragraph-text paragraph) "local value after apply")
    (is (null (apply-remote-operation registry journal operation))
        "Applying the same remote operation again should be a no-op.")
    (is (string= "local value after apply" (paragraph-text paragraph))
        "Duplicate remote operations should not reapply model mutations.")
    (is (= 1 (length (journal-operations journal)))
        "Duplicate remote operations should not be appended again.")))

(deftest apply-operation-journal-replays-in-append-order ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"))
         (paragraph (make-paragraph :text "draft" :registry registry)))
    (record-operation
     journal
     (make-workspace-operation
      :id "replay-op-1"
      :type :set-slot
      :target-id (object-id paragraph)
      :payload (list :slot :text :value "first replay value")
      :timestamp 300))
    (record-operation
     journal
     (make-workspace-operation
      :id "replay-op-2"
      :type :set-slot
      :target-id (object-id paragraph)
      :payload (list :slot :text :value "second replay value")
      :timestamp 301))
    (is (equal (list paragraph paragraph)
               (apply-operation-journal registry journal))
        "Journal replay should return each operation application result.")
    (is (string= "second replay value" (paragraph-text paragraph))
        "Journal replay should apply operations in append order.")))

(deftest local-operations-carry-journal-identity-and-clock ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (first-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "first")))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "second"))))
    (is (string= "local-user" (operation-actor-id first-operation))
        "Local operations should default to the journal actor.")
    (is (string= "local-session" (operation-session-id first-operation))
        "Local operations should default to the journal session.")
    (is (equal '(("local-user" . 1))
               (operation-clock first-operation))
        "The first local operation should carry the first local clock tick.")
    (is (equal '(("local-user" . 2))
               (operation-clock second-operation))
        "The second local operation should advance the local actor clock.")
    (is (= 2 (journal-clock-position journal "local-user"))
        "The journal should retain the latest local actor clock position.")
    (is (equal '(("local-user" . 2))
               (journal-vector-clock journal))
        "The journal should expose a serializer-friendly causal clock.")))

(deftest remote-operations-merge-causal-clock ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (local-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text :value "local")))
         (remote-operation
          (make-workspace-operation
           :id "causal-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "remote")
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("local-user" . 1)
                    ("peer-user" . 3)))))
    (declare (ignore local-operation))
    (apply-remote-operation registry journal remote-operation)
    (is (= 1 (journal-clock-position journal "local-user"))
        "Remote apply should preserve the known local clock position.")
    (is (= 3 (journal-clock-position journal "peer-user"))
        "Remote apply should merge the peer clock position.")
    (is (equal '(("local-user" . 1)
                 ("peer-user" . 3))
               (journal-vector-clock journal))
        "Merged clocks should stay deterministic and serializer-friendly.")
    (apply-remote-operation registry journal remote-operation)
    (is (equal '(("local-user" . 1)
                 ("peer-user" . 3))
               (journal-vector-clock journal))
        "Duplicate remote operations should not advance causal state twice.")))

(deftest concurrent-set-slot-operations-record-conflict ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (local-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text
                                                 :value "local draft")))
         (remote-operation
          (make-workspace-operation
           :id "conflicting-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "peer draft")
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("peer-user" . 1)))))
    (setf (paragraph-text paragraph) "local draft")
    (is (null (apply-remote-operation registry journal remote-operation))
        "Conflicting remote operations should not overwrite pending local values.")
    (is (string= "local draft" (paragraph-text paragraph))
        "The local object value should remain visible after a conflict.")
    (is (journal-recorded-operation-p journal remote-operation)
        "Conflicting remote operations should still be recorded for convergence.")
    (let ((conflicts (journal-conflicts journal)))
      (is (= 1 (length conflicts))
          "Concurrent same-slot operations should create one conflict record.")
      (let ((conflict (first conflicts)))
        (is (equal (object-id paragraph)
                   (sync-conflict-target-id conflict))
            "Conflict records should identify the semantic target object.")
        (is (eq :text (sync-conflict-slot conflict))
            "Conflict records should identify the semantic slot.")
        (is (equal (operation-id local-operation)
                   (sync-conflict-local-operation-id conflict))
            "Conflict records should identify the pending local operation.")
        (is (equal (operation-id remote-operation)
                   (sync-conflict-remote-operation-id conflict))
            "Conflict records should identify the remote operation.")))))

(deftest remote-set-slot-applies-when-pending-local-slot-differs ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (remote-operation
          (make-workspace-operation
           :id "non-conflicting-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :metadata
                          :value (list :reviewed t))
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("peer-user" . 1)))))
    (record-local-operation journal
                            :set-slot
                            :target-id (object-id paragraph)
                            :payload (list :slot :text
                                           :value "local draft"))
    (is (eq paragraph
            (apply-remote-operation registry journal remote-operation))
        "Remote operations for different slots should still apply.")
    (is (equal (list :reviewed t)
               (object-property paragraph :metadata))
        "Non-conflicting remote values should update the target object.")
    (is (null (journal-conflicts journal))
        "Non-conflicting remote operations should not create conflict records.")))

(deftest operation-journal-replay-skips-open-conflict-overwrites ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (local-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text
                                                 :value "local draft")))
         (remote-operation
          (make-workspace-operation
           :id "conflicting-replay-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "peer draft")
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("peer-user" . 1)))))
    (declare (ignore local-operation))
    (setf (paragraph-text paragraph) "local draft")
    (apply-remote-operation registry journal remote-operation)
    (setf (paragraph-text paragraph) "restored draft")
    (is (equal (list paragraph nil)
               (apply-operation-journal registry journal))
        "Journal replay should expose skipped conflicted operations.")
    (is (string= "local draft" (paragraph-text paragraph))
        "Journal replay should preserve local values over open remote conflicts.")))

(deftest local-operations-enter-pending-queue-until-acknowledged ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (first-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "first")))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "second"))))
    (is (eq :pending
            (journal-operation-queue-status journal first-operation))
        "Local operations should enter the journal queue as pending.")
    (is (equal (list first-operation second-operation)
               (journal-pending-operations journal))
        "Pending operations should preserve journal append order.")
    (is (eq first-operation
            (acknowledge-journal-operation journal first-operation))
        "Acknowledging should return the acknowledged operation.")
    (is (eq :acknowledged
            (journal-operation-queue-status journal first-operation))
        "Acknowledged operations should retain their acknowledgement state.")
    (is (equal (list second-operation)
               (journal-pending-operations journal))
        "Acknowledged operations should leave the pending queue.")))

(deftest failed-local-operations-leave-pending-queue ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "draft"))))
    (is (eq operation
            (fail-journal-operation journal operation))
        "Failing should return the failed operation.")
    (is (eq :failed
            (journal-operation-queue-status journal operation))
        "Failed operations should retain their failed queue state.")
    (is (null (journal-pending-operations journal))
        "Failed operations should leave the pending queue.")
    (is (equal (list operation)
               (journal-failed-operations journal))
        "Failed operations should be inspectable in journal order.")))

(deftest remote-operations-do-not-enter-local-queue ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (remote-operation
          (make-workspace-operation
           :id "queue-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "remote")
           :actor-id "peer-user"
           :session-id "peer-session")))
    (apply-remote-operation registry journal remote-operation)
    (is (null (journal-operation-queue-status journal remote-operation))
        "Remote operations should not become local queue entries.")
    (is (null (journal-pending-operations journal))
        "Remote operations should not appear in the pending local queue.")))

(deftest workspace-operation-plist-round-trips-sync-fields ()
  (let* ((payload (list :id "sync-op-1"
                        :type :set-slot
                        :target-id "paragraph-1"
                        :payload (list :slot :text :value "synced")
                        :actor-id "actor-1"
                        :session-id "session-1"
                        :timestamp 123
                        :clock '(("actor-1" . 2))
                        :sequence 9))
         (operation (make-workspace-operation-from-plist payload)))
    (is (string= "sync-op-1" (operation-id operation))
        "Operation plist readers should preserve operation identity.")
    (is (eq :set-slot (operation-type operation))
        "Operation plist readers should preserve operation type.")
    (is (string= "paragraph-1" (operation-target-id operation))
        "Operation plist readers should preserve target identity.")
    (is (equal (list :slot :text :value "synced")
               (operation-payload operation))
        "Operation plist readers should preserve semantic payloads.")
    (is (string= "actor-1" (operation-actor-id operation))
        "Operation plist readers should preserve actor identity.")
    (is (string= "session-1" (operation-session-id operation))
        "Operation plist readers should preserve session identity.")
    (is (= 123 (operation-timestamp operation))
        "Operation plist readers should preserve timestamps.")
    (is (equal '(("actor-1" . 2))
               (operation-clock operation))
        "Operation plist readers should preserve causal clocks.")
    (is (= 9 (operation-sequence operation))
        "Operation plist readers should preserve local sequences.")
    (is (equal payload
               (workspace-operation-plist operation))
        "Operation plist serialization should round-trip sync fields.")))

(deftest journal-pending-sync-payload-exports-queued-operations ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (first-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "first")
                                  :timestamp 200))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "second")
                                  :timestamp 201)))
    (acknowledge-journal-operation journal first-operation)
    (let ((payload (journal-pending-sync-payload journal)))
      (is (equal "workspace-1" (getf payload :workspace-id))
          "Sync payloads should identify the workspace.")
      (is (equal "actor-1" (getf payload :actor-id))
          "Sync payloads should identify the local actor.")
      (is (equal "session-1" (getf payload :session-id))
          "Sync payloads should identify the local session.")
      (is (equal '(("actor-1" . 2))
                 (getf payload :clock))
          "Sync payloads should include the latest local causal clock.")
      (is (equal (list (workspace-operation-plist second-operation))
                 (getf payload :operations))
          "Sync payloads should include only pending operations in journal order."))))

(deftest sync-acknowledgement-payload-clears-confirmed-local-operations ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (first-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "first")
                                  :timestamp 300))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "second")
                                  :timestamp 301))
         (payload
          (list :workspace-id "workspace-1"
                :acknowledged-operation-ids
                (list (operation-id first-operation)))))
    (is (equal (list first-operation)
               (apply-sync-acknowledgement-payload journal payload))
        "Acknowledgement payloads should return acknowledged operations.")
    (is (eq :acknowledged
            (journal-operation-queue-status journal first-operation))
        "Acknowledged sync operations should leave the pending queue.")
    (is (eq :pending
            (journal-operation-queue-status journal second-operation))
        "Unacknowledged sync operations should remain pending.")
    (is (equal (list second-operation)
               (journal-pending-operations journal))
        "Only operations confirmed by the server should leave pending state.")))

(deftest sync-acknowledgement-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "draft")))
         (payload
          (list :workspace-id "workspace-2"
                :acknowledged-operation-ids
                (list (operation-id operation)))))
    (handler-case
        (progn
          (apply-sync-acknowledgement-payload journal payload)
          (is nil "Acknowledgements from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Workspace mismatch errors should identify the payload workspace.")))
    (is (eq :pending
            (journal-operation-queue-status journal operation))
        "Rejected acknowledgements should not mutate local queue state.")))

(deftest remote-sync-payload-applies-operation-plists ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "draft" :registry registry))
         (payload
          (list :workspace-id "workspace-1"
                :actor-id "peer-user"
                :session-id "peer-session"
                :clock '(("peer-user" . 2))
                :operations
                (list (list :id "sync-remote-op-1"
                            :type :set-slot
                            :target-id (object-id paragraph)
                            :payload (list :slot :text :value "first remote")
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :timestamp 300
                            :clock '(("peer-user" . 1)))
                      (list :id "sync-remote-op-2"
                            :type :set-slot
                            :target-id (object-id paragraph)
                            :payload (list :slot :text :value "second remote")
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :timestamp 301
                            :clock '(("peer-user" . 2)))))))
    (is (equal (list paragraph paragraph)
               (apply-remote-sync-payload registry journal payload))
        "Remote sync payloads should apply new operation plists in payload order.")
    (is (string= "second remote" (paragraph-text paragraph))
        "Remote sync payloads should apply semantic model changes.")
    (is (= 2 (length (journal-operations journal)))
        "Remote sync payloads should record applied operations.")
    (is (equal '(("peer-user" . 2))
               (journal-vector-clock journal))
        "Remote sync payloads should merge operation causal clocks.")
    (setf (paragraph-text paragraph) "local after apply")
    (is (null (apply-remote-sync-payload registry journal payload))
        "Reapplying an already-recorded sync payload should return no results.")
    (is (string= "local after apply" (paragraph-text paragraph))
        "Duplicate sync payloads should not replay operation side effects.")
    (is (= 2 (length (journal-operations journal)))
        "Duplicate sync payloads should not append duplicate operations.")))

(deftest remote-sync-payload-rejects-other-workspaces ()
  (let ((registry (make-object-registry))
        (journal (make-operation-journal :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"))
        (payload (list :workspace-id "workspace-2"
                       :operations nil)))
    (handler-case
        (progn
          (apply-remote-sync-payload registry journal payload)
          (is nil "Sync payloads from another workspace should not be applied."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-operations journal))
        "Rejected sync payloads should not mutate the journal.")))

(deftest local-presence-sync-payload-is-serializer-friendly ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (presence
          (record-local-presence journal
                                 :focus-id "paragraph-1"
                                 :cursor-position 12
                                 :status :editing
                                 :updated-at 400
                                 :metadata (list :color "blue")))
         (payload (journal-presence-sync-payload journal)))
    (is (eq presence
            (find-journal-presence journal "actor-1" "session-1"))
        "Local presence should be visible in the journal by actor/session.")
    (is (equal (list :workspace-id "workspace-1"
                     :actor-id "actor-1"
                     :session-id "session-1"
                     :presence
                     (list :actor-id "actor-1"
                           :session-id "session-1"
                           :focus-id "paragraph-1"
                           :cursor-position 12
                           :status :editing
                           :updated-at 400
                           :metadata (list :color "blue")))
               payload)
        "Presence payloads should remain simple plists for sync transport.")))

(deftest presence-sync-payload-updates-remote-presence ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (older-payload
          (list :workspace-id "workspace-1"
                :presence
                (list :actor-id "peer-user"
                      :session-id "peer-session"
                      :focus-id "paragraph-1"
                      :cursor-position 3
                      :status :editing
                      :updated-at 100
                      :metadata (list :color "red"))))
         (stale-payload
          (list :workspace-id "workspace-1"
                :presence
                (list :actor-id "peer-user"
                      :session-id "peer-session"
                      :focus-id "paragraph-2"
                      :cursor-position 5
                      :status :idle
                      :updated-at 99
                      :metadata (list :color "green"))))
         (newer-payload
          (list :workspace-id "workspace-1"
                :presence
                (list :actor-id "peer-user"
                      :session-id "peer-session"
                      :focus-id "paragraph-3"
                      :cursor-position 8
                      :status :active
                      :updated-at 101
                      :metadata (list :color "blue")))))
    (let ((applied (apply-presence-sync-payload journal older-payload)))
      (is (= 1 (length applied))
          "Presence sync should return the presence entries it accepted.")
      (is (string= "paragraph-1"
                   (collaborator-presence-focus-id (first applied)))
          "Remote presence should preserve focus identity."))
    (apply-presence-sync-payload journal stale-payload)
    (let ((presence (find-journal-presence journal
                                           "peer-user"
                                           "peer-session")))
      (is (string= "paragraph-1"
                   (collaborator-presence-focus-id presence))
          "Stale presence should not replace newer local state.")
      (is (= 100 (collaborator-presence-updated-at presence))
          "Stale presence should not rewind update time."))
    (apply-presence-sync-payload journal newer-payload)
    (let ((presence (find-journal-presence journal
                                           "peer-user"
                                           "peer-session")))
      (is (string= "paragraph-3"
                   (collaborator-presence-focus-id presence))
          "Newer presence should replace older presence for the same session.")
      (is (= 101 (collaborator-presence-updated-at presence))
          "Newer presence should advance update time.")
      (is (equal (list :color "blue")
                 (collaborator-presence-metadata presence))
          "Presence metadata should remain available for future UI rendering."))))

(deftest presence-sync-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-2"
                :presence
                (list :actor-id "peer-user"
                      :session-id "peer-session"
                      :focus-id "paragraph-1"
                      :cursor-position 3
                      :status :editing
                      :updated-at 100))))
    (handler-case
        (progn
          (apply-presence-sync-payload journal payload)
          (is nil "Presence from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Presence workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-presences journal))
        "Rejected presence payloads should not mutate local presence state.")))

(deftest local-comment-sync-payload-is-serializer-friendly ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (comment
          (record-local-comment journal
                                :id "comment-1"
                                :target-id "paragraph-1"
                                :body "Needs a citation."
                                :status :open
                                :created-at 500
                                :updated-at 501
                                :metadata (list :range (list 0 17))))
         (payload (journal-comment-sync-payload journal comment)))
    (is (eq comment
            (find-journal-comment journal "comment-1"))
        "Local comments should be visible in the journal by comment id.")
    (is (equal (list :workspace-id "workspace-1"
                     :actor-id "actor-1"
                     :session-id "session-1"
                     :comment
                     (list :id "comment-1"
                           :target-id "paragraph-1"
                           :body "Needs a citation."
                           :actor-id "actor-1"
                           :session-id "session-1"
                           :status :open
                           :created-at 500
                           :updated-at 501
                           :metadata (list :range (list 0 17))))
               payload)
        "Comment payloads should remain simple plists for sync transport.")))

(deftest local-comment-recording-assigns-default-id ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "actor-1"
                                          :session-id "session-1"))
         (comment
          (record-local-comment journal
                                :target-id "paragraph-1"
                                :body "Draft note."
                                :created-at 600)))
    (is (stringp (collaboration-comment-id comment))
        "Local comments should receive an id when callers do not supply one.")
    (is (eq comment
            (find-journal-comment journal (collaboration-comment-id comment)))
        "Generated comment ids should be immediately usable for journal lookup.")
    (is (string= "Draft note."
                 (collaboration-comment-body comment))
        "Local comments should preserve the supplied body.")))

(deftest comment-sync-payload-updates-remote-comment ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (older-payload
          (list :workspace-id "workspace-1"
                :comment
                (list :id "comment-remote-1"
                      :target-id "paragraph-1"
                      :body "Please clarify this."
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :status :open
                      :created-at 100
                      :updated-at 101
                      :metadata (list :kind :review))))
         (stale-payload
          (list :workspace-id "workspace-1"
                :comment
                (list :id "comment-remote-1"
                      :target-id "paragraph-2"
                      :body "Stale body."
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :status :open
                      :created-at 100
                      :updated-at 100
                      :metadata (list :kind :stale))))
         (newer-payload
          (list :workspace-id "workspace-1"
                :comment
                (list :id "comment-remote-1"
                      :target-id "paragraph-3"
                      :body "Resolved after edit."
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :status :resolved
                      :created-at 100
                      :updated-at 102
                      :metadata (list :kind :review :resolution :accepted)))))
    (let ((applied (apply-comment-sync-payload journal older-payload)))
      (is (= 1 (length applied))
          "Comment sync should return the comment entries it accepted.")
      (is (string= "Please clarify this."
                   (collaboration-comment-body (first applied)))
          "Remote comments should preserve comment body text."))
    (apply-comment-sync-payload journal stale-payload)
    (let ((comment (find-journal-comment journal "comment-remote-1")))
      (is (string= "paragraph-1"
                   (collaboration-comment-target-id comment))
          "Stale comments should not replace newer local state.")
      (is (= 101 (collaboration-comment-updated-at comment))
          "Stale comments should not rewind update time."))
    (apply-comment-sync-payload journal newer-payload)
    (let ((comment (find-journal-comment journal "comment-remote-1")))
      (is (string= "paragraph-3"
                   (collaboration-comment-target-id comment))
          "Newer comments should replace older comments for the same id.")
      (is (eq :resolved
              (collaboration-comment-status comment))
          "Newer comments should update status.")
      (is (equal (list :kind :review :resolution :accepted)
                 (collaboration-comment-metadata comment))
          "Comment metadata should remain available for future UI rendering.")
      (is (equal (list comment)
                 (journal-comments journal))
          "Journal comments should be inspectable in deterministic order."))))

(deftest comment-sync-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-2"
                :comment
                (list :id "comment-remote-1"
                      :target-id "paragraph-1"
                      :body "Wrong workspace."
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :status :open
                      :created-at 100
                      :updated-at 100))))
    (handler-case
        (progn
          (apply-comment-sync-payload journal payload)
          (is nil "Comments from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Comment workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-comments journal))
        "Rejected comment payloads should not mutate local comment state.")))

(deftest local-membership-sync-payload-is-serializer-friendly ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (member
          (record-workspace-member journal
                                   :actor-id "peer-user"
                                   :display-name "Peer User"
                                   :role :editor
                                   :status :active
                                   :updated-at 700
                                   :metadata (list :color "blue")))
         (payload (journal-membership-sync-payload journal member)))
    (is (eq member
            (find-journal-member journal "peer-user"))
        "Workspace members should be visible in the journal by actor id.")
    (is (equal (list :workspace-id "workspace-1"
                     :actor-id "local-user"
                     :session-id "local-session"
                     :member
                     (list :actor-id "peer-user"
                           :display-name "Peer User"
                           :role :editor
                           :status :active
                           :updated-at 700
                           :metadata (list :color "blue")))
               payload)
        "Membership payloads should remain simple plists for sync transport.")))

(deftest membership-sync-payload-updates-remote-member ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (older-payload
          (list :workspace-id "workspace-1"
                :member
                (list :actor-id "peer-user"
                      :display-name "Peer User"
                      :role :editor
                      :status :active
                      :updated-at 100
                      :metadata (list :color "red"))))
         (stale-payload
          (list :workspace-id "workspace-1"
                :member
                (list :actor-id "peer-user"
                      :display-name "Stale Peer"
                      :role :viewer
                      :status :removed
                      :updated-at 99
                      :metadata (list :color "green"))))
         (newer-payload
          (list :workspace-id "workspace-1"
                :member
                (list :actor-id "peer-user"
                      :display-name "Peer Admin"
                      :role :admin
                      :status :active
                      :updated-at 101
                      :metadata (list :color "blue")))))
    (let ((applied (apply-membership-sync-payload journal older-payload)))
      (is (= 1 (length applied))
          "Membership sync should return the member entries it accepted.")
      (is (string= "Peer User"
                   (workspace-member-display-name (first applied)))
          "Remote membership should preserve display names."))
    (apply-membership-sync-payload journal stale-payload)
    (let ((member (find-journal-member journal "peer-user")))
      (is (string= "Peer User"
                   (workspace-member-display-name member))
          "Stale membership should not replace newer local state.")
      (is (= 100 (workspace-member-updated-at member))
          "Stale membership should not rewind update time."))
    (apply-membership-sync-payload journal newer-payload)
    (let ((member (find-journal-member journal "peer-user")))
      (is (string= "Peer Admin"
                   (workspace-member-display-name member))
          "Newer membership should replace older membership for the same actor.")
      (is (eq :admin
              (workspace-member-role member))
          "Newer membership should update roles.")
      (is (equal (list :color "blue")
                 (workspace-member-metadata member))
          "Membership metadata should remain available for future auth/UI use.")
      (is (equal (list member)
                 (journal-members journal))
          "Journal members should be inspectable in deterministic order."))))

(deftest membership-sync-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-2"
                :member
                (list :actor-id "peer-user"
                      :display-name "Wrong Workspace"
                      :role :editor
                      :status :active
                      :updated-at 100))))
    (handler-case
        (progn
          (apply-membership-sync-payload journal payload)
          (is nil "Members from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Membership workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-members journal))
        "Rejected membership payloads should not mutate local membership state.")))

(deftest local-attachment-sync-payload-is-serializer-friendly ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (attachment
          (record-local-attachment journal
                                   :id "attachment-1"
                                   :target-id "paragraph-1"
                                   :content-type "image/png"
                                   :byte-size 4096
                                   :digest "sha256:abc"
                                   :storage-ref "local://attachment-1"
                                   :status :pending
                                   :created-at 800
                                   :updated-at 801
                                   :metadata (list :filename "diagram.png")))
         (payload (journal-attachment-sync-payload journal attachment)))
    (is (eq attachment
            (find-journal-attachment journal "attachment-1"))
        "Workspace attachments should be visible in the journal by attachment id.")
    (is (equal (list :workspace-id "workspace-1"
                     :actor-id "local-user"
                     :session-id "local-session"
                     :attachment
                     (list :id "attachment-1"
                           :target-id "paragraph-1"
                           :content-type "image/png"
                           :byte-size 4096
                           :digest "sha256:abc"
                           :storage-ref "local://attachment-1"
                           :status :pending
                           :actor-id "local-user"
                           :session-id "local-session"
                           :created-at 800
                           :updated-at 801
                           :metadata (list :filename "diagram.png")))
               payload)
        "Attachment payloads should remain simple plists for sync transport.")))

(deftest attachment-sync-payload-updates-remote-attachment ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (older-payload
          (list :workspace-id "workspace-1"
                :attachment
                (list :id "attachment-remote-1"
                      :target-id "paragraph-1"
                      :content-type "application/pdf"
                      :byte-size 1024
                      :digest "sha256:old"
                      :storage-ref "server://pending"
                      :status :pending
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :created-at 100
                      :updated-at 101
                      :metadata (list :filename "draft.pdf"))))
         (stale-payload
          (list :workspace-id "workspace-1"
                :attachment
                (list :id "attachment-remote-1"
                      :target-id "paragraph-2"
                      :content-type "application/pdf"
                      :byte-size 2048
                      :digest "sha256:stale"
                      :storage-ref "server://stale"
                      :status :available
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :created-at 100
                      :updated-at 99
                      :metadata (list :filename "stale.pdf"))))
         (newer-payload
          (list :workspace-id "workspace-1"
                :attachment
                (list :id "attachment-remote-1"
                      :target-id "paragraph-3"
                      :content-type "application/pdf"
                      :byte-size 4096
                      :digest "sha256:new"
                      :storage-ref "server://attachments/remote-1"
                      :status :available
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :created-at 100
                      :updated-at 102
                      :metadata (list :filename "final.pdf")))))
    (let ((applied (apply-attachment-sync-payload journal older-payload)))
      (is (= 1 (length applied))
          "Attachment sync should return the attachment entries it accepted.")
      (is (string= "server://pending"
                   (workspace-attachment-storage-ref (first applied)))
          "Remote attachments should preserve storage coordination references."))
    (apply-attachment-sync-payload journal stale-payload)
    (let ((attachment (find-journal-attachment journal "attachment-remote-1")))
      (is (string= "paragraph-1"
                   (workspace-attachment-target-id attachment))
          "Stale attachments should not replace newer local state.")
      (is (= 101 (workspace-attachment-updated-at attachment))
          "Stale attachments should not rewind update time."))
    (apply-attachment-sync-payload journal newer-payload)
    (let ((attachment (find-journal-attachment journal "attachment-remote-1")))
      (is (string= "paragraph-3"
                   (workspace-attachment-target-id attachment))
          "Newer attachments should replace older attachments for the same id.")
      (is (eq :available
              (workspace-attachment-status attachment))
          "Newer attachments should update server coordination status.")
      (is (equal (list :filename "final.pdf")
                 (workspace-attachment-metadata attachment))
          "Attachment metadata should remain available for future media rendering.")
      (is (equal (list attachment)
                 (journal-attachments journal))
          "Journal attachments should be inspectable in deterministic order."))))

(deftest attachment-sync-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-2"
                :attachment
                (list :id "attachment-remote-1"
                      :target-id "paragraph-1"
                      :content-type "image/jpeg"
                      :byte-size 200
                      :status :pending
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :created-at 100
                      :updated-at 100))))
    (handler-case
        (progn
          (apply-attachment-sync-payload journal payload)
          (is nil "Attachments from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Attachment workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-attachments journal))
        "Rejected attachment payloads should not mutate local attachment state.")))

(deftest local-checkpoint-sync-payload-is-serializer-friendly ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "saved")
                                  :timestamp 900))
         (checkpoint
          (record-local-checkpoint journal
                                   :id "checkpoint-1"
                                   :checkpoint-at 901
                                   :storage-ref "local://checkpoint-1"
                                   :byte-size 8192
                                   :digest "sha256:checkpoint"
                                   :status :pending
                                   :created-at 902
                                   :updated-at 903
                                   :metadata (list :path "checkpoint.sexp")))
         (payload (journal-checkpoint-sync-payload journal checkpoint)))
    (declare (ignore operation))
    (is (eq checkpoint
            (find-journal-checkpoint journal "checkpoint-1"))
        "Workspace checkpoints should be visible in the journal by checkpoint id.")
    (is (equal (list :workspace-id "workspace-1"
                     :actor-id "local-user"
                     :session-id "local-session"
                     :checkpoint
                     (list :id "checkpoint-1"
                           :checkpoint-at 901
                           :storage-ref "local://checkpoint-1"
                           :byte-size 8192
                           :digest "sha256:checkpoint"
                           :status :pending
                           :actor-id "local-user"
                           :session-id "local-session"
                           :clock '(("local-user" . 1))
                           :created-at 902
                           :updated-at 903
                           :metadata (list :path "checkpoint.sexp")))
               payload)
        "Checkpoint payloads should remain simple plists for sync transport.")))

(deftest checkpoint-sync-payload-updates-remote-checkpoint ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (older-payload
          (list :workspace-id "workspace-1"
                :checkpoint
                (list :id "checkpoint-remote-1"
                      :checkpoint-at 100
                      :storage-ref "server://pending"
                      :byte-size 1024
                      :digest "sha256:old"
                      :status :pending
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :clock '(("peer-user" . 1))
                      :created-at 100
                      :updated-at 101
                      :metadata (list :kind :automatic))))
         (stale-payload
          (list :workspace-id "workspace-1"
                :checkpoint
                (list :id "checkpoint-remote-1"
                      :checkpoint-at 99
                      :storage-ref "server://stale"
                      :byte-size 2048
                      :digest "sha256:stale"
                      :status :available
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :clock '(("peer-user" . 2))
                      :created-at 100
                      :updated-at 99
                      :metadata (list :kind :stale))))
         (newer-payload
          (list :workspace-id "workspace-1"
                :checkpoint
                (list :id "checkpoint-remote-1"
                      :checkpoint-at 102
                      :storage-ref "server://checkpoints/remote-1"
                      :byte-size 4096
                      :digest "sha256:new"
                      :status :available
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :clock '(("peer-user" . 3))
                      :created-at 100
                      :updated-at 103
                      :metadata (list :kind :manual)))))
    (let ((applied (apply-checkpoint-sync-payload journal older-payload)))
      (is (= 1 (length applied))
          "Checkpoint sync should return the checkpoint entries it accepted.")
      (is (string= "server://pending"
                   (workspace-checkpoint-storage-ref (first applied)))
          "Remote checkpoints should preserve storage coordination references."))
    (apply-checkpoint-sync-payload journal stale-payload)
    (let ((checkpoint (find-journal-checkpoint journal "checkpoint-remote-1")))
      (is (= 100 (workspace-checkpoint-checkpoint-at checkpoint))
          "Stale checkpoints should not replace newer local state.")
      (is (= 101 (workspace-checkpoint-updated-at checkpoint))
          "Stale checkpoints should not rewind update time."))
    (apply-checkpoint-sync-payload journal newer-payload)
    (let ((checkpoint (find-journal-checkpoint journal "checkpoint-remote-1")))
      (is (= 102 (workspace-checkpoint-checkpoint-at checkpoint))
          "Newer checkpoints should replace older checkpoints for the same id.")
      (is (eq :available
              (workspace-checkpoint-status checkpoint))
          "Newer checkpoints should update server storage status.")
      (is (equal '(("peer-user" . 3))
                 (workspace-checkpoint-clock checkpoint))
          "Checkpoint metadata should retain the causal frontier it captures.")
      (is (equal (list checkpoint)
                 (journal-checkpoints journal))
          "Journal checkpoints should be inspectable in deterministic order."))))

(deftest checkpoint-sync-payload-rejects-other-workspaces ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-2"
                :checkpoint
                (list :id "checkpoint-remote-1"
                      :checkpoint-at 100
                      :storage-ref "server://wrong"
                      :byte-size 200
                      :status :pending
                      :actor-id "peer-user"
                      :session-id "peer-session"
                      :created-at 100
                      :updated-at 100))))
    (handler-case
        (progn
          (apply-checkpoint-sync-payload journal payload)
          (is nil "Checkpoints from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Checkpoint workspace mismatch errors should identify the payload workspace.")))
    (is (null (journal-checkpoints journal))
        "Rejected checkpoint payloads should not mutate local checkpoint state.")))

(deftest sync-authentication-plist-round-trips ()
  (let* ((payload (list :token-id "token-1"
                        :workspace-id "workspace-1"
                        :actor-id "local-user"
                        :session-id "local-session"
                        :scopes (list :sync :attachments)
                        :issued-at 100
                        :expires-at 200
                        :metadata (list :device "laptop")))
         (authentication
          (make-sync-authentication-from-plist payload)))
    (is (string= "token-1" (sync-authentication-token-id authentication))
        "Sync auth plist readers should preserve token identity.")
    (is (string= "workspace-1" (sync-authentication-workspace-id authentication))
        "Sync auth plist readers should preserve workspace identity.")
    (is (string= "local-user" (sync-authentication-actor-id authentication))
        "Sync auth plist readers should preserve actor identity.")
    (is (string= "local-session" (sync-authentication-session-id authentication))
        "Sync auth plist readers should preserve session identity.")
    (is (equal (list :sync :attachments)
               (sync-authentication-scopes authentication))
        "Sync auth plist readers should preserve authorization scopes.")
    (is (= 100 (sync-authentication-issued-at authentication))
        "Sync auth plist readers should preserve issue timestamps.")
    (is (= 200 (sync-authentication-expires-at authentication))
        "Sync auth plist readers should preserve expiry timestamps.")
    (is (equal (list :device "laptop")
               (sync-authentication-metadata authentication))
        "Sync auth plist readers should preserve metadata.")
    (is (equal payload
               (sync-authentication-plist authentication))
        "Sync auth serialization should round-trip simple plist payloads.")))

(deftest journal-sync-request-payload-includes-authentication ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (authentication
          (make-sync-authentication :token-id "token-1"
                                    :workspace-id "workspace-1"
                                    :actor-id "local-user"
                                    :session-id "local-session"
                                    :scopes (list :sync)
                                    :issued-at 100
                                    :expires-at 200))
         (payload (journal-sync-request-payload journal
                                                :authentication authentication)))
    (is (equal (sync-authentication-plist authentication)
               (getf payload :auth))
        "Sync request envelopes should include serializer-friendly auth metadata.")
    (is (equal "local-user" (getf payload :actor-id))
        "Auth metadata should not replace the normal envelope actor identity.")
    (is (equal "local-session" (getf payload :session-id))
        "Auth metadata should not replace the normal envelope session identity.")))

(deftest sync-request-authorization-accepts-active-member ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "server"
                                          :session-id "server-session"))
         (authentication
          (make-sync-authentication :token-id "token-1"
                                    :workspace-id "workspace-1"
                                    :actor-id "local-user"
                                    :session-id "local-session"
                                    :scopes (list :sync :attachments)
                                    :issued-at 100
                                    :expires-at 200))
         (payload (list :workspace-id "workspace-1"
                        :actor-id "local-user"
                        :session-id "local-session"
                        :auth (sync-authentication-plist authentication))))
    (record-workspace-member journal
                             :actor-id "local-user"
                             :display-name "Local User"
                             :role :editor
                             :status :active
                             :updated-at 99)
    (is (equalp (sync-authentication-plist authentication)
                (sync-authentication-plist
                 (authorize-sync-request-payload journal
                                                 payload
                                                 :now 150
                                                 :required-scope :sync)))
        "Server-side sync auth should accept active workspace members with the required scope.")))

(deftest sync-request-authorization-rejects-invalid-auth ()
  (let ((journal (make-operation-journal :workspace-id "workspace-1"
                                         :actor-id "server"
                                         :session-id "server-session"))
        (expired-payload
         (list :workspace-id "workspace-1"
               :actor-id "local-user"
               :session-id "local-session"
               :auth (list :token-id "expired-token"
                           :workspace-id "workspace-1"
                           :actor-id "local-user"
                           :session-id "local-session"
                           :scopes (list :sync)
                           :issued-at 100
                           :expires-at 120)))
        (mismatched-payload
         (list :workspace-id "workspace-1"
               :actor-id "local-user"
               :session-id "local-session"
               :auth (list :token-id "mismatched-token"
                           :workspace-id "workspace-1"
                           :actor-id "other-user"
                           :session-id "local-session"
                           :scopes (list :sync)
                           :issued-at 100
                           :expires-at 200))))
    (record-workspace-member journal
                             :actor-id "local-user"
                             :display-name "Local User"
                             :role :editor
                             :status :active
                             :updated-at 99)
    (handler-case
        (progn
          (authorize-sync-request-payload journal
                                          expired-payload
                                          :now 150
                                          :required-scope :sync)
          (is nil "Expired sync auth should not authorize requests."))
      (error (condition)
        (is (search "expired" (string-downcase (princ-to-string condition)))
            "Expired auth errors should explain the rejected token.")))
    (handler-case
        (progn
          (authorize-sync-request-payload journal
                                          mismatched-payload
                                          :now 150
                                          :required-scope :sync)
          (is nil "Auth with a mismatched actor should not authorize requests."))
      (error (condition)
        (is (search "actor" (string-downcase (princ-to-string condition)))
            "Mismatched auth errors should explain the rejected actor.")))))

(deftest sync-request-authorization-rejects-nonmembers ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "server"
                                          :session-id "server-session"))
         (payload (list :workspace-id "workspace-1"
                        :actor-id "removed-user"
                        :session-id "removed-session"
                        :auth (list :token-id "token-1"
                                    :workspace-id "workspace-1"
                                    :actor-id "removed-user"
                                    :session-id "removed-session"
                                    :scopes (list :sync)
                                    :issued-at 100
                                    :expires-at 200))))
    (record-workspace-member journal
                             :actor-id "removed-user"
                             :display-name "Removed User"
                             :role :editor
                             :status :removed
                             :updated-at 99)
    (handler-case
        (progn
          (authorize-sync-request-payload journal
                                          payload
                                          :now 150
                                          :required-scope :sync)
          (is nil "Removed members should not authorize sync requests."))
      (error (condition)
        (is (search "removed-user" (princ-to-string condition))
            "Membership auth errors should identify the rejected actor.")))))

(deftest sync-coordinator-registers-workspace-members ()
  (let* ((coordinator (make-sync-coordinator))
         (journal (ensure-sync-coordinator-workspace coordinator
                                                     "workspace-1"))
         (member (sync-coordinator-register-member coordinator
                                                   "workspace-1"
                                                   "local-user"
                                                   :display-name "Local User"
                                                   :role :owner
                                                   :status :active
                                                   :updated-at 150)))
    (is (typep coordinator 'sync-coordinator)
        "Sync coordinator construction should return a coordinator object.")
    (is (eq journal
            (sync-coordinator-workspace-journal coordinator "workspace-1"))
        "Sync coordinators should expose their server-side workspace journals.")
    (is (eq member (find-journal-member journal "local-user"))
        "Registering a coordinator member should record membership in the workspace journal.")
    (is (eq :owner (workspace-member-role member))
        "Coordinator membership registration should preserve explicit roles.")
    (is (= 150 (workspace-member-updated-at member))
        "Coordinator membership registration should preserve update timestamps.")))

(deftest sync-coordinator-accepts-authorized-request ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (operation (record-local-operation client-journal
                                            :set-slot
                                            :target-id "paragraph-1"
                                            :payload (list :slot :text
                                                           :value "local")
                                            :timestamp 151))
         (presence (record-local-presence client-journal
                                          :focus-id "paragraph-1"
                                          :cursor-position 5
                                          :status :editing
                                          :updated-at 152))
         (comment (record-local-comment client-journal
                                        :id "comment-1"
                                        :target-id "paragraph-1"
                                        :body "Looks good."
                                        :created-at 153
                                        :updated-at 154))
         (member (record-workspace-member client-journal
                                          :actor-id "peer-user"
                                          :display-name "Peer User"
                                          :role :editor
                                          :status :active
                                          :updated-at 155))
         (attachment (record-local-attachment client-journal
                                              :id "attachment-1"
                                              :target-id "paragraph-1"
                                              :content-type "image/png"
                                              :byte-size 2048
                                              :digest "sha256:image"
                                              :storage-ref "local://attachment-1"
                                              :status :available
                                              :created-at 156
                                              :updated-at 157))
         (checkpoint (record-local-checkpoint client-journal
                                              :id "checkpoint-1"
                                              :checkpoint-at 158
                                              :storage-ref "local://checkpoint-1"
                                              :byte-size 4096
                                              :digest "sha256:checkpoint"
                                              :status :available
                                              :created-at 159
                                              :updated-at 160)))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :owner
                                      :status :active
                                      :updated-at 149)
    (let* ((request (journal-sync-request-payload client-journal
                                                  :authentication auth
                                                  :comments (list comment)
                                                  :members (list member)
                                                  :attachments (list attachment)
                                                  :checkpoints (list checkpoint)))
           (response (handle-sync-request coordinator request :now 200)))
      (is (equal "workspace-1" (getf response :workspace-id))
          "Coordinator sync responses should identify the workspace.")
      (is (equal (list (operation-id operation))
                 (getf response :acknowledged-operation-ids))
          "Coordinator sync responses should acknowledge accepted local operations.")
      (is (journal-recorded-operation-p server-journal operation)
          "Coordinator request handling should record accepted operations server-side.")
      (is (equal (collaborator-presence-plist presence)
                 (collaborator-presence-plist
                  (find-journal-presence server-journal
                                         "local-user"
                                         "local-session")))
          "Coordinator request handling should record accepted presence server-side.")
      (is (find-journal-comment server-journal "comment-1")
          "Coordinator request handling should record accepted comments server-side.")
      (is (find-journal-member server-journal "peer-user")
          "Coordinator request handling should record accepted membership changes server-side.")
      (is (find-journal-attachment server-journal "attachment-1")
          "Coordinator request handling should record accepted attachment metadata server-side.")
      (is (find-journal-checkpoint server-journal "checkpoint-1")
          "Coordinator request handling should record accepted checkpoint metadata server-side.")
      (is (= 1 (journal-clock-position server-journal "local-user"))
          "Coordinator request handling should merge client causal clocks server-side."))))

(deftest sync-coordinator-distributes-remote-state ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (local-operation (record-local-operation client-journal
                                                  :set-slot
                                                  :target-id "paragraph-1"
                                                  :payload (list :slot :text
                                                                 :value "local")
                                                  :timestamp 201))
         (peer-operation (make-workspace-operation :id "peer-op-1"
                                                   :type :set-slot
                                                   :target-id "paragraph-1"
                                                   :payload (list :slot :text
                                                                  :value "peer")
                                                   :actor-id "peer-user"
                                                   :session-id "peer-session"
                                                   :timestamp 202
                                                   :clock '(("peer-user" . 1)))))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (record-operation server-journal peer-operation)
    (record-local-presence server-journal
                           :actor-id "peer-user"
                           :session-id "peer-session"
                           :focus-id "paragraph-1"
                           :cursor-position 9
                           :status :editing
                           :updated-at 203)
    (let* ((request (journal-sync-request-payload client-journal
                                                  :authentication auth))
           (response (handle-sync-request coordinator request :now 200))
           (operation-ids (mapcar (lambda (operation-plist)
                                    (getf operation-plist :id))
                                  (getf response :operations)))
           (presence-actors (mapcar (lambda (presence-plist)
                                      (getf presence-plist :actor-id))
                                    (getf response :presences))))
      (is (equal (list (operation-id local-operation))
                 (getf response :acknowledged-operation-ids))
          "Coordinator sync responses should acknowledge the caller's accepted operations.")
      (is (equal (list "peer-op-1") operation-ids)
          "Coordinator sync responses should distribute peer operations without echoing caller operations.")
      (is (equal (list "peer-user") presence-actors)
          "Coordinator sync responses should distribute peer presence without echoing the caller session."))))

(deftest sync-coordinator-rejects-unauthorized-request ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "unknown-user"
                                                 :session-id "unknown-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "unknown-user"
                                         :session-id "unknown-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300)))
    (record-local-operation client-journal
                            :set-slot
                            :target-id "paragraph-1"
                            :payload (list :slot :text :value "local")
                            :timestamp 250)
    (record-local-presence client-journal
                           :focus-id "paragraph-1"
                           :cursor-position 3
                           :status :editing
                           :updated-at 251)
    (handler-case
        (progn
          (handle-sync-request coordinator
                               (journal-sync-request-payload client-journal
                                                             :authentication auth)
                               :now 200)
          (is nil "Coordinator should reject sync requests from nonmembers."))
      (error (condition)
        (is (search "unknown-user" (princ-to-string condition))
            "Coordinator membership errors should identify the rejected actor.")))
    (is (null (journal-operations server-journal))
        "Rejected coordinator requests should not record operations.")
    (is (null (journal-presences server-journal))
        "Rejected coordinator requests should not record presence.")))

(deftest sync-coordinator-coordinates-attachment-storage ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (attachment (record-local-attachment client-journal
                                              :id "attachment-1"
                                              :target-id "paragraph-1"
                                              :content-type "image/png"
                                              :byte-size 2048
                                              :digest "sha256:image"
                                              :storage-ref "local://attachment-1"
                                              :status :available
                                              :created-at 201
                                              :updated-at 202)))
    (declare (ignore attachment))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (let* ((response
            (handle-sync-request
             coordinator
             (journal-sync-request-payload client-journal
                                           :authentication auth
                                           :attachments
                                           (journal-attachments client-journal))
             :now 200))
           (server-attachment
            (find-journal-attachment server-journal "attachment-1"))
           (response-attachment (first (getf response :attachments))))
      (is (string= "server://workspace-1/attachments/attachment-1"
                   (workspace-attachment-storage-ref server-attachment))
          "Coordinator attachment storage should replace local refs with server refs.")
      (is (eq :pending (workspace-attachment-status server-attachment))
          "Coordinator attachment storage should stay pending until byte transfer exists.")
      (is (equal (workspace-attachment-plist server-attachment)
                 response-attachment)
          "Coordinator responses should distribute the server-coordinated attachment metadata."))))

(deftest sync-coordinator-coordinates-checkpoint-storage ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (checkpoint (record-local-checkpoint client-journal
                                              :id "checkpoint-1"
                                              :checkpoint-at 210
                                              :storage-ref "local://checkpoint-1"
                                              :byte-size 4096
                                              :digest "sha256:checkpoint"
                                              :status :available
                                              :created-at 211
                                              :updated-at 212)))
    (declare (ignore checkpoint))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (let* ((response
            (handle-sync-request
             coordinator
             (journal-sync-request-payload client-journal
                                           :authentication auth
                                           :checkpoints
                                           (journal-checkpoints client-journal))
             :now 200))
           (server-checkpoint
            (find-journal-checkpoint server-journal "checkpoint-1"))
           (response-checkpoint (first (getf response :checkpoints))))
      (is (string= "server://workspace-1/checkpoints/checkpoint-1"
                   (workspace-checkpoint-storage-ref server-checkpoint))
          "Coordinator checkpoint storage should replace local refs with server refs.")
      (is (eq :pending (workspace-checkpoint-status server-checkpoint))
          "Coordinator checkpoint storage should stay pending until byte transfer exists.")
      (is (equal (workspace-checkpoint-plist server-checkpoint)
                 response-checkpoint)
          "Coordinator responses should distribute the server-coordinated checkpoint metadata."))))

(deftest sync-coordinator-rebases-foreign-storage-references ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300)))
    (record-local-attachment client-journal
                             :id "attachment-1"
                             :target-id "paragraph-1"
                             :content-type "image/png"
                             :byte-size 2048
                             :digest "sha256:image"
                             :storage-ref "server://workspace-2/attachments/attachment-1"
                             :status :available
                             :created-at 201
                             :updated-at 202)
    (record-local-checkpoint client-journal
                             :id "checkpoint-1"
                             :checkpoint-at 210
                             :storage-ref "server://workspace-2/checkpoints/checkpoint-1"
                             :byte-size 4096
                             :digest "sha256:checkpoint"
                             :status :available
                             :created-at 211
                             :updated-at 212)
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (handle-sync-request coordinator
                         (journal-sync-request-payload
                          client-journal
                          :authentication auth
                          :attachments (journal-attachments client-journal)
                          :checkpoints (journal-checkpoints client-journal))
                         :now 200)
    (is (string= "server://workspace-1/attachments/attachment-1"
                 (workspace-attachment-storage-ref
                  (find-journal-attachment server-journal "attachment-1")))
        "Coordinator attachment storage should not trust foreign server refs.")
    (is (string= "server://workspace-1/checkpoints/checkpoint-1"
                 (workspace-checkpoint-storage-ref
                  (find-journal-checkpoint server-journal "checkpoint-1")))
        "Coordinator checkpoint storage should not trust foreign server refs.")))

(deftest journal-sync-request-payload-batches-local-sync-state ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (first-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "first")
                                  :timestamp 800))
         (second-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text :value "second")
                                  :timestamp 801))
         (presence
          (record-local-presence journal
                                 :focus-id "paragraph-1"
                                 :cursor-position 5
                                 :status :editing
                                 :updated-at 802))
         (comment
          (record-local-comment journal
                                :id "comment-1"
                                :target-id "paragraph-1"
                                :body "Review this."
                                :created-at 803
                                :updated-at 804))
         (member
          (record-workspace-member journal
                                   :actor-id "local-user"
                                   :display-name "Local User"
                                   :role :owner
                                   :status :active
                                   :updated-at 805))
         (attachment
          (record-local-attachment journal
                                   :id "attachment-1"
                                   :target-id "paragraph-1"
                                   :content-type "image/png"
                                   :byte-size 4096
                                   :digest "sha256:abc"
                                   :storage-ref "local://attachment-1"
                                   :status :pending
                                   :created-at 806
                                   :updated-at 807))
         (checkpoint
          (record-local-checkpoint journal
                                   :id "checkpoint-1"
                                   :checkpoint-at 808
                                   :storage-ref "local://checkpoint-1"
                                   :byte-size 8192
                                   :digest "sha256:checkpoint"
                                   :status :pending
                                   :created-at 809
                                   :updated-at 810)))
    (acknowledge-journal-operation journal first-operation)
    (let ((payload (journal-sync-request-payload journal
                                                 :comments (list comment)
                                                 :members (list member)
                                                 :attachments (list attachment)
                                                 :checkpoints (list checkpoint))))
      (is (equal "workspace-1" (getf payload :workspace-id))
          "Sync request envelopes should identify the workspace.")
      (is (equal "local-user" (getf payload :actor-id))
          "Sync request envelopes should identify the local actor.")
      (is (equal "local-session" (getf payload :session-id))
          "Sync request envelopes should identify the local session.")
      (is (equal '(("local-user" . 2))
                 (getf payload :clock))
          "Sync request envelopes should include the local causal clock.")
      (is (equal (list (workspace-operation-plist second-operation))
                 (getf payload :operations))
          "Sync request envelopes should batch pending operations.")
      (is (equal (list (collaborator-presence-plist presence))
                 (getf payload :presences))
          "Sync request envelopes should batch local presence.")
      (is (equal (list (collaboration-comment-plist comment))
                 (getf payload :comments))
          "Sync request envelopes should batch selected comments.")
      (is (equal (list (workspace-member-plist member))
                 (getf payload :members))
          "Sync request envelopes should batch selected workspace members.")
      (is (equal (list (workspace-attachment-plist attachment))
                 (getf payload :attachments))
          "Sync request envelopes should batch selected workspace attachments.")
      (is (equal (list (workspace-checkpoint-plist checkpoint))
                 (getf payload :checkpoints))
          "Sync request envelopes should batch selected workspace checkpoints."))))

(deftest sync-response-payload-applies-distributed-state ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "local" :registry registry))
         (local-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text :value "local draft")
                                  :timestamp 900))
         (payload
          (list :workspace-id "workspace-1"
                :clock '(("local-user" . 1) ("peer-user" . 2))
                :acknowledged-operation-ids
                (list (operation-id local-operation))
                :operations
                (list (list :id "remote-op-1"
                            :type :set-slot
                            :target-id (object-id paragraph)
                            :payload (list :slot :text :value "remote")
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :timestamp 901
                            :clock '(("peer-user" . 2))))
                :presences
                (list (list :actor-id "peer-user"
                            :session-id "peer-session"
                            :focus-id (object-id paragraph)
                            :cursor-position 3
                            :status :editing
                            :updated-at 902))
                :comments
                (list (list :id "comment-remote-1"
                            :target-id (object-id paragraph)
                            :body "Remote comment."
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :status :open
                            :created-at 903
                            :updated-at 904))
                :members
                (list (list :actor-id "peer-user"
                            :display-name "Peer User"
                            :role :editor
                            :status :active
                            :updated-at 905))
                :attachments
                (list (list :id "attachment-remote-1"
                            :target-id (object-id paragraph)
                            :content-type "image/png"
                            :byte-size 4096
                            :digest "sha256:remote"
                            :storage-ref "server://attachments/remote-1"
                            :status :available
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :created-at 906
                            :updated-at 907))
                :checkpoints
                (list (list :id "checkpoint-remote-1"
                            :checkpoint-at 908
                            :storage-ref "server://checkpoints/remote-1"
                            :byte-size 8192
                            :digest "sha256:checkpoint"
                            :status :available
                            :actor-id "peer-user"
                            :session-id "peer-session"
                            :clock '(("peer-user" . 2))
                            :created-at 909
                            :updated-at 910)))))
    (let ((result (apply-sync-response-payload registry journal payload)))
      (is (equal (list local-operation)
                 (getf result :acknowledged-operations))
          "Sync responses should acknowledge local operations.")
      (is (equal (list paragraph)
                 (getf result :applied-operations))
          "Sync responses should return applied semantic operation results.")
      (is (= 1 (length (getf result :presences)))
          "Sync responses should return accepted presence updates.")
      (is (= 1 (length (getf result :comments)))
          "Sync responses should return accepted comment updates.")
      (is (= 1 (length (getf result :members)))
          "Sync responses should return accepted membership updates.")
      (is (= 1 (length (getf result :attachments)))
          "Sync responses should return accepted attachment updates.")
      (is (= 1 (length (getf result :checkpoints)))
          "Sync responses should return accepted checkpoint updates."))
    (is (eq :acknowledged
            (journal-operation-queue-status journal local-operation))
        "Sync responses should clear acknowledged local operations from pending state.")
    (is (string= "remote" (paragraph-text paragraph))
        "Sync responses should apply distributed semantic operations.")
    (is (= 2 (journal-clock-position journal "peer-user"))
        "Sync responses should merge distributed causal clocks.")
    (is (find-journal-presence journal "peer-user" "peer-session")
        "Sync responses should store distributed presence.")
    (is (find-journal-comment journal "comment-remote-1")
        "Sync responses should store distributed comments.")
    (is (find-journal-member journal "peer-user")
        "Sync responses should store distributed workspace members.")
    (is (find-journal-attachment journal "attachment-remote-1")
        "Sync responses should store distributed workspace attachments.")
    (is (find-journal-checkpoint journal "checkpoint-remote-1")
        "Sync responses should store distributed workspace checkpoints.")))

(deftest sync-response-payload-accepts-coordinated-storage-state ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session")))
    (record-local-attachment journal
                             :id "attachment-1"
                             :target-id "paragraph-1"
                             :content-type "image/png"
                             :byte-size 2048
                             :digest "sha256:image"
                             :storage-ref "local://attachment-1"
                             :status :available
                             :created-at 201
                             :updated-at 202)
    (record-local-checkpoint journal
                             :id "checkpoint-1"
                             :checkpoint-at 210
                             :storage-ref "local://checkpoint-1"
                             :byte-size 4096
                             :digest "sha256:checkpoint"
                             :status :available
                             :created-at 211
                             :updated-at 212)
    (let ((result
           (apply-sync-response-payload
            registry
            journal
            (list :workspace-id "workspace-1"
                  :actor-id "sync-server"
                  :session-id "sync-server"
                  :clock '(("local-user" . 1))
                  :attachments
                  (list (list :id "attachment-1"
                              :target-id "paragraph-1"
                              :content-type "image/png"
                              :byte-size 2048
                              :digest "sha256:image"
                              :storage-ref
                              "server://workspace-1/attachments/attachment-1"
                              :status :pending
                              :actor-id "local-user"
                              :session-id "local-session"
                              :created-at 201
                              :updated-at 202))
                  :checkpoints
                  (list (list :id "checkpoint-1"
                              :checkpoint-at 210
                              :storage-ref
                              "server://workspace-1/checkpoints/checkpoint-1"
                              :byte-size 4096
                              :digest "sha256:checkpoint"
                              :status :pending
                              :actor-id "local-user"
                              :session-id "local-session"
                              :clock '(("local-user" . 1))
                              :created-at 211
                              :updated-at 212))))))
      (is (= 1 (length (getf result :attachments)))
          "Sync responses should accept coordinated attachment metadata.")
      (is (= 1 (length (getf result :checkpoints)))
          "Sync responses should accept coordinated checkpoint metadata."))
    (let ((attachment (find-journal-attachment journal "attachment-1"))
          (checkpoint (find-journal-checkpoint journal "checkpoint-1")))
      (is (string= "server://workspace-1/attachments/attachment-1"
                   (workspace-attachment-storage-ref attachment))
          "Coordinator attachment refs should replace local attachment refs.")
      (is (eq :pending (workspace-attachment-status attachment))
          "Coordinator attachment status should replace local attachment status.")
      (is (string= "server://workspace-1/checkpoints/checkpoint-1"
                   (workspace-checkpoint-storage-ref checkpoint))
          "Coordinator checkpoint refs should replace local checkpoint refs.")
      (is (eq :pending (workspace-checkpoint-status checkpoint))
          "Coordinator checkpoint status should replace local checkpoint status."))))

(deftest sync-journal-with-coordinator-round-trips-client-state ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (registry (make-object-registry))
         (paragraph (make-paragraph :text "local" :registry registry))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (local-operation
          (record-local-operation client-journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text
                                                 :value "local draft")
                                  :timestamp 201))
         (peer-operation
          (make-workspace-operation :id "peer-op-1"
                                    :type :set-slot
                                    :target-id (object-id paragraph)
                                    :payload (list :slot :text
                                                   :value "peer")
                                    :actor-id "peer-user"
                                    :session-id "peer-session"
                                    :timestamp 202
                                    :clock '(("peer-user" . 1))))
         (attachment
          (record-local-attachment client-journal
                                   :id "attachment-1"
                                   :target-id (object-id paragraph)
                                   :content-type "image/png"
                                   :byte-size 2048
                                   :digest "sha256:image"
                                   :storage-ref "local://attachment-1"
                                   :status :available
                                   :created-at 203
                                   :updated-at 204)))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (record-operation server-journal peer-operation)
    (let* ((exchange
            (sync-journal-with-coordinator
             registry
             client-journal
             coordinator
             auth
             :now 200
             :attachments (list attachment)))
           (result (getf exchange :result))
           (response (getf exchange :response))
           (client-attachment
            (find-journal-attachment client-journal "attachment-1")))
      (is (equal (list (operation-id local-operation))
                 (getf response :acknowledged-operation-ids))
          "Sync exchange responses should acknowledge submitted local operations.")
      (is (eq :acknowledged
              (journal-operation-queue-status client-journal local-operation))
          "Sync exchanges should acknowledge queued local operations client-side.")
      (is (journal-recorded-operation-p server-journal local-operation)
          "Sync exchanges should submit local operations to the coordinator.")
      (is (equal (list paragraph)
                 (getf result :applied-operations))
          "Sync exchanges should apply distributed peer operations locally.")
      (is (string= "peer" (paragraph-text paragraph))
          "Sync exchanges should update local objects from peer operations.")
      (is (string= "server://workspace-1/attachments/attachment-1"
                   (workspace-attachment-storage-ref client-attachment))
          "Sync exchanges should apply coordinated attachment refs locally.")
      (is (eq :pending (workspace-attachment-status client-attachment))
          "Sync exchanges should apply coordinated attachment status locally."))))

(deftest sync-journal-with-transport-round-trips-client-state ()
  (let* ((coordinator (make-sync-coordinator))
         (transport (make-local-sync-transport coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (registry (make-object-registry))
         (paragraph (make-paragraph :text "local" :registry registry))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (local-operation
          (record-local-operation client-journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text
                                                 :value "local draft")
                                  :timestamp 201))
         (peer-operation
          (make-workspace-operation :id "peer-op-1"
                                    :type :set-slot
                                    :target-id (object-id paragraph)
                                    :payload (list :slot :text
                                                   :value "peer")
                                    :actor-id "peer-user"
                                    :session-id "peer-session"
                                    :timestamp 202
                                    :clock '(("peer-user" . 1)))))
    (is (typep transport 'sync-transport)
        "Local sync transports should implement the transport protocol.")
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (record-operation server-journal peer-operation)
    (let* ((exchange
            (sync-journal-with-transport
             registry
             client-journal
             transport
             auth
             :now 200))
           (request (getf exchange :request))
           (response (getf exchange :response))
           (result (getf exchange :result)))
      (is (equal "workspace-1" (getf request :workspace-id))
          "Transport sync exchanges should expose the submitted request.")
      (is (equal (list (operation-id local-operation))
                 (getf response :acknowledged-operation-ids))
          "Transport sync exchanges should expose the coordinator response.")
      (is (eq :acknowledged
              (journal-operation-queue-status client-journal local-operation))
          "Transport sync exchanges should acknowledge local operations.")
      (is (journal-recorded-operation-p server-journal local-operation)
          "Transport sync exchanges should submit local operations.")
      (is (equal (list paragraph)
                 (getf result :applied-operations))
          "Transport sync exchanges should apply distributed operations.")
      (is (string= "peer" (paragraph-text paragraph))
          "Transport sync exchanges should update local objects."))))

(deftest sync-message-envelope-round-trips-through-string-codec ()
  (let* ((payload (list :workspace-id "workspace-1"
                        :actor-id "local-user"
                        :session-id "session-1"
                        :clock '(("local-user" . 2))
                        :operations nil))
         (message (make-sync-message :request payload))
         (encoded (encode-sync-message message))
         (decoded (decode-sync-message encoded :expected-type :request)))
    (is (equal (list :kind :orra-sync-message
                     :version (sync-message-version)
                     :type :request
                     :payload payload)
               message)
        "Sync messages should expose a stable versioned plist envelope.")
    (is (stringp encoded)
        "Sync message encoding should produce a transport-ready string.")
    (is (equal message decoded)
        "Sync message decoding should round-trip the message envelope.")
    (is (equal payload (sync-message-payload decoded :expected-type :request))
        "Sync message readers should expose the wrapped semantic payload.")))

(deftest sync-message-envelope-rejects-unsupported-messages ()
  (handler-case
      (progn
        (make-sync-message :unknown (list :workspace-id "workspace-1"))
        (error "Expected invalid sync message type to be rejected."))
    (error (condition)
      (is (search "Unsupported sync message type"
                  (princ-to-string condition))
          "Invalid sync message types should produce an explanatory error.")))
  (handler-case
      (progn
        (decode-sync-message
         (with-output-to-string (stream)
           (with-standard-io-syntax
             (prin1 (list :kind :orra-sync-message
                          :version (1+ (sync-message-version))
                          :type :request
                          :payload nil)
                    stream))))
        (error "Expected unsupported sync message versions to be rejected."))
    (error (condition)
      (is (search "Unsupported sync message version"
                  (princ-to-string condition))
          "Unsupported sync message versions should produce an explanatory error."))))

(deftest local-sync-transport-exchanges-versioned-messages ()
  (let* ((coordinator (make-sync-coordinator))
         (transport (make-local-sync-transport coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (operation (record-local-operation client-journal
                                            :set-slot
                                            :target-id "paragraph-1"
                                            :payload (list :slot :text
                                                           :value "draft")
                                            :timestamp 201))
         (request-payload (journal-sync-request-payload client-journal
                                                        :authentication auth))
         (request-message (make-sync-message :request request-payload)))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (let* ((response-message (sync-transport-send-message transport
                                                          request-message
                                                          :now 200))
           (response-payload (sync-message-payload response-message
                                                   :expected-type :response)))
      (is (equal (list (operation-id operation))
                 (getf response-payload :acknowledged-operation-ids))
          "Local transports should return versioned response messages.")
      (is (journal-recorded-operation-p server-journal operation)
          "Local message transports should submit semantic operations."))))

(deftest encoded-sync-message-handler-round-trips-strings ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (operation (record-local-operation client-journal
                                            :set-slot
                                            :target-id "paragraph-1"
                                            :payload (list :slot :text
                                                           :value "draft")
                                            :timestamp 201))
         (request-message
          (make-sync-message
           :request
           (journal-sync-request-payload client-journal
                                         :authentication auth))))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (let* ((encoded-response (handle-encoded-sync-message
                              coordinator
                              (encode-sync-message request-message)
                              :now 200))
           (response-message (decode-sync-message encoded-response
                                                  :expected-type :response))
           (response-payload (sync-message-payload response-message
                                                   :expected-type :response)))
      (is (stringp encoded-response)
          "Encoded sync handlers should return encoded response strings.")
      (is (equal (list (operation-id operation))
                 (getf response-payload :acknowledged-operation-ids))
          "Encoded sync handlers should preserve coordinator responses.")
      (is (journal-recorded-operation-p server-journal operation)
          "Encoded sync handlers should submit semantic operations."))))

(deftest encoded-sync-transport-round-trips-client-state ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (saw-encoded-request nil)
         (transport (make-encoded-sync-transport
                     (lambda (encoded-request &key now)
                       (setf saw-encoded-request (stringp encoded-request))
                       (handle-encoded-sync-message coordinator
                                                    encoded-request
                                                    :now now))))
         (registry (make-object-registry))
         (paragraph (make-paragraph :text "local" :registry registry))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (local-operation
          (record-local-operation client-journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text
                                                 :value "local draft")
                                  :timestamp 201))
         (peer-operation
          (make-workspace-operation :id "peer-op-1"
                                    :type :set-slot
                                    :target-id (object-id paragraph)
                                    :payload (list :slot :text
                                                   :value "peer")
                                    :actor-id "peer-user"
                                    :session-id "peer-session"
                                    :timestamp 202
                                    :clock '(("peer-user" . 1)))))
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "local-user"
                                      :display-name "Local User"
                                      :role :editor
                                      :status :active
                                      :updated-at 199)
    (record-operation server-journal peer-operation)
    (let ((exchange (sync-journal-with-transport registry
                                                 client-journal
                                                 transport
                                                 auth
                                                 :now 200)))
      (is saw-encoded-request
          "Encoded transports should pass encoded request strings to their exchange function.")
      (is (eq :acknowledged
              (journal-operation-queue-status client-journal local-operation))
          "Encoded transport sync should acknowledge local operations.")
      (is (journal-recorded-operation-p server-journal local-operation)
          "Encoded transport sync should submit local operations.")
      (is (equal (list paragraph)
                 (getf (getf exchange :result) :applied-operations))
          "Encoded transport sync should apply distributed operations.")
      (is (string= "peer" (paragraph-text paragraph))
          "Encoded transport sync should update local objects."))))

(deftest encoded-sync-handler-returns-error-for-malformed-message ()
  (let* ((encoded-response (handle-encoded-sync-message
                            (make-sync-coordinator)
                            "not-a-sync-message"
                            :now 200))
         (response-message (decode-sync-message encoded-response
                                                :expected-type :error))
         (response-payload (sync-message-payload response-message
                                                 :expected-type :error)))
    (is (eq :malformed-message (getf response-payload :code))
        "Malformed encoded sync messages should return protocol error codes.")
    (is (stringp (getf response-payload :message))
        "Protocol error responses should carry printable diagnostic text.")
    (is (not (getf response-payload :retryable))
        "Malformed sync messages should not be marked retryable by default.")))

(deftest encoded-sync-handler-returns-error-for-rejected-request ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "unknown-user"
                                                 :session-id "unknown-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "unknown-user"
                                         :session-id "unknown-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300)))
    (record-local-operation client-journal
                            :set-slot
                            :target-id "paragraph-1"
                            :payload (list :slot :text :value "draft")
                            :timestamp 201)
    (let* ((encoded-request
            (encode-sync-message
             (make-sync-message
              :request
              (journal-sync-request-payload client-journal
                                            :authentication auth))))
           (encoded-response (handle-encoded-sync-message
                              coordinator
                              encoded-request
                              :now 200))
           (response-message (decode-sync-message encoded-response
                                                  :expected-type :error))
           (response-payload (sync-message-payload response-message
                                                   :expected-type :error)))
      (is (eq :request-rejected (getf response-payload :code))
          "Rejected encoded sync requests should return protocol error codes.")
      (is (search "unknown-user" (getf response-payload :message))
          "Rejected request errors should retain the coordinator diagnostic.")
      (is (null (journal-operations server-journal))
          "Rejected encoded sync requests should not mutate server operations."))))

(deftest encoded-sync-transport-signals-protocol-errors ()
  (let* ((transport
          (make-encoded-sync-transport
           (lambda (encoded-request &key now)
             (declare (ignore encoded-request now))
             (encode-sync-message
              (make-sync-message
               :error
               (list :code :request-rejected
                     :message "No sync access."
                     :retryable nil))))))
         (request-message (make-sync-message
                           :request
                           (list :workspace-id "workspace-1"
                                 :actor-id "local-user"
                                 :session-id "local-session"))))
    (handler-case
        (progn
          (sync-transport-send-message transport request-message :now 200)
          (is nil "Encoded transports should surface protocol errors."))
      (error (condition)
        (is (search "request-rejected"
                    (string-downcase (princ-to-string condition)))
            "Encoded transport protocol errors should identify the remote error code.")))))

(deftest sync-response-payload-rejects-other-workspaces ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (paragraph (make-paragraph :text "local" :registry registry))
         (local-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id (object-id paragraph)
                                  :payload (list :slot :text :value "local draft")))
         (payload
          (list :workspace-id "workspace-2"
                :acknowledged-operation-ids
                (list (operation-id local-operation))
                :members
                (list (list :actor-id "peer-user"
                            :display-name "Wrong Workspace"
                            :role :editor
                            :status :active
                            :updated-at 100)))))
    (handler-case
        (progn
          (apply-sync-response-payload registry journal payload)
          (is nil "Sync responses from another workspace should not apply."))
      (error (condition)
        (is (search "workspace-2" (princ-to-string condition))
            "Sync response workspace mismatch errors should identify the payload workspace.")))
    (is (eq :pending
            (journal-operation-queue-status journal local-operation))
        "Rejected sync responses should not mutate local queue state.")
    (is (null (journal-members journal))
        "Rejected sync responses should not mutate local membership state.")))

(defun slot-metadata-test-summary-default (object slot)
  (declare (ignore slot))
  (format nil "summary: ~A" (paragraph-text object)))

(defun slot-metadata-test-display-name (object slot)
  (declare (ignore slot))
  (format nil "~A ~A"
          (object-property object :first-name :default "")
          (object-property object :last-name :default "")))

(defvar *slot-metadata-test-display-name-computations* 0)
(defvar *slot-metadata-test-initials-computations* 0)

(defun slot-metadata-test-counted-display-name (object slot)
  (incf *slot-metadata-test-display-name-computations*)
  (slot-metadata-test-display-name object slot))

(defun slot-metadata-test-counted-initials (object slot)
  (declare (ignore slot))
  (incf *slot-metadata-test-initials-computations*)
  (format nil "~A~A"
          (char (object-property object :first-name :default "?") 0)
          (char (object-property object :last-name :default "?") 0)))

(deftest slot-metadata-defaults-participate-in-property-lookup ()
  (let* ((registry (make-object-registry))
         (prototype (make-paragraph :text "proto" :registry registry))
         (child (make-paragraph :text "child" :registry registry)))
    (setf (object-prototype child) prototype)
    (set-object-slot-metadata prototype :font :default :serif)
    (set-object-slot-metadata child :font :label "Font")
    (set-object-slot-metadata child
                              :summary
                              :default-function
                              'slot-metadata-test-summary-default)
    (is (string= "Font" (object-slot-metadata child :font :label))
        "Local slot metadata should be visible on the child object.")
    (is (eql :serif (object-property child :font))
        "Inherited slot defaults should remain visible when the child has other metadata for the same slot.")
    (is (string= "summary: child" (object-property child :summary))
        "Dynamic slot defaults should be computed against the requesting object.")
    (set-object-property prototype :font :mono)
    (is (eql :mono (object-property child :font))
        "Inherited explicit properties should override inherited slot defaults.")
    (set-object-property child :font :sans)
    (is (eql :sans (object-property child :font))
        "Local explicit properties should override all defaults.")
    (is (eql :fallback (object-property child :missing :default :fallback))
        "Explicit lookup defaults should still work when no slot default exists.")))

(deftest slot-metadata-persists-through-workspace-save ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (paragraph (make-paragraph :text "metadata target"
                                    :registry registry)))
    (set-object-slot-metadata paragraph :title :label "Title")
    (set-object-slot-metadata paragraph :title :default "Untitled")
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "metadata target"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (string= "Title"
                            (object-slot-metadata loaded-paragraph
                                                  :title
                                                  :label))
                   "Slot metadata should survive persistence.")
               (is (string= "Untitled"
                            (object-property loaded-paragraph :title))
                   "Persisted slot defaults should participate in property lookup.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest computed-slots-participate-in-property-lookup ()
  (let* ((registry (make-object-registry))
         (prototype (make-paragraph :text "proto" :registry registry))
         (child (make-paragraph :text "child" :registry registry)))
    (setf (object-prototype child) prototype)
    (set-object-property child :first-name "Ada")
    (set-object-property child :last-name "Lovelace")
    (set-object-computed-slot prototype
                              :display-name
                              'slot-metadata-test-display-name
                              :depends-on '(:first-name :last-name))
    (is (string= "Ada Lovelace"
                 (object-property child :display-name))
        "Inherited computed slots should run against the requesting object.")
    (set-object-property child :last-name "Byron")
    (is (string= "Ada Byron"
                 (object-property child :display-name))
        "Computed slots should reflect dependency changes without stale values.")
    (is (equal '(:first-name :last-name)
               (object-slot-metadata child
                                     :display-name
                                     :depends-on))
        "Computed slot dependency metadata should be queryable.")
    (set-object-slot-metadata prototype
                              :display-name
                              :default
                              "Unnamed")
    (is (string= "Ada Byron"
                 (object-property child :display-name))
        "Computed slots should take precedence over slot defaults.")
    (set-object-property child :display-name "Countess")
    (is (string= "Countess"
                 (object-property child :display-name))
        "Explicit properties should override computed slots.")))

(deftest computed-slots-persist-through-workspace-save ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (paragraph (make-paragraph :text "metadata target"
                                    :registry registry)))
    (set-object-property paragraph :first-name "Grace")
    (set-object-property paragraph :last-name "Hopper")
    (set-object-computed-slot paragraph
                              :display-name
                              'slot-metadata-test-display-name
                              :depends-on '(:first-name :last-name))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "metadata target"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (string= "Grace Hopper"
                            (object-property loaded-paragraph
                                             :display-name))
                   "Persisted computed slots should participate in property lookup.")
               (is (equal '(:first-name :last-name)
                          (object-slot-metadata loaded-paragraph
                                                :display-name
                                                :depends-on))
                   "Computed slot dependency metadata should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest cached-computed-slots-invalidate-selectively ()
  (let* ((registry (make-object-registry))
         (paragraph (make-paragraph :text "target" :registry registry)))
    (setf *slot-metadata-test-display-name-computations* 0)
    (setf *slot-metadata-test-initials-computations* 0)
    (set-object-property paragraph :first-name "Ada")
    (set-object-property paragraph :last-name "Lovelace")
    (set-object-computed-slot paragraph
                              :display-name
                              'slot-metadata-test-counted-display-name
                              :depends-on '(:first-name :last-name)
                              :cached t)
    (set-object-computed-slot paragraph
                              :initials
                              'slot-metadata-test-counted-initials
                              :depends-on '(:first-name :last-name)
                              :cached t)
    (is (string= "Ada Lovelace"
                 (object-property paragraph :display-name))
        "Cached computed slots should compute on first read.")
    (is (string= "Ada Lovelace"
                 (object-property paragraph :display-name))
        "Cached computed slots should reuse the cached value on repeated reads.")
    (is (= 1 *slot-metadata-test-display-name-computations*)
        "Repeated reads should not recompute an unchanged cached slot.")
    (is (string= "AL" (object-property paragraph :initials))
        "A second cached computed slot should compute independently.")
    (set-object-property paragraph :unrelated t)
    (is (string= "Ada Lovelace"
                 (object-property paragraph :display-name))
        "Unrelated property changes should not invalidate cached slots.")
    (is (= 1 *slot-metadata-test-display-name-computations*)
        "Unrelated property changes should preserve cached values.")
    (set-object-property paragraph :last-name "Byron")
    (is (string= "Ada Byron"
                 (object-property paragraph :display-name))
        "Changing a declared dependency should invalidate and recompute the slot.")
    (is (= 2 *slot-metadata-test-display-name-computations*)
        "Dependency changes should recompute affected cached slots once.")
    (is (= 1 *slot-metadata-test-initials-computations*)
        "Dependency changes should not recompute other cached slots until they are read.")
    (is (string= "AB" (object-property paragraph :initials))
        "Other affected cached slots should recompute lazily when read.")
    (is (= 2 *slot-metadata-test-initials-computations*)
        "Each affected cached slot should be invalidated independently.")))

(deftest computed-slot-cache-is-transient ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (paragraph (make-paragraph :text "metadata target"
                                    :registry registry)))
    (setf *slot-metadata-test-display-name-computations* 0)
    (set-object-property paragraph :first-name "Grace")
    (set-object-property paragraph :last-name "Hopper")
    (set-object-computed-slot paragraph
                              :display-name
                              'slot-metadata-test-counted-display-name
                              :depends-on '(:first-name :last-name)
                              :cached t)
    (is (string= "Grace Hopper"
                 (object-property paragraph :display-name))
        "Cached computed slot should compute before persistence.")
    (is (= 1 *slot-metadata-test-display-name-computations*)
        "The pre-save read should populate the transient cache.")
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (setf *slot-metadata-test-display-name-computations* 0)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "metadata target"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (string= "Grace Hopper"
                            (object-property loaded-paragraph
                                             :display-name))
                   "Loaded computed slots should still compute through metadata.")
               (is (= 1 *slot-metadata-test-display-name-computations*)
                   "Computed slot cache values should not be persisted.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest notebook-to-cell-tree ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :title "N" :registry registry))
         (section (make-section :title "S" :registry registry))
         (paragraph (make-paragraph :text "hello" :registry registry))
         (tree nil))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (setf tree (build-workspace-cell-tree workspace registry))
    (is (= 2 (length (children-of tree)))
        "Workspace view should render header and notebook.")
    (is (typep (second (children-of tree)) 'container-cell)
        "Notebook cell should be a container.")))

(deftest rich-notebook-nodes-render ()
  (let ((application (make-application :backend (make-null-backend))))
    (invoke-command application 'append-quote-block "Lisp is a medium." "Orra")
    (invoke-command application 'append-list-block '("one" "two") t)
    (invoke-command application
                    'append-table-block
                    '("Name" "Value")
                    '(("language" "Common Lisp")))
    (invoke-command application
                    'append-task-list
                    (list (make-task-item :text "ship notebook nodes"
                                          :done t)
                          "write object reference lens"))
    (let ((focused (focused-model application)))
      (invoke-command application
                      'append-reference-block
                      (object-id focused)
                      "current focus"
                      "object-native link")
      (render-application application)
      (let ((texts (collect-text-cells (application-root-cell application))))
        (is (find (format nil "> Lisp is a medium.~%-- Orra")
                  texts
                  :test #'string=)
            "Quote blocks should render quote text and attribution.")
        (is (find "1. one" texts :test #'string=)
            "Ordered list blocks should render numbered items.")
        (is (find "Name | Value" texts :test #'string=)
            "Table blocks should render column headings.")
        (is (find "[x] ship notebook nodes" texts :test #'string=)
            "Task lists should render completed tasks.")
        (is (some (lambda (text)
                    (and (search "@ current focus ->" text)
                         (search (object-id focused) text)
                         (search "object-native link" text)))
                  texts)
            "Reference blocks should render their target object and note.")))))

(deftest inspector-block-renders-target-object-details ()
  (let* ((application (make-application :backend (make-null-backend)))
         (target (focused-model application)))
    (invoke-command application
                    'append-inspector-block
                    (object-id target)
                    "focused object")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Inspector: focused object" texts :test #'string=)
          "Embedded inspector blocks should render a notebook-local heading.")
      (is (find (format nil "object: ~A" (object-summary-string target))
                texts
                :test #'string=)
          "Embedded inspector blocks should describe the target object.")
      (is (find "text-len: 50" texts :test #'string=)
          "Embedded inspector blocks should render target-specific inspection lines."))))

(defun source-browser-test-target (value)
  "Documentation for source browser tests."
  (+ value 10))

(deftest source-browser-block-renders-live-symbol-details ()
  (let* ((application (make-application :backend (make-null-backend)))
         (browser (invoke-command application
                                  'append-source-browser-block
                                  "ORRA.TESTS"
                                  "SOURCE-BROWSER-TEST-TARGET"
                                  "test source")))
    (is (typep browser 'source-browser-block)
        "Appending a source browser should create a source browser block.")
    (is (string= "ORRA.TESTS" (source-browser-block-package browser))
        "Source browsers should retain the target package name.")
    (is (string= "SOURCE-BROWSER-TEST-TARGET"
                 (source-browser-block-symbol browser))
        "Source browsers should retain the target symbol name.")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Source: test source" texts :test #'string=)
          "Source browsers should render their notebook-local heading.")
      (is (find "symbol: ORRA.TESTS::SOURCE-BROWSER-TEST-TARGET"
                texts
                :test #'string=)
          "Source browsers should resolve and render their target symbol.")
      (is (find "function: yes" texts :test #'string=)
          "Source browsers should report function bindings.")
      (is (find "documentation: Documentation for source browser tests."
                texts
                :test #'string=)
          "Source browsers should render function documentation.")
      (is (some (lambda (text)
                  (search "source:" text))
                texts)
          "Source browsers should expose source availability information."))))

(deftest source-browser-block-persists-target ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (browser (make-source-browser-block
                   :package-name "ORRA.TESTS"
                   :symbol-name "SOURCE-BROWSER-TEST-TARGET"
                   :label "saved source"
                   :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section browser)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-browser
                     (find-if (lambda (object)
                                (typep object 'source-browser-block))
                              (children-of loaded-section))))
               (is (string= "ORRA.TESTS"
                            (source-browser-block-package loaded-browser))
                   "Source browser package names should survive persistence.")
               (is (string= "SOURCE-BROWSER-TEST-TARGET"
                            (source-browser-block-symbol loaded-browser))
                   "Source browser symbol names should survive persistence.")
               (is (string= "saved source"
                            (source-browser-block-label loaded-browser))
                   "Source browser labels should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest cross-reference-browser-block-renders-workspace-references ()
  (let* ((application (make-application :backend (make-null-backend)))
         (first-reference
          (invoke-command
           application
           'append-code-block
           "(orra.tests::source-browser-test-target 41)"))
         (second-reference
          (invoke-command
           application
           'append-code-block
           "(mapcar #'orra.tests::source-browser-test-target '(1 2))"))
         (browser
          (invoke-command application
                          'append-cross-reference-browser-block
                          "ORRA.TESTS"
                          "SOURCE-BROWSER-TEST-TARGET"
                          "test xrefs")))
    (is (typep browser 'cross-reference-browser-block)
        "Appending a cross-reference browser should create a browser block.")
    (is (string= "ORRA.TESTS"
                 (cross-reference-browser-block-package browser))
        "Cross-reference browsers should retain the target package name.")
    (is (string= "SOURCE-BROWSER-TEST-TARGET"
                 (cross-reference-browser-block-symbol browser))
        "Cross-reference browsers should retain the target symbol name.")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "XRef: test xrefs" texts :test #'string=)
          "Cross-reference browsers should render their notebook-local heading.")
      (is (find "target: ORRA.TESTS::SOURCE-BROWSER-TEST-TARGET"
                texts
                :test #'string=)
          "Cross-reference browsers should render the target symbol.")
      (is (find "references: 2" texts :test #'string=)
          "Cross-reference browsers should count matching code references.")
      (is (some (lambda (text)
                  (and (search (object-id first-reference) text)
                       (search "orra.tests::source-browser-test-target"
                               text)))
                texts)
          "Cross-reference browsers should list the first referencing block.")
      (is (some (lambda (text)
                  (and (search (object-id second-reference) text)
                       (search "mapcar" text)))
                texts)
          "Cross-reference browsers should list the second referencing block."))))

(deftest cross-reference-browser-block-persists-target ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (browser (make-cross-reference-browser-block
                   :package-name "ORRA.TESTS"
                   :symbol-name "SOURCE-BROWSER-TEST-TARGET"
                   :label "saved xrefs"
                   :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section browser)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-browser
                     (find-if (lambda (object)
                                (typep object 'cross-reference-browser-block))
                              (children-of loaded-section))))
               (is (string= "ORRA.TESTS"
                            (cross-reference-browser-block-package
                             loaded-browser))
                   "Cross-reference browser package names should survive persistence.")
               (is (string= "SOURCE-BROWSER-TEST-TARGET"
                            (cross-reference-browser-block-symbol
                             loaded-browser))
                   "Cross-reference browser symbol names should survive persistence.")
               (is (string= "saved xrefs"
                            (cross-reference-browser-block-label
                             loaded-browser))
                   "Cross-reference browser labels should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest stack-frame-browser-block-renders-error-result-frames ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "de"))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block)))
         (browser (invoke-command application
                                  'append-stack-frame-browser-block
                                  (object-id result)
                                  "last error")))
    (is (typep browser 'stack-frame-browser-block)
        "Appending a stack frame browser should create a browser block.")
    (is (eq result (stack-frame-browser-block-target browser))
        "Stack frame browsers should retain the target result block.")
    (is (string= "last error" (stack-frame-browser-block-label browser))
        "Stack frame browsers should retain their notebook-local label.")
    (is (getf (result-block-environment result) :stack-frames)
        "Error results should record stack frame metadata.")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Stack: last error" texts :test #'string=)
          "Stack frame browsers should render their heading.")
      (is (find (format nil "target: ~A" (object-summary-string result))
                texts
                :test #'string=)
          "Stack frame browsers should render the target result.")
      (is (find "status: ERROR" texts :test #'string=)
          "Stack frame browsers should render the target result status.")
      (is (some (lambda (text)
                  (search "condition:" text))
                texts)
          "Stack frame browsers should render the captured condition.")
      (is (some (lambda (text)
                  (search "frame 0:" text))
                texts)
          "Stack frame browsers should render at least one frame line."))))

(deftest stack-frame-browser-block-persists-target ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (result (make-result-block
                  :presentation "Evaluation error: boom"
                  :input-source "(error \"boom\")"
                  :input-forms '((error "boom"))
                  :package-name "ORRA.TESTS"
                  :evaluated-at 123
                  :environment
                  (list :condition-type "SIMPLE-ERROR"
                        :condition-message "boom"
                        :stack-frames
                        (list (list :index 0
                                    :function "EVALUATE-FORMS"
                                    :summary "boom")))
                  :registry registry))
         (browser (make-stack-frame-browser-block
                   :target result
                   :label "saved stack"
                   :registry registry)))
    (set-result-block-status result :error)
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section browser)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-browser
                     (find-if (lambda (object)
                                (typep object 'stack-frame-browser-block))
                              (children-of loaded-section)))
                    (loaded-result
                     (stack-frame-browser-block-target loaded-browser)))
               (is (typep loaded-result 'result-block)
                   "Stack frame browser targets should reload as result blocks.")
               (is (string= "saved stack"
                            (stack-frame-browser-block-label loaded-browser))
                   "Stack frame browser labels should survive persistence.")
               (is (eq :error (result-block-status loaded-result))
                   "Target result status should survive persistence.")
               (is (equal (list (list :index 0
                                      :function "EVALUATE-FORMS"
                                      :summary "boom"))
                          (getf (result-block-environment loaded-result)
                                :stack-frames))
                   "Stack frame metadata should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest condition-browser-block-renders-error-restarts ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "de"))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block)))
         (browser (invoke-command application
                                  'append-condition-browser-block
                                  (object-id result)
                                  "last condition"))
         (restarts (getf (result-block-environment result) :restart-options)))
    (is (typep browser 'condition-browser-block)
        "Appending a condition browser should create a browser block.")
    (is (eq result (condition-browser-block-target browser))
        "Condition browsers should retain the target result block.")
    (is (string= "last condition" (condition-browser-block-label browser))
        "Condition browsers should retain their notebook-local label.")
    (is restarts
        "Error results should record restart metadata.")
    (is (find "ABORT-EVALUATION" restarts
              :key (lambda (restart)
                     (getf restart :name))
              :test #'string=)
        "Evaluation errors should expose the Orra abort-evaluation restart.")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Condition: last condition" texts :test #'string=)
          "Condition browsers should render their heading.")
      (is (find (format nil "target: ~A" (object-summary-string result))
                texts
                :test #'string=)
          "Condition browsers should render the target result.")
      (is (find "status: ERROR" texts :test #'string=)
          "Condition browsers should render the target result status.")
      (is (some (lambda (text)
                  (search "condition: UNBOUND-VARIABLE" text))
                texts)
          "Condition browsers should render the captured condition.")
      (is (some (lambda (text)
                  (and (search "restart " text)
                       (search "ABORT-EVALUATION" text)))
                texts)
          "Condition browsers should render captured restart options."))))

(deftest condition-browser-block-persists-target ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (result (make-result-block
                  :presentation "Evaluation error: boom"
                  :input-source "(error \"boom\")"
                  :input-forms '((error "boom"))
                  :package-name "ORRA.TESTS"
                  :evaluated-at 123
                  :environment
                  (list :condition-type "SIMPLE-ERROR"
                        :condition-message "boom"
                        :restart-options
                        (list (list :index 0
                                    :name "ABORT-EVALUATION"
                                    :description
                                    "Abort evaluation and store an error result.")))
                  :registry registry))
         (browser (make-condition-browser-block
                   :target result
                   :label "saved condition"
                   :registry registry)))
    (set-result-block-status result :error)
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section browser)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-browser
                     (find-if (lambda (object)
                                (typep object 'condition-browser-block))
                              (children-of loaded-section)))
                    (loaded-result
                     (condition-browser-block-target loaded-browser)))
               (is (typep loaded-result 'result-block)
                   "Condition browser targets should reload as result blocks.")
               (is (string= "saved condition"
                            (condition-browser-block-label loaded-browser))
                   "Condition browser labels should survive persistence.")
               (is (eq :error (result-block-status loaded-result))
                   "Target result status should survive persistence.")
               (is (equal (list (list :index 0
                                      :name "ABORT-EVALUATION"
                                      :description
                                      "Abort evaluation and store an error result."))
                          (getf (result-block-environment loaded-result)
                                :restart-options))
                   "Restart metadata should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest repl-block-evaluates-and-renders-transcript ()
  (let* ((application (make-application :backend (make-null-backend)))
         (repl (invoke-command application 'append-repl-block "Image REPL"))
         (entry (invoke-command application
                                'evaluate-repl-entry
                                (object-id repl)
                                "(+ 1 2)"))
         (result (repl-entry-result entry)))
    (is (typep repl 'repl-block)
        "Appending a REPL should create a notebook-local REPL block.")
    (is (eq entry (first (children-of repl)))
        "Evaluated REPL entries should be appended to the transcript.")
    (is (string= "(+ 1 2)" (repl-entry-input-source entry))
        "REPL entries should retain the submitted source.")
    (is (typep result 'result-block)
        "REPL entries should own normal result blocks.")
    (is (eq :ok (result-block-status result))
        "Successful REPL evaluation should produce an OK result.")
    (is (string= "3" (result-block-presentation result))
        "REPL evaluation should render the final value.")
    (is (string= (package-name *package*) (result-block-package result))
        "REPL results should record the evaluation package.")
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "REPL: Image REPL [COMMON-LISP-USER]" texts :test #'string=)
          "REPL blocks should render a transcript heading.")
      (is (find "COMMON-LISP-USER> (+ 1 2)" texts :test #'string=)
          "REPL entries should render their prompt and input.")
      (is (find "=> 3" texts :test #'string=)
          "REPL entries should render through the shared result lens."))))

(deftest repl-block-persists-transcript ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (repl (make-repl-block :title "Saved REPL"
                                :package-name "COMMON-LISP-USER"
                                :registry registry))
         (entry (make-repl-entry :input-source "(+ 2 3)"
                                 :registry registry))
         (result (make-result-block :value 5
                                    :presentation "5"
                                    :input-source "(+ 2 3)"
                                    :input-forms '((+ 2 3))
                                    :package-name "COMMON-LISP-USER"
                                    :evaluated-at 123
                                    :registry registry)))
    (setf (repl-entry-result entry) result)
    (set-result-block-status result :ok)
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section repl)
    (append-child repl entry)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-repl
                     (find-if (lambda (object)
                                (typep object 'repl-block))
                              (children-of loaded-section)))
                    (loaded-entry (first (children-of loaded-repl)))
                    (loaded-result (repl-entry-result loaded-entry)))
               (is (string= "Saved REPL" (repl-block-title loaded-repl))
                   "REPL titles should survive persistence.")
               (is (string= "(+ 2 3)"
                            (repl-entry-input-source loaded-entry))
                   "REPL entry source should survive persistence.")
               (is (string= "5"
                            (result-block-presentation loaded-result))
                   "REPL result presentation should survive persistence.")
               (is (equal '((+ 2 3))
                          (result-block-input-forms loaded-result))
                   "REPL result metadata should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest rich-notebook-nodes-persist ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (paragraph (make-paragraph :text "target" :registry registry))
         (quote (make-quote-block :text "quoted" :attribution "source"
                                  :registry registry))
         (reference (make-reference-block :target paragraph
                                          :label "target paragraph"
                                          :note "same workspace"
                                          :registry registry))
         (inspector (make-inspector-block :target paragraph
                                          :label "inspect target"
                                          :registry registry))
         (list-block (make-list-block :items '("alpha" "beta")
                                      :ordered-p t
                                      :registry registry))
         (table (make-table-block :columns '("A" "B")
                                  :rows '(("1" "2"))
                                  :registry registry))
         (tasks (make-task-list
                 :items (list (make-task-item :text "done" :done t)
                              (make-task-item :text "todo"))
                 :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (dolist (node (list paragraph quote reference inspector list-block table tasks))
      (append-child section node))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace path :registry registry)
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (children (children-of loaded-section))
                    (loaded-reference
                     (find-if (lambda (object)
                                (typep object 'reference-block))
                              children))
                    (loaded-inspector
                     (find-if (lambda (object)
                                (typep object 'inspector-block))
                              children))
                    (loaded-tasks
                     (find-if (lambda (object)
                                (typep object 'task-list))
                              children)))
               (is (find-if (lambda (object)
                              (and (typep object 'quote-block)
                                   (string= "quoted"
                                            (quote-block-text object))))
                            children)
                   "Quote blocks should survive persistence.")
               (is (typep (reference-block-target loaded-reference)
                          'paragraph)
                   "Reference block targets should reload as objects.")
               (is (typep (inspector-block-target loaded-inspector)
                          'paragraph)
                   "Inspector block targets should reload as objects.")
               (is (string= "inspect target"
                            (inspector-block-label loaded-inspector))
                   "Inspector block labels should survive persistence.")
               (is (equal '("alpha" "beta")
                          (list-block-items
                           (find-if (lambda (object)
                                      (typep object 'list-block))
                                    children)))
                   "List block items should survive persistence.")
               (is (equal '(("1" "2"))
                          (table-block-rows
                           (find-if (lambda (object)
                                      (typep object 'table-block))
                                    children)))
                   "Table block rows should survive persistence.")
               (is (= 1
                      (count-if #'task-item-done-p
                                (task-list-items loaded-tasks)))
                   "Task completion state should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest reference-block-registration-handles-cycles ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (reference (make-reference-block :target workspace
                                          :label "root"
                                          :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section reference)
    (orra::register-tree registry workspace)
    (is (eq workspace (find-object registry (object-id workspace)))
        "Registering a tree with a backward object reference should terminate.")
    (is (eq reference (find-object registry (object-id reference)))
        "Reference blocks should still be registered when cycles are skipped.")))

(deftest application-shell-includes-inspector ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (is (= 3 (length (children-of (application-root-cell application))))
        "Application shell should render workspace, inspector, and status.")
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Inspector" texts :test #'string=)
          "Inspector header should be present.")
      (is (find (format nil "focused: ~A"
                        (format nil "~A ~A"
                                (object-kind (focused-model application))
                                (object-id (focused-model application))))
                texts
                :test #'string=)
          "Inspector should describe the focused model."))))

(deftest code-block-structure-preview-renders ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Parse OK  |  1 top-level form" texts :test #'string=)
          "Code blocks should render a parse status line.")
      (is (find "Selected form 1/1  |  list (3 items)" texts :test #'string=)
          "Code blocks should render the current selected top-level form.")
      (is (find "Structure" texts :test #'string=)
          "Code blocks with structure enabled should render a structure heading.")
      (is (find "> form 1: list (3 items)" texts :test #'string=)
          "Code blocks should render a structural summary for parsed forms."))))

(deftest inspector-describes-code-block-parse-state ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (setf (orra::application-focused-model-id application)
          (object-id
           (find-first-node-of-type
            (application-workspace application)
            'code-block)))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "parse: Parse OK  |  1 top-level form" texts :test #'string=)
          "Inspector should describe code-block parse status.")
      (is (find "structure: visible" texts :test #'string=)
          "Inspector should report whether structure preview is visible."))))

(deftest focus-navigation ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (is (typep (focused-model application) 'paragraph)
        "Initial focus should prefer the first editable model.")
    (let ((first-focus (object-id (focused-model application))))
      (focus-next-model application)
      (is (not (string= first-focus
                        (object-id (focused-model application))))
          "Focus should advance to a different object.")
      (focus-previous-model application)
      (is (string= first-focus
                   (object-id (focused-model application)))
          "Focus should move back to the original object."))))

(deftest click-focused-editable-model-starts-editing ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (let* ((focused (focused-model application))
           (cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id focused)))
           (bounds (cell-bounds cell)))
      (orra::focus-model-at-pixel application
                                  (orra::bounds-x bounds)
                                  (orra::bounds-y bounds)))
    (is (editing-active-p application)
        "Clicking the already-focused paragraph should begin editing.")))

(deftest multiline-paragraph-renders-with-multiple-rows ()
  (let ((application (make-application :backend (make-null-backend))))
    (setf (paragraph-text (focused-model application))
          (format nil "alpha~%beta"))
    (render-application application)
    (let* ((cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id (focused-model application))))
           (bounds (cell-bounds cell)))
      (is (= 2 (orra::bounds-height bounds))
          "Multiline paragraph cells should reserve one row per line."))))

(deftest clicking-focused-multiline-cell-places-cursor ()
  (let ((application (make-application :backend (make-null-backend))))
    (setf (paragraph-text (focused-model application))
          (format nil "alpha~%beta"))
    (render-application application)
    (let* ((cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id (focused-model application))))
           (bounds (cell-bounds cell)))
      (orra::focus-model-at-pixel application
                                  (+ (orra::bounds-x bounds) 2)
                                  (+ (orra::bounds-y bounds) 1))
      (is (editing-active-p application)
          "Clicking focused multiline text should begin editing.")
      (is (= 7 (text-buffer-cursor
                (orra::application-active-text-buffer application)))
          "Cursor placement should respect the clicked line and column."))))

(deftest focused-editable-cell-renders-prospective-caret ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend)))
    (render-application application)
    (let* ((model (focused-model application))
           (cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id model)))
           (caret (find-frame-operation backend
                                        :caret
                                        (lambda (operation)
                                          (not (fourth operation))))))
      (is caret
          "Focused editable cells should show the insertion caret before editing starts.")
      (is (= (second caret)
             (+ (orra::bounds-x (cell-bounds cell))
                1
                (length (paragraph-text model))))
          "The prospective caret should render immediately after its text prefix."))))

(deftest long-line-editing-keeps-cursor-visible ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend))
         (content (with-output-to-string (stream)
                    (loop for index from 0 below 80
                          do (format stream "~2,'0D" index)))))
    (setf (paragraph-text (focused-model application)) content)
    (begin-editing-focused-model application)
    (render-application application)
    (let ((*application* application))
      (let* ((cell (find-text-cell-for-model (application-root-cell application)
                                             (object-id (focused-model application))))
             (bounds (cell-bounds cell))
             (visible-line (first (orra::visible-lines-for-cell cell)))
             (caret (find-frame-operation backend
                                          :caret
                                          (lambda (operation)
                                            (fourth operation)))))
        (is (not (search "|" visible-line))
            "Caret rendering should not mutate visible text.")
        (is caret
            "Editing should draw an active caret operation.")
        (is (< (second caret)
               (+ (orra::bounds-x bounds)
                  (orra::bounds-width bounds)))
            "The active caret should stay horizontally inside the cell.")))))

(deftest active-text-selection-renders-highlight ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend)))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-home application)
    (move-active-buffer-cursor-right application :extend-selection t)
    (move-active-buffer-cursor-right application :extend-selection t)
    (render-application application)
    (let* ((cell (find-text-cell-for-model (application-root-cell application)
                                           (object-id (focused-model application))))
           (selection (find-frame-operation backend :selection)))
      (is selection
          "Active text selections should render a highlight operation.")
      (is (equal (list (+ (orra::bounds-x (cell-bounds cell)) 1)
                       (+ (orra::bounds-y (cell-bounds cell)) 1)
                       2)
                 (rest selection))
          "Selection highlights should cover the selected text range."))))

(deftest multiline-text-selection-highlights-line-break ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend))
         (model (focused-model application)))
    (setf (paragraph-text model) (format nil "ab~%cd"))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-up application)
    (move-active-buffer-cursor-home application)
    (move-active-buffer-cursor-right application :extend-selection t)
    (move-active-buffer-cursor-right application :extend-selection t)
    (move-active-buffer-cursor-right application :extend-selection t)
    (render-application application)
    (let ((selection (find-frame-operation backend :selection)))
      (is selection
          "Multiline selection should render a highlight operation.")
      (is (= 3 (fourth selection))
          "Selecting through a line break should highlight a trailing cell."))))

(deftest viewport-scroll-renders-offscreen-content ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)
                                     :layout-height 14))
         (application (make-application :backend backend))
         (bottom-text "bottom marker")
         (bottom-node nil))
    (loop for index from 0 below 20
          do (invoke-command application
                             'append-paragraph
                             (format nil "filler ~D" index)))
    (setf bottom-node
          (invoke-command application 'append-paragraph bottom-text))
    (render-application application)
    (is (find-frame-operation backend :scrollbar)
        "Overflowing layouts should render a scrollbar.")
    (is (null (find-frame-operation
               backend
               :text
               (lambda (operation)
                 (string= (fourth operation) bottom-text))))
        "Offscreen content should not render before scrolling.")
    (let ((cell (find-text-cell-for-model (application-root-cell application)
                                          (object-id bottom-node))))
      (scroll-application application
                          (orra::bounds-y (cell-bounds cell))))
    (render-application application)
    (is (plusp (application-viewport-y application))
        "Scrolling should update the logical viewport.")
    (is (find-frame-operation
         backend
         :text
         (lambda (operation)
           (string= (fourth operation) bottom-text)))
        "Scrolled content should render once it enters the viewport.")))

(deftest focus-navigation-scrolls-focused-cell-into-view ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)
                                     :layout-height 14))
         (application (make-application :backend backend)))
    (loop for index from 0 below 20
          do (invoke-command application
                             'append-paragraph
                             (format nil "filler ~D" index)))
    (render-application application)
    (loop repeat 24
          do (focus-next-model application))
    (is (plusp (application-viewport-y application))
        "Focus navigation should scroll the focused cell into view.")))

(deftest edit-history-persists-across-editing-sessions ()
  (let* ((application (make-application :backend (make-null-backend)))
         (original (paragraph-text (focused-model application))))
    (begin-editing-focused-model application)
    (insert-into-active-buffer application " More")
    (stop-editing application)
    (begin-editing-focused-model application)
    (undo-active-buffer-edit application)
    (is (string= original (paragraph-text (focused-model application)))
        "Undo history should survive leaving and re-entering the same editable model.")
    (redo-active-buffer-edit application)
    (is (string= (concatenate 'string original " More")
                 (paragraph-text (focused-model application)))
        "Redo history should survive leaving and re-entering the same editable model.")))

(deftest toggle-code-structure-command ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (find-first-node-of-type
                 (application-workspace application)
                 'code-block)))
    (render-application application)
    (is (code-block-structure-visible-p block)
        "Scratch code blocks should start with structure preview enabled.")
    (invoke-command application 'toggle-code-structure (object-id block))
    (is (not (code-block-structure-visible-p block))
        "Toggle command should hide the structural preview.")
    (is (not (find "Structure"
                   (collect-text-cells (application-root-cell application))
                   :test #'string=))
        "Hidden structural previews should disappear from the rendered cell tree.")
    (invoke-command application 'toggle-code-structure (object-id block))
    (is (code-block-structure-visible-p block)
        "Toggle command should restore the structural preview.")))

(deftest code-form-selection-navigation ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                (format nil "(list :alpha)~%(+ 20 22)"))))
    (render-application application)
    (is (= 0 (code-block-selected-form-index block))
        "Multi-form code blocks should default to the first top-level form.")
    (invoke-command application 'select-next-code-form (object-id block))
    (is (= 1 (code-block-selected-form-index block))
        "Next-form navigation should advance the structural selection.")
    (invoke-command application 'select-next-code-form (object-id block))
    (is (= 1 (code-block-selected-form-index block))
        "Next-form navigation should clamp at the last top-level form.")
    (invoke-command application 'select-previous-code-form (object-id block))
    (is (= 0 (code-block-selected-form-index block))
        "Previous-form navigation should move back to the earlier form.")
    (invoke-command application 'select-previous-code-form (object-id block))
    (is (= 0 (code-block-selected-form-index block))
        "Previous-form navigation should clamp at the first top-level form.")))

(deftest code-form-selection-renders-and-reaches-inspector ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                (format nil "(list :alpha)~%(+ 20 22)"))))
    (set-object-property block :show-structure t)
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Selected form 2/2  |  list (3 items)" texts :test #'string=)
          "Code blocks should render the current top-level structural selection.")
      (is (find "> form 2: list (3 items)" texts :test #'string=)
          "The structure preview should visibly mark the selected top-level form.")
      (is (find "selection: Selected form 2/2  |  list (3 items)"
                texts
                :test #'string=)
          "The inspector should describe the selected top-level form.")
      (is (find "selected-form-index: 1" texts :test #'string=)
          "The inspector should show the selected form index."))))

(deftest nested-code-form-selection-navigation ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (is (equal '(0) (code-block-selected-form-path block))
        "Code blocks should default to selecting the first top-level form path.")
    (invoke-command application 'select-child-code-form (object-id block))
    (is (equal '(0 0) (code-block-selected-form-path block))
        "Descending should select the first child of the current form.")
    (invoke-command application 'select-next-code-form (object-id block))
    (is (equal '(0 1) (code-block-selected-form-path block))
        "Sibling navigation should operate at the current selection depth.")
    (invoke-command application 'select-child-code-form (object-id block))
    (is (equal '(0 1 0) (code-block-selected-form-path block))
        "Nested descent should continue selecting the first child.")
    (invoke-command application 'select-parent-code-form (object-id block))
    (is (equal '(0 1) (code-block-selected-form-path block))
        "Ascending should return to the parent selection.")
    (is (string= "Selected path 1.2  |  list (3 items)"
                 (code-block-selection-status-line block))
        "Nested selections should report their structural path.")))

(deftest nested-code-form-selection-renders-and-reaches-inspector ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (set-object-property block :show-structure t)
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Selected path 1.2  |  list (3 items)" texts :test #'string=)
          "Nested selections should render their structural path.")
      (is (find-if (lambda (text)
                     (search "> [1]: list (3 items)" text))
                   texts)
          "The structure preview should visibly mark the selected nested form.")
      (is (find "selection: Selected path 1.2  |  list (3 items)"
                texts
                :test #'string=)
          "The inspector should describe the selected nested form.")
      (is (find "selected-form-path: (0 1)" texts :test #'string=)
          "The inspector should show the selected structural path."))))

(deftest code-block-editing-starts-at-selected-structural-form ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (is (editing-active-p application)
        "Focused code blocks should enter text edit mode.")
    (is (= 6 (text-buffer-cursor
              (orra::application-active-text-buffer application)))
        "Code-block text editing should start at the selected structural form.")
    (stop-editing application)))

(deftest code-block-edit-cursor-updates-structural-selection ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-right application)
    (is (equal '(0 1 0) (code-block-selected-form-path block))
        "Moving inside the selected subform should tighten selection to the deepest enclosing child.")
    (move-active-buffer-cursor-right application)
    (move-active-buffer-cursor-right application)
    (is (equal '(0 1 1) (code-block-selected-form-path block))
        "Moving onto another nested atom should retarget the structural selection.")
    (stop-editing application)))

(deftest code-block-editing-updates-parse-cache-incrementally ()
  (let* ((application (make-application :backend (make-null-backend)))
         (source (format nil "(list :a)~%(+ 1 2)~%(list :c)"))
         (block (invoke-command application 'append-code-block source))
         (old-info (code-block-parse-info block))
         (old-forms (getf old-info :forms)))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (move-buffer-cursor-to
     (orra::application-active-text-buffer application)
     (1+ (search "2)" source :test #'char=)))
    (insert-into-active-buffer application "0")
    (is (string= (format nil "(list :a)~%(+ 1 20)~%(list :c)")
                 (code-block-source block))
        "Code editing should update the code-block source.")
    (let* ((info (code-block-parse-info block))
           (forms (getf info :forms)))
      (is (getf info :incremental-p)
          "Live code editing should leave cached incremental parse info on the block.")
      (is (= 1 (getf info :reused-prefix-count))
          "Live incremental parsing should reuse clean prefix forms.")
      (is (= 1 (getf info :reused-suffix-count))
          "Live incremental parsing should reuse clean suffix forms.")
      (is (eq (first forms) (first old-forms))
          "Live incremental parsing should preserve the unchanged prefix form object.")
      (is (eq (third forms) (third old-forms))
          "Live incremental parsing should preserve the unchanged suffix form object.")
      (is (equal '(1 2) (code-block-selected-form-path block info))
          "Live incremental parsing should keep structural selection synced to the cursor."))
    (undo-active-buffer-edit application)
    (let ((info (code-block-parse-info block)))
      (is (getf info :incremental-p)
          "Undo should refresh cached parse info through the incremental diff path.")
      (is (equal old-forms (getf info :forms))
          "Undo should restore the original parsed top-level forms."))
    (stop-editing application)))

(deftest code-block-selection-replacement-updates-source ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)")))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (move-buffer-cursor-to (orra::application-active-text-buffer application) 3)
    (move-active-buffer-cursor-right application :extend-selection t)
    (move-active-buffer-cursor-right application :extend-selection t)
    (insert-into-active-buffer application "9")
    (is (string= "(+ 9 22)" (code-block-source block))
        "Replacing selected code should update the source through the incremental edit path.")
    (stop-editing application)))

(deftest code-form-selection-navigation-stays-live-while-editing ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (is (editing-active-p application)
        "Structural selection commands should keep code-block edit mode active.")
    (is (equal '(0 1) (code-block-selected-form-path block))
        "Selection commands should still update the structural path while editing.")
    (is (= 6
           (text-buffer-cursor
            (orra::application-active-text-buffer application)))
        "Selection commands should move the live cursor to the selected form.")
    (invoke-command application 'select-parent-code-form (object-id block))
    (is (= 0
           (text-buffer-cursor
            (orra::application-active-text-buffer application)))
        "Ascending the structural selection should retarget the live cursor.")
    (stop-editing application)))

(deftest structural-code-edit-keeps-active-buffer-in-sync ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (invoke-command application 'wrap-code-form (object-id block))
    (is (editing-active-p application)
        "Structural rewrites should keep code-block edit mode active.")
    (is (string= "(LIST (PROGN (+ 1 2)) (* 3 4))"
                 (code-block-source block))
        "Wrapping while editing should still rewrite the selected form.")
    (is (string= (code-block-source block)
                 (text-buffer-content
                  (orra::application-active-text-buffer application)))
        "The active text buffer should stay synchronized with structural rewrites.")
    (is (= 6
           (text-buffer-cursor
            (orra::application-active-text-buffer application)))
        "The live cursor should remain anchored to the rewritten selected form.")
    (undo-active-buffer-edit application)
    (is (string= "(list (+ 1 2) (* 3 4))"
                 (code-block-source block))
        "Undo in the active edit session should reverse the structural rewrite.")
    (stop-editing application)))

(deftest deleting-selected-code-form-rewrites-source ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                (format nil "(list :alpha)~%(+ 20 22)"))))
    (invoke-command application 'select-next-code-form (object-id block))
    (invoke-command application 'delete-code-form (object-id block))
    (is (string= "(LIST :ALPHA)" (code-block-source block))
        "Deleting the selected top-level form should rewrite the block source.")
    (is (= 0 (code-block-selected-form-index block))
        "Deleting the last top-level form should clamp selection to the remaining form.")
    (is (string= "Selected form 1/1  |  list (2 items)"
                 (code-block-selection-status-line block))
        "Selection status should update after structural deletion.")))

(deftest deleting-nested-code-form-rewrites-source ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list :alpha :beta :gamma)")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (invoke-command application 'delete-code-form (object-id block))
    (is (string= "(LIST :ALPHA :GAMMA)" (code-block-source block))
        "Deleting a nested selected form should rewrite the block source.")
    (is (equal '(0 2) (code-block-selected-form-path block))
        "Nested deletion should keep selection on the surviving sibling at the same depth.")))

(deftest wrap-and-splice-code-form-rewrite-source ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)")))
    (invoke-command application 'wrap-code-form (object-id block))
    (is (string= "(PROGN (+ 20 22))" (code-block-source block))
        "Wrapping should rewrite the selected form as a PROGN wrapper.")
    (invoke-command application 'splice-code-form (object-id block))
    (is (string= "(+ 20 22)" (code-block-source block))
        "Splicing should remove the PROGN wrapper and restore the body form.")
    (is (= 0 (code-block-selected-form-index block))
        "Selection should stay on the first form after wrap/splice.")))

(deftest wrap-and-splice-nested-code-form-rewrite-source ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 20 22) :done)")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (invoke-command application 'wrap-code-form (object-id block))
    (is (string= "(LIST (PROGN (+ 20 22)) :DONE)" (code-block-source block))
        "Wrapping a nested selection should rewrite only the selected nested form.")
    (invoke-command application 'splice-code-form (object-id block))
    (is (string= "(LIST (+ 20 22) :DONE)" (code-block-source block))
        "Splicing a nested selection should remove only the selected nested wrapper.")
    (is (equal '(0 1) (code-block-selected-form-path block))
        "Selection should stay on the nested form after wrap/splice.")))

(deftest replace-selected-code-form-source-rewrites-span ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application
                                'append-code-block
                                "(list (+ 1 2) (* 3 4))")))
    (invoke-command application 'select-child-code-form (object-id block))
    (invoke-command application 'select-next-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (render-application application)
    (invoke-command application
                    'replace-code-form-source
                    (object-id block)
                    "(- 9 4)")
    (is (string= "(list (- 9 4) (* 3 4))"
                 (code-block-source block))
        "Replacing a selected structural form should rewrite only that source span.")
    (is (equal '(0 1) (code-block-selected-form-path block))
        "Replacing a selected form should keep structural selection at the replacement path.")
    (is (string= (code-block-source block)
                 (text-buffer-content
                  (orra::application-active-text-buffer application)))
        "The active edit buffer should mirror structural source replacement.")
    (is (application-dirty-p application)
        "Structural source replacement should schedule a redraw.")
    (undo-active-buffer-edit application)
    (is (string= "(list (+ 1 2) (* 3 4))"
                 (code-block-source block))
        "Undo should restore source replaced through the structural lens.")
    (stop-editing application)))

(deftest structural-code-edit-invalidates-existing-result ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)"))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block))))
    (is (eq :ok (result-block-status result))
        "Expected a successful result before structural edits.")
    (invoke-command application 'wrap-code-form (object-id block))
    (is (eq :stale (result-block-status result))
        "Structural source edits should invalidate previous evaluation results.")
    (is (string= "Result invalidated by source changes."
                 (result-block-presentation result))
        "Stale results should explain why they are no longer current.")
    (render-application application)
    (is (find ".. Result invalidated by source changes."
              (collect-text-cells (application-root-cell application))
              :test #'string=)
        "Invalidated results should render explicitly in the workspace.")))

(deftest structural-code-edits-enter-text-edit-history ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)")))
    (invoke-command application 'wrap-code-form (object-id block))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (undo-active-buffer-edit application)
    (is (string= "(+ 20 22)" (code-block-source block))
        "Undo should treat a structural rewrite as part of the editable code history.")
    (redo-active-buffer-edit application)
    (is (string= "(PROGN (+ 20 22))" (code-block-source block))
        "Redo should reapply the structural rewrite from code-block edit history.")
    (stop-editing application)))

(deftest structural-code-edits-preserve-prior-text-history ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)")))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-end application)
    (move-active-buffer-cursor-left application)
    (insert-into-active-buffer application "0")
    (stop-editing application)
    (invoke-command application 'wrap-code-form (object-id block))
    (begin-editing-focused-model application)
    (undo-active-buffer-edit application)
    (is (string= "(+ 20 220)" (code-block-source block))
        "Undo after a structural rewrite should restore the prior text-edited source first.")
    (undo-active-buffer-edit application)
    (is (string= "(+ 20 22)" (code-block-source block))
        "Earlier text edits should remain reachable beneath structural history entries.")
    (redo-active-buffer-edit application)
    (is (string= "(+ 20 220)" (code-block-source block))
        "Redo should restore the earlier text edit before reapplying later structural edits.")
    (redo-active-buffer-edit application)
    (is (string= "(+ 20 (PROGN 220))" (code-block-source block))
        "Redo should reapply the structural rewrite after the prior text edit.")
    (stop-editing application)))

(deftest invalid-code-evaluation-produces-error-result ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "de"))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block))))
    (is (typep result 'result-block)
        "Evaluating invalid code should still produce a result block.")
    (is (eq :error (result-block-status result))
        "Invalid code should mark the result block as an error.")
    (is (search "Evaluation error:" (result-block-presentation result))
        "Runtime failures should be rendered as evaluation errors.")
    (render-application application)
    (is (find-if (lambda (text)
                   (search "!! Evaluation error:" text))
                 (collect-text-cells (application-root-cell application)))
        "Error results should render in the workspace instead of entering the debugger.")))

(deftest malformed-code-evaluation-produces-error-result ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(list :hello"))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block))))
    (is (eq :error (result-block-status result))
        "Reader failures should mark the result block as an error.")
    (is (search "Parse error@" (result-block-presentation result))
        "Malformed source should reuse the parse error message as the result.")))

(deftest evaluation-records-reproducible-result-metadata ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)"))
         (before (get-universal-time))
         (result (invoke-command application 'evaluate-code-block
                                 (object-id block)))
         (after (get-universal-time)))
    (is (string= "(+ 20 22)" (result-block-input-source result))
        "Evaluation results should retain the source they evaluated.")
    (is (equal '((+ 20 22)) (result-block-input-forms result))
        "Evaluation results should retain the parsed input forms.")
    (is (string= (package-name *package*)
                 (result-block-package result))
        "Evaluation results should record the package used for evaluation.")
    (is (<= before (result-block-evaluated-at result) after)
        "Evaluation results should record a universal-time timestamp.")
    (is (eq :ok (result-block-status result))
        "Successful evaluation should retain its status metadata.")))

(deftest evaluation-metadata-renders-in-result-block ()
  (let* ((application (make-application :backend (make-null-backend)))
         (block (invoke-command application 'append-code-block "(+ 20 22)")))
    (invoke-command application 'evaluate-code-block (object-id block))
    (render-application application)
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "=> 42" texts :test #'string=)
          "Result value should still render.")
      (is (find-if (lambda (text)
                     (and (search "input: (+ 20 22)" text)
                          (search "package:" text)
                          (search "evaluated-at:" text)))
                   texts)
          "Rendered results should expose reproducible evaluation metadata."))))

(deftest editing-focused-model ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (is (typep (focused-model application) 'paragraph)
        "Expected to focus the initial paragraph.")
    (begin-editing-focused-model application)
    (insert-into-active-buffer application " More")
    (is (string= "This image is live. Commands and objects are data. More"
                 (paragraph-text (focused-model application)))
        "Paragraph text should update during editing.")
    (stop-editing application)
    (is (not (editing-active-p application))
        "Editing state should clear when stopped.")))

(deftest persistence-round-trip ()
  (let* ((application (make-application))
         (block nil))
    (setf block (invoke-command application 'append-code-block "(+ 20 22)"))
    (render-application application)
    (loop repeat 8
          until (string= (object-id (focused-model application))
                         (object-id block))
          do (focus-next-model application))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-end application)
    (move-active-buffer-cursor-left application)
    (insert-into-active-buffer application "0")
    (stop-editing application)
    (invoke-command application 'evaluate-code-block (object-id block))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file (application-workspace application)
                                     path
                                     :registry (application-registry application))
             (let* ((registry (make-object-registry))
                    (workspace (load-workspace-from-file path :registry registry))
                    (loaded-notebook (root-notebook workspace))
                    (loaded-section (first (children-of loaded-notebook)))
                    (loaded-block (find-if (lambda (object)
                                             (and (typep object 'code-block)
                                                  (string= "(+ 20 220)"
                                                           (code-block-source
                                                            object))))
                                           (children-of loaded-section))))
               (is (typep loaded-block 'code-block)
                   "Expected to load a code block.")
               (is (string= "240"
                            (result-block-presentation
                             (code-block-result loaded-block)))
                   "Expected persisted evaluation result.")
               (is (eq :ok
                       (result-block-status
                        (code-block-result loaded-block)))
                   "Expected persisted evaluation status.")
               (is (string= "(+ 20 220)"
                            (result-block-input-source
                             (code-block-result loaded-block)))
                   "Expected persisted evaluation input source.")
               (is (string= (package-name *package*)
                            (result-block-package
                             (code-block-result loaded-block)))
                   "Expected persisted evaluation package metadata.")
               (is (integerp
                    (result-block-evaluated-at
                     (code-block-result loaded-block)))
                   "Expected persisted evaluation timestamp.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest workspace-clone-command-writes-loadable-copy ()
  (let* ((application (make-application :save-path "active-workspace.sexp"))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "clone-only paragraph"))
         (original-save-path (application-save-path application)))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (invoke-command application 'clone-workspace path)
             (is (string= original-save-path
                          (application-save-path application))
                 "Cloning should not replace the active save path.")
             (let* ((payload (read-persisted-payload path))
                    (registry (make-object-registry))
                    (workspace (load-workspace-from-file path
                                                         :registry registry))
                    (loaded-section
                     (first (children-of (root-notebook workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "clone-only paragraph"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (eq :clone (getf payload :mode))
                   "Clone files should carry clone mode metadata.")
               (is (integerp (getf payload :saved-at))
                   "Clone files should record when they were written.")
               (is (string= (object-id paragraph)
                            (object-id loaded-paragraph))
                   "Clone files should load the same object graph.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest workspace-archive-command-writes-loadable-archive ()
  (let* ((application (make-application :save-path "active-workspace.sexp"))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "archive-only paragraph"))
         (original-save-path (application-save-path application)))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (invoke-command application 'archive-workspace path)
             (is (string= original-save-path
                          (application-save-path application))
                 "Archiving should not replace the active save path.")
             (let* ((payload (read-persisted-payload path))
                    (registry (make-object-registry))
                    (workspace (load-workspace-from-file path
                                                         :registry registry))
                    (loaded-section
                     (first (children-of (root-notebook workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "archive-only paragraph"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (eq :archive (getf payload :mode))
                   "Archive files should carry archive mode metadata.")
               (is (integerp (getf payload :saved-at))
                   "Archive files should record when they were written.")
               (is (integerp (getf payload :archived-at))
                   "Archive files should record archive-specific timestamp metadata.")
               (is (string= (object-id paragraph)
                            (object-id loaded-paragraph))
                   "Archive files should remain loadable workspace files.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest checkpoint-workspace-command-writes-loadable-checkpoint ()
  (let* ((application (make-application :save-path "active-workspace.sexp"))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "checkpoint-only paragraph"))
         (original-save-path (application-save-path application)))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (invoke-command application 'checkpoint-workspace path)
             (is (string= original-save-path
                          (application-save-path application))
                 "Checkpointing should not replace the active save path.")
             (let* ((payload (read-persisted-payload path))
                    (registry (make-object-registry))
                    (workspace (load-workspace-from-file path
                                                         :registry registry))
                    (loaded-section
                     (first (children-of (root-notebook workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "checkpoint-only paragraph"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (eq :checkpoint (getf payload :mode))
                   "Checkpoint files should carry checkpoint mode metadata.")
               (is (integerp (getf payload :checkpoint-at))
                   "Checkpoint files should record checkpoint-specific timestamp metadata.")
               (is (string= (object-id paragraph)
                            (object-id loaded-paragraph))
                   "Checkpoint files should remain loadable workspace files.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest periodic-checkpoint-runs-only-when-interval-is-due ()
  (let ((directory (make-test-directory "orra-checkpoints")))
    (unwind-protect
         (let ((application (make-application :checkpoint-directory directory
                                              :checkpoint-interval 10)))
           (setf (application-last-checkpoint-at application) 100)
           (is (null (maybe-checkpoint-workspace application :now 109))
               "Checkpointing before the interval should do nothing.")
           (is (null (directory (merge-pathnames "*.sexp" directory)))
               "No checkpoint file should be created before the interval is due.")
           (let ((path (maybe-checkpoint-workspace application :now 110)))
             (is path
                 "Checkpointing should return the checkpoint path when due.")
             (is (probe-file path)
                 "Checkpointing should create a file when due.")
             (is (= 110 (application-last-checkpoint-at application))
                 "Checkpointing should record the completed checkpoint time.")
             (is (equal (truename path)
                        (truename (application-last-checkpoint-path application)))
                 "Checkpointing should remember the last checkpoint path.")
             (let ((payload (read-persisted-payload path)))
               (is (eq :checkpoint (getf payload :mode))
                   "Periodic checkpoints should use checkpoint mode metadata.")
               (is (= 110 (getf payload :checkpoint-at))
                   "Periodic checkpoints should use the triggering timestamp."))))
      (delete-test-directory directory))))

(deftest latest-workspace-checkpoint-ignores-invalid-files ()
  (let ((directory (make-test-directory "orra-recovery")))
    (unwind-protect
         (let* ((old-application (make-application
                                  :checkpoint-directory directory))
                (new-application (make-application
                                  :checkpoint-directory directory))
                (save-application (make-application
                                   :checkpoint-directory directory))
                (old-path (merge-pathnames "old-checkpoint.sexp" directory))
                (new-path (merge-pathnames "new-checkpoint.sexp" directory))
                (save-path (merge-pathnames "normal-save.sexp" directory))
                (bad-path (merge-pathnames "bad-checkpoint.sexp" directory)))
           (invoke-command old-application
                           'append-paragraph
                           "old checkpoint")
           (invoke-command new-application
                           'append-paragraph
                           "new checkpoint")
           (invoke-command save-application
                           'append-paragraph
                           "normal save")
           (checkpoint-workspace-to-file
            (application-workspace old-application)
            old-path
            :registry (application-registry old-application)
            :timestamp 100)
           (checkpoint-workspace-to-file
            (application-workspace new-application)
            new-path
            :registry (application-registry new-application)
            :timestamp 200)
           (save-workspace-to-file
            (application-workspace save-application)
            save-path
            :registry (application-registry save-application))
           (with-open-file (stream bad-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-line "not a workspace payload" stream))
           (is (equal (truename new-path)
                      (truename (latest-workspace-checkpoint directory)))
               "Recovery should choose the newest valid checkpoint file."))
      (delete-test-directory directory))))

(deftest recover-workspace-command-loads-latest-checkpoint ()
  (let ((directory (make-test-directory "orra-recovery")))
    (unwind-protect
         (let* ((source (make-application :checkpoint-directory directory))
                (application (make-application
                              :save-path "active-workspace.sexp"
                              :checkpoint-directory directory)))
           (invoke-command source
                           'append-paragraph
                           "recovered checkpoint paragraph")
           (maybe-checkpoint-workspace source :now 300 :force t)
           (invoke-command application
                           'append-paragraph
                           "unrecovered paragraph")
           (invoke-command application 'recover-workspace)
           (is (string= "active-workspace.sexp"
                        (application-save-path application))
               "Recovering a checkpoint should preserve the active save path.")
           (is (workspace-has-paragraph-text-p
                (application-workspace application)
                "recovered checkpoint paragraph")
               "Recovery should load the latest checkpoint workspace.")
           (is (not (workspace-has-paragraph-text-p
                     (application-workspace application)
                     "unrecovered paragraph"))
               "Recovery should replace the current workspace.")
           (is (probe-file (application-last-checkpoint-path application))
               "Recovery should remember the checkpoint path it loaded."))
      (delete-test-directory directory))))

(deftest image-restored-application-loads-workspace-path ()
  (let ((source (make-application)))
    (invoke-command source
                    'append-paragraph
                    "image-restored workspace paragraph")
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file (application-workspace source)
                                     path
                                     :registry (application-registry source))
             (let ((application
                    (make-image-restored-application
                     :backend (make-null-backend)
                     :workspace-path path)))
               (is (workspace-has-paragraph-text-p
                    (application-workspace application)
                    "image-restored workspace paragraph")
                   "Image restore should load the requested workspace path.")
               (is (equal (truename path)
                          (truename (application-save-path application)))
                   "Image restore should keep the loaded workspace as the save path.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest image-restored-application-recovers-latest-checkpoint ()
  (let ((directory (make-test-directory "orra-image-restore")))
    (unwind-protect
         (uiop:with-temporary-file (:pathname save-path :keep t)
           (unwind-protect
                (let ((saved (make-application))
                      (checkpointed
                       (make-application :checkpoint-directory directory)))
                  (invoke-command saved
                                  'append-paragraph
                                  "saved image workspace")
                  (save-workspace-to-file
                   (application-workspace saved)
                   save-path
                   :registry (application-registry saved))
                  (invoke-command checkpointed
                                  'append-paragraph
                                  "checkpoint image workspace")
                  (maybe-checkpoint-workspace checkpointed
                                              :now 400
                                              :force t)
                  (let ((application
                         (make-image-restored-application
                          :backend (make-null-backend)
                          :workspace-path save-path
                          :checkpoint-directory directory
                          :recover-checkpoint-p t)))
                    (is (workspace-has-paragraph-text-p
                         (application-workspace application)
                         "checkpoint image workspace")
                        "Image restore should recover the newest checkpoint when requested.")
                    (is (not (workspace-has-paragraph-text-p
                              (application-workspace application)
                              "saved image workspace"))
                        "Recovered checkpoint state should replace the saved workspace state.")
                    (is (equal (truename save-path)
                               (truename (application-save-path application)))
                        "Checkpoint image restore should preserve the normal save path.")))
             (when (probe-file save-path)
               (delete-file save-path))))
      (delete-test-directory directory))))

(deftest workspace-payload-migration-loads-version-one-files ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (paragraph (make-paragraph :text "legacy paragraph"
                                    :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section paragraph)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (let* ((legacy-payload
                   (list :version 1
                         :workspace-id (object-id workspace)
                         :objects (mapcar #'orra::serialize-object-record
                                          (orra::collect-workspace-objects
                                           workspace))))
                  (migrated (migrate-workspace-payload legacy-payload)))
             (orra::write-workspace-payload-to-file legacy-payload path)
             (is (= (workspace-file-version)
                    (getf migrated :version))
                 "Legacy payloads should migrate to the current schema version.")
             (is (eq :save (getf migrated :mode))
                 "Legacy payloads should default to save mode.")
             (is (= 0 (getf migrated :saved-at))
                 "Legacy payloads should receive a deterministic unknown timestamp.")
             (let* ((loaded-registry (make-object-registry))
                    (loaded-workspace
                     (load-workspace-from-file path
                                               :registry loaded-registry))
                    (loaded-section
                     (first (children-of (root-notebook loaded-workspace))))
                    (loaded-paragraph
                     (find-if (lambda (object)
                                (and (typep object 'paragraph)
                                     (string= "legacy paragraph"
                                              (paragraph-text object))))
                              (children-of loaded-section))))
               (is (typep loaded-paragraph 'paragraph)
                   "Migrated legacy payloads should still load as workspaces.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest workspace-payload-migration-rejects-future-versions ()
  (let ((payload (list :version (1+ (workspace-file-version))
                       :workspace-id "future"
                       :objects nil)))
    (handler-case
        (progn
          (migrate-workspace-payload payload)
          (is nil "Future workspace schemas should not be loaded silently."))
      (error (condition)
        (is (search "Unsupported workspace file version"
                    (princ-to-string condition))
            "Future workspace schemas should fail with a migration error.")))))

(defun run-all-tests ()
  (let ((passed 0)
        (failed 0))
    (dolist (test (reverse *tests*))
      (handler-case
          (progn
            (funcall test)
            (incf passed)
            (format t "~&PASS ~A~%" test))
        (error (condition)
          (incf failed)
          (format t "~&FAIL ~A~%  ~A~%" test condition))))
    (format t "~&~D passed, ~D failed.~%" passed failed)
    (when (plusp failed)
      (error "Test failures encountered."))
    t))
