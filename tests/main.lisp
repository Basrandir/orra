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

(deftest keymap-modified-events-do-not-trigger-plain-commands ()
  (let* ((application (make-application))
         (block (invoke-command application 'append-code-block "(+ 20 22)"))
         (journal (application-operation-journal application))
         (operation-count (length (journal-operations journal))))
    (setf (orra::application-focused-model-id application) (object-id block))
    (dolist (event (list (make-key-event :key :e :altp t)
                         (make-key-event :key :e :metap t)
                         (make-key-event :key :e :altp t :metap t)))
      (is (not (dispatch-application-key application event))
          "Extra modifiers must not fall through to an unmodified evaluation command."))
    (is (and (null (code-block-result block))
             (= operation-count (length (journal-operations journal))))
        "Unbound modified shortcuts must not evaluate code or record semantic operations.")
    (is (and (null (orra::find-key-binding application :focus :q nil nil t nil))
             (null (orra::find-key-binding application :focus :q nil nil nil t)))
        "Alt/Meta+Q must not invoke the plain quit shortcut.")
    (is (dispatch-application-key application (make-key-event :key :e))
        "The existing unmodified evaluation shortcut should remain available.")
    (is (eq :ok (result-block-status (code-block-result block)))
        "Ordinary shortcut dispatch should retain its behavior.")))

(deftest keymap-modified-events-do-not-move-or-edit-text ()
  (let* ((application (make-application))
         (paragraph (focused-model application))
         (original-text (paragraph-text paragraph)))
    (dolist (event (list (make-key-event :key :a :text "a" :controlp t)
                         (make-key-event :key :a :text "a" :altp t)
                         (make-key-event :key :a :text "a" :metap t)))
      (is (not (dispatch-application-key application event))
          "Unbound modified events should not begin editing from focus mode."))
    (is (not (orra::editing-active-p application))
        "Ignoring modified input should preserve the focus/edit mode.")
    (begin-editing-focused-model application)
    (let* ((buffer (orra::application-active-text-buffer application))
           (cursor (text-buffer-cursor buffer))
           (journal (application-operation-journal application))
           (operation-count (length (journal-operations journal))))
      (dolist (event (list (make-key-event :key :a :text "a" :controlp t)
                           (make-key-event :key :a :text "a" :altp t)
                           (make-key-event :key :a :text "a" :metap t)
                           (make-key-event :key :backspace :altp t)
                           (make-key-event :key :left :controlp t :metap t)))
        (is (not (dispatch-application-key application event))
            "Modified input must not insert text, delete text, or reuse a Ctrl-only binding."))
      (is (and (string= original-text (text-buffer-content buffer))
               (string= original-text (paragraph-text paragraph))
               (= cursor (text-buffer-cursor buffer))
               (= operation-count (length (journal-operations journal))))
          "Ignored modified input must preserve the editor and semantic journal.")
      (is (dispatch-application-key application
                                    (make-key-event :key :a :text "A" :shiftp t))
          "Shifted text should remain ordinary text input."))))

(deftest keymap-supports-explicit-alt-and-meta-bindings ()
  (let ((application (make-application))
        (last-binding nil)
        (bindings nil))
    (dolist (specification '((nil nil :plain) (t nil :alt) (nil t :meta) (t t :both)))
      (destructuring-bind (alt meta label) specification
        (let ((binding
               (make-instance 'key-binding :context :global :key :a
                              :control t :shift :any :alt alt :meta meta
                              :documentation (symbol-name label)
                              :function (lambda (application event)
                                          (declare (ignore application event))
                                          (setf last-binding label)
                                          t))))
          (push binding bindings)
          (setf (gethash (orra::binding-descriptor binding)
                         (application-keymap application)) binding))))
    (dolist (editingp '(nil t))
      (when editingp (begin-editing-focused-model application))
      (dolist (specification '((nil nil :plain) (t nil :alt) (nil t :meta) (t t :both)))
        (destructuring-bind (alt meta label) specification
          (is (dispatch-application-key
               application (make-key-event :key :a :controlp t :shiftp t
                                           :altp alt :metap meta))
              "Global explicit modifier bindings should match in either context.")
          (is (eq label last-binding)
              "Bindings differing only in Alt/Meta must stay distinct."))))
    (is (every (lambda (binding) (member binding (list-key-bindings application))) bindings)
        "All modifier combinations should remain inspectable as keymap data.")
    (is (equal '(:focus :a t nil)
               (orra::key-binding-descriptor :focus :a t nil))
        "Existing four-element user keymap descriptors must keep working.")))

(deftest keymap-modifier-bindings-preserve-context-and-shift-precedence ()
  (let ((application (make-application)))
    (flet ((binding (context shift documentation)
             (make-instance 'key-binding :context context :key :a
                            :alt t :shift shift :documentation documentation
                            :function (constantly t))))
      (let ((global (binding :global t "global"))
            (contextual (binding :focus :any "contextual"))
            (exact (binding :focus t "exact")))
        (dolist (candidate (list global contextual exact))
          (setf (gethash (orra::binding-descriptor candidate)
                         (application-keymap application)) candidate))
        (is (eq exact (orra::find-key-binding application :focus :a nil t t nil))
            "Exact Shift matches should win over wildcard matches.")
        (remhash (orra::binding-descriptor exact) (application-keymap application))
        (is (eq contextual (orra::find-key-binding application :focus :a nil t t nil))
            "Contextual wildcard matches should win over global matches.")
        (is (eq global (orra::find-key-binding application :edit :a nil t t nil))
            "Global bindings should apply when no contextual binding matches.")
        (is (null (orra::find-key-binding application :focus :a nil t t t))
            "An extra Meta modifier should not reuse an Alt-only binding.")))))

(deftest define-key-binding-registers-explicit-modifiers ()
  (let ((orra::*key-binding-definitions* nil))
    (define-key-binding (:focus :a :alt t :meta t :shift :any)
        (application event)
      "Test explicit modifiers."
      (declare (ignore application event))
      t)
    (let* ((application (make-application))
           (binding (orra::find-key-binding application :focus :a nil t t t)))
      (is (and binding (key-binding-alt binding) (key-binding-meta binding))
          "The public macro should preserve Alt/Meta configuration.")
      (is (dispatch-application-key application
                                    (make-key-event :key :a :altp t :metap t :shiftp t))
          "Macro-defined modifier bindings should dispatch through the live application."))))

(deftest keyboard-help-labels-display-modifiers-and-special-keys ()
  (dolist (specification '((:return nil nil nil nil "Enter")
                           (:escape nil nil nil nil "Esc")
                           (:pageup nil nil nil nil "PgUp")
                           (:leftbracket t nil nil nil "Ctrl+[")
                           (:z t t t t "Ctrl+Alt+Meta+Shift+Z")
                           (:left nil nil nil :any "[Shift+]Left")
                           (:f1 nil nil nil nil "F1")))
    (destructuring-bind (key control alt meta shift expected) specification
      (let ((binding (make-instance 'key-binding :context :focus :key key
                                    :control control :alt alt :meta meta :shift shift
                                    :documentation "Test shortcut." :function (constantly t))))
        (is (string= expected (key-binding-label binding))
            "Shortcut labels should describe actual modifier combinations and key names.")))))

(deftest keyboard-help-describes-effective-bindings-and-partial-shadows ()
  (let* ((application (make-application))
         (global (make-instance 'key-binding :context :global :key :a :shift :any
                                :documentation "Global A action." :function (constantly t)))
         (contextual (make-instance 'key-binding :context :focus :key :a
                                    :documentation "Context A action." :function (constantly t))))
    (dolist (binding (list global contextual))
      (setf (gethash (orra::binding-descriptor binding) (application-keymap application)) binding))
    (let ((lines (orra::key-binding-help-lines application :focus)))
      (is (find "A  -  Context A action." lines :test #'string=)
          "Context-specific shortcuts should be described instead of shadowed global variants.")
      (is (find "Shift+A  -  Global A action." lines :test #'string=)
          "A partially shadowed wildcard should describe only its effective Shift variant.")
      (is (not (find "[Shift+]A  -  Global A action." lines :test #'string=))
          "Help must not claim that a shadowed unshifted global binding is usable."))
    (setf (gethash (orra::key-binding-descriptor :focus :a nil t)
                   (application-keymap application))
          (make-instance 'key-binding :context :focus :key :a :shift t
                         :documentation "Shift context action." :function (constantly t)))
    (is (not (some (lambda (line) (search "Global A action." line))
                   (orra::key-binding-help-lines application :focus)))
        "Fully shadowed global bindings must disappear from contextual help.")
    (is (not (some (lambda (line) (search "Global A action." line))
                   (orra::key-binding-help-lines application :help)))
        "Read-only help controls must not advertise unrelated global shortcuts.")))

(deftest keyboard-help-wraps-long-descriptions-without-clipping ()
  (dolist (width '(1 6 20 80))
    (dolist (text (list "" "supercalifragilistic"
			"  leading spaces and a descriptive keyboard shortcut"
			(format nil "first line~%~%a second line with several words")))
      (is (every (lambda (line) (<= (length line) width))
                 (orra::split-lines (orra::wrap-keyboard-help-text text width)))
          "Wrapped help must fit even when a word exceeds the available width.")))
  (is (string= (format nil "alpha~%beta~%gamma")
               (orra::wrap-keyboard-help-text "alpha beta gamma" 6))
      "Help wrapping should break at word boundaries when possible."))

(deftest keyboard-help-opens-from-focus-and-restores-the-notebook ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream) :layout-height 18))
         (application (make-application :backend backend))
         (focused (focused-model application))
         (object-count (length (registry-objects-list (application-registry application)))))
    (render-application application)
    (is (find-frame-operation backend :text
                              (lambda (operation) (search "F1 keyboard help" (fourth operation))))
        "A new workspace should make keyboard help discoverable near the top.")
    (scroll-application application 7)
    (let ((viewport-y (application-viewport-y application)))
      (dispatch-application-key application (make-key-event :key :f1))
      (render-application application)
      (is (and (application-help-visible-p application)
               (eq :help (orra::application-key-context application)))
          "F1 should open a separate read-only help input context.")
      (is (find-frame-operation backend :text
                                (lambda (operation)
                                  (string= "Keyboard help - Focus mode" (fourth operation))))
          "The visible frame should describe the mode help was opened from.")
      (is (and (eq focused (focused-model application))
               (= viewport-y (application-viewport-y application))
               (null (find-frame-operation backend :caret)))
          "Help rendering must not move focus, scroll the notebook, or draw its caret.")
      (dispatch-application-key application (make-key-event :key :escape))
      (render-application application)
      (is (and (not (application-help-visible-p application))
               (not (editing-active-p application))
               (eq focused (focused-model application))
               (= viewport-y (application-viewport-y application)))
          "Esc from help should return to the notebook rather than quit.")
      (is (= object-count (length (registry-objects-list (application-registry application))))
          "Transient help cells must not be registered as semantic objects."))))

(deftest keyboard-help-preserves-editor-selection-and-undo-history ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend))
         (paragraph (focused-model application))
         (original-text (paragraph-text paragraph)))
    (begin-editing-focused-model application)
    (insert-into-active-buffer application " more")
    (move-active-buffer-cursor-home application)
    (move-active-buffer-cursor-right application :extend-selection t)
    (move-active-buffer-cursor-right application :extend-selection t)
    (render-application application)
    (let* ((buffer (orra::application-active-text-buffer application))
           (state (orra::text-buffer-state buffer))
           (operation-count (length (journal-operations (application-operation-journal application)))))
      (dispatch-application-key application (make-key-event :key :f1))
      (render-application application)
      (is (find "Keyboard help - Edit mode"
                (collect-text-cells (orra::application-help-root-cell application)) :test #'string=)
          "Help should show editing shortcuts without ending the active editor.")
      (dispatch-application-key application (make-key-event :key :f1))
      (render-application application)
      (is (and (editing-active-p application)
               (eq buffer (orra::application-active-text-buffer application))
               (equal state (orra::text-buffer-state buffer))
               (= operation-count (length (journal-operations
                                           (application-operation-journal application)))))
          "Opening/closing help must preserve the same buffer, selection, history, and journal.")
      (undo-active-buffer-edit application)
      (is (string= original-text (paragraph-text paragraph))
          "Existing undo history should still work after returning from help."))))

(deftest keyboard-help-isolates-text-shortcuts-and-mouse-input ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend))
         (block (invoke-command application 'append-code-block "(+ 20 22)"))
         (global-fired-p nil))
    (setf (orra::application-focused-model-id application) (object-id block))
    (begin-editing-focused-model application)
    (render-application application)
    (let* ((buffer (orra::application-active-text-buffer application))
           (state (orra::text-buffer-state buffer))
           (operation-count (length (journal-operations (application-operation-journal application)))))
      (setf (gethash (orra::key-binding-descriptor :global :a nil nil)
                     (application-keymap application))
            (make-instance 'key-binding :context :global :key :a
                           :documentation "Unrelated global action."
                           :function (lambda (application event)
                                       (declare (ignore application event))
                                       (setf global-fired-p t))))
      (dispatch-application-key application (make-key-event :key :f1))
      (dolist (key '(:e :q :s :a :return :delete :backspace :f12))
        (is (not (dispatch-application-key application (make-key-event :key key :text "x")))
            "Help must not reuse editing, focus, or unrelated global command bindings."))
      (is (not (dispatch-application-key application (make-key-event :key :f1 :altp t)))
          "Modified F1 should not accidentally dismiss help.")
      (orra::focus-model-at-pixel application 10 10)
      (render-application application)
      (is (and (application-help-visible-p application)
               (eq block (focused-model application))
               (equal state (orra::text-buffer-state buffer))
               (not global-fired-p)
               (not (application-debug-visible-p application))
               (null (code-block-result block))
               (= operation-count (length (journal-operations
                                           (application-operation-journal application)))))
          "Help input must leave code, selection, focus, and semantic operations untouched."))))

(deftest keyboard-help-scrolls-independently-and-clamps-after-resize ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)
                                     :layout-width 32 :layout-height 12))
         (application (make-application :backend backend)))
    (render-application application)
    (scroll-application application 10)
    (let ((viewport-y (application-viewport-y application)))
      (invoke-command application 'toggle-keyboard-help)
      (render-application application)
      (is (find-frame-operation backend :scrollbar)
          "Long help should have a scrollbar rather than be limited by monitor height.")
      (dispatch-application-key application (make-key-event :key :pagedown))
      (is (and (plusp (orra::application-help-viewport-y application))
               (= viewport-y (application-viewport-y application)))
          "Help paging should not change the notebook viewport.")
      (dispatch-application-key application (make-key-event :key :home))
      (is (zerop (orra::application-help-viewport-y application))
          "Home should return to the top of help.")
      (scroll-application application 3)
      (is (= 3 (orra::application-help-viewport-y application))
          "The mouse-wheel scrolling path should scroll help while it is visible.")
      (dispatch-application-key application (make-key-event :key :end))
      (is (= (orra::application-help-max-viewport-y application)
             (orra::application-help-viewport-y application))
          "End should reach the bottom of help.")
      (render-application application)
      (is (every (lambda (text)
                   (every (lambda (line) (<= (length line) 26))
                          (orra::split-lines text)))
                 (collect-text-cells (orra::application-help-root-cell application)))
          "All help descriptions should be wrapped to the actual cell content width.")
      (setf (orra::backend-null-layout-width backend) 160
            (orra::backend-null-layout-height backend) 1000)
      (render-application application)
      (is (zerop (orra::application-help-viewport-y application))
          "Resizing to fit all help should clamp stale help scroll offsets.")
      (is (not (find-frame-operation backend :scrollbar))
          "A fitted help view should no longer need a scrollbar."))))

(deftest keyboard-help-refreshes-live-bindings ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)))
         (application (make-application :backend backend))
         (descriptor (orra::key-binding-descriptor :focus :b nil nil)))
    (invoke-command application 'toggle-keyboard-help)
    (setf (gethash descriptor (application-keymap application))
          (make-instance 'key-binding :context :focus :key :b
                         :documentation "A newly installed live action." :function (constantly t)))
    (render-application application)
    (is (find "B  -  A newly installed live action."
              (collect-text-cells (orra::application-help-root-cell application)) :test #'string=)
        "Help should reflect keymap changes made while it is open.")
    (remhash descriptor (application-keymap application))
    (render-application application)
    (is (not (some (lambda (text) (search "newly installed live action" text))
                   (collect-text-cells (orra::application-help-root-cell application))))
        "Removed bindings should not leave stale help entries.")))

(deftest keyboard-help-does-not-change-persisted-workspace-data ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream) :layout-height 18))
         (application (make-application :backend backend)))
    (begin-editing-focused-model application)
    (insert-into-active-buffer application " saved while help is open")
    (render-application application)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (invoke-command application 'save-workspace path)
             (let ((before (read-persisted-payload path)))
               (invoke-command application 'toggle-keyboard-help)
               (scroll-application-page application 1)
               (render-application application)
               (invoke-command application 'save-workspace path)
               (let* ((after (read-persisted-payload path))
                      (loaded (make-image-restored-application
                               :backend (make-null-backend :stream (make-string-output-stream))
                               :workspace-path path)))
                 (is (and (equal (getf before :objects) (getf after :objects))
                          (equal (getf before :operation-journal)
                                 (getf after :operation-journal)))
                     "Saving with help open must preserve exactly the same objects and journal.")
                 (is (and (application-help-visible-p application)
                          (editing-active-p application)
                          (not (application-help-visible-p loaded))
                          (null (orra::application-help-root-cell loaded))
                          (workspace-has-paragraph-text-p
                           (application-workspace loaded)
                           (paragraph-text (focused-model application))))
                     "Workspace restore must keep edited text but not transient help or editor state."))))
        (when (probe-file path)
          (delete-file path))))))

(deftest keyboard-help-sdl-f1-mapping-and-dispatch ()
  (let ((application (make-application)))
    (sdl2:with-sdl-event (event :keydown)
      (setf (event :key :keysym :scancode) (sdl2:scancode-key-to-value :scancode-f1)
            (event :key :keysym :sym) 0
            (event :key :keysym :mod) 0)
      (is (eq :f1 (orra::sdl2-scancode-key (event :key :keysym)))
          "SDL F1 events should map to the same logical key as headless tests.")
      (orra::handle-sdl2-keydown application (event :key :keysym))
      (is (application-help-visible-p application)
          "Real SDL key symbol handling should open the keyboard help view."))))

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

(deftest operation-journal-plist-round-trips-sync-state ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (pending-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text
                                                 :value "pending")
                                  :timestamp 101))
         (acknowledged-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text
                                                 :value "acknowledged")
                                  :timestamp 102))
         (failed-operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text
                                                 :value "failed")
                                  :timestamp 103))
         (remote-operation
          (make-workspace-operation :id "remote-op-1"
                                    :type :set-slot
                                    :target-id "paragraph-1"
                                    :payload (list :slot :text
                                                   :value "remote")
                                    :actor-id "peer-user"
                                    :session-id "peer-session"
                                    :timestamp 104
                                    :clock '(("peer-user" . 4))
                                    :sequence 9)))
    (record-operation journal remote-operation)
    (acknowledge-journal-operation journal acknowledged-operation)
    (fail-journal-operation journal failed-operation)
    (record-local-presence journal
                           :focus-id "paragraph-1"
                           :cursor-position 5
                           :status :editing
                           :updated-at 105
                           :metadata (list :color "blue"))
    (record-local-comment journal
                          :id "comment-1"
                          :target-id "paragraph-1"
                          :body "Persist review note."
                          :created-at 106
                          :updated-at 107
                          :metadata (list :severity :info))
    (record-workspace-member journal
                             :actor-id "local-user"
                             :display-name "Local User"
                             :role :owner
                             :status :active
                             :updated-at 108)
    (record-local-attachment journal
                             :id "attachment-1"
                             :target-id "paragraph-1"
                             :content-type "image/png"
                             :byte-size 4096
                             :digest "sha256:image"
                             :storage-ref "server://workspace-1/attachments/attachment-1"
                             :status :available
                             :created-at 109
                             :updated-at 110)
    (record-local-checkpoint journal
                             :id "checkpoint-1"
                             :checkpoint-at 111
                             :storage-ref "server://workspace-1/checkpoints/checkpoint-1"
                             :byte-size 8192
                             :digest "sha256:checkpoint"
                             :status :available
                             :created-at 112
                             :updated-at 113)
    (apply-conflict-sync-payload
     journal
     (list :workspace-id "workspace-1"
           :conflict
           (list :id "conflict-1"
                 :target-id "paragraph-1"
                 :slot :text
                 :local-operation-id (operation-id pending-operation)
                 :remote-operation-id (operation-id remote-operation)
                 :status :open
                 :created-at 114
                 :resolved-at nil)))
    (let* ((payload (operation-journal-plist journal))
           (restored (make-operation-journal-from-plist payload)))
      (is (equal payload
                 (operation-journal-plist restored))
          "Journal snapshots should round-trip as deterministic plists.")
      (is (= 4 (journal-next-sequence restored))
          "Restored journals should preserve the next local sequence.")
      (is (equal (list (operation-id pending-operation)
                       (operation-id acknowledged-operation)
                       (operation-id failed-operation)
                       (operation-id remote-operation))
                 (mapcar #'operation-id (journal-operations restored)))
          "Restored journals should preserve operation append order.")
      (is (eq :pending
              (journal-operation-queue-status restored pending-operation))
          "Restored journals should preserve pending queue entries.")
      (is (eq :acknowledged
              (journal-operation-queue-status restored acknowledged-operation))
          "Restored journals should preserve acknowledged queue entries.")
      (is (eq :failed
              (journal-operation-queue-status restored failed-operation))
          "Restored journals should preserve failed queue entries.")
      (is (null (journal-operation-queue-status restored remote-operation))
          "Restored remote operations should stay outside the local queue.")
      (is (= 3 (journal-clock-position restored "local-user"))
          "Restored journals should preserve local vector clocks.")
      (is (= 4 (journal-clock-position restored "peer-user"))
          "Restored journals should preserve merged peer vector clocks.")
      (is (find-journal-presence restored "local-user" "local-session")
          "Restored journals should preserve presence state.")
      (is (find-journal-comment restored "comment-1")
          "Restored journals should preserve comments.")
      (is (find-journal-member restored "local-user")
          "Restored journals should preserve workspace membership.")
      (is (find-journal-attachment restored "attachment-1")
          "Restored journals should preserve attachment metadata.")
      (is (find-journal-checkpoint restored "checkpoint-1")
          "Restored journals should preserve checkpoint metadata.")
      (is (find-journal-conflict restored "conflict-1")
          "Restored journals should preserve conflict records."))))

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

(deftest apply-workspace-operation-creates-target-object ()
  (let* ((registry (make-object-registry))
         (section (make-section :title "Remote Section"
                                :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-create-op-1"
           :type :create-object
           :target-id "paragraph-remote-1"
           :payload (list :kind :paragraph
                          :slots (list :text "created remotely")
                          :parent-id (object-id section))
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 210)))
    (let ((created (apply-workspace-operation registry operation)))
      (is (typep created 'paragraph)
          "Create-object should instantiate the requested notebook object kind.")
      (is (string= "paragraph-remote-1" (object-id created))
          "Create-object should use the operation target as stable object identity.")
      (is (eq created (find-object registry "paragraph-remote-1"))
          "Create-object should register created objects for later operations.")
      (is (string= "created remotely" (paragraph-text created))
          "Create-object should initialize semantic slots from the payload.")
      (is (eq section (parent-of created))
          "Create-object should attach the created object to its semantic parent.")
      (is (equal (list created) (children-of section))
          "Create-object should update the parent child list."))))

(deftest apply-workspace-operation-inserts-text-range ()
  (let* ((registry (make-object-registry))
         (paragraph (make-paragraph :text "hello world"
                                    :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-insert-op-1"
           :type :insert-text-range
           :target-id (object-id paragraph)
           :payload (list :offset 5 :text ", remote")
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 220)))
    (is (eq paragraph
            (apply-workspace-operation registry operation))
        "Insert-text-range should return the target text object.")
    (is (string= "hello, remote world" (paragraph-text paragraph))
        "Insert-text-range should splice text into the target text slot.")))

(deftest apply-workspace-operation-deletes-code-text-range ()
  (let* ((registry (make-object-registry))
         (result (make-result-block :value 42
                                    :presentation "=> 42"
                                    :registry registry))
         (block (make-code-block :source "(+ 20 220)"
                                 :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-delete-op-1"
           :type :delete-text-range
           :target-id (object-id block)
           :payload (list :slot :source :offset 7 :length 1)
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 221)))
    (setf (code-block-result block) result)
    (is (eq block
            (apply-workspace-operation registry operation))
        "Delete-text-range should return the target text object.")
    (is (string= "(+ 20 20)" (code-block-source block))
        "Delete-text-range should remove the requested text span.")
    (is (eq :stale (result-block-status result))
        "Deleting source text should use code block source replacement and invalidate results.")))

(deftest apply-workspace-operation-reorders-children ()
  (let* ((registry (make-object-registry))
         (section (make-section :title "Remote Section"
                                :registry registry))
         (first-paragraph (make-paragraph :text "first"
                                          :registry registry))
         (block (make-code-block :source "(+ 1 2)"
                                 :registry registry))
         (last-paragraph (make-paragraph :text "last"
                                         :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-reorder-op-1"
           :type :reorder-children
           :target-id (object-id section)
           :payload (list :child-ids (list (object-id block)
                                           (object-id last-paragraph)
                                           (object-id first-paragraph)))
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 230)))
    (append-child section first-paragraph)
    (append-child section block)
    (append-child section last-paragraph)
    (is (eq section
            (apply-workspace-operation registry operation))
        "Reorder-children should return the target parent object.")
    (is (equal (list block last-paragraph first-paragraph)
               (children-of section))
        "Reorder-children should order children by stable object id payload.")
    (is (eq section (parent-of first-paragraph))
        "Reordered children should keep parent links intact.")
    (is (eq section (parent-of block))
        "Reordered code children should keep parent links intact.")
    (is (eq section (parent-of last-paragraph))
        "Reordered trailing children should keep parent links intact.")))

(deftest apply-workspace-operation-attaches-metadata ()
  (let* ((registry (make-object-registry))
         (paragraph (make-paragraph :text "draft"
                                    :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-metadata-op-1"
           :type :attach-metadata
           :target-id (object-id paragraph)
           :payload (list :metadata (list :reviewed-p t
                                          :tags (list "sync" "review")
                                          :origin "peer"))
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 240)))
    (is (eq paragraph
            (apply-workspace-operation registry operation))
        "Attach-metadata should return the target object.")
    (is (eql t (gethash :reviewed-p (object-metadata paragraph)))
        "Attach-metadata should write boolean metadata values.")
    (is (equal (list "sync" "review")
               (gethash :tags (object-metadata paragraph)))
        "Attach-metadata should write structured metadata values.")
    (is (string= "peer"
                 (gethash :origin (object-metadata paragraph)))
        "Attach-metadata should write string metadata values.")))

(deftest apply-workspace-operation-links-objects ()
  (let* ((registry (make-object-registry))
         (source (make-paragraph :text "source"
                                 :registry registry))
         (target (make-code-block :source "(+ 1 2)"
                                  :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-link-op-1"
           :type :link-object
           :target-id (object-id source)
           :payload (list :target-id (object-id target)
                          :relation :references
                          :label "supporting code"
                          :metadata (list :origin "peer"))
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 250)))
    (is (eq source
            (apply-workspace-operation registry operation))
        "Link-object should return the source object.")
    (let ((link (first (gethash :links (object-metadata source))))
          (backlink (first (gethash :backlinks (object-metadata target)))))
      (is (string= "remote-link-op-1" (getf link :id))
          "Link-object should store the operation id as link identity.")
      (is (string= (object-id target) (getf link :target-id))
          "Link-object should record the linked target id on the source.")
      (is (eq :references (getf link :relation))
          "Link-object should record the semantic relation.")
      (is (string= "supporting code" (getf link :label))
          "Link-object should preserve a display label for future lenses.")
      (is (equal (list :origin "peer") (getf link :metadata))
          "Link-object should preserve structured relation metadata.")
      (is (string= "remote-link-op-1" (getf backlink :id))
          "Link-object should create a matching backlink identity.")
      (is (string= (object-id source) (getf backlink :source-id))
          "Link-object should record the source id on the target backlink.")
      (is (eq :references (getf backlink :relation))
          "Link-object should keep source and target relation metadata aligned."))))

(deftest apply-workspace-operation-applies-evaluation-result ()
  (let* ((registry (make-object-registry))
         (block (make-code-block :source "(+ 999 1)"
                                 :registry registry))
         (operation
          (make-workspace-operation
           :id "remote-eval-op-1"
           :type :evaluate-cell
           :target-id (object-id block)
           :payload (list :result-id "remote-result-1"
                          :status :ok
                          :value 42
                          :presentation "42"
                          :input-source "(+ 20 22)"
                          :input-forms '((+ 20 22))
                          :package-name "COMMON-LISP-USER"
                          :evaluated-at 400
                          :environment (list :package "COMMON-LISP-USER"
                                             :language :common-lisp))
           :actor-id "peer-1"
           :session-id "session-2"
           :timestamp 260)))
    (let ((result (apply-workspace-operation registry operation)))
      (is (typep result 'result-block)
          "Evaluate-cell should materialize a result block.")
      (is (string= "remote-result-1" (object-id result))
          "Evaluate-cell should use the result id supplied by the operation.")
      (is (eq result (find-object registry "remote-result-1"))
          "Evaluate-cell should register the result block for later operations.")
      (is (eq result (code-block-result block))
          "Evaluate-cell should attach the result to the target code block.")
      (is (eq :ok (result-block-status result))
          "Evaluate-cell should preserve captured result status.")
      (is (= 42 (result-block-value result))
          "Evaluate-cell should preserve captured result value without re-evaluating source.")
      (is (string= "42" (result-block-presentation result))
          "Evaluate-cell should preserve captured result presentation.")
      (is (string= "(+ 20 22)" (result-block-input-source result))
          "Evaluate-cell should preserve captured input source.")
      (is (equal '((+ 20 22)) (result-block-input-forms result))
          "Evaluate-cell should preserve captured input forms.")
      (is (string= "COMMON-LISP-USER" (result-block-package result))
          "Evaluate-cell should preserve captured package metadata.")
      (is (= 400 (result-block-evaluated-at result))
          "Evaluate-cell should preserve captured evaluation timestamp.")
      (is (equal (list :package "COMMON-LISP-USER" :language :common-lisp)
                 (result-block-environment result))
          "Evaluate-cell should preserve captured evaluation environment."))))

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

(deftest operation-journal-replays-create-before-slot-update ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"))
         (section (make-section :title "Replay Section"
                                :registry registry)))
    (record-operation
     journal
     (make-workspace-operation
      :id "replay-create-op-1"
      :type :create-object
      :target-id "paragraph-replay-1"
      :payload (list :kind :paragraph
                     :slots (list :text "initial replay")
                     :parent-id (object-id section))
      :timestamp 310))
    (record-operation
     journal
     (make-workspace-operation
      :id "replay-create-op-2"
      :type :set-slot
      :target-id "paragraph-replay-1"
      :payload (list :slot :text :value "updated replay")
      :timestamp 311))
    (let ((created (apply-operation-journal registry journal)))
      (is (= 2 (length created))
          "Journal replay should return each create/update application result.")
      (let ((paragraph (find-object registry "paragraph-replay-1")))
        (is (typep paragraph 'paragraph)
            "Journal replay should materialize created objects before later updates.")
        (is (string= "updated replay" (paragraph-text paragraph))
            "Journal replay should apply later slot updates to created objects.")
        (is (eq section (parent-of paragraph))
            "Journal replay should preserve semantic parent links for created objects.")))))

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

(deftest resolving-conflict-with-local-keeps-remote-suppressed ()
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
           :id "local-resolution-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "peer draft")
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("peer-user" . 1)))))
    (declare (ignore local-operation))
    (setf (paragraph-text paragraph) "local draft")
    (apply-remote-operation registry journal remote-operation)
    (let ((conflict (first (journal-conflicts journal))))
      (is (eq paragraph
              (resolve-sync-conflict registry journal conflict :local
                                     :resolved-at 42))
          "Resolving toward local should return the affected object.")
      (is (eq :resolved-local (sync-conflict-status conflict))
          "Local conflict resolution should be recorded on the conflict.")
      (is (= 42 (sync-conflict-resolved-at conflict))
          "Local conflict resolution should record the resolution timestamp."))
    (setf (paragraph-text paragraph) "restored draft")
    (is (equal (list paragraph nil)
               (apply-operation-journal registry journal))
        "Replaying a locally resolved conflict should keep the remote write suppressed.")
    (is (string= "local draft" (paragraph-text paragraph))
        "Local conflict resolution should keep the local semantic slot value.")))

(deftest resolving-conflict-with-remote-applies-remote-value ()
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
           :id "remote-resolution-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "peer draft")
           :actor-id "peer-user"
           :session-id "peer-session"
           :clock '(("peer-user" . 1)))))
    (setf (paragraph-text paragraph) "local draft")
    (apply-remote-operation registry journal remote-operation)
    (let ((conflict (first (journal-conflicts journal))))
      (is (eq paragraph
              (resolve-sync-conflict registry journal conflict :remote
                                     :resolved-at 43))
          "Resolving toward remote should apply and return the affected object.")
      (is (eq :resolved-remote (sync-conflict-status conflict))
          "Remote conflict resolution should be recorded on the conflict.")
      (is (= 43 (sync-conflict-resolved-at conflict))
          "Remote conflict resolution should record the resolution timestamp."))
    (is (string= "peer draft" (paragraph-text paragraph))
        "Remote conflict resolution should apply the remote semantic slot value.")
    (is (eq :failed
            (journal-operation-queue-status journal local-operation))
        "Accepting remote should retire the losing local pending operation.")
    (setf (paragraph-text paragraph) "restored draft")
    (is (equal (list paragraph paragraph)
               (apply-operation-journal registry journal))
        "Replaying a remotely resolved conflict should include the remote write.")
    (is (string= "peer draft" (paragraph-text paragraph))
        "Journal replay should converge on the accepted remote slot value.")))

(deftest local-conflict-sync-payload-is-serializer-friendly ()
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
                                                 :value "local draft")
                                  :timestamp 50))
         (remote-operation
          (make-workspace-operation
           :id "conflict-payload-remote-op"
           :type :set-slot
           :target-id (object-id paragraph)
           :payload (list :slot :text :value "peer draft")
           :actor-id "peer-user"
           :session-id "peer-session"
           :timestamp 51
           :clock '(("peer-user" . 1)))))
    (declare (ignore local-operation))
    (setf (paragraph-text paragraph) "local draft")
    (apply-remote-operation registry journal remote-operation)
    (let* ((conflict (first (journal-conflicts journal)))
           (payload (journal-conflict-sync-payload journal conflict)))
      (is (eq conflict
              (find-journal-conflict journal (sync-conflict-id conflict)))
          "Local conflicts should be visible in the journal by conflict id.")
      (is (equal (list :workspace-id "workspace-1"
                       :actor-id "local-user"
                       :session-id "local-session"
                       :conflict (sync-conflict-plist conflict))
                 payload)
          "Conflict sync payloads should contain serializer-friendly conflict plists."))))

(deftest conflict-sync-payload-updates-remote-conflict ()
  (let* ((journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (open-payload
          (list :workspace-id "workspace-1"
                :conflict
                (list :id "conflict-1"
                      :target-id "paragraph-1"
                      :slot :text
                      :local-operation-id "local-op-1"
                      :remote-operation-id "remote-op-1"
                      :status :open
                      :created-at 100
                      :resolved-at nil)))
         (resolved-payload
          (list :workspace-id "workspace-1"
                :conflict
                (list :id "conflict-1"
                      :target-id "paragraph-1"
                      :slot :text
                      :local-operation-id "local-op-1"
                      :remote-operation-id "remote-op-1"
                      :status :resolved-remote
                      :created-at 100
                      :resolved-at 110))))
    (let ((applied (apply-conflict-sync-payload journal open-payload)))
      (is (= 1 (length applied))
          "Conflict sync should return newly accepted conflict records.")
      (is (eq :open
              (sync-conflict-status
               (find-journal-conflict journal "conflict-1")))
          "Conflict sync should record open remote conflicts."))
    (let ((applied (apply-conflict-sync-payload journal resolved-payload)))
      (is (= 1 (length applied))
          "Conflict sync should accept newer resolution records.")
      (let ((conflict (find-journal-conflict journal "conflict-1")))
        (is (eq :resolved-remote (sync-conflict-status conflict))
            "Newer conflict records should update conflict resolution status.")
        (is (= 110 (sync-conflict-resolved-at conflict))
            "Newer conflict records should update resolution timestamps.")))
    (apply-conflict-sync-payload journal open-payload)
    (let ((conflict (find-journal-conflict journal "conflict-1")))
      (is (eq :resolved-remote (sync-conflict-status conflict))
          "Open conflict payloads should not downgrade resolved conflicts.")
      (is (= 110 (sync-conflict-resolved-at conflict))
          "Open conflict payloads should not erase resolution timestamps."))))

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
                                              :updated-at 160))
         (conflict
          (first
           (apply-conflict-sync-payload
            client-journal
            (list :workspace-id "workspace-1"
                  :conflict
                  (list :id "conflict-1"
                        :target-id "paragraph-1"
                        :slot :text
                        :local-operation-id (operation-id operation)
                        :remote-operation-id "peer-op-1"
                        :status :resolved-local
                        :created-at 161
                        :resolved-at 162))))))
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
                                                  :checkpoints (list checkpoint)
                                                  :conflicts (list conflict)))
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
      (is (find-journal-conflict server-journal "conflict-1")
          "Coordinator request handling should record accepted conflict metadata server-side.")
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
    (apply-conflict-sync-payload
     server-journal
     (list :workspace-id "workspace-1"
           :conflict
           (list :id "peer-conflict-1"
                 :target-id "paragraph-1"
                 :slot :text
                 :local-operation-id "local-op-1"
                 :remote-operation-id "peer-op-1"
                 :status :open
                 :created-at 204
                 :resolved-at nil)))
    (let* ((request (journal-sync-request-payload client-journal
                                                  :authentication auth))
           (response (handle-sync-request coordinator request :now 200))
           (operation-ids (mapcar (lambda (operation-plist)
                                    (getf operation-plist :id))
                                  (getf response :operations)))
           (presence-actors (mapcar (lambda (presence-plist)
                                      (getf presence-plist :actor-id))
                                    (getf response :presences)))
           (conflict-ids (mapcar (lambda (conflict-plist)
                                   (getf conflict-plist :id))
                                 (getf response :conflicts))))
      (is (equal (list (operation-id local-operation))
                 (getf response :acknowledged-operation-ids))
          "Coordinator sync responses should acknowledge the caller's accepted operations.")
      (is (equal (list "peer-op-1") operation-ids)
          "Coordinator sync responses should distribute peer operations without echoing caller operations.")
      (is (equal (list "peer-user") presence-actors)
          "Coordinator sync responses should distribute peer presence without echoing the caller session.")
      (is (equal (list "peer-conflict-1") conflict-ids)
          "Coordinator sync responses should distribute shared conflict records."))))

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

(deftest sync-coordinator-updates-coordinated-storage-status ()
  (let* ((coordinator (make-sync-coordinator))
         (server-journal (ensure-sync-coordinator-workspace coordinator
                                                            "workspace-1"))
         (client-journal (make-operation-journal :workspace-id "workspace-1"
                                                 :actor-id "local-user"
                                                 :session-id "local-session"))
         (peer-journal (make-operation-journal :workspace-id "workspace-1"
                                               :actor-id "peer-user"
                                               :session-id "peer-session"))
         (auth (make-sync-authentication :token-id "token-1"
                                         :workspace-id "workspace-1"
                                         :actor-id "local-user"
                                         :session-id "local-session"
                                         :scopes (list :sync)
                                         :issued-at 100
                                         :expires-at 300))
         (peer-auth (make-sync-authentication :token-id "token-2"
                                              :workspace-id "workspace-1"
                                              :actor-id "peer-user"
                                              :session-id "peer-session"
                                              :scopes (list :sync)
                                              :issued-at 100
                                              :expires-at 300)))
    (record-local-attachment client-journal
                             :id "attachment-1"
                             :target-id "paragraph-1"
                             :content-type "image/png"
                             :byte-size 2048
                             :digest "sha256:image"
                             :storage-ref "local://attachment-1"
                             :status :available
                             :created-at 201
                             :updated-at 202)
    (record-local-checkpoint client-journal
                             :id "checkpoint-1"
                             :checkpoint-at 210
                             :storage-ref "local://checkpoint-1"
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
    (sync-coordinator-register-member coordinator
                                      "workspace-1"
                                      "peer-user"
                                      :display-name "Peer User"
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
    (sync-coordinator-update-attachment-storage coordinator
                                                "workspace-1"
                                                "attachment-1"
                                                :available
                                                :updated-at 250
                                                :metadata
                                                (list :stored-by "blob-store"))
    (sync-coordinator-update-checkpoint-storage coordinator
						"workspace-1"
						"checkpoint-1"
						:failed
						:updated-at 251
						:metadata
						(list :reason "digest mismatch"))
    (let* ((attachment (find-journal-attachment server-journal
                                                "attachment-1"))
           (checkpoint (find-journal-checkpoint server-journal
                                                "checkpoint-1"))
           (response
            (handle-sync-request
             coordinator
             (journal-sync-request-payload peer-journal
                                           :authentication peer-auth)
             :now 260)))
      (is (eq :available (workspace-attachment-status attachment))
          "Coordinator storage updates should mark attachments available after byte transfer.")
      (is (= 250 (workspace-attachment-updated-at attachment))
          "Coordinator attachment storage updates should set the server update timestamp.")
      (is (string= "server://workspace-1/attachments/attachment-1"
                   (workspace-attachment-storage-ref attachment))
          "Coordinator attachment storage updates should preserve coordinated storage refs.")
      (is (equal (list :stored-by "blob-store")
                 (workspace-attachment-metadata attachment))
          "Coordinator attachment storage updates should expose storage metadata.")
      (is (eq :failed (workspace-checkpoint-status checkpoint))
          "Coordinator storage updates should mark failed checkpoint transfers.")
      (is (= 251 (workspace-checkpoint-updated-at checkpoint))
          "Coordinator checkpoint storage updates should set the server update timestamp.")
      (is (equal (list :reason "digest mismatch")
                 (workspace-checkpoint-metadata checkpoint))
          "Coordinator checkpoint storage updates should expose failure metadata.")
      (is (equal (list (workspace-attachment-plist attachment))
                 (getf response :attachments))
          "Coordinator responses should distribute updated attachment storage state.")
      (is (equal (list (workspace-checkpoint-plist checkpoint))
                 (getf response :checkpoints))
          "Coordinator responses should distribute updated checkpoint storage state."))))

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
                                   :updated-at 810))
         (conflict
          (first
           (apply-conflict-sync-payload
            journal
            (list :workspace-id "workspace-1"
                  :conflict
                  (list :id "conflict-1"
                        :target-id "paragraph-1"
                        :slot :text
                        :local-operation-id (operation-id second-operation)
                        :remote-operation-id "remote-op-1"
                        :status :open
                        :created-at 811
                        :resolved-at nil))))))
    (acknowledge-journal-operation journal first-operation)
    (let ((payload (journal-sync-request-payload journal
                                                 :comments (list comment)
                                                 :members (list member)
                                                 :attachments (list attachment)
                                                 :checkpoints (list checkpoint)
                                                 :conflicts (list conflict))))
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
          "Sync request envelopes should batch selected workspace checkpoints.")
      (is (equal (list (sync-conflict-plist conflict))
                 (getf payload :conflicts))
          "Sync request envelopes should batch selected conflict records."))))

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

(deftest sync-response-payload-applies-distributed-conflicts ()
  (let* ((registry (make-object-registry))
         (journal (make-operation-journal :workspace-id "workspace-1"
                                          :actor-id "local-user"
                                          :session-id "local-session"))
         (payload
          (list :workspace-id "workspace-1"
                :clock '(("peer-user" . 2))
                :conflicts
                (list (list :id "conflict-remote-1"
                            :target-id "paragraph-1"
                            :slot :text
                            :local-operation-id "local-op-1"
                            :remote-operation-id "remote-op-1"
                            :status :resolved-local
                            :created-at 100
                            :resolved-at 120)))))
    (let ((result (apply-sync-response-payload registry journal payload)))
      (is (= 1 (length (getf result :conflicts)))
          "Sync responses should return accepted conflict records."))
    (let ((conflict (find-journal-conflict journal "conflict-remote-1")))
      (is conflict
          "Sync responses should store distributed conflict records.")
      (is (eq :resolved-local (sync-conflict-status conflict))
          "Sync responses should preserve distributed conflict resolution status.")
      (is (= 120 (sync-conflict-resolved-at conflict))
          "Sync responses should preserve distributed conflict resolution timestamps."))))

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
    (is (= 4 (length (children-of (application-root-cell application))))
        "Application shell should render a mode/help hint, workspace, inspector, and status.")
    (let ((texts (collect-text-cells (application-root-cell application))))
      (is (find "Focus mode  |  F1 keyboard help" texts :test #'string=)
          "The shell should explain the current mode and how to open keyboard help.")
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

(defclass interrupted-workspace-payload ()
  ((staging-path :initform nil :accessor interrupted-workspace-staging-path)))

(defmethod print-object ((payload interrupted-workspace-payload) stream)
  (setf (interrupted-workspace-staging-path payload) (pathname stream))
  (write-string "(:incomplete-workspace" stream)
  (finish-output stream)
  (error "Simulated interrupted workspace write."))

(deftest workspace-write-failure-preserves-the-previous-save ()
  (let ((directory (make-test-directory "orra-save-failure")))
    (unwind-protect
         (let* ((path (merge-pathnames "workspace.sexp" directory))
                (application (make-application))
                (payload (make-instance 'interrupted-workspace-payload)))
           (save-workspace-to-file (application-workspace application) path
                                   :registry (application-registry application))
           (let ((previous (uiop:read-file-string path)))
             (is (handler-case (progn (orra::write-workspace-payload-to-file payload path) nil)
                   (error () t))
                 "An interrupted writer must signal its failure.")
             (is (string= previous (uiop:read-file-string path))
                 "A partially written payload must leave the previous file byte-for-byte intact.")
             (is (load-workspace-from-file path :registry (make-object-registry))
                 "The previous save must remain loadable after a write failure.")
             (is (equal (uiop:pathname-directory-pathname path)
                        (uiop:pathname-directory-pathname
                         (interrupted-workspace-staging-path payload)))
                 "Staging must use the destination directory, not a different filesystem.")
             (is (not (probe-file (interrupted-workspace-staging-path payload)))
                 "A failed write must clean up its partial staging file.")))
      (delete-test-directory directory))))

(deftest workspace-write-failure-does-not-create-a-new-destination ()
  (let ((directory (make-test-directory "orra-new-save-failure")))
    (unwind-protect
         (let ((path (merge-pathnames "new-workspace.sexp" directory))
               (payload (make-instance 'interrupted-workspace-payload)))
           (is (handler-case (progn (orra::write-workspace-payload-to-file payload path) nil)
                 (error () t))
               "Interrupted creation should signal an error.")
           (is (and (not (probe-file path))
                    (not (probe-file (interrupted-workspace-staging-path payload))))
               "Neither a misleading new save nor its staging file should survive failure."))
      (delete-test-directory directory))))

(deftest workspace-write-keeps-extensionless-and-relative-paths-exact ()
  (let ((directory (make-test-directory "orra-extensionless-save")))
    (unwind-protect
         (let* ((*default-pathname-defaults* directory)
                (application (make-application))
                (path (merge-pathnames "workspace" directory)))
           (is (string= "workspace" (save-workspace-to-file (application-workspace application)
                                                            "workspace"))
               "Relative save requests should retain their original return value.")
           (is (and (probe-file path)
                    (not (probe-file (merge-pathnames "workspace.tmp" directory))))
               "An extensionless target must not inherit the staging file's suffix.")
           (invoke-command application 'append-paragraph "extensionless replacement")
           (save-workspace-to-file (application-workspace application) "workspace")
           (is (workspace-has-paragraph-text-p
                (load-workspace-from-file path :registry (make-object-registry))
                "extensionless replacement")
               "Relative, extensionless paths should also support complete replacements."))
      (delete-test-directory directory))))

(deftest workspace-write-round-trips-utf8-text-and-pathnames ()
  (let ((directory (make-test-directory "orra-utf8-save")))
    (unwind-protect
         (let* ((text (coerce (mapcar #'code-char '(#x3bb #x4e2d #x1f642)) 'string))
                (path (merge-pathnames (format nil "workspace-~A.sexp" text) directory))
                (application (make-application)))
           (invoke-command application 'append-paragraph text)
           (save-workspace-to-file (application-workspace application) path
                                   :registry (application-registry application))
           (is (workspace-has-paragraph-text-p
                (load-workspace-from-file path :registry (make-object-registry)) text)
               "UTF-8 saves should preserve non-ASCII text and work with non-ASCII pathnames."))
      (delete-test-directory directory))))

(deftest workspace-write-replaces-complete-files-and-rejects-directories ()
  (let ((directory (make-test-directory "orra-save-replacement")))
    (unwind-protect
         (let* ((path (merge-pathnames "workspace with spaces.sexp" directory))
                (application (make-application)))
           (save-workspace-to-file (application-workspace application) path
                                   :registry (application-registry application))
           (invoke-command application 'append-paragraph "complete replacement")
           (is (equal path (save-workspace-to-file (application-workspace application) path
                                                   :registry (application-registry application)))
               "A completed replacement should retain the writer's pathname return value.")
           (is (workspace-has-paragraph-text-p
                (load-workspace-from-file path :registry (make-object-registry))
                "complete replacement")
               "Replacement files should contain the entire updated workspace.")
           (is (= 1 (length (uiop:directory-files directory)))
               "Successful replacements must not leave staging files behind.")
           (is (handler-case (progn (orra::write-workspace-payload-to-file nil directory) nil)
                 (error () t))
               "A directory must not be mistaken for a save filename.")
           (is (load-workspace-from-file path :registry (make-object-registry))
               "Rejecting an invalid destination must not damage nearby saves."))
      (delete-test-directory directory))))

#+(and sbcl unix)
(deftest workspace-write-preserves-permissions-and-symbolic-links ()
  (let ((directory (make-test-directory "orra-save-permissions")))
    (unwind-protect
         (let* ((path (merge-pathnames "workspace.sexp" directory))
                (link (merge-pathnames "workspace-link.sexp" directory))
                (application (make-application)))
           (save-workspace-to-file (application-workspace application) path
                                   :registry (application-registry application))
           (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat path))))
               "New workspace saves should be private to their owner.")
           (sb-posix:chmod path #o640)
           (sb-posix:symlink (namestring path) (namestring link))
           (invoke-command application 'append-paragraph "saved through a symbolic link")
           (save-workspace-to-file (application-workspace application) link
                                   :registry (application-registry application))
           (is (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat link)))
               "Saving through a symbolic link should replace its target, not the link.")
           (is (= #o640 (logand #o777 (sb-posix:stat-mode (sb-posix:stat path))))
               "Replacement should preserve the original POSIX permission bits.")
           (is (workspace-has-paragraph-text-p
                (load-workspace-from-file path :registry (make-object-registry))
                "saved through a symbolic link")
               "The resolved link target should contain the updated workspace."))
      (delete-test-directory directory))))

(deftest keyboard-save-preserves-editor-selection-and-history ()
  (uiop:with-temporary-file (:pathname path :keep t)
    (unwind-protect
         (let* ((backend (make-null-backend :stream (make-string-output-stream)
                                            :layout-height 18))
                (application (make-application :backend backend :save-path path))
                (paragraph (focused-model application))
                (original (paragraph-text paragraph)))
           (begin-editing-focused-model application)
           (insert-into-active-buffer application " saved from edit mode")
           (move-active-buffer-cursor-home application)
           (move-active-buffer-cursor-right application :extend-selection t)
           (render-application application)
           (let* ((buffer (orra::application-active-text-buffer application))
                  (state (orra::text-buffer-state buffer))
                  (journal (application-operation-journal application))
                  (operation-count (length (journal-operations journal))))
             (is (dispatch-application-key application (make-key-event :key :s :controlp t))
                 "Ctrl+S should be handled without becoming an editor keystroke.")
             (render-application application)
             (is (and (editing-active-p application)
                      (eq buffer (orra::application-active-text-buffer application))
                      (equal state (orra::text-buffer-state buffer))
                      (eq journal (application-operation-journal application))
                      (= operation-count (length (journal-operations journal))))
                 "Saving must retain the buffer, selection, undo history, and semantic journal.")
             (is (and (eq :save (getf (application-notification application) :kind))
                      (find-frame-operation backend :text
                                            (lambda (operation)
                                              (search "Saved" (fourth operation))))
                      (find-frame-operation backend :caret))
                 "Save feedback and the active editor caret should both remain visible.")
             (is (workspace-has-paragraph-text-p
                  (load-workspace-from-file path :registry (make-object-registry))
                  (paragraph-text paragraph))
                 "The active editor's current text must actually reach the saved file.")
             (undo-active-buffer-edit application)
             (is (string= original (paragraph-text paragraph))
                 "Undo should still work after saving in edit mode.")))
      (when (probe-file path) (delete-file path)))))

(defclass restartable-save-paragraph (paragraph) ())

(defmethod orra::serialize-object-record :around ((object restartable-save-paragraph))
  (restart-case (error "Simulated restartable save failure.")
    (continue-save () (call-next-method))))

(deftest lisp-save-callers-retain-original-restarts ()
  (uiop:with-temporary-file (:pathname path :keep t)
    (unwind-protect
         (let* ((application (make-application :save-path path))
                (paragraph (make-instance 'restartable-save-paragraph
                                          :id (fresh-id "paragraph") :kind :paragraph
                                          :text "restartable save"))
                (restarted-p nil))
           (register-object (application-registry application) paragraph)
           (append-child (first (children-of (root-notebook (application-workspace application))))
                         paragraph)
           (handler-bind ((error (lambda (condition)
                                   (let ((restart (find-restart 'continue-save condition)))
                                     (when restart
                                       (setf restarted-p t)
                                       (invoke-restart restart))))))
             (invoke-command application 'save-workspace))
           (is (and restarted-p
                    (eq :save (getf (application-notification application) :kind))
                    (workspace-has-paragraph-text-p
                     (load-workspace-from-file path :registry (make-object-registry))
                     "restartable save"))
               "Save feedback must not unwind away the live Lisp restart or prevent resumed saving."))
      (when (probe-file path) (delete-file path)))))

(deftest failed-save-reports-errors-and-retains-the-working-save-path ()
  (let ((directory (make-test-directory "orra-ui-save-failure")))
    (unwind-protect
         (let* ((good (merge-pathnames "workspace.sexp" directory))
                (bad (merge-pathnames "missing/workspace.sexp" directory))
                (backend (make-null-backend :stream (make-string-output-stream)))
                (application (make-application :backend backend :save-path good)))
           (invoke-command application 'save-workspace)
           (is (handler-case (progn (invoke-command application 'save-workspace bad) nil)
                 (error () t))
               "Explicit Lisp save commands must still signal a failed save.")
           (is (and (equal good (application-save-path application))
                    (eq :error (getf (application-notification application) :kind))
                    (application-dirty-p application)
                    (not (probe-file bad)))
               "A failed Save As must keep the old path and request visible error feedback.")
           (setf (application-save-path application) bad)
           (dolist (event (list (make-key-event :key :s)
                                (make-key-event :key :s :controlp t)))
             (is (dispatch-application-key application event)
                 "Both keyboard save routes must consume failures rather than open the debugger."))
           (begin-editing-focused-model application)
           (let ((state (orra::text-buffer-state (orra::application-active-text-buffer application))))
             (dispatch-application-key application (make-key-event :key :s :controlp t))
             (is (equal state (orra::text-buffer-state
                               (orra::application-active-text-buffer application)))
                 "Failed keyboard saves must leave the active editor intact."))
           (render-application application)
           (is (find-frame-operation backend :text
                                     (lambda (operation) (search "Save failed" (fourth operation))))
               "Save failures must be visible without opening the debug panel."))
      (delete-test-directory directory))))

(deftest notifications-stay-visible-while-scrolling-and-can-be-dismissed ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream)
                                     :layout-width 40 :layout-height 12))
         (application (make-application :backend backend)))
    (render-application application)
    (scroll-application application 15)
    (let ((viewport-y (application-viewport-y application))
          (object-count (length (registry-objects-list (application-registry application)))))
      (orra::set-application-notification application :error
                                          "Save failed: a lengthy explanation of a filesystem error.")
      (render-application application)
      (let ((lines (orra::application-notification-lines application)))
        (is (and (= 3 (length lines))
                 (= 9 (orra::application-viewport-height application))
                 (= viewport-y (application-viewport-y application))
                 (every (lambda (line) (<= (length line) 38)) lines)
                 (every (lambda (line)
                          (find-frame-operation
                           backend :text
                           (lambda (operation)
                             (and (string= line (fourth operation))
                                  (<= 9 (third operation) 11)))))
                        lines))
            "Notification lines should wrap into reserved visible footer rows, independent of scrolling."))
      (dispatch-application-key application (make-key-event :key :g :controlp t))
      (render-application application)
      (is (and (null (application-notification application))
               (= 12 (orra::application-viewport-height application))
               (= object-count (length (registry-objects-list (application-registry application)))))
          "Ctrl+G should dismiss feedback without creating semantic objects."))))

(deftest keyboard-help-isolates-save-but-allows-notification-dismissal ()
  (let ((directory (make-test-directory "orra-help-save")))
    (unwind-protect
         (let* ((path (merge-pathnames "workspace.sexp" directory))
                (application (make-application :save-path path)))
           (invoke-command application 'toggle-keyboard-help)
           (is (not (dispatch-application-key application (make-key-event :key :s :controlp t)))
               "Read-only help must not inherit the global save shortcut.")
           (is (not (probe-file path)) "Saving from help should not create a workspace file.")
           (orra::set-application-notification application :error "Checkpoint failed.")
           (dispatch-application-key application (make-key-event :key :g :controlp t))
           (is (and (application-help-visible-p application)
                    (null (application-notification application)))
               "Notification dismissal should remain available without leaving help."))
      (delete-test-directory directory))))

(deftest notification-feedback-does-not-change-persisted-data ()
  (uiop:with-temporary-file (:pathname path :keep t)
    (unwind-protect
         (let ((application (make-application :save-path path)))
           (invoke-command application 'save-workspace)
           (let ((before (read-persisted-payload path)))
             (orra::set-application-notification application :error "A transient save error.")
             (invoke-command application 'save-workspace)
             (let* ((after (read-persisted-payload path))
                    (loaded (make-image-restored-application :workspace-path path)))
               (is (and (equal (getf before :objects) (getf after :objects))
                        (equal (getf before :operation-journal) (getf after :operation-journal))
                        (null (application-notification loaded)))
                   "Notifications must neither add journal operations nor leak into persisted workspace state."))))
      (when (probe-file path) (delete-file path)))))

(deftest automatic-checkpoint-failures-do-not-interrupt-rendering-or-retry-every-frame ()
  (let ((directory (make-test-directory "orra-checkpoint-failure")))
    (unwind-protect
         (let* ((blocker (merge-pathnames "blocked" directory))
                (backend (make-null-backend :stream (make-string-output-stream)))
                (application (make-application :backend backend
                                               :checkpoint-directory (merge-pathnames "blocked/" directory)
                                               :checkpoint-interval 10)))
           (save-workspace-to-file (application-workspace application) blocker)
           (setf (application-last-checkpoint-at application) 100
                 (application-last-checkpoint-path application) blocker)
           (is (render-application-if-needed application)
               "Checkpoint failure should not prevent rendering the workspace.")
           (let ((attempt (orra::application-last-checkpoint-attempt-at application))
                 (event-count (length (application-event-log application))))
             (is (and (integerp attempt)
                      (= 100 (application-last-checkpoint-at application))
                      (equal blocker (application-last-checkpoint-path application))
                      (eq :error (getf (application-notification application) :kind))
                      (find-frame-operation backend :text
                                            (lambda (operation)
                                              (search "Checkpoint failed" (fourth operation)))))
                 "Failed attempts must be reported without advancing the last successful checkpoint.")
             (is (null (render-application-if-needed application))
                 "An immediate idle frame should not retry checkpointing or redraw repeatedly.")
             (is (and (= event-count (length (application-event-log application)))
                      (not (orra::checkpoint-interval-due-p application (+ attempt 9)))
                      (orra::checkpoint-interval-due-p application (+ attempt 10)))
                 "Failure retries should use the configured interval without flooding the event log.")
             (is (handler-case (progn (maybe-checkpoint-workspace application :force t) nil)
                   (error () t))
                 "Explicit Lisp checkpoint attempts must still signal their errors.")
             (setf (application-checkpoint-directory application) directory)
             (is (maybe-checkpoint-workspace application :now (+ attempt 10))
                 "Checkpointing should recover once the destination is usable.")
             (is (and (= (+ attempt 10) (application-last-checkpoint-at application))
                      (null (application-notification application)))
                 "Successful retry should update checkpoint metadata and clear its stale error.")))
      (delete-test-directory directory))))

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

(deftest workspace-persistence-round-trips-operation-journal ()
  (let* ((application (make-application :backend (make-null-backend)))
         (workspace (application-workspace application))
         (journal (make-operation-journal
                   :workspace-id (object-id workspace)
                   :actor-id "local-user"
                   :session-id "local-session"))
         (operation
          (record-local-operation journal
                                  :set-slot
                                  :target-id "paragraph-1"
                                  :payload (list :slot :text
                                                 :value "durable draft")
                                  :timestamp 100)))
    (record-local-presence journal
                           :focus-id "paragraph-1"
                           :cursor-position 7
                           :status :editing
                           :updated-at 101
                           :metadata (list :color "amber"))
    (record-local-comment journal
                          :id "comment-1"
                          :target-id "paragraph-1"
                          :body "Persist this review note."
                          :created-at 102
                          :updated-at 103
                          :metadata (list :kind :review))
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file workspace
                                     path
                                     :registry (application-registry application)
                                     :journal journal)
             (let* ((payload (read-persisted-payload path))
                    (serialized-journal (getf payload :operation-journal))
                    (restored-journal (load-operation-journal-from-file path)))
               (is (equal (operation-journal-plist journal)
                          serialized-journal)
                   "Workspace saves should embed operation journal snapshots.")
               (is (equal (operation-journal-plist journal)
                          (operation-journal-plist restored-journal))
                   "Persisted operation journals should load deterministically.")
               (is (eq :pending
                       (journal-operation-queue-status restored-journal
                                                       operation))
                   "Persisted journals should preserve pending queue state.")
               (is (find-journal-presence restored-journal
                                          "local-user"
                                          "local-session")
                   "Persisted journals should preserve local presence state.")
               (is (find-journal-comment restored-journal "comment-1")
                   "Persisted journals should preserve comment state."))
             (checkpoint-workspace-to-file workspace
                                           path
                                           :registry
                                           (application-registry application)
                                           :timestamp 200
                                           :journal journal)
             (let ((payload (read-persisted-payload path))
                   (restored-journal
                    (load-operation-journal-from-file path)))
               (is (eq :checkpoint (getf payload :mode))
                   "Checkpoint saves should preserve checkpoint mode.")
               (is (= 200 (getf payload :checkpoint-at))
                   "Checkpoint saves should preserve checkpoint timestamps.")
               (is (equal (operation-journal-plist journal)
                          (operation-journal-plist restored-journal))
                   "Checkpoint saves should embed operation journal snapshots.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest application-owns-workspace-operation-journal ()
  (let* ((application (make-application :backend (make-null-backend)))
         (workspace (application-workspace application))
         (journal (application-operation-journal application)))
    (is (typep journal 'operation-journal)
        "Applications should own an operation journal.")
    (is (string= (object-id workspace)
                 (journal-workspace-id journal))
        "Application journals should belong to the active workspace.")
    (is (stringp (journal-actor-id journal))
        "Application journals should have a local actor identity.")
    (is (stringp (journal-session-id journal))
        "Application journals should have a local session identity.")))

(deftest application-save-and-load-preserve-operation-journal ()
  (let* ((source (make-application :backend (make-null-backend)))
         (source-journal (application-operation-journal source))
         (target-id (object-id (focused-model source)))
         (operation
          (record-local-operation source-journal
                                  :set-slot
                                  :target-id target-id
                                  :payload (list :slot :text
                                                 :value "queued edit")
                                  :timestamp 100)))
    (record-local-presence source-journal
                           :focus-id target-id
                           :cursor-position 4
                           :status :editing
                           :updated-at 101)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (invoke-command source 'save-workspace path)
             (let ((loaded (make-application :backend (make-null-backend))))
               (invoke-command loaded 'load-workspace path)
               (let ((loaded-journal
                      (application-operation-journal loaded)))
                 (is (equal (operation-journal-plist source-journal)
                            (operation-journal-plist loaded-journal))
                     "Application save/load should restore collaboration state.")
                 (is (eq :pending
                         (journal-operation-queue-status loaded-journal
                                                         operation))
                     "Application save/load should restore pending operations."))))
        (when (probe-file path)
          (delete-file path))))))

(deftest application-load-without-journal-creates-workspace-journal ()
  (let* ((source (make-application :backend (make-null-backend)))
         (loaded (make-application :backend (make-null-backend)))
         (previous-journal (application-operation-journal loaded))
         (actor-id (journal-actor-id previous-journal))
         (session-id (journal-session-id previous-journal)))
    (record-local-operation previous-journal
                            :set-slot
                            :target-id "discarded-target"
                            :payload (list :slot :text :value "discarded")
                            :timestamp 100)
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             (save-workspace-to-file
              (application-workspace source)
              path
              :registry (application-registry source))
             (invoke-command loaded 'load-workspace path)
             (let ((journal (application-operation-journal loaded)))
               (is (not (eq previous-journal journal))
                   "Loading a journal-free workspace should replace stale state.")
               (is (string= (object-id (application-workspace loaded))
                            (journal-workspace-id journal))
                   "Replacement journals should belong to the loaded workspace.")
               (is (string= actor-id (journal-actor-id journal))
                   "Journal-free loads should preserve local actor identity.")
               (is (string= session-id (journal-session-id journal))
                   "Journal-free loads should preserve local session identity.")
               (is (null (journal-operations journal))
                   "Journal-free loads should not retain operations from the prior workspace.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest append-text-and-code-blocks-record-replayable-create-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "created locally"))
         (block (invoke-command application
                                'append-code-block
                                "(+ 20 22)"
                                :common-lisp))
         (operations (journal-operations journal))
         (paragraph-operation (first operations))
         (block-operation (second operations))
         (parent (parent-of paragraph)))
    (is (= 2 (length operations))
        "Appending text and code blocks should record two operations.")
    (is (eq :create-object (operation-type paragraph-operation))
        "Appending a paragraph should record a create-object operation.")
    (is (string= (object-id paragraph)
                 (operation-target-id paragraph-operation))
        "Paragraph creation should retain the new object identity.")
    (is (equal (list :kind :paragraph
                     :slots (list :text "created locally")
                     :parent-id (object-id parent))
               (operation-payload paragraph-operation))
        "Paragraph creation should record semantic slots and parent identity.")
    (is (eq :pending
            (journal-operation-queue-status journal paragraph-operation))
        "Paragraph creation should enter the local pending queue.")
    (is (eq :create-object (operation-type block-operation))
        "Appending a code block should record a create-object operation.")
    (is (string= (object-id block)
                 (operation-target-id block-operation))
        "Code block creation should retain the new object identity.")
    (is (equal (list :kind :code-block
                     :slots (list :language :common-lisp
                                  :source "(+ 20 22)")
                     :parent-id (object-id parent))
               (operation-payload block-operation))
        "Code creation should record language, source, and parent identity.")
    (is (eq :pending
            (journal-operation-queue-status journal block-operation))
        "Code creation should enter the local pending queue.")
    (let* ((peer-registry (make-object-registry))
           (peer-parent (make-instance 'section
                                       :id (object-id parent)
                                       :kind :section
                                       :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (let ((peer-paragraph
             (apply-workspace-operation peer-registry paragraph-operation))
            (peer-block
             (apply-workspace-operation peer-registry block-operation)))
        (is (string= "created locally"
                     (paragraph-text peer-paragraph))
            "Recorded paragraph creation should replay its text.")
        (is (string= "(+ 20 22)"
                     (code-block-source peer-block))
            "Recorded code creation should replay its source.")
        (is (eq :common-lisp (code-block-language peer-block))
            "Recorded code creation should replay its language.")
        (is (equal (list peer-paragraph peer-block)
                   (children-of peer-parent))
            "Replayed creations should preserve append order.")))))

(deftest paragraph-editing-records-replayable-text-range-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "hello world")))
    (setf (orra::application-focused-model-id application)
          (object-id paragraph))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-end application)
    (loop repeat 5
          do (move-active-buffer-cursor-left application
                                             :extend-selection t))
    (insert-into-active-buffer application "Orra")
    (undo-active-buffer-edit application)
    (redo-active-buffer-edit application)
    (stop-editing application)
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object
                   :delete-text-range
                   :insert-text-range
                   :delete-text-range
                   :insert-text-range
                   :delete-text-range
                   :insert-text-range)
                 (mapcar #'operation-type operations))
          "Replacement, undo, and redo should record ordered range operations.")
      (is (equal (list :slot :text :offset 6 :length 5)
                 (operation-payload (second operations)))
          "Selection replacement should first delete the selected paragraph text.")
      (is (equal (list :slot :text :offset 6 :text "Orra")
                 (operation-payload (third operations)))
          "Selection replacement should insert replacement text at the same offset.")
      (is (equal (list :slot :text :offset 6 :length 4)
                 (operation-payload (fourth operations)))
          "Undo should delete the replacement text as a new operation.")
      (is (equal (list :slot :text :offset 6 :text "world")
                 (operation-payload (fifth operations)))
          "Undo should restore the original text as a new operation.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Locally recorded text edits should remain pending until synchronized.")
      (let* ((peer-registry (make-object-registry))
             (parent (parent-of paragraph))
             (peer-parent (make-instance 'section
                                         :id (object-id parent)
                                         :kind :section
                                         :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let ((peer-paragraph
               (find-object peer-registry (object-id paragraph))))
          (is (string= "hello Orra" (paragraph-text peer-paragraph))
              "Replaying replacement, undo, and redo should converge on local text."))))))

(deftest code-editing-records-source-range-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (block (invoke-command application
                                'append-code-block
                                "(+ 1 2)")))
    (setf (orra::application-focused-model-id application)
          (object-id block))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-end application)
    (insert-into-active-buffer application " ")
    (stop-editing application)
    (let* ((operations (journal-operations journal))
           (insert-operation (second operations)))
      (is (equal '(:create-object :insert-text-range)
                 (mapcar #'operation-type operations))
          "Code insertion should follow creation with one range operation.")
      (is (equal (list :slot :source :offset 7 :text " ")
                 (operation-payload insert-operation))
          "Code edits should explicitly target the source slot.")
      (let* ((peer-registry (make-object-registry))
             (parent (parent-of block))
             (peer-parent (make-instance 'section
                                         :id (object-id parent)
                                         :kind :section
                                         :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (is (string= "(+ 1 2) "
                     (code-block-source
                      (find-object peer-registry (object-id block))))
            "Replaying a code edit should converge on the local source.")))))

(deftest code-evaluation-records-replayable-result-operation ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (block (invoke-command application
                                'append-code-block
                                "(+ 20 22)"))
         (result (invoke-command application
                                 'evaluate-code-block
                                 (object-id block)))
         (operations (journal-operations journal))
         (evaluation-operation (second operations)))
    (is (equal '(:create-object :evaluate-cell)
               (mapcar #'operation-type operations))
        "Code evaluation should follow creation with one semantic operation.")
    (is (equal (object-id block)
               (operation-target-id evaluation-operation))
        "Evaluation operations should target the evaluated code block.")
    (is (equal (list :result-id (object-id result)
                     :status :ok
                     :value 42
                     :presentation "42"
                     :input-source "(+ 20 22)"
                     :input-forms '((+ 20 22))
                     :package-name (package-name *package*)
                     :evaluated-at (result-block-evaluated-at result)
                     :environment (list :package (package-name *package*)
                                        :language :common-lisp))
               (operation-payload evaluation-operation))
        "Evaluation operations should capture the complete reproducible result.")
    (is (eq :pending
            (journal-operation-queue-status journal evaluation-operation))
        "Local evaluations should remain pending until synchronized.")
    (let* ((peer-registry (make-object-registry))
           (parent (parent-of block))
           (peer-parent (make-instance 'section
                                       :id (object-id parent)
                                       :kind :section
                                       :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (dolist (operation operations)
        (apply-workspace-operation peer-registry operation))
      (let* ((peer-block (find-object peer-registry (object-id block)))
             (peer-result (code-block-result peer-block)))
        (is (typep peer-result 'result-block)
            "Peer replay should materialize the captured result block.")
        (is (string= (object-id result) (object-id peer-result))
            "Peer replay should preserve result identity.")
        (is (eq :ok (result-block-status peer-result))
            "Peer replay should preserve successful evaluation status.")
        (is (= 42 (result-block-value peer-result))
            "Peer replay should use the captured value without evaluating source.")
        (is (equal (result-block-environment result)
                   (result-block-environment peer-result))
            "Peer replay should preserve evaluation environment metadata.")))))

(deftest failed-code-evaluation-records-replayable-result-operation ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (block (invoke-command application
                                'append-code-block
                                "missing-collaboration-value"))
         (result (invoke-command application
                                 'evaluate-code-block
                                 (object-id block)))
         (evaluation-operation
          (find :evaluate-cell
                (journal-operations journal)
                :key #'operation-type)))
    (is evaluation-operation
        "Failed evaluations should still record a semantic operation.")
    (is (eq :error (getf (operation-payload evaluation-operation) :status))
        "Failed evaluation operations should capture error status.")
    (is (string= (result-block-presentation result)
                 (getf (operation-payload evaluation-operation)
                       :presentation))
        "Failed evaluation operations should capture the displayed diagnostic.")
    (is (string= "UNBOUND-VARIABLE"
                 (getf (getf (operation-payload evaluation-operation)
                             :environment)
                       :condition-type))
        "Failed evaluation operations should preserve condition metadata.")
    (let* ((peer-registry (make-object-registry))
           (parent (parent-of block))
           (peer-parent (make-instance 'section
                                       :id (object-id parent)
                                       :kind :section
                                       :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (dolist (operation (journal-operations journal))
        (apply-workspace-operation peer-registry operation))
      (let ((peer-result
             (code-block-result
              (find-object peer-registry (object-id block)))))
        (is (eq :error (result-block-status peer-result))
            "Peer replay should preserve failed evaluation status.")
        (is (string= (result-block-presentation result)
                     (result-block-presentation peer-result))
            "Peer replay should preserve the captured evaluation diagnostic.")
        (is (equal (result-block-environment result)
                   (result-block-environment peer-result))
            "Peer replay should preserve captured condition metadata.")))))

(deftest rich-block-creation-records-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (quote (invoke-command application
                                'append-quote-block
                                "Lisp is a medium."
                                "Orra"))
         (list-block (invoke-command application
                                     'append-list-block
                                     '("one" "two")
                                     t))
         (table (invoke-command application
                                'append-table-block
                                '("Name" "Value")
                                '(("language" "Common Lisp"))))
         (tasks (invoke-command application
                                'append-task-list
                                (list "draft"
                                      (make-task-item :text "ship"
                                                      :done t))))
         (objects (list quote list-block table tasks))
         (operations (journal-operations journal))
         (parent (parent-of quote)))
    (is (equal '(:create-object :create-object :create-object :create-object)
               (mapcar #'operation-type operations))
        "Rich block commands should record one creation operation each.")
    (is (equal (mapcar #'object-id objects)
               (mapcar #'operation-target-id operations))
        "Rich block operations should preserve local object identities.")
    (is (equal (list (list :kind :quote-block
                           :slots (list :text "Lisp is a medium."
                                        :attribution "Orra")
                           :parent-id (object-id parent))
                     (list :kind :list-block
                           :slots (list :items '("one" "two")
                                        :ordered-p t)
                           :parent-id (object-id parent))
                     (list :kind :table-block
                           :slots (list :columns '("Name" "Value")
                                        :rows '(("language" "Common Lisp")))
                           :parent-id (object-id parent))
                     (list :kind :task-list
                           :slots (list :items
                                        (list (make-task-item :text "draft")
                                              (make-task-item :text "ship"
                                                              :done t)))
                           :parent-id (object-id parent)))
               (mapcar #'operation-payload operations))
        "Rich block operations should capture normalized semantic slots.")
    (is (every (lambda (operation)
                 (eq :pending
                     (journal-operation-queue-status journal operation)))
               operations)
        "Locally created rich blocks should remain pending until synchronized.")
    (let ((peer-registry (make-object-registry))
          (peer-parent (make-instance 'section
                                      :id (object-id parent)
                                      :kind :section
                                      :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (dolist (operation operations)
        (apply-workspace-operation peer-registry operation))
      (let ((peer-quote (find-object peer-registry (object-id quote)))
            (peer-list (find-object peer-registry (object-id list-block)))
            (peer-table (find-object peer-registry (object-id table)))
            (peer-tasks (find-object peer-registry (object-id tasks))))
        (is (string= "Lisp is a medium." (quote-block-text peer-quote))
            "Peer replay should preserve quote text.")
        (is (string= "Orra" (quote-block-attribution peer-quote))
            "Peer replay should preserve quote attribution.")
        (is (and (equal '("one" "two") (list-block-items peer-list))
                 (list-block-ordered-p peer-list))
            "Peer replay should preserve ordered list semantics.")
        (is (equal '(("language" "Common Lisp"))
                   (table-block-rows peer-table))
            "Peer replay should preserve normalized table data.")
        (is (equal (list (make-task-item :text "draft")
                         (make-task-item :text "ship" :done t))
                   (task-list-items peer-tasks))
            "Peer replay should preserve normalized task state.")
        (is (equal (list peer-quote peer-list peer-table peer-tasks)
                   (children-of peer-parent))
            "Peer replay should preserve rich block append order.")))))

(deftest lens-block-creation-records-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (target (invoke-command application
                                 'append-paragraph
                                 "Inspectable target"))
         (reference (invoke-command application
                                    'append-reference-block
                                    (object-id target)
                                    "target reference"
                                    "Follow this object"))
         (inspector (invoke-command application
                                    'append-inspector-block
                                    (object-id target)
                                    "target inspector"))
         (source-browser
          (invoke-command application
                          'append-source-browser-block
                          "COMMON-LISP"
                          "CAR"
                          "CAR source"))
         (cross-reference-browser
          (invoke-command application
                          'append-cross-reference-browser-block
                          "COMMON-LISP"
                          "CAR"
                          "CAR references"))
         (code (invoke-command application
                               'append-code-block
                               "(+ 20 22)"))
         (result (invoke-command application
                                 'evaluate-code-block
                                 (object-id code)))
         (stack-browser
          (invoke-command application
                          'append-stack-frame-browser-block
                          (object-id result)
                          "evaluation stack"))
         (condition-browser
          (invoke-command application
                          'append-condition-browser-block
                          (object-id result)
                          "evaluation condition"))
         (lenses (list reference
                       inspector
                       source-browser
                       cross-reference-browser
                       stack-browser
                       condition-browser))
         (operations (journal-operations journal))
         (lens-operations
          (mapcar (lambda (lens)
                    (find (object-id lens)
                          operations
                          :key #'operation-target-id
                          :test #'equal))
                  lenses))
         (parent (parent-of target)))
    (is (every (lambda (operation)
                 (and operation
                      (eq :create-object (operation-type operation))))
               lens-operations)
        "Every local lens command should record a creation operation.")
    (is (equal (object-id target)
               (getf (getf (operation-payload (first lens-operations)) :slots)
                     :target-id))
        "Reference operations should encode object targets by id.")
    (is (equal (object-id target)
               (getf (getf (operation-payload (second lens-operations)) :slots)
                     :target-id))
        "Inspector operations should encode object targets by id.")
    (is (equal (object-id result)
               (getf (getf (operation-payload (fifth lens-operations)) :slots)
                     :target-id))
        "Stack browser operations should encode result targets by id.")
    (is (every (lambda (operation)
                 (eq :pending
                     (journal-operation-queue-status journal operation)))
               lens-operations)
        "Locally created lenses should remain pending until synchronized.")
    (let ((peer-registry (make-object-registry))
          (peer-parent (make-instance 'section
                                      :id (object-id parent)
                                      :kind :section
                                      :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (dolist (operation operations)
        (apply-workspace-operation peer-registry operation))
      (let* ((peer-target (find-object peer-registry (object-id target)))
             (peer-result (find-object peer-registry (object-id result)))
             (peer-reference (find-object peer-registry (object-id reference)))
             (peer-inspector (find-object peer-registry (object-id inspector)))
             (peer-source (find-object peer-registry (object-id source-browser)))
             (peer-cross-reference
              (find-object peer-registry (object-id cross-reference-browser)))
             (peer-stack (find-object peer-registry (object-id stack-browser)))
             (peer-condition
              (find-object peer-registry (object-id condition-browser))))
        (is (eq peer-target (reference-block-target peer-reference))
            "Peer reference lenses should resolve their target object.")
        (is (eq peer-target (inspector-block-target peer-inspector))
            "Peer inspector lenses should resolve their target object.")
        (is (and (string= "COMMON-LISP"
                          (source-browser-block-package peer-source))
                 (string= "CAR" (source-browser-block-symbol peer-source))
                 (string= "CAR source" (source-browser-block-label peer-source)))
            "Peer source browsers should preserve symbol coordinates and labels.")
        (is (and (string= "COMMON-LISP"
                          (cross-reference-browser-block-package
                           peer-cross-reference))
                 (string= "CAR"
                          (cross-reference-browser-block-symbol
                           peer-cross-reference)))
            "Peer cross-reference browsers should preserve symbol coordinates.")
        (is (eq peer-result (stack-frame-browser-block-target peer-stack))
            "Peer stack browsers should resolve captured result objects.")
        (is (eq peer-result
                (condition-browser-block-target peer-condition))
            "Peer condition browsers should resolve captured result objects.")
        (is (equal (mapcar (lambda (object)
                             (find-object peer-registry (object-id object)))
                           (list target
                                 reference
                                 inspector
                                 source-browser
                                 cross-reference-browser
                                 code
                                 stack-browser
                                 condition-browser))
                   (children-of peer-parent))
            "Peer replay should preserve lens append order.")))))

(deftest repl-evaluation-records-replayable-transcript-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (repl (invoke-command application
                               'append-repl-block
                               "Peer REPL"
                               "COMMON-LISP-USER"))
         (entry (invoke-command application
                                'evaluate-repl-entry
                                (object-id repl)
                                "(+ 2 3)"))
         (result (repl-entry-result entry))
         (operations (journal-operations journal))
         (parent (parent-of repl)))
    (is (equal '(:create-object :create-object :evaluate-cell)
               (mapcar #'operation-type operations))
        "REPL evaluation should record transcript creation before its result.")
    (is (equal (list :kind :repl-block
                     :slots (list :title "Peer REPL"
                                  :package-name "COMMON-LISP-USER")
                     :parent-id (object-id parent))
               (operation-payload (first operations)))
        "REPL creation should capture title, package, and parent identity.")
    (is (equal (list :kind :repl-entry
                     :slots (list :input-source "(+ 2 3)")
                     :parent-id (object-id repl))
               (operation-payload (second operations)))
        "REPL entry creation should capture source and transcript identity.")
    (is (equal (object-id entry)
               (operation-target-id (third operations)))
        "REPL evaluation operations should target the transcript entry.")
    (is (equal (object-id result)
               (getf (operation-payload (third operations)) :result-id))
        "REPL evaluation operations should preserve result identity.")
    (is (every (lambda (operation)
                 (eq :pending
                     (journal-operation-queue-status journal operation)))
               operations)
        "Local REPL transcript operations should remain pending until synchronized.")
    (let ((peer-registry (make-object-registry))
          (peer-parent (make-instance 'section
                                      :id (object-id parent)
                                      :kind :section
                                      :title "Peer Section")))
      (register-object peer-registry peer-parent)
      (dolist (operation operations)
        (apply-workspace-operation peer-registry operation))
      (let* ((peer-repl (find-object peer-registry (object-id repl)))
             (peer-entry (find-object peer-registry (object-id entry)))
             (peer-result (find-object peer-registry (object-id result))))
        (is (and (string= "Peer REPL" (repl-block-title peer-repl))
                 (string= "COMMON-LISP-USER" (repl-block-package peer-repl)))
            "Peer replay should preserve REPL identity and package context.")
        (is (equal (list peer-entry) (children-of peer-repl))
            "Peer replay should append the entry to its transcript.")
        (is (string= "(+ 2 3)" (repl-entry-input-source peer-entry))
            "Peer replay should preserve entry source.")
        (is (eq peer-result (repl-entry-result peer-entry))
            "Peer replay should attach the captured result to the entry.")
        (is (eq peer-entry (parent-of peer-result))
            "Peer replay should preserve the result's entry parent.")
        (is (and (eq :ok (result-block-status peer-result))
                 (= 5 (result-block-value peer-result)))
            "Peer replay should use the captured REPL result without evaluation.")))))

(deftest application-slot-updates-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (target (invoke-command application
                                 'append-paragraph
                                 "Initial target"))
         (quote (invoke-command application
                                'append-quote-block
                                "Quoted text"
                                "Original attribution"))
         (list-block (invoke-command application
                                     'append-list-block
                                     '("initial")))
         (reference (invoke-command application
                                    'append-reference-block
                                    (object-id target)
                                    "Reference"))
         (parent (parent-of target)))
    (invoke-command application
                    'set-object-slot
                    (object-id quote)
                    :attribution
                    42)
    (invoke-command application
                    'set-object-slot
                    (object-id list-block)
                    :items
                    '(1 "two"))
    (invoke-command application
                    'set-object-slot
                    (object-id reference)
                    :target-id
                    (object-id quote))
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object
                   :create-object
                   :create-object
                   :create-object
                   :set-slot
                   :set-slot
                   :set-slot)
                 (mapcar #'operation-type operations))
          "Application slot commands should follow creation with semantic writes.")
      (is (equal (list (list :slot :attribution :value "42")
                       (list :slot :items :value '("1" "two"))
                       (list :slot :target-id :value (object-id quote)))
                 (mapcar #'operation-payload (subseq operations 4)))
          "Slot operations should capture normalized serializer-friendly values.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 (subseq operations 4))
          "Local slot writes should remain pending until synchronized.")
      (is (and (string= "42" (quote-block-attribution quote))
               (equal '("1" "two") (list-block-items list-block))
               (eq quote (reference-block-target reference)))
          "The command should apply normalized values to the local model.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let ((peer-quote (find-object peer-registry (object-id quote)))
              (peer-list (find-object peer-registry (object-id list-block)))
              (peer-reference
               (find-object peer-registry (object-id reference))))
          (is (string= "42" (quote-block-attribution peer-quote))
              "Peer replay should preserve normalized scalar slots.")
          (is (equal '("1" "two") (list-block-items peer-list))
              "Peer replay should preserve normalized structured slots.")
          (is (eq peer-quote (reference-block-target peer-reference))
              "Peer replay should resolve retargeted object-reference slots."))))))

(deftest application-slot-update-synchronizes-active-editor ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "draft text")))
    (setf (orra::application-focused-model-id application)
          (object-id paragraph))
    (begin-editing-focused-model application)
    (move-active-buffer-cursor-end application)
    (invoke-command application
                    'set-object-slot
                    (object-id paragraph)
                    :text
                    "published")
    (is (string= "published" (paragraph-text paragraph))
        "Slot writes should update an actively edited model.")
    (is (string= "published"
                 (text-buffer-content
                  (orra::application-active-text-buffer application)))
        "Slot writes should keep the active editor buffer synchronized.")
    (is (= (length "published")
           (text-buffer-cursor
            (orra::application-active-text-buffer application)))
        "Slot writes should clamp the active cursor to the new content.")
    (stop-editing application)
    (is (string= "published" (paragraph-text paragraph))
        "Stopping editing should not restore stale buffer content.")
    (is (equal '(:create-object :set-slot)
               (mapcar #'operation-type (journal-operations journal)))
        "Stopping after synchronization should not record duplicate text edits.")))

(deftest application-child-reorders-record-replayable-operations ()
  (let* ((workspace (make-workspace :title "Local Workspace"))
         (notebook (make-notebook :title "Local Notebook"))
         (section (make-section :title "Local Section")))
    (append-child workspace notebook)
    (append-child notebook section)
    (let* ((application (make-application :workspace workspace
                                          :backend (make-null-backend)))
           (journal (application-operation-journal application))
           (paragraph (invoke-command application
                                      'append-paragraph
                                      "first"))
           (block (invoke-command application
                                  'append-code-block
                                  "(+ 1 2)"))
           (quote (invoke-command application
                                  'append-quote-block
                                  "last"))
           (ordered-children (list quote paragraph block))
           (ordered-child-ids (mapcar #'object-id ordered-children)))
      (setf (orra::application-focused-model-id application)
            (object-id paragraph))
      (begin-editing-focused-model application)
      (clear-application-dirty application)
      (invoke-command application
                      'reorder-children
                      (object-id section)
                      ordered-child-ids)
      (let* ((operations (journal-operations journal))
             (operation (car (last operations))))
        (is (eq :reorder-children (operation-type operation))
            "Application child reorders should record a semantic operation.")
        (is (string= (object-id section) (operation-target-id operation))
            "Reorder operations should target the semantic parent.")
        (is (equal (list :child-ids ordered-child-ids)
                   (operation-payload operation))
            "Reorder operations should capture stable child identity order.")
        (is (eq :pending
                (journal-operation-queue-status journal operation))
            "Local child reorders should remain pending until synchronized.")
        (is (equal ordered-children (children-of section))
            "The command should reorder the local semantic children.")
        (is (application-dirty-p application)
            "Reordering should schedule the updated cell tree for rendering.")
        (is (and (eq paragraph (focused-model application))
                 (editing-active-p application))
            "Reordering should preserve focus and active editor ownership.")
        (let ((peer-registry (make-object-registry))
              (peer-section (make-instance 'section
                                           :id (object-id section)
                                           :kind :section
                                           :title "Peer Section")))
          (register-object peer-registry peer-section)
          (dolist (recorded-operation operations)
            (apply-workspace-operation peer-registry recorded-operation))
          (is (equal ordered-child-ids
                     (mapcar #'object-id (children-of peer-section)))
              "Peer replay should converge on the local child order.")
          (is (every (lambda (child)
                       (eq peer-section (parent-of child)))
                     (children-of peer-section))
              "Peer replay should preserve reordered child parent links."))))))

(deftest application-rejects-invalid-child-reorders-before-recording ()
  (let* ((workspace (make-workspace :title "Local Workspace"))
         (notebook (make-notebook :title "Local Notebook"))
         (section (make-section :title "Local Section")))
    (append-child workspace notebook)
    (append-child notebook section)
    (let* ((application (make-application :workspace workspace
                                          :backend (make-null-backend)))
           (journal (application-operation-journal application))
           (first (invoke-command application 'append-paragraph "first"))
           (second (invoke-command application 'append-paragraph "second"))
           (original-children (copy-list (children-of section)))
           (operation-count (length (journal-operations journal)))
           (signaledp nil))
      (handler-case
          (invoke-command application
                          'reorder-children
                          (object-id section)
                          (list (object-id first) (object-id first)))
        (error ()
          (setf signaledp t)))
      (is signaledp
          "Duplicate child identities should reject a local reorder.")
      (is (equal original-children (children-of section))
          "Rejected reorders should leave semantic children unchanged.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected reorders should not advance the local operation journal.")
      (is (eq section (parent-of second))
          "Rejected reorders should preserve existing parent links."))))

(deftest application-metadata-attachments-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "metadata target"))
         (parent (parent-of paragraph))
         (tags (list "sync" "review"))
         (metadata (list :reviewed-p t
                         :tags tags
                         :origin "local")))
    (setf (orra::application-focused-model-id application)
          (object-id paragraph))
    (clear-application-dirty application)
    (is (eq paragraph
            (invoke-command application
                            'attach-object-metadata
                            (object-id paragraph)
                            metadata))
        "Metadata attachment should return its semantic target.")
    (let* ((operations (journal-operations journal))
           (operation (car (last operations))))
      (is (equal '(:create-object :attach-metadata)
                 (mapcar #'operation-type operations))
          "Metadata attachment should follow target creation in the journal.")
      (is (string= (object-id paragraph) (operation-target-id operation))
          "Metadata operations should target stable object identity.")
      (is (equal (list :metadata
                       (list :reviewed-p t
                             :tags (list "sync" "review")
                             :origin "local"))
                 (operation-payload operation))
          "Metadata operations should capture a serializer-friendly snapshot.")
      (is (eq :pending
              (journal-operation-queue-status journal operation))
          "Local metadata attachments should remain pending until synchronized.")
      (is (and (eql t (gethash :reviewed-p (object-metadata paragraph)))
               (equal (list "sync" "review")
                      (gethash :tags (object-metadata paragraph)))
               (string= "local"
                        (gethash :origin (object-metadata paragraph))))
          "Metadata attachment should update the local semantic object.")
      (is (application-dirty-p application)
          "Metadata attachment should schedule inspector and lens rendering.")
      (setf (first tags) "mutated outside Orra")
      (is (equal (list "sync" "review")
                 (gethash :tags (object-metadata paragraph)))
          "Caller mutation should not bypass the semantic operation boundary.")
      (is (equal (list "sync" "review")
                 (getf (getf (operation-payload operation) :metadata) :tags))
          "Caller mutation should not alter the recorded operation snapshot.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (recorded-operation operations)
          (apply-workspace-operation peer-registry recorded-operation))
        (let ((peer-paragraph
               (find-object peer-registry (object-id paragraph))))
          (is (equal (list "sync" "review")
                     (gethash :tags (object-metadata peer-paragraph)))
              "Peer replay should preserve structured metadata.")
          (is (string= "local"
                       (gethash :origin (object-metadata peer-paragraph)))
              "Peer replay should converge on scalar metadata."))))))

(deftest application-rejects-malformed-metadata-before-recording ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (paragraph (invoke-command application
                                    'append-paragraph
                                    "metadata target"))
         (operation-count (length (journal-operations journal)))
         (signaledp nil))
    (handler-case
        (invoke-command application
                        'attach-object-metadata
                        (object-id paragraph)
                        (list :reviewed-p))
      (error ()
        (setf signaledp t)))
    (is signaledp
        "Odd-length metadata property lists should be rejected.")
    (is (null (gethash :reviewed-p (object-metadata paragraph)))
        "Rejected metadata should leave the semantic object unchanged.")
    (is (= operation-count (length (journal-operations journal)))
        "Rejected metadata should not advance the local operation journal.")))

(deftest application-object-links-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (source (invoke-command application
                                 'append-paragraph
                                 "link source"))
         (target (invoke-command application
                                 'append-code-block
                                 "(+ 1 2)"))
         (parent (parent-of source))
         (evidence (list "local test"))
         (metadata (list :origin "local"
                         :evidence evidence)))
    (clear-application-dirty application)
    (is (eq source
            (invoke-command application
                            'link-objects
                            (object-id source)
                            (object-id target)
                            :references
                            "supporting code"
                            metadata))
        "Object linking should return the semantic source.")
    (let* ((operations (journal-operations journal))
           (operation (car (last operations)))
           (link (first (gethash :links (object-metadata source))))
           (backlink
            (first (gethash :backlinks (object-metadata target)))))
      (is (equal '(:create-object :create-object :link-object)
                 (mapcar #'operation-type operations))
          "Object linking should follow endpoint creation in the journal.")
      (is (equal (list :target-id (object-id target)
                       :relation :references
                       :label "supporting code"
                       :metadata (list :origin "local"
                                       :evidence (list "local test")))
                 (operation-payload operation))
          "Link operations should capture canonical relation data.")
      (is (eq :pending
              (journal-operation-queue-status journal operation))
          "Local object links should remain pending until synchronized.")
      (is (and (string= (operation-id operation) (getf link :id))
               (string= (operation-id operation) (getf backlink :id)))
          "Forward links and backlinks should share operation identity.")
      (is (and (string= (object-id target) (getf link :target-id))
               (string= (object-id source) (getf backlink :source-id))
               (eq :references (getf link :relation))
               (string= "supporting code" (getf backlink :label)))
          "Local link metadata should describe both relation directions.")
      (is (application-dirty-p application)
          "Object linking should schedule relation lenses for rendering.")
      (setf (first evidence) "mutated outside Orra")
      (is (equal (list "local test")
                 (getf (getf link :metadata) :evidence))
          "Caller list mutation should not alter live relation metadata.")
      (is (equal (list "local test")
                 (getf (getf (operation-payload operation) :metadata)
                       :evidence))
          "Caller list mutation should not alter the recorded link operation.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (recorded-operation operations)
          (apply-workspace-operation peer-registry recorded-operation))
        (let* ((peer-source (find-object peer-registry (object-id source)))
               (peer-target (find-object peer-registry (object-id target)))
               (peer-link
                (first (gethash :links (object-metadata peer-source))))
               (peer-backlink
                (first (gethash :backlinks (object-metadata peer-target)))))
          (is (string= (operation-id operation) (getf peer-link :id))
              "Peer replay should preserve forward-link identity.")
          (is (string= (getf peer-link :id) (getf peer-backlink :id))
              "Peer replay should preserve matching backlink identity.")
          (is (equal (getf link :metadata) (getf peer-link :metadata))
              "Peer replay should converge on relation metadata."))))))

(deftest application-rejects-invalid-object-links-before-recording ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (source (invoke-command application
                                 'append-paragraph
                                 "link source"))
         (target (invoke-command application
                                 'append-code-block
                                 "(+ 1 2)"))
         (operation-count (length (journal-operations journal)))
         (signaledp nil))
    (set-object-metadata target :backlinks :invalid-link-storage)
    (handler-case
        (invoke-command application
                        'link-objects
                        (object-id source)
                        (object-id target)
                        :references
                        "invalid"
                        nil)
      (error ()
        (setf signaledp t)))
    (is signaledp
        "Non-list relation storage should reject a local object link.")
    (is (null (gethash :links (object-metadata source)))
        "Rejected links should not create a partial forward link.")
    (is (eq :invalid-link-storage
            (gethash :backlinks (object-metadata target)))
        "Rejected links should leave target metadata unchanged.")
    (is (= operation-count (length (journal-operations journal)))
        "Rejected links should not advance the local operation journal.")))

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
                              :checkpoint-directory directory))
                (recovery-operation nil))
           (invoke-command source
                           'append-paragraph
                           "recovered checkpoint paragraph")
           (let ((target-id (object-id (focused-model source))))
             (setf recovery-operation
                   (record-local-operation
                    (application-operation-journal source)
                    :set-slot
                    :target-id target-id
                    :payload (list :slot :text
                                   :value "queued recovery edit")
                    :timestamp 299)))
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
           (is (find (operation-id recovery-operation)
                     (journal-pending-operations
                      (application-operation-journal application))
                     :key #'operation-id
                     :test #'string=)
               "Recovery should restore pending collaboration operations.")
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

(deftest project-task-is-notebook-native-and-persistent ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (task (make-project-task
                :title "Ship knowledge layer"
                :summary "Keep tasks inside the shared object model."
                :status :blocked
                :priority :high
                :assignee-id "actor-1"
                :due-at 200
                :created-at 100
                :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section task)
    (let ((texts (collect-text-cells
                  (build-workspace-cell-tree workspace registry))))
      (is (find "Task [BLOCKED]: Ship knowledge layer" texts :test #'string=)
          "Project tasks should render as notebook-native cells.")
      (is (find "Keep tasks inside the shared object model."
                texts
                :test #'string=)
          "Project task summaries should render in the notebook.")
      (is (find "priority: HIGH  |  assignee: actor-1  |  due: 200"
                texts
                :test #'string=)
          "Project task scheduling data should render as task metadata."))
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
                    (loaded-task
                     (find-if (lambda (object)
                                (typep object 'project-task))
                              (children-of loaded-section))))
               (is (string= "Ship knowledge layer"
                            (knowledge-item-title loaded-task))
                   "Project task titles should survive persistence.")
               (is (string= "Keep tasks inside the shared object model."
                            (knowledge-item-summary loaded-task))
                   "Project task summaries should survive persistence.")
               (is (and (eq :blocked (project-task-status loaded-task))
                        (eq :high (project-task-priority loaded-task))
                        (string= "actor-1"
                                 (project-task-assignee-id loaded-task))
                        (= 200 (project-task-due-at loaded-task))
                        (= 100 (project-task-created-at loaded-task)))
                   "Project task workflow data should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest project-task-commands-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (task (invoke-command application
                               'append-project-task
                               "Implement issues"
                               "Follow the task substrate."
                               :todo
                               :urgent
                               "actor-2"
                               400))
         (parent (parent-of task)))
    (invoke-command application 'set-object-slot
                    (object-id task) :status :in-progress)
    (invoke-command application 'set-object-slot
                    (object-id task) :summary "Model issues as objects.")
    (invoke-command application 'set-object-slot
                    (object-id task) :due-at 500)
    (let ((operation-count (length (journal-operations journal)))
          (rejectedp nil))
      (handler-case
          (invoke-command application 'set-object-slot
                          (object-id task) :status :unknown)
        (error ()
          (setf rejectedp t)))
      (is rejectedp "Unsupported task statuses should be rejected.")
      (is (eq :in-progress (project-task-status task))
          "Rejected task updates should preserve the current status.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected task updates should not advance the operation journal."))
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object :set-slot :set-slot :set-slot)
                 (mapcar #'operation-type operations))
          "Project task commands should use the semantic operation protocol.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Local project task operations should remain pending until sync.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let ((peer-task (find-object peer-registry (object-id task))))
          (is (typep peer-task 'project-task)
              "Peer replay should create the project task object.")
          (is (and (string= "Implement issues"
                            (knowledge-item-title peer-task))
                   (string= "Model issues as objects."
                            (knowledge-item-summary peer-task))
                   (eq :in-progress (project-task-status peer-task))
                   (eq :urgent (project-task-priority peer-task))
                   (string= "actor-2"
                            (project-task-assignee-id peer-task))
                   (= 500 (project-task-due-at peer-task))
                   (= (project-task-created-at task)
                      (project-task-created-at peer-task)))
              "Peer replay should converge on project task workflow state.")
          (is (equal (list peer-task) (children-of peer-parent))
              "Peer replay should keep project tasks in the notebook tree."))))))

(deftest project-issue-is-notebook-native-and-persistent ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (issue (make-project-issue
                 :title "Fullscreen aborts"
                 :summary "Resizing the render surface corrupts memory."
                 :status :triaged
                 :severity :critical
                 :reporter-id "actor-1"
                 :assignee-id "actor-2"
                 :created-at 100
                 :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section issue)
    (let ((texts (collect-text-cells
                  (build-workspace-cell-tree workspace registry))))
      (is (find "Issue [TRIAGED]: Fullscreen aborts" texts :test #'string=)
          "Project issues should render as notebook-native cells.")
      (is (find "Resizing the render surface corrupts memory."
                texts
                :test #'string=)
          "Project issue summaries should render in the notebook.")
      (is (find "severity: CRITICAL  |  reporter: actor-1  |  assignee: actor-2"
                texts
                :test #'string=)
          "Project issue ownership data should render as issue metadata."))
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
                    (loaded-issue
                     (find-if (lambda (object)
                                (typep object 'project-issue))
                              (children-of loaded-section))))
               (is (string= "Fullscreen aborts"
                            (knowledge-item-title loaded-issue))
                   "Project issue titles should survive persistence.")
               (is (string= "Resizing the render surface corrupts memory."
                            (knowledge-item-summary loaded-issue))
                   "Project issue summaries should survive persistence.")
               (is (and (eq :triaged (project-issue-status loaded-issue))
                        (eq :critical
                            (project-issue-severity loaded-issue))
                        (string= "actor-1"
                                 (project-issue-reporter-id loaded-issue))
                        (string= "actor-2"
                                 (project-issue-assignee-id loaded-issue))
                        (= 100 (project-issue-created-at loaded-issue)))
                   "Project issue workflow data should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest project-issue-commands-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (issue (invoke-command application
                                'append-project-issue
                                "Caret is displaced"
                                "The insertion point and caret disagree."
                                :open
                                :high
                                "actor-1"
                                "actor-2"))
         (parent (parent-of issue)))
    (invoke-command application 'set-object-slot
                    (object-id issue) :status :in-progress)
    (invoke-command application 'set-object-slot
                    (object-id issue) :severity :critical)
    (invoke-command application 'set-object-slot
                    (object-id issue) :assignee-id "actor-3")
    (let ((operation-count (length (journal-operations journal)))
          (rejectedp nil))
      (handler-case
          (invoke-command application 'set-object-slot
                          (object-id issue) :severity :catastrophic)
        (error ()
          (setf rejectedp t)))
      (is rejectedp "Unsupported issue severities should be rejected.")
      (is (eq :critical (project-issue-severity issue))
          "Rejected issue updates should preserve the current severity.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected issue updates should not advance the operation journal."))
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object :set-slot :set-slot :set-slot)
                 (mapcar #'operation-type operations))
          "Project issue commands should use the semantic operation protocol.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Local project issue operations should remain pending until sync.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let ((peer-issue (find-object peer-registry (object-id issue))))
          (is (typep peer-issue 'project-issue)
              "Peer replay should create the project issue object.")
          (is (and (string= "Caret is displaced"
                            (knowledge-item-title peer-issue))
                   (string= "The insertion point and caret disagree."
                            (knowledge-item-summary peer-issue))
                   (eq :in-progress (project-issue-status peer-issue))
                   (eq :critical (project-issue-severity peer-issue))
                   (string= "actor-1"
                            (project-issue-reporter-id peer-issue))
                   (string= "actor-3"
                            (project-issue-assignee-id peer-issue))
                   (= (project-issue-created-at issue)
                      (project-issue-created-at peer-issue)))
              "Peer replay should converge on project issue workflow state.")
          (is (equal (list peer-issue) (children-of peer-parent))
              "Peer replay should keep project issues in the notebook tree."))))))

(deftest project-decision-is-notebook-native-and-persistent ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (decision (make-project-decision
                    :title "Keep the live image central"
                    :summary "The application needs Lisp-machine semantics."
                    :status :accepted
                    :outcome "Run the product inside SBCL."
                    :rationale "Runtime state must remain inspectable."
                    :decided-by-id "actor-1"
                    :decided-at 200
                    :created-at 100
                    :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section decision)
    (let ((texts (collect-text-cells
                  (build-workspace-cell-tree workspace registry))))
      (is (find "Decision [ACCEPTED]: Keep the live image central"
                texts
                :test #'string=)
          "Project decisions should render as notebook-native cells.")
      (is (find "outcome: Run the product inside SBCL."
                texts
                :test #'string=)
          "Project decision outcomes should render in the notebook.")
      (is (find "rationale: Runtime state must remain inspectable."
                texts
                :test #'string=)
          "Project decision rationale should render in the notebook.")
      (is (find "decided-by: actor-1  |  decided-at: 200"
                texts
                :test #'string=)
          "Project decision authorship should render as metadata."))
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
                    (loaded-decision
                     (find-if (lambda (object)
                                (typep object 'project-decision))
                              (children-of loaded-section))))
               (is (string= "Keep the live image central"
                            (knowledge-item-title loaded-decision))
                   "Project decision titles should survive persistence.")
               (is (string= "The application needs Lisp-machine semantics."
                            (knowledge-item-summary loaded-decision))
                   "Project decision context should survive persistence.")
               (is (and (eq :accepted
                            (project-decision-status loaded-decision))
                        (string= "Run the product inside SBCL."
                                 (project-decision-outcome loaded-decision))
                        (string= "Runtime state must remain inspectable."
                                 (project-decision-rationale loaded-decision))
                        (string= "actor-1"
                                 (project-decision-decided-by-id
                                  loaded-decision))
                        (= 200
                           (project-decision-decided-at loaded-decision))
                        (= 100
                           (project-decision-created-at loaded-decision)))
                   "Project decision records should survive persistence.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest project-decision-commands-record-replayable-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (decision (invoke-command application
                                   'append-project-decision
                                   "Choose text shaping"
                                   "Native-quality text is required."
                                   :proposed
                                   ""
                                   "HarfBuzz supports complex scripts."))
         (parent (parent-of decision)))
    (invoke-command application 'set-object-slot
                    (object-id decision) :status :accepted)
    (invoke-command application 'set-object-slot
                    (object-id decision) :outcome "Use HarfBuzz and FreeType.")
    (invoke-command application 'set-object-slot
                    (object-id decision) :decided-by-id "actor-2")
    (invoke-command application 'set-object-slot
                    (object-id decision) :decided-at 300)
    (let ((operation-count (length (journal-operations journal)))
          (rejectedp nil))
      (handler-case
          (invoke-command application 'set-object-slot
                          (object-id decision) :status :implemented)
        (error ()
          (setf rejectedp t)))
      (is rejectedp "Unsupported decision statuses should be rejected.")
      (is (eq :accepted (project-decision-status decision))
          "Rejected decision updates should preserve the current status.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected decision updates should not advance the operation journal."))
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object :set-slot :set-slot :set-slot :set-slot)
                 (mapcar #'operation-type operations))
          "Project decision commands should use the semantic operation protocol.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Local project decision operations should remain pending until sync.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let ((peer-decision
               (find-object peer-registry (object-id decision))))
          (is (typep peer-decision 'project-decision)
              "Peer replay should create the project decision object.")
          (is (and (string= "Choose text shaping"
                            (knowledge-item-title peer-decision))
                   (string= "Native-quality text is required."
                            (knowledge-item-summary peer-decision))
                   (eq :accepted (project-decision-status peer-decision))
                   (string= "Use HarfBuzz and FreeType."
                            (project-decision-outcome peer-decision))
                   (string= "HarfBuzz supports complex scripts."
                            (project-decision-rationale peer-decision))
                   (string= "actor-2"
                            (project-decision-decided-by-id peer-decision))
                   (= 300 (project-decision-decided-at peer-decision))
                   (= (project-decision-created-at decision)
                      (project-decision-created-at peer-decision)))
              "Peer replay should converge on the project decision record.")
          (is (equal (list peer-decision) (children-of peer-parent))
              "Peer replay should keep project decisions in the notebook tree."))))))

(deftest project-meeting-note-is-composite-and-persistent ()
  (let* ((registry (make-object-registry))
         (workspace (make-workspace :registry registry))
         (notebook (make-notebook :registry registry))
         (section (make-section :registry registry))
         (meeting (make-project-meeting-note
                   :title "Renderer planning"
                   :summary "Review the long-term GPU architecture."
                   :status :completed
                   :occurred-at 200
                   :facilitator-id "actor-1"
                   :attendee-ids '("actor-1" "actor-2")
                   :location "Orra workspace"
                   :created-at 100
                   :registry registry))
         (notes (make-paragraph :text "Keep draw lists Lisp-owned."
                                :registry registry))
         (decision (make-project-decision
                    :title "Use an explicit renderer protocol"
                    :status :accepted
                    :outcome "Separate cells from backend drawing calls."
                    :created-at 150
                    :registry registry)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section meeting)
    (append-child meeting notes)
    (append-child meeting decision)
    (let ((texts (collect-text-cells
                  (build-workspace-cell-tree workspace registry))))
      (is (find "Meeting [COMPLETED]: Renderer planning"
                texts
                :test #'string=)
          "Meeting notes should render as notebook-native containers.")
      (is (find "occurred-at: 200  |  facilitator: actor-1  |  attendees: actor-1, actor-2"
                texts
                :test #'string=)
          "Meeting participation should render as metadata.")
      (is (find "Keep draw lists Lisp-owned." texts :test #'string=)
          "Meeting notes should render embedded paragraphs.")
      (is (find "Decision [ACCEPTED]: Use an explicit renderer protocol"
                texts
                :test #'string=)
          "Meeting notes should render embedded knowledge objects."))
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
                    (loaded-meeting
                     (find-if (lambda (object)
                                (typep object 'project-meeting-note))
                              (children-of loaded-section)))
                    (loaded-children (children-of loaded-meeting)))
               (is (and (eq :completed
                            (project-meeting-note-status loaded-meeting))
                        (= 200
                           (project-meeting-note-occurred-at loaded-meeting))
                        (string= "actor-1"
                                 (project-meeting-note-facilitator-id
                                  loaded-meeting))
                        (equal '("actor-1" "actor-2")
                               (project-meeting-note-attendee-ids
                                loaded-meeting))
                        (string= "Orra workspace"
                                 (project-meeting-note-location loaded-meeting))
                        (= 100
                           (project-meeting-note-created-at loaded-meeting)))
                   "Meeting metadata should survive persistence.")
               (is (and (= 2 (length loaded-children))
                        (typep (first loaded-children) 'paragraph)
                        (typep (second loaded-children) 'project-decision))
                   "Meeting child order and types should survive persistence.")
               (is (every (lambda (child)
                            (eq loaded-meeting (parent-of child)))
                          loaded-children)
                   "Loaded meeting children should retain their parent.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest project-meeting-note-commands-record-nested-operations ()
  (let* ((application (make-application :backend (make-null-backend)))
         (journal (application-operation-journal application))
         (meeting (invoke-command application
                                  'append-project-meeting-note
                                  "Collaboration review"
                                  "Review operation replay."
                                  :in-progress
                                  300
                                  "actor-1"
                                  '("actor-1" "actor-2")
                                  "Remote"))
         (notes (invoke-command application
                                'append-paragraph
                                "Record all nested edits."
                                (object-id meeting)))
         (parent (parent-of meeting)))
    (invoke-command application 'set-object-slot
                    (object-id meeting) :status :completed)
    (invoke-command application 'set-object-slot
                    (object-id meeting) :attendee-ids
                    '("actor-1" "actor-2" "actor-3"))
    (let ((operation-count (length (journal-operations journal)))
          (rejectedp nil))
      (handler-case
          (invoke-command application 'set-object-slot
                          (object-id meeting) :status :archived)
        (error ()
          (setf rejectedp t)))
      (is rejectedp "Unsupported meeting statuses should be rejected.")
      (is (eq :completed (project-meeting-note-status meeting))
          "Rejected meeting updates should preserve the current status.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected meeting updates should not advance the operation journal."))
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object :create-object :set-slot :set-slot)
                 (mapcar #'operation-type operations))
          "Meeting and nested paragraph commands should record semantic operations.")
      (is (string= (object-id meeting)
                   (getf (operation-payload (second operations)) :parent-id))
          "Nested paragraph creation should target the meeting object.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Local meeting operations should remain pending until sync.")
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section
                                        :id (object-id parent)
                                        :kind :section
                                        :title "Peer Section")))
        (register-object peer-registry peer-parent)
        (dolist (operation operations)
          (apply-workspace-operation peer-registry operation))
        (let* ((peer-meeting
                (find-object peer-registry (object-id meeting)))
               (peer-notes
                (find-object peer-registry (object-id notes))))
          (is (typep peer-meeting 'project-meeting-note)
              "Peer replay should create the meeting note object.")
          (is (and (eq :completed
                       (project-meeting-note-status peer-meeting))
                   (equal '("actor-1" "actor-2" "actor-3")
                          (project-meeting-note-attendee-ids peer-meeting))
                   (= 300 (project-meeting-note-occurred-at peer-meeting))
                   (string= "Remote"
                            (project-meeting-note-location peer-meeting)))
              "Peer replay should converge on meeting metadata.")
          (is (and (equal (list peer-notes) (children-of peer-meeting))
                   (eq peer-meeting (parent-of peer-notes))
                   (string= "Record all nested edits."
                            (paragraph-text peer-notes)))
              "Peer replay should preserve nested meeting content."))))))

(deftest project-code-reference-is-notebook-native-and-persistent ()
  (let* ((workspace (make-workspace))
         (notebook (make-notebook))
         (section (make-section))
         (meeting (make-project-meeting-note :title "Review"))
         (target (make-code-block :source "(list :hello (+ 20 22))"))
         (reference (make-project-code-reference
                     :title "Answer calculation"
                     :summary "Discuss the nested expression."
                     :target target :form-path '(0 2) :created-at 100)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section meeting)
    (append-child meeting reference)
    (let ((texts (collect-text-cells
                  (build-workspace-cell-tree workspace
                                             (make-object-registry)))))
      (is (find "Code reference [RESOLVED]: Answer calculation"
                texts :test #'string=)
          "Code references should render inside meeting notes.")
      (is (find "Discuss the nested expression." texts :test #'string=)
          "Code references should render their knowledge summary.")
      (is (find "(+ 20 22)" texts :test #'string=)
          "Structural references should show the cited source excerpt."))
    (replace-code-block-source target "(list :hello (+ 21 22))")
    (uiop:with-temporary-file (:pathname path :keep t)
      (unwind-protect
           (progn
             ;; The referenced target is detached; collection must follow the citation.
             (save-workspace-to-file workspace path)
             (let* ((registry (make-object-registry))
                    (loaded (load-workspace-from-file path :registry registry))
                    (loaded-reference
                     (find-object registry (object-id reference)))
                    (loaded-target (find-object registry (object-id target)))
                    (application (make-application :workspace loaded)))
               (is (and (typep loaded-reference 'project-code-reference)
                        (eq loaded-target
                            (project-code-reference-target loaded-reference))
                        (string= "Answer calculation"
                                 (knowledge-item-title loaded-reference))
                        (string= "Discuss the nested expression."
                                 (knowledge-item-summary loaded-reference))
                        (= 100 (project-code-reference-created-at loaded-reference))
                        (equal '(0 2)
                               (project-code-reference-form-path loaded-reference)))
                   "Code citations and their targets should survive persistence.")
               (is (and (eq :stale
                            (project-code-reference-status loaded-reference))
                        (string= "(+ 20 22)"
                                 (project-code-reference-excerpt loaded-reference))
                        (string= "(list :hello (+ 21 22))"
                                 (code-block-source loaded-target)))
                   "Loading should preserve the old citation independently of live source.")
               (is (eq loaded-target
                       (find-object (application-registry application)
                                    (object-id target)))
                   "Application registration should follow referenced code targets.")
               (is (find "Code reference [STALE]: Answer calculation"
                         (collect-text-cells (application-root-cell application))
                         :test #'string=)
                   "Stale citations should be visible in the notebook.")))
        (when (probe-file path)
          (delete-file path))))))

(deftest project-code-reference-commands-record-replayable-anchors ()
  (let* ((application (make-application))
         (journal (application-operation-journal application))
         (code (invoke-command application 'append-code-block
                               "(list :hello (+ 20 22))"))
         (meeting (invoke-command application 'append-project-meeting-note
                                  "Code review"))
         (reference (invoke-command application 'append-project-code-reference
                                    "Answer" (object-id code) "Review this form."
                                    '(0 2) (object-id meeting)))
         (peer-registry (make-object-registry))
         (peer-parent (make-instance 'section :id (object-id (parent-of meeting))
                                     :kind :section :title "Peer Section")))
    (invoke-command application 'set-object-slot (object-id code)
                    :source "(list :changed (* 6 7))")
    (is (eq :stale (project-code-reference-status reference))
        "Source edits should invalidate structural citations.")
    (invoke-command application 'retarget-project-code-reference
                    (object-id reference) (object-id code) '(0 2))
    (invoke-command application 'set-object-slot
                    (object-id reference) :summary "Accepted during review.")
    (let ((operations (journal-operations journal)))
      (is (equal '(:create-object :create-object :create-object
                   :set-slot :set-slot :set-slot)
                 (mapcar #'operation-type operations))
          "Code citations should use the existing semantic operation protocol.")
      (is (eq :anchor (getf (operation-payload (fifth operations)) :slot))
          "Retargeting should update the entire citation in one operation.")
      (is (every (lambda (operation)
                   (eq :pending
                       (journal-operation-queue-status journal operation)))
                 operations)
          "Local citation operations should remain pending until sync.")
      (register-object peer-registry peer-parent)
      (dolist (operation operations)
        (apply-workspace-operation peer-registry operation))
      (let ((peer-reference (find-object peer-registry (object-id reference)))
            (peer-code (find-object peer-registry (object-id code)))
            (peer-meeting (find-object peer-registry (object-id meeting))))
        (is (and (eq peer-code (project-code-reference-target peer-reference))
                 (equal (project-code-reference-anchor reference)
                        (project-code-reference-anchor peer-reference))
                 (eq :resolved (project-code-reference-status peer-reference))
                 (string= "(* 6 7)"
                          (project-code-reference-excerpt peer-reference))
                 (string= "Accepted during review."
                          (knowledge-item-summary peer-reference))
                 (= (project-code-reference-created-at reference)
                    (project-code-reference-created-at peer-reference)))
            "Peer replay should converge on the complete citation and metadata.")
        (is (and (eq peer-meeting (parent-of peer-reference))
                 (equal (list peer-reference) (children-of peer-meeting)))
            "Peer replay should keep citations nested in their meeting note.")))))

(deftest project-code-reference-replay-retains-historical-source ()
  (let* ((application (make-application))
         (code (invoke-command application 'append-code-block "(+ 20 22)"))
         (reference (invoke-command application 'append-project-code-reference
                                    "Original answer" (object-id code) "" '(0)))
         (journal (application-operation-journal application))
         (peer-registry (make-object-registry))
         (peer-parent (make-instance 'section
                                     :id (object-id (parent-of reference))
                                     :kind :section)))
    (register-object peer-registry peer-parent)
    (apply-workspace-operation peer-registry (first (journal-operations journal)))
    (let ((peer-code (find-object peer-registry (object-id code))))
      (replace-code-block-source peer-code "(* 6 7)")
      (apply-workspace-operation peer-registry (second (journal-operations journal)))
      (let ((peer-reference (find-object peer-registry (object-id reference))))
        (is (and (eq peer-code (project-code-reference-target peer-reference))
                 (eq :stale (project-code-reference-status peer-reference))
                 (string= "(+ 20 22)"
                          (project-code-reference-excerpt peer-reference)))
            "Replay must retain the sender's citation even when peer source has changed.")))))

(deftest project-code-reference-rejects-invalid-citations-before-recording ()
  (let* ((application (make-application))
         (registry (application-registry application))
         (journal (application-operation-journal application))
         (code (invoke-command application 'append-code-block "(+ 20 22)"))
         (reference (invoke-command application 'append-project-code-reference
                                    "Answer" (object-id code) "" '(0)))
         (anchor (project-code-reference-anchor reference))
         (operation-count (length (journal-operations journal)))
         (object-count (length (registry-objects-list registry))))
    (flet ((reject (thunk)
             (let ((rejectedp nil))
               (handler-case (funcall thunk)
                 (error () (setf rejectedp t)))
               (is rejectedp "Invalid citations must be rejected."))))
      (dolist (path '((-1) (99) (0 99) (0 . 1) "0"))
        (reject (lambda ()
                  (invoke-command application 'append-project-code-reference
                                  "Invalid" (object-id code) "" path))))
      (is (= object-count (length (registry-objects-list registry)))
          "Rejected creations must not leave registered citation objects.")
      (dolist (value (list nil
                           (list :target-id (object-id code))
                           (list :target-id "missing" :form-path nil
                                 :source-snapshot "")
                           (list :target-id (object-id code) :form-path '(0)
                                 :source-snapshot nil)
                           (list :target-id (object-id code) :form-path '(99)
                                 :source-snapshot "(+ 20 22)")))
        (reject (lambda ()
                  (invoke-command application 'set-object-slot
                                  (object-id reference) :anchor value))))
      (reject (lambda ()
                (invoke-command application 'set-object-slot
                                (object-id reference) :form-path '(0 1))))
      (reject (lambda ()
                (invoke-command application 'set-object-slot
                                (object-id reference) :created-at 0)))
      (is (equal anchor (project-code-reference-anchor reference))
          "Rejected anchor updates must preserve the entire citation.")
      (is (= operation-count (length (journal-operations journal)))
          "Rejected citation commands must not advance the journal."))))

(deftest project-code-reference-open-restores-cursor-and-notebook ()
  (let* ((workspace (make-workspace))
         (code-notebook (make-notebook :title "Code"))
         (notes-notebook (make-notebook :title "Notes"))
         (code-section (make-section))
         (notes-section (make-section))
         (source "(list :hello (+ 20 22))")
         (code (make-code-block :source source))
         (reference (make-project-code-reference
                     :title "Operand" :target code :form-path '(0 2 1))))
    (append-child workspace code-notebook)
    (append-child workspace notes-notebook)
    (append-child code-notebook code-section)
    (append-child notes-notebook notes-section)
    (dotimes (index 20)
      (append-child code-section (make-paragraph :text (format nil "Line ~D" index))))
    (append-child code-section code)
    (append-child notes-section reference)
    (let ((application (make-application
                        :workspace workspace
                        :backend (make-null-backend :layout-height 10))))
      (orra::begin-editing-model application code)
      (move-buffer-cursor-to (orra::application-active-text-buffer application)
                             (length source))
      (orra::stop-editing application)
      (setf (orra::workspace-current-notebook workspace) notes-notebook
            (orra::application-focused-model-id application) (object-id reference))
      (rebuild-root-cell application)
      (let ((operation-count
             (length (journal-operations
                      (application-operation-journal application)))))
        (is (dispatch-application-key
             application (make-key-event :key :return))
            "Enter should open a focused code citation.")
        (is (and (eq code-notebook (root-notebook workspace))
                 (eq code (orra::focused-model application))
                 (= (search "20" source)
                    (text-buffer-cursor
                     (orra::application-active-text-buffer application)))
                 (equal '(0 2 1) (code-block-selected-form-path code)))
            "Opening should switch notebooks and override the previously saved cursor.")
        (is (plusp (application-viewport-y application))
            "Opening a citation should scroll its caret into view.")
        (is (= operation-count
               (length (journal-operations
                        (application-operation-journal application))))
            "Citation navigation should not create semantic edit operations.")))))

(deftest project-code-reference-open-declines-stale-and-detached-targets ()
  (let* ((application (make-application))
         (code (invoke-command application 'append-code-block "(+ 20 22)"))
         (reference (invoke-command application 'append-project-code-reference
                                    "Answer" (object-id code) "" '(0)))
         (original-editor (orra::focused-model application)))
    (orra::begin-editing-model application original-editor)
    (replace-code-block-source code "(* 6 7)")
    (multiple-value-bind (target status)
        (invoke-command application 'open-project-code-reference (object-id reference))
      (is (and (null target) (eq :stale status)
               (eq original-editor (orra::focused-model application))
               (string= (object-id original-editor)
                        (orra::application-active-editor-model-id application)))
          "Opening a stale citation must preserve the current focus and editor."))
    (let* ((detached (make-code-block :source "(+ 1 2)"
                                      :registry (application-registry application)))
           (detached-reference
            (invoke-command application 'append-project-code-reference
                            "Detached" (object-id detached))))
      (multiple-value-bind (target status)
          (invoke-command application 'open-project-code-reference
                          (object-id detached-reference))
        (is (and (null target) (eq :detached-target status))
            "Detached target objects must not open an invisible editor.")))))

(deftest project-code-reference-whole-block-follows-live-source ()
  (let* ((target (make-code-block :language :text :source "unparsed source"))
         (reference (make-project-code-reference :title "Source" :target target)))
    (replace-code-block-source target "changed source")
    (multiple-value-bind (resolved span status)
        (resolve-project-code-reference reference)
      (is (and (eq target resolved) (eq :resolved status)
               (equal '(0 14) span)
               (string= "changed source" (project-code-reference-excerpt reference)))
          "Whole-block references should follow current source without parsing it."))
    (let ((rejectedp nil))
      (handler-case
          (make-project-code-reference :target target :form-path '(0))
        (error () (setf rejectedp t)))
      (is rejectedp "Structural citations require a supported language."))
    (setf (code-block-language target) :common-lisp)
    (replace-code-block-source target "(+")
    (is (eq :resolved (project-code-reference-status reference))
        "Whole-block references should also work while source is incomplete.")))

(deftest object-link-queries-filter-and-isolate-results ()
  (let* ((application (make-application))
         (source (invoke-command application 'append-project-task "Renderer"))
         (target (invoke-command application 'append-project-task "Text shaping")))
    (invoke-command application 'link-objects
                    (object-id source) (object-id target) :depends-on "shaping"
                    (list :evidence (list "review")))
    (invoke-command application 'link-objects
                    (object-id source) (object-id target) :related "GPU")
    (let* ((links (object-links source :relation :depends-on))
           (backlinks (object-backlinks target :relation :depends-on)))
      (is (and (= 1 (length links)) (= 1 (length backlinks))
               (= 2 (length (object-links source)))
               (string= (getf (first links) :id) (getf (first backlinks) :id))
               (string= (object-id source) (getf (first backlinks) :source-id)))
          "Relation queries should filter both directions with matching edge identities.")
      (setf (getf (first links) :label) "changed query result"
            (first (getf (getf (first backlinks) :metadata) :evidence)) "changed evidence")
      (is (and (string= "shaping"
                        (getf (first (object-links source :relation :depends-on)) :label))
               (equal '("review")
                      (getf (getf (first (object-backlinks target :relation :depends-on))
                                  :metadata)
                            :evidence)))
          "Query result mutation must not alter live relation storage."))))

(deftest link-browser-is-notebook-native-persistent-and-replayable ()
  (let* ((application (make-application))
         (registry (application-registry application))
         (journal (application-operation-journal application))
         (task (invoke-command application 'append-project-task "GPU renderer"))
         (dependency (invoke-command application 'append-project-task "Glyph atlas"))
         (decision (invoke-command application 'append-project-decision "Use OpenGL"))
         (meeting (invoke-command application 'append-project-meeting-note "Planning")))
    (invoke-command application 'link-objects
                    (object-id task) (object-id dependency) :depends-on "atlas first")
    (invoke-command application 'link-objects
                    (object-id decision) (object-id task) :implements "chosen renderer")
    (let* ((browser (invoke-command application 'append-link-browser-block
                                    (object-id task) :both nil "Renderer relations"
                                    (object-id meeting)))
           (texts (collect-text-cells (application-root-cell application))))
      (is (and (find "Links: Renderer relations" texts :test #'string=)
               (some (lambda (text) (and (search "-> DEPENDS-ON" text)
                                         (search "Glyph atlas" text)
                                         (search "atlas first" text)))
                     texts)
               (some (lambda (text) (and (search "<- IMPLEMENTS" text)
                                         (search "Use OpenGL" text)))
                     texts))
          "Link browsers should show live titled endpoints in both directions.")
      (is (eq meeting (parent-of browser)) "Link lenses should nest inside meeting notes.")
      (invoke-command application 'step-link-browser-selection (object-id browser) 1)
      (let ((selected-key (link-browser-entry-key (link-browser-selected-entry browser))))
        (uiop:with-temporary-file (:pathname path :keep t)
          (unwind-protect
               (progn
                 (save-workspace-to-file (application-workspace application) path
                                         :registry registry :journal journal)
                 (let* ((loaded-registry (make-object-registry))
                        (loaded (load-workspace-from-file path :registry loaded-registry))
                        (loaded-browser (find-object loaded-registry (object-id browser)))
                        (loaded-task (find-object loaded-registry (object-id task))))
                   (is (and (eq loaded-task (link-browser-block-target loaded-browser))
                            (eq :both (link-browser-block-direction loaded-browser))
                            (string= "Renderer relations" (link-browser-block-label loaded-browser))
                            (equal selected-key
                                   (link-browser-entry-key
                                    (link-browser-selected-entry loaded-browser)))
                            (equal (link-browser-entries browser)
                                   (link-browser-entries loaded-browser)))
                       "Persistence should retain the lens, relations, and local selection.")
                   (let ((loaded-application (make-application :workspace loaded)))
                     (is (some (lambda (text) (search "Glyph atlas" text))
                               (collect-text-cells (application-root-cell loaded-application)))
                         "Loaded link lenses should still resolve endpoint titles."))))
            (when (probe-file path) (delete-file path)))))
      (invoke-command application 'set-object-slot (object-id browser) :direction :incoming)
      (invoke-command application 'set-object-slot (object-id browser) :relation :implements)
      (let ((peer-registry (make-object-registry))
            (peer-parent (make-instance 'section :id (object-id (parent-of meeting))
                                        :kind :section)))
        (register-object peer-registry peer-parent)
        (dolist (operation (journal-operations journal))
          (apply-workspace-operation peer-registry operation))
        (let ((peer-browser (find-object peer-registry (object-id browser)))
              (peer-task (find-object peer-registry (object-id task))))
          (is (and (eq peer-task (link-browser-block-target peer-browser))
                   (eq :incoming (link-browser-block-direction peer-browser))
                   (eq :implements (link-browser-block-relation peer-browser))
                   (equal (link-browser-entries browser) (link-browser-entries peer-browser))
                   (= 1 (length (link-browser-entries peer-browser))))
              "Peer replay should recreate the lens and its current relation filter.")
          (dolist (operation (journal-operations journal))
            (when (eq :link-object (operation-type operation))
              (apply-workspace-operation peer-registry operation)))
          (is (= 1 (length (object-backlinks peer-task)))
              "Repeated relation replay must not duplicate browser rows."))))))

(deftest link-browser-headless-rendering-resolves-workspace-endpoints ()
  (let* ((workspace (make-workspace))
         (notebook (make-notebook))
         (section (make-section))
         (task (make-project-task :title "Headless source"))
         (target (make-project-task :title "Headless target"))
         (browser (make-link-browser-block :target task)))
    (append-child workspace notebook)
    (append-child notebook section)
    (append-child section task)
    (append-child section target)
    (append-child section browser)
    (set-object-metadata task :links
                         (list (list :id "headless-link" :target-id (object-id target)
                                     :relation :related :label "headless")))
    (is (some (lambda (text) (and (search "Headless target" text)
                                  (search "-> RELATED" text)))
              (collect-text-cells (build-workspace-cell-tree workspace)))
        "Standalone cell construction should resolve endpoints without an application registry.")))

(deftest link-browser-selection-tracks-identity-and-direction ()
  (let* ((application (make-application))
         (task (invoke-command application 'append-project-task "Self review"))
         (other (invoke-command application 'append-project-task "New review")))
    (invoke-command application 'link-objects (object-id task) (object-id task)
                    :related "self")
    (let* ((browser (invoke-command application 'append-link-browser-block (object-id task)))
           (initial-key (link-browser-entry-key (link-browser-selected-entry browser))))
      (is (= 2 (length (link-browser-entries browser)))
          "A self-link should appear once in each direction.")
      (invoke-command application 'step-link-browser-selection (object-id browser) 1)
      (let ((incoming-key (link-browser-entry-key (link-browser-selected-entry browser))))
        (is (and (not (equal initial-key incoming-key))
                 (equal (second initial-key) (second incoming-key)))
            "Self-link selections must distinguish direction even with the same operation id.")
        (invoke-command application 'link-objects (object-id other) (object-id task)
                        :related "new incoming link")
        (is (equal incoming-key
                   (link-browser-entry-key (link-browser-selected-entry browser)))
            "Newly inserted relations must not move the selection to another edge.")
        (invoke-command application 'set-object-slot (object-id browser) :direction :incoming)
        (is (equal incoming-key
                   (link-browser-entry-key (link-browser-selected-entry browser)))
            "Direction changes should retain a selection still present in the projection."))
      (invoke-command application 'set-object-slot (object-id browser) :relation :depends-on)
      (is (null (link-browser-selected-entry browser))
          "An empty relation filter should clear hidden selections.")
      (is (find "No matching links."
                (collect-text-cells (application-root-cell application)) :test #'string=)
          "Empty projections should render an explicit empty state."))))

(deftest link-browser-keyboard-and-click-selection-remain-local ()
  (let* ((backend (make-null-backend :stream (make-string-output-stream) :layout-height 12))
         (application (make-application :backend backend))
         (task (invoke-command application 'append-project-task "Selected task"))
         (one (invoke-command application 'append-project-task "First endpoint"))
         (two (invoke-command application 'append-project-task "Second endpoint")))
    (invoke-command application 'link-objects (object-id task) (object-id one) :related)
    (invoke-command application 'link-objects (object-id task) (object-id two) :related)
    (let ((browser (invoke-command application 'append-link-browser-block
                                   (object-id task) :outgoing)))
      (setf (orra::application-focused-model-id application) (object-id browser))
      (render-application application)
      (let ((operation-count (length (journal-operations (application-operation-journal application)))))
        (is (dispatch-application-key application (make-key-event :key :rightbracket))
            "Bracket keys should select object links in a focused browser.")
        (is (string= (object-id one)
                     (getf (link-browser-selected-entry browser) :endpoint-id))
            "Keyboard selection should move to the next endpoint.")
        (is (plusp (application-viewport-y application))
            "Keyboard selection should scroll the selected row into view.")
        (let* ((cell (orra::cell-for-model-id (application-root-cell application) (object-id browser)))
               (row (find-if (lambda (child)
                               (let ((key (object-property child :link-key :inherit nil)))
                                 (and key
                                      (not (equal key (link-browser-entry-key
                                                       (link-browser-selected-entry browser)))))))
                             (children-of cell)))
               (bounds (cell-bounds row)))
          (setf (application-viewport-y application) (max 0 (- (orra::bounds-y bounds) 2)))
          (orra::focus-model-at-pixel application (1+ (orra::bounds-x bounds))
                                      (- (orra::bounds-y bounds) (application-viewport-y application)))
          (is (string= (object-id two)
                       (getf (link-browser-selected-entry browser) :endpoint-id))
              "Clicking a relation row should select that row, even in the same browser."))
        (is (= operation-count
               (length (journal-operations (application-operation-journal application))))
            "Keyboard and click selection should not record semantic edits.")))))

(deftest link-browser-follows-links-and-backlinks-across-notebooks ()
  (let* ((workspace (make-workspace))
         (notes (make-notebook :title "Notes"))
         (code-notebook (make-notebook :title "Code"))
         (notes-section (make-section))
         (code-section (make-section))
         (code (make-code-block :source "(error \"Navigation must not evaluate code\")")))
    (append-child workspace notes)
    (append-child workspace code-notebook)
    (append-child notes notes-section)
    (append-child code-notebook code-section)
    (dotimes (index 20)
      (append-child code-section (make-paragraph :text (format nil "filler ~D" index))))
    (append-child code-section code)
    (let* ((application (make-application :workspace workspace
                                          :backend (make-null-backend :layout-height 12)))
           (source (invoke-command application 'append-paragraph "Design discussion")))
      (invoke-command application 'link-objects (object-id source) (object-id code)
                      :references "implementation")
      (let* ((browser (invoke-command application 'append-link-browser-block
                                      (object-id source) :outgoing))
             (operation-count (length (journal-operations (application-operation-journal application)))))
        (setf (orra::application-focused-model-id application) (object-id browser))
        (dispatch-application-key application (make-key-event :key :return))
        (is (and (eq code-notebook (root-notebook workspace))
                 (eq code (focused-model application))
                 (null (code-block-result code))
                 (not (orra::editing-active-p application))
                 (plusp (application-viewport-y application)))
            "Following an outgoing link should focus visible code without evaluating or editing it.")
        (is (= operation-count
               (length (journal-operations (application-operation-journal application))))
            "Following a link should not advance the semantic journal."))
      (let ((browser (invoke-command application 'append-link-browser-block
                                     (object-id code) :incoming)))
        (invoke-command application 'open-link-browser-entry (object-id browser))
        (is (and (eq notes (root-notebook workspace))
                 (eq source (focused-model application)))
            "Following a backlink should return to its source notebook.")))))

(deftest link-browser-declines-empty-missing-and-detached-endpoints ()
  (let* ((application (make-application))
         (registry (application-registry application))
         (task (invoke-command application 'append-project-task "Linked task"))
         (browser (invoke-command application 'append-link-browser-block (object-id task)))
         (original (focused-model application)))
    (begin-editing-focused-model application)
    (multiple-value-bind (target status)
        (invoke-command application 'open-link-browser-entry (object-id browser))
      (is (and (null target) (eq :empty status)) "Empty lenses should decline navigation."))
    (set-object-metadata task :links
                         (list (list :id "missing-edge" :target-id "missing-object"
                                     :relation :related :label "unresolved")))
    (rebuild-root-cell application)
    (is (some (lambda (text) (search "missing-object (MISSING-TARGET)" text))
              (collect-text-cells (application-root-cell application)))
        "Unresolved relation endpoints should remain visible with their stable ids.")
    (multiple-value-bind (target status)
        (invoke-command application 'open-link-browser-entry (object-id browser))
      (is (and (null target) (eq :missing-target status)
               (eq original (focused-model application))
               (orra::editing-active-p application))
          "Missing targets should not replace the current focus or editor."))
    (let ((detached (make-project-task :title "Detached" :registry registry)))
      (invoke-command application 'link-objects (object-id task) (object-id detached) :related)
      (invoke-command application 'step-link-browser-selection (object-id browser) -1)
      (multiple-value-bind (target status)
          (invoke-command application 'open-link-browser-entry (object-id browser))
        (is (and (null target) (eq :detached-target status)
                 (eq original (focused-model application)))
            "Detached endpoints should not create invisible focus state.")))
    (set-object-metadata task :links :malformed)
    (rebuild-root-cell application)
    (is (some (lambda (text) (search "Cannot read links:" text))
              (collect-text-cells (application-root-cell application)))
        "Malformed relation storage should render an error without breaking the workspace.")
    (multiple-value-bind (target status)
        (invoke-command application 'open-link-browser-entry (object-id browser))
      (is (and (null target) (eq :invalid-links status))
          "Malformed relations should not enter the Lisp debugger during navigation."))))

(deftest link-browser-rejects-invalid-configuration-before-recording ()
  (let* ((application (make-application))
         (task (invoke-command application 'append-project-task "Task"))
         (browser (invoke-command application 'append-link-browser-block (object-id task)))
         (registry (application-registry application))
         (journal (application-operation-journal application))
         (operation-count (length (journal-operations journal)))
         (object-count (length (registry-objects-list registry))))
    (dolist (arguments (list (list 'append-link-browser-block (object-id task) :invalid)
                             (list 'append-link-browser-block "missing-target")
                             (list 'append-link-browser-block (object-id task) :both nil "" "missing-parent")
                             (list 'set-object-slot (object-id browser) :direction :invalid)
                             (list 'set-object-slot (object-id browser) :target-id "missing-target")))
      (let ((rejectedp nil))
        (handler-case (apply #'invoke-command application arguments)
          (error () (setf rejectedp t)))
        (is rejectedp "Invalid link browser configuration must be rejected.")))
    (is (and (= operation-count (length (journal-operations journal)))
             (= object-count (length (registry-objects-list registry)))
             (eq task (link-browser-block-target browser))
             (eq :both (link-browser-block-direction browser)))
        "Rejected lens changes must preserve semantic state and the journal.")))

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
