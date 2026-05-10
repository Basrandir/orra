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

(deftest property-inheritance ()
  (let* ((registry (make-object-registry))
         (prototype (make-paragraph :text "proto" :registry registry))
         (child (make-paragraph :text "child" :registry registry)))
    (setf (object-prototype child) prototype)
    (set-object-property prototype :font :serif)
    (is (eql :serif (object-property child :font))
        "Prototype property lookup failed.")))

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
        "Workspace shell should render header and notebook.")
    (is (typep (second (children-of tree)) 'container-cell)
        "Notebook cell should be a container.")))

(deftest focus-navigation ()
  (let ((application (make-application :backend (make-null-backend))))
    (render-application application)
    (let ((first-focus (object-id (focused-model application))))
      (focus-next-model application)
      (is (not (string= first-focus
                        (object-id (focused-model application))))
          "Focus should advance to a different object.")
      (focus-previous-model application)
      (is (string= first-focus
                   (object-id (focused-model application)))
          "Focus should move back to the original object."))))

(deftest persistence-round-trip ()
  (let* ((application (make-application))
         (block nil))
    (setf block (invoke-command application 'append-code-block "(+ 20 22)"))
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
                                                  (string= "(+ 20 22)"
                                                           (code-block-source
                                                            object))))
                                           (children-of loaded-section))))
               (is (typep loaded-block 'code-block)
                   "Expected to load a code block.")
               (is (string= "42"
                            (result-block-presentation
                             (code-block-result loaded-block)))
                   "Expected persisted evaluation result.")))
        (when (probe-file path)
          (delete-file path))))))

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
