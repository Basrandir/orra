(asdf:defsystem "orra-tests"
  :depends-on ("orra")
  :serial t
  :components ((:file "tests/package")
               (:file "tests/main"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call :orra.tests :run-all-tests)))
