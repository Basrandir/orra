(asdf:defsystem "orra"
    :description "Lisp-first structural editor"
    :author "Bassam Saeed"
    :license "Apache-2.0"
    :version "0.1.0"
    :serial t
    :depends-on ("sdl2" "sdl2-ttf" "trivial-garbage")
    :components ((:file "src/package")
                 (:file "src/util")
                 (:file "src/model")
                 (:file "src/operations")
                 (:file "src/text-buffer")
                 (:file "src/notebook")
                 (:file "src/code-lens")
                 (:file "src/commands")
                 (:file "src/cells")
                 (:file "src/backends")
                 (:file "src/persistence")
                 (:file "src/runtime")))
