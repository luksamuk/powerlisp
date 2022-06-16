(asdf:defsystem #:powerlisp
  :description "Task automation tool for Unix powerusers."
  :author      "Lucas S. Vieira <lucasvieira@protonmail.com>"
  :license     "MIT"
  :version     "2.0"
  :serial      t
  :depends-on  (#:uiop)
  :components  ((:file "package")
                (:file "defaults")
                (:file "customization")
                (:file "load-config")
                (:file "macros-procs")
                (:file "input")
                (:file "tools")
                (:file "entry-point")
                (:static-file "config-example.sexp")))
