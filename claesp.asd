(asdf:defsystem claesp
  :name "claesp"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description ""
  :licence "MIT"
  :depends-on ()
  :serial t
  :components ((:file "sort")
	       (:file "claesp")
	       (:file "claesp-user")
	       (:file "utilities")
	       (:file "location")
	       (:file "errors")
	       (:file "value")
	       (:file "types")
	       (:file "macro")
	       (:file "io")
	       (:file "deque")
	       (:file "form")
	       (:file "read")
	       (:file "repl")))
