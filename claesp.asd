(asdf:defsystem claesp
  :name "claesp"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description ""
  :licence "MIT"
  :depends-on ()
  :serial t
  :components ((:file "claesp")
	       (:file "utilities")
	       (:file "location")
	       (:file "value-type")
	       (:file "value")
	       (:file "deque")
	       (:file "form")
	       (:file "read")
	       (:file "repl")))
