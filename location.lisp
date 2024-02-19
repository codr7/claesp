(in-package claesp)

(defstruct (location (:conc-name nil))
	   (source (error "Missing source"))
	   (line 1 :type integer)
	   (column 0 :type integer))

(defun new-location (source)
  (make-location :source source))

(defmethod clone ((location location))
  (copy-structure location))

(defmethod print-object ((location location) out)
  (format out "~a@~a:~a" (source location) (line location) (column location)))
