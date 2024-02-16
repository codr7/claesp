(in-package claesp)

(defstruct (location (:conc-name nil))
	   (source (error "Missing source") :type string)
	   (line 1 :type integer)
	   (column 0 :type integer))

(defun new-location (source)
  (make-location :source source))

(defmethod clone ((loc location))
  (copy-structure loc))

(defmethod print-object ((loc location) out)
  (format out "~a@~a:~a" (source loc) (line loc) (column loc)))
