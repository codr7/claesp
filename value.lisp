(in-package claesp)

(defstruct value
  (type (error "Missing type") :type value-type)
  (data (error "Missing data")))

(defun new-value (type data)
  (make-value :type type :data data))

(defmethod emit-lisp ((value value) args out)
  (emit-value-lisp (value-type value) (value-data value) args out))

(defmethod emit-id-lisp ((value value) args out)
  (emit-value-id-lisp (value-type value) (value-data value) args out))

(defmethod print-object ((value value) out)
  (print-value (value-type value) (value-data value) out))
