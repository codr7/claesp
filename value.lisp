(in-package claesp)

(defclass value-type ()
  ((name :initarg :name :initform (error "Missing name") :reader name)))

(defmethod emit-value-lisp ((type value-type) value args out)
  (cons value out))

(defmethod emit-value-call-lisp ((type value-type) value args out)
  (emit-call-lisp (value-data value) args out))

(defmethod emit-value-id-lisp ((type value-type) value args out)
  (emit-value-lisp type value args out))

(defmethod print-value ((type value-type) value out)
  (print-object value out))

(defmethod print-object ((type value-type) out)
  (format out "(Type ~a)" (name type)))

(defstruct value
  (type (error "Missing type") :type value-type)
  (data (error "Missing data")))

(defun new-value (type data)
  (make-value :type type :data data))

(defmethod emit-lisp ((value value) args out)
  (emit-value-lisp (value-type value) value args out))

(defmethod emit-call-lisp ((value value) args out)
  (emit-value-call-lisp (value-type value) value args out))

(defmethod emit-id-lisp ((value value) args out)
  (emit-value-id-lisp (value-type value) value args out))

(defmethod print-object ((value value) out)
  (print-value (value-type value) (value-data value) out))
