(in-package claesp)

(defclass value-type ()
  ((name :initarg :name :initform (error "Missing name") :reader name)))

(defmethod emit-value-lisp ((type value-type) value args out)
  (cons value out))

(defmethod emit-value-call-lisp ((type value-type) value location args out)
  (emit-call-lisp (value-data value) location args out))

(defmethod emit-value-id-lisp ((type value-type) value args out)
  (emit-value-lisp type value args out))

(defmethod value-T? ((type value-type) value)
  (value-data value))

(defmethod value-compare ((type value-type) x y)
  (compare (value-data x) (value-data y)))

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

(defmethod emit-call-lisp ((value value) location args out)
  (emit-value-call-lisp (value-type value) value location args out))

(defmethod emit-id-lisp ((value value) args out)
  (emit-value-id-lisp (value-type value) value args out))

(defmethod T? ((value value))
  (value-T? (value-type value) value))

(defmethod compare ((x value) (y value))
  (let ((xt (value-type x))
	(yt (value-type y)))
    (if (eq xt yt)
	(value-compare (value-type x) x y)
	(compare (name xt) (name yt)))))
    
(defmethod print-object ((value value) out)
  (print-value (value-type value) (value-data value) out))
