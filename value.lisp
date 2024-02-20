(in-package claesp)

(defclass value-type ()
  ((name :initarg :name :initform (error "Missing name") :reader name)))

(defmethod emit-value-lisp ((type value-type) value args out)
  (cons value out))

(defmethod emit-value-call-lisp ((type value-type) value location args out)
  (emit-call-lisp (value-data value) location args out))

(defmethod emit-value-id-lisp ((type value-type) value args out)
  (emit-value-lisp type value args out))

(defmethod value-equals? ((type value-type) x y)
  (equal-values (value-data x) (value-data y)))

(defmethod value-T? ((type value-type) value)
  (T? (value-data value)))

(defmethod value-compare ((type value-type) x y)
  (compare (value-data x) (value-data y)))

(defmethod print-value ((type value-type) value out)
  (print-object (value-data value) out))

(defmethod print-object ((type value-type) out)
  (format out "(Type ~a)" (name type)))

(defmethod say-value ((type value-type) value out)
  (print-value type value out))

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

(defmethod T? (value)
  t)

(defmethod T? ((value (eql 0)))
  nil)

(defmethod T? ((value (eql nil))))

(defmethod T? ((value (eql t)))
  t)

(defmethod T? ((value string))
  (not (string= value "")))

(defmethod compare ((x value) (y value))
  (let ((xt (value-type x))
	(yt (value-type y)))
    (if (eq xt yt)
	(value-compare (value-type x) x y)
	(compare (name xt) (name yt)))))
    
(defmethod print-object ((value value) out)
  (print-value (value-type value) value out))

(defmethod say ((value value) out)
  (say-value (value-type value) value out))

(defmethod equal-values? (x y)
  (equalp x y))

(defmethod equal-values? ((x number) (y number))
  (= x y))

(defmethod equal-values? ((x string) (y string))
  (string= x y))

(defmethod equal-values? ((x value) (y value))
  (let ((xt (value-type x))
	(yt (value-type y)))
    (when (eq xt yt)
      (value-equals? xt x y))))
