(in-package claesp)

(defclass value-type ()
  ((name :initarg :name :initform (error "Missing name") :reader name)))

(defmethod emit-value-lisp ((type value-type) value args out)
  (cons value out))

(defmethod emit-value-id-lisp ((type value-type) value args out)
  (emit-value-lisp type value args out))

(defmethod print-value ((type value-type) value out)
  (print-object value))

(defclass number-type (value-type)
  ())

(defvar number-type (make-instance 'number-type :name "Number"))

(defclass pair-type (value-type)
  ())

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod print-value ((type value-type) value out)
  (format out "~a:~a" (first value) (rest value)))

