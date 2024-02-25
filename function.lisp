(in-package claesp)

(defclass user-function ()
  ((name :initarg :name :initform (error "Missing name") :reader name)
   (lisp-name :initarg :lisp-name :initform (error "Missing lisp-name") :reader lisp-name)
   (args :initarg :args :initform (error "Missing args") :reader args)
   (location :initarg :location :initform (error "Missing location") :reader location)))

(defun new-function (name lisp-name location args)
  (make-instance 'user-function :name name :lisp-name lisp-name :args args :location location))

(defmethod call ((target function) location args)
  (apply target args))

(defmethod call ((target user-function) location args)
  (call (find-id (lisp-name target)) location args))

(defmethod emit-call-lisp ((func user-function) location args out)
  (cons `(,(lisp-name func)
	  ,@(emit-forms args))
	out))

(defmethod emit-lisp ((func user-function) args out)
  (cons (intern (name func) 'claesp-user) out))

(defclass function-type (value-type)
  ())

(defvar function-type (make-instance 'function-type :name "Function"))

(defmethod lisp-value-type ((type function-type))
  'user-function)

(defmethod print-value ((type function-type) value out)
  (format out "(Function ~a)" (value-data value)))

(defmethod say-value ((type function-type) value out)
  (say (find-id (value-data value)) out))

(defclass lambda-type (value-type)
  ())

(defmethod lisp-value-type ((type function-type))
  'function)

(defmethod print-value ((type function-type) value out)
  (format out "(Lambda ~a)" (value-data value)))

(defvar lambda-type (make-instance 'lambda-type :name "Lambda"))

