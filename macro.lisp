(in-package claesp)

(defstruct macro
  (name (error "Missing name") :type string)
  (body (error "Missing body") :type function))

(defun new-macro (name body)
  (let ((macro (make-macro :name "check" :body body)))
    (bind-id name (new-value macro-type macro))
    macro))

(defmethod emit-call-lisp ((macro macro) args out)
  (funcall (macro-body macro) args out))

(defmethod print-object ((macro macro) out)
  (format out "(Macro ~a)" (macro-name macro)))

(defvar check-macro (new-macro "check"
			       (lambda (args out)

				 )))
