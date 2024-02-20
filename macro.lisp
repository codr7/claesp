(in-package claesp)

(defstruct macro
  (name (error "Missing name") :type string)
  (body (error "Missing body") :type function))

(defun new-macro (name body)
  (let ((macro (make-macro :name "check" :body body)))
    (bind-id name (new-value macro-type macro))
    macro))

(defmethod emit-call-lisp ((macro macro) location args out)
  (funcall (macro-body macro) location args out))

(defmethod print-object ((macro macro) out)
  (format out "(Macro ~a)" (macro-name macro)))

(new-macro "check"
	   (lambda (location args out)
	     (let ((expected (emit-form (pop-front args)))
		   (actual (emit-forms args)))
	       (cons `(let ((expected (progn ,@expected))
			    (actual (progn ,@actual)))
			(unless (eq (compare expected actual)
				    :eq)
			  (check-error ,location
				       ',(first expected)
				       ',(first actual))))
		     out))))

(new-macro "load"
	   (lambda (location args out)
	     (let ((path (first (emit-form (pop-front args)))))
	       (format t "PATH: ~a~%" path)
	       (cons `(eval-from (value-data ,path)) out))))
