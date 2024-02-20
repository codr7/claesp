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

(new-macro "benchmark"
	   (lambda (location args out)
	     (declare (ignore location))
	     (let ((max (value-data (first (emit-form (pop-front args))))))
	       (cons
		`(let ((run-t (get-internal-run-time))
		       (real-t (get-internal-real-time)))
		   (dotimes (i ,max)
		     (progn
		       ,@(emit-forms args)))
		   (new-value pair-type
			      (cons (new-value number-type
					       (/ (- (get-internal-run-time) run-t)
						  internal-time-units-per-second))
				    (new-value number-type
					       (/ (- (get-internal-real-time) real-t)
						  internal-time-units-per-second)))))
		out))))

(new-macro "check"
	   (lambda (location args out)
	     (let ((expected (emit-form (pop-front args)))
		   (actual (emit-forms args)))
	       (cons `(let ((expected (progn ,@expected))
			    (actual (progn ,@actual)))
			(unless (equal-values? expected actual)
			  (check-error ,location
				       ',(first expected)
				       ',(first actual))))
		     out))))

(new-macro "load"
	   (lambda (location args out)
	     (declare (ignore location))
	     (let ((path (first (emit-form (pop-front args)))))
	       (cons `(eval-from (value-data ,path)) out))))
