(in-package claesp)

(defstruct macro
  (name (error "Missing name") :type string)
  (body (error "Missing body") :type function))

(defun new-macro (name body)
  (let ((macro (make-macro :name name :body body)))
    (bind-id name (new-value macro-type macro))
    macro))

(defmethod emit-call-lisp ((macro macro) location args out)
  (funcall (macro-body macro) location args out))

(defmethod print-object ((macro macro) out)
  (format out "(Macro ~a)" (macro-name macro)))

(new-macro "=0"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(new-value bit-type
			       (zerop (value-data ,(first (emit-forms args)))))
		   out)))

(new-macro "+"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(new-value number-type
			       (apply #' + (mapcar #'value-data 
						   (list ,@(emit-forms args)))))
		   out)))

(new-macro "-"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(new-value number-type
			       (apply #'- (mapcar #'value-data 
						  (list ,@(emit-forms args)))))
		   out)))

(new-macro "<"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(labels ((rec (prev in out)
			       (if (and in (eq out :lt))
				   (rec (first in) 
					(rest in) 
					(compare prev (first in)))
				   out)))
		      (let ((in (list ,@(emit-forms args))))
			(new-value bit-type (eq :lt (rec (first in) (rest in) :lt)))))
		   out)))

(new-macro ">"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(labels ((rec (prev in out)
			       (if (and in (eq out :gt))
				   (rec (first in) 
					(rest in) 
					(compare prev (first in)))
				   out)))
		      (let ((in (list ,@(emit-forms args))))
			(new-value bit-type (eq :gt (rec (first in) (rest in) :gt)))))
		   out)))

(new-macro "="
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(labels ((rec (prev in out)
			       (if (and in out)
				   (rec (first in) (rest in) (equal-values? prev (first in)))
				   out)))
		      (let ((in (list ,@(emit-forms args))))
			(new-value bit-type (rec (first in) (rest in) t))))
		   out)))

(new-macro "and"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(first (member-if-not #'T? (list ,@(emit-forms args))))
		   out)))

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
					       (float
						(/ (- (get-internal-run-time) run-t)
						   internal-time-units-per-second)))
				    (new-value number-type
					       (float
						(/ (- (get-internal-real-time) real-t)
						   internal-time-units-per-second))))))
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

(new-macro "do"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(progn ,@(emit-forms args))
		   out)))

(new-macro "^"
	   (lambda (location args out)
	     (declare (ignore location))
	     (let* ((id-form (pop-front args))
		    (id (id-form-name id-form))
		    (function-args-form (pop-front args))
		    (function-args (deque-items 
				    (vector-form-items function-args-form))))
	       (bind-id id (new-value function-type id))
	       (dolist (arg function-args)
		 (let ((arg-id (id-form-name arg)))
		   (bind-id arg-id (new-value variable-type arg-id))))
	       (cons `(defmethod ,(intern id 'claesp-user) 
			  (,@(mapcar (lambda (f)
				       (intern (id-form-name f)))
			      function-args))
			,@(emit-forms args))
		     out))))

(new-macro "if"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(if (T? ,(first (emit-form (pop-front args))))
			,(first (emit-form (pop-front args)))
			,(first (emit-form (pop-front args))))
		   out)))

(new-macro "load"
	   (lambda (location args out)
	     (declare (ignore location))
	     (let ((path (first (emit-form (pop-front args)))))
	       (cons `(eval-from (value-data ,path)) out))))

(new-macro "or"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(first (member-if #'T? (list ,@(emit-forms args))))
		   out)))
