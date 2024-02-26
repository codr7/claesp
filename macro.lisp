(in-package claesp)

(defclass macro-type (value-type)
  ())

(defvar macro-type (make-instance 'macro-type :name "Macro"))

(defmethod lisp-value-type ((type macro-type))
  'macro)

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

(new-macro "^"
	   (lambda (location args out)
	     (let* ((id-form (pop-front args))
		    (id? (eq (type-of id-form) 'id-form))
		    (id (when id? (id-form-name id-form)))
		    (lisp-id (intern (symbol-name (gensym (string-upcase id)))))
		    (function-args-form (if id? (pop-front args) id-form))
		    (function-args (vector-form-items function-args-form)))
	       (when id?
		 (bind-id id (new-value function-type 
					(new-function id 
						      lisp-id 
						      location 
						      function-args))))

	       (let ((function-arg-lisp-ids nil))
		 (do-deque (arg function-args)
		   (let* ((arg-id (id-form-name arg))
			  (lisp-id (intern (symbol-name (gensym (string-upcase arg-id))))))
		     (bind-id arg-id (new-value variable-type lisp-id))
		     (push lisp-id function-arg-lisp-ids)))

		 (cons (if id?
			   `(defun ,lisp-id 
				(,@(nreverse function-arg-lisp-ids))
			      ,@(emit-forms args))
			   `(new-value lambda-type 
				       (lambda (,@(nreverse function-arg-lisp-ids))
					 ,@(emit-forms args))))
		       out)))))

(new-macro "and"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(first (member-if-not #'T? (list ,@(emit-forms args)))) out)))

(new-macro "backup"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(save-lisp-and-die 
		     (value-data ,(first (emit-form (pop-front args))))
                     :toplevel #'repl
                     :executable t)
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
			      (new-pair (new-value number-type
						   (float
						    (/ (- (get-internal-run-time) run-t)
						       internal-time-units-per-second)))
					(new-value number-type
						   (float
						    (/ (- (get-internal-real-time) real-t)
						       internal-time-units-per-second))))))
		out))))

(new-macro "call"
	   (lambda (location args out)
	     (let ((args-lisp (emit-forms args)))
	       (cons `(call ,(first args-lisp) ,location (list ,@(rest args-lisp)))
		     out))))

(new-macro "check"
	   (lambda (location args out)
	     (let ((expected (emit-form (pop-front args)))
		   (actual (emit-forms args)))
	       (cons `(let ((expected (progn ,@expected))
			    (actual (progn ,@actual)))
			(unless (equal-values? expected actual)
			  (check-error ,location
				       `,(progn ,@expected)
				       `,(progn ,@actual))))
		     out))))

(new-macro "do"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(progn ,@(emit-forms args)) out)))

(new-macro "if"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(if (T? ,(first (emit-form (pop-front args))))
			,(first (emit-form (pop-front args)))
			,(first (emit-form (pop-front args))))
		   out)))

(new-macro "isa?"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(let ((xt (value-type (progn 
					    ,@(emit-form (pop-front args)))))
			  (yt (value-data (progn 
					    ,@(emit-form (pop-front args))))))
		      (new-value bit-type (subtypep (type-of xt) (type-of yt))))
		   out)))

(new-macro "load"
	   (lambda (location args out)
	     (declare (ignore location))
	     (let ((path (first (emit-form (pop-front args)))))
	       (cons `(eval-from (value-data ,path)) out))))

(new-macro "or"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(first (member-if #'T? (list ,@(emit-forms args)))) out)))
