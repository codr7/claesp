(in-package claesp)

(defmethod say (value out)
  (print-object value out))

(new-macro "say"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(progn
		      (let ((i 0))
			(dolist (it (list ,@(emit-forms args)))
			  (unless (zerop i)
			    (write-char #\space))
			  (say it *standard-output*)
			  (incf i)))
		      (terpri))
		   out)))
