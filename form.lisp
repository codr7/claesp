(in-package claesp)

(defstruct form
  (location (error "Missing location") :type location))

(defstruct (id-form (:include form))
  (name (error "Missing name") :type keyword))

(defun new-id-form (location name)
  (make-id-form :location location :name name))

(defmethod print-object ((f id-form) out)
  (write-string (symbol-name (id-form-name f)) out))

(defmethod emit-lisp ((f id-form) args out)
  (let ((v (or (find-id ns (id-name f))
	       (error "Unknown identifier: ~a" f))))
    (emit-id-lisp v args out)))

(defstruct (literal-form (:include form))
  (value (error "Missing value")))

(defun new-literal-form (location value)
  (make-literal-form :location location :value value))

(defmethod print-object ((f literal-form) out)
  (print-object (literal-form-value f) out))

(defmethod emit-lisp ((f literal-form) args out)
  (emit-lisp (literal-form-value f) args out))

(defstruct (pair-form (:include form))
  (left (error "Missing left"))
  (right (error "Missing right")))

(defun new-pair-form (location left right)
  (make-pair-form :location location :left left :right right))

(defmethod print-object ((f pair-form) out)
  (format out "~a:~a" (pair-form-left f) (pair-form-right f)))

(defmethod emit-lisp ((f pair-form) args out)
  (cons `(new-value pair-type
		    (cons ,(first (emit-lisp (pair-form-left f) args nil))
			  ,(first (emit-lisp (pair-form-right f) args nil))))
	out))

(defun compile-forms (in)
  (let (out)
    (tagbody
     next
       (let ((f (pop-front in)))
	 (when f
	   (setf out (emit-lisp f in out))
	   (go next))))
    (setf out (nreverse out))
    
    (compile nil `(lambda ()
                    (declare (optimize (debug ,(if debug-mode 3 0))
                                       (speed ,(if debug-mode 0 3))
                                       (safety ,(if debug-mode 0 3))))
                    ,@out))))
