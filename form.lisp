(in-package claesp)

(defstruct form
  (location (error "Missing location") :type location))

(defstruct (call-form (:include form))
  (target (error "Missing target") :type form)
  (args (error "Missing args") :type deque))

(defun new-call-form (location target args)
  (make-call-form :location location :target target :args args))

(defmethod print-object ((f call-form) out)
  (format out "(~a" (call-form-target f))
  (do-deque (arg (call-form-args f))
    (write-char #\space out)
    (print-object arg out))
  (write-char #\) out))

(defmethod emit-lisp ((f call-form) args out)
  (let* ((id (id-form-name (call-form-target f)))
	 (v (find-id id)))
    (unless v
      (compile-error (form-location f) "Unknown id: ~a" id))
    (emit-call-lisp v (form-location f) (call-form-args f) out)))

(defstruct (id-form (:include form))
  (name (error "Missing name") :type string))

(defun new-id-form (location name)
  (make-id-form :location location :name name))

(defmethod print-object ((f id-form) out)
  (write-string (id-form-name f) out))

(defmethod emit-lisp ((f id-form) args out)
  (let ((v (or (find-id (id-form-name f))
	       (compile-error (form-location f) "Unknown id: ~a" f))))
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
		    (new-pair ,(first (emit-lisp (pair-form-left f) args nil))
			      ,(first (emit-lisp (pair-form-right f) args nil))))
	out))

(defstruct (vector-form (:include form))
  (items (error "Missing items")))

(defun new-vector-form (location items)
  (make-vector-form :location location :items items))

(defmethod print-object ((f vector-form) out)
  (write-char #\[ out)
  (let ((i 0))
    (do-deque (item (vector-form-items f))
      (unless (zerop i)
	(write-char #\space out))
      (print-object item out)
      (incf i))
    (write-char #\] out)))

(defmethod emit-lisp ((f vector-form) args out)
  (let* ((items-lisp (emit-forms (vector-form-items f)))
	 (n (length items-lisp)))
    (cons `(new-value vector-type
		      (make-array ,n
				  :element-type 'value
				  :fill-pointer ,n
				  :initial-contents (list ,@items-lisp)))
	  out)))

(defun emit-forms (in)
  (format t "~a~%" in)
  
  (let (out)
    (tagbody
     next
       (let ((f (pop-front in)))
	 (when f
	   (setf out (emit-lisp f in out))
	   (go next))))

    (setf out (nreverse out))
    (format t "~a~%" out)
    out))

(defun emit-form (in)
  (emit-forms (new-deque in)))

(defun compile-lisp (in)
  (eval `(lambda ()
           (declare (optimize (debug ,(if debug-mode 3 0))
			      (speed ,(if debug-mode 0 3))
			      (safety ,(if debug-mode 0 3))))
	   ,@in)))
