(in-package claesp)

(defun id-char? (c)
  (and c
       (not (member c '(#\newline #\tab #\space #\( #\) #\[ #\] #\: #\")
		    :test #'char=))
       (graphic-char-p c)))
    
(defun read-id (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-id))
    (unread-char c in)
    (unless (id-char? c)
      (return-from read-id)))

  (let* ((flocation (clone location))
	 (s (with-output-to-string (s)
              (tagbody
               next
                 (let ((c (read-char in nil)))
                   (when (id-char? c)
		     (incf (column location))
		     (write-char c s)
		     (go next))
		   (when c
                     (unread-char c in)))))))
    (push-back out (new-id-form flocation s))
    t))

(defun read-digits (in location multiplier)
  (let ((v 0)
	(n 0))
    (tagbody
     next
       (let ((c (read-char in nil)))
	 (when c
	   (if (digit-char-p c)
	       (progn
		 (incf (column location))
		 (setf v (+ (* v multiplier) (char-digit c)))
		 (incf n)
		 (go next))
               (unread-char c in)))))
  (values v n)))

(defun read-number (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-number))
    (unread-char c in)
    (unless (or (char= c #\.) (digit-char-p c))
      (return-from read-number)))
  
  (let ((flocation (clone location))
	(v 0))
    (tagbody
     next
       (let ((c (read-char in nil)))
	 (when c
	   (cond
	     ((char= c #\.)
	      (incf (column location))
	      (multiple-value-bind (dv dn) (read-digits in location 10)
		  (setf v (+ v (/ dv (expt 10 dn))))))
	     ((digit-char-p c)
	      (unread-char c in)
	      (setf v (read-digits in location 10))
	      (go next))
	     (t (unread-char c in))))))
    
    (push-back out (new-literal-form flocation (new-value number-type v))))
    
    t)

(defun read-pair (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-pair))
    (unless (char= c #\:)
      (unread-char c in)
      (return-from read-pair)))

  (incf (column location))
  
  (unless (read-form in out location)
    (syntax-error location "Missing right side of pair"))
  
  (let ((right (pop-back out))
	(left (pop-back out)))
    (push-back out (new-pair-form (form-location left) left right)))

  t)

(defun read-string (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-string))
    (unless (char= c #\")
      (unread-char c in)
      (return-from read-string)))

  (let ((flocation (clone location))
	(s (with-output-to-string (s)
             (tagbody
              next
		(incf (column location))
                (let ((c (read-char in nil)))
		  (unless c
		    (syntax-error location "Unexpected end of string"))
		  (unless (char= c #\")
		    (write-char c s)
		    (go next)))))))
    (incf (column location))
    (push-back out (new-literal-form flocation (new-value string-type s))))
  
  t)

(defun read-vector (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-vector))
    (unless (char= c #\[)
      (unread-char c in)
      (return-from read-vector)))

  (let ((flocation (clone location))
	(items (new-deque)))
    (incf (column location))
    
    (tagbody
     next
       (read-ws in out location)
       
       (let ((c (read-char in nil)))
	 (unless c
	   (syntax-error location "Unexpected end of vector"))
	 
	 (unless (char= c #\])
	   (unread-char c in)
	   
	   (unless (read-form in items location)
	     (syntax-error location "Unexpected end of vector"))

	   (go next))))

    (incf (column location))
    (push-back out (new-vector-form flocation items)))

  t)

(defun read-call (in out location)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-call))
    (unless (char= c #\()
      (unread-char c in)
      (return-from read-call)))

  (let ((flocation (clone location))
	target
	(args (new-deque)))
    (incf (column location))

    (unless (read-form in out location)
      (syntax-error location "Missing call target"))
    
    (setf target (pop-back out))
    
    (tagbody
     next
       (read-ws in out location)
       
       (let ((c (read-char in nil)))
	 (unless c
	   (syntax-error location "Unexpected end of call"))
	 
	 (unless (char= c #\))
	   (unread-char c in)
	   
	   (unless (read-form in args location)
	     (syntax-error location "Unexpected end of call"))

	   (go next))))

    (incf (column location))
    (push-back out (new-call-form flocation target args)))
  t)

(defun read-ws (in out location)
  (declare (ignore out))
  
  (tagbody
   next
     (let ((c (read-char in nil)))
       (when c
         (case c
           (#\newline
            (incf (line location))
            (setf (column location) 1))
           ((#\space #\tab)
            (incf (column location)))
	   (otherwise
            (unread-char c in)
	    (return-from read-ws)))
	 (go next))))

  nil)

(defun read-form (in out location)
  (dolist (r (list #'read-ws
		   #'read-number
		   #'read-id
		   #'read-string
		   #'read-call
		   #'read-pair
		   #'read-vector))
    (when (funcall r in out location)
      (read-pair in out location)
      (return-from read-form t)))
  nil)

(defun read-forms (in location)
  (let ((out (new-deque)))
    (tagbody
     next
       (when (read-form in out location)
	 (go next)))
    out))
