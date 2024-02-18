(in-package claesp)

(defun id-char? (c)
  (and c
       (not (member c '(#\newline #\tab #\space #\( #\) #\[ #\] #\:) :test #'char=))
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
    (unless (or (char= c #\-) (char= c #\.) (digit-char-p c))
      (return-from read-number)))
  
  (let ((flocation (clone location))
	(v 0)
	negative)
    (tagbody
     next
       (let ((c (read-char in nil)))
	 (when c
	   (cond
	     ((char= c #\-)
	      (incf (column location))
	      (setf negative t)
	      (go next))
	     ((char= c #\.)
	      (incf (column location))
	      (multiple-value-bind (dv dn) (read-digits in location 10)
		  (setf v (+ v (/ dv (expt 10 dn))))))
	     ((digit-char-p c)
	      (unread-char c in)
	      (setf v (read-digits in location 10))
	      (go next))
	     (t (unread-char c in))))))
    
    (push-back out
	       (new-literal-form flocation (new-value number-type
						 (if negative (- v) v)))))
    
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
    (error "Syntax error"))
  
  (let ((right (pop-back out))
	(left (pop-back out)))
    (push-back out (new-pair-form (form-location left) left right)))

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
	   (error "Syntax error"))
	 
	 (unless (char= c #\])
	   (unread-char c in)
	   
	   (unless (read-form in items location)
	     (error "Syntax error"))

	   (go next))))
       
    (push-back out (new-vector-form flocation items)))
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
            (setf (column location) 0))
           ((#\space #\tab)
            (incf (column location)))
	   (otherwise
            (unread-char c in)
	    (return-from read-ws)))
	 (go next))))
  nil)

(defun read-form (in out location)
  (dolist (r (list #'read-ws #'read-number #'read-id #'read-pair #'read-vector))
    (when (funcall r in out location)
      (return-from read-form t)))
  nil)

(defun read-forms (in location)
  (let ((out (new-deque)))
    (tagbody
     next
       (when (read-form in out location)
	 (go next)))
    out))
