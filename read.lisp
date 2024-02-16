(in-package claesp)

(defun id-char? (c)
  (and c
       (not (member c '(#\newline #\tab #\space #\( #\) #\[ #\] #\:) :test #'char=))
       (graphic-char-p c)))
    
(defun read-id (in out loc)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-id))
    (unread-char c in)
    (unless (id-char? c)
      (return-from read-id)))

  (let* ((floc (clone loc))
	 (s (with-output-to-string (s)
              (tagbody
               next
                 (let ((c (read-char in nil)))
                   (when (id-char? c)
		     (incf (column loc))
		     (write-char c s)
		     (go next))
		   (when c
                     (unread-char c in)))))))
    (push-back out (new-id-form floc (intern s :keyword)))
    t))

(defun read-number (in out loc)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-number))
    (unread-char c in)
    (unless (or (char= c #\-) (char= c #\.) (digit-char-p c))
      (return-from read-number)))
  
  (let ((floc (clone loc))
	(v 0)
	negative)
    (tagbody
     next
       (let ((c (read-char in nil)))
         (when c
	   (cond
	     ((char= c #\-)
	      (setf negative t)
	      (go next))
	     ((char= c #\.)
	      (format t "TODO: Handle decimals~%"))
	     ((digit-char-p c)
              (incf (column loc))
              (setf v (+ (* v 10) (char-digit c)))
              (go next)))
           (unread-char c in))))
    
    (push-back out
	       (new-literal-form floc (new-value number-type
						 (if negative (- v) v))))
    
    t))

(defun read-pair (in out loc)
  (let ((c (read-char in nil)))
    (unless c
      (return-from read-pair))
    (unless (char= c #\:)
      (unread-char c in)
      (return-from read-pair)))
  
  (unless (read-form in out loc)
    (error "Syntax error"))
  
  (let ((left (pop-front out))
	(right (pop-front out)))
    (push-back out (new-pair-form (form-location left) left right)))

  t)

(defun read-ws (in out loc)
  (declare (ignore out))
  
  (tagbody
   next
     (let ((c (read-char in nil)))
       (when c
         (case c
           (#\newline
            (incf (line loc))
            (setf (column loc) 0))
           ((#\space #\tab)
            (incf (column loc)))
	   (otherwise
            (unread-char c in)
	    (return-from read-ws)))
	 (go next))))
  nil)

(defun read-form (in out loc)
  (dolist (r (list #'read-ws #'read-number #'read-id #'read-pair))
    (when (funcall r in out loc)
      (return-from read-form t)))
  nil)

(defun read-forms (in loc)
  (let ((out (new-deque)))
    (tagbody
     next
       (when (read-form in out loc)
	 (go next)))
    out))
