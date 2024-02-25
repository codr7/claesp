(in-package claesp)

(defstruct (deque-item (:conc-name))
  (prev nil)
  (next nil)
  (it (error "Missing :it")))

(defun new-deque-item (it &optional prev next)
  (let ((item (make-deque-item :it it :prev prev :next next)))
    (unless prev (setf (prev item) item))
    (unless next (setf (next item) item))
    item))

(defmethod print-object ((item deque-item) out)
  (format out "~a<-~a->~a" (it (prev item)) (it item) (it (next item))))

(defun deque-item-append (head item)
  (let ((next (next head)))
    (setf (next head) item
	  (prev next) item
	  (prev item) head
	  (next item) next)
    item))

(defun deque-item-remove (item)
  (let ((prev (prev item))
	(next (next item)))
    (setf (next prev) (next item))
    (setf (prev next) (prev item)))
  (it item))

(defclass deque ()
  ((head :initform (error "Missing head") :initarg :head :reader head)
   (len :initform 0 :initarg :len :accessor len)))

(defun new-deque (&rest in)
  (let* ((head (new-deque-item nil))
	 (deque (make-instance 'deque :head head)))
    (dolist (item in)
      (push-back deque item))
    deque))

(defun push-front (deque it)
  (incf (len deque))
  (append (head deque)
	  (new-deque-item it)))

(defun pop-front (deque)
  (with-slots (len) deque
    (unless (zerop len)
      (decf len)
      (deque-item-remove (next (head deque))))))

(defun push-back (deque it)
  (incf (len deque))
  (deque-item-append (prev (head deque))
		     (new-deque-item it)))

(defun pop-back (deque)
  (with-slots (len) deque
    (unless (zerop len)
      (decf len)
      (deque-item-remove (prev (head deque))))))

(defun deque-items (deque)
  (let ((result nil))
    (do-deque (it deque)
      (push it result))
    (nreverse result)))
  
(defmethod print-object ((deque deque) out)
  (write-char #\< out)
  (let ((i 0))
    (do-deque (it deque)
      (unless (zerop i)
	(write-char #\space out))
      (print-object it out)
      (incf i)))
  (write-char #\> out))

(defmacro do-deque ((item deque) &body body)
  (let (($head (gensym))
	($prev (gensym))
	($next (gensym)))
    `(let* ((,$head (head ,deque))
	    (,$prev ,$head)
	    (,item nil))
       (tagbody
	  rec
	  (let ((,$next (next ,$prev)))
	    (unless (eq ,$next ,$head)
	      (setf ,item (it ,$next)
		    ,$prev ,$next)
	      ,@body
	      (go rec)))))))

(defun run-deque-tests ()
  (let ((q (new-deque 1 2)))
    (assert (= (len q) 2))
    (push-back q 3)
    (assert (= (len q) 3))
    (assert (= (pop-back q) 3))
    (dotimes (i 2)
      (assert (= (pop-front q) (1+ i))))
    (assert (zerop (len q)))
    (assert (null (pop-back q)))))
