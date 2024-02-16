(in-package claesp)

(defstruct deque
  (front (error "Missing front") :type list)
  (back (error "Missing back") :type list)
  (count 0 :type integer))

(defun new-deque (&rest in)
  (let ((in2 (cons nil in)))
    (make-deque :front in2 :back (last in2) :count (1- (length in2)))))

(defun push-front (dq it)
  (with-slots (front back count) dq
    (push it (rest front))
    (when (zerop count)
      (setf back (rest back)))
    (incf count)))

(defun push-back (dq it)
  (with-slots (back count) dq
    (setf back (push it (rest back)))
    (incf count)))

(defun pop-front (dq)
  (with-slots (front back count) dq
    (let ((v (pop (rest front))))
      (when (zerop (decf count))
	(setf back front))
      v)))

(defmethod print-object ((dq deque) out)
  (print-object (rest (deque-front dq)) out))
