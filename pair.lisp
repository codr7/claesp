(in-package claesp)

(defstruct (pair (:conc-name))
  (left (error "Missing left") :type value)
  (right (error "Missing right") :type value))

(defun new-pair (left right)
  (make-pair :left left :right right))

(defmethod peek-value ((pair pair))
  (left pair))

(defmethod print-object ((pair pair) out)
  (format out "~a:~a" (left pair) (right pair)))

(defmethod push-value ((pair pair) it)
  (new-pair it (new-value pair-type pair)))

(defmethod pop-value ((pair pair))
  (values (left pair) (right pair)))
