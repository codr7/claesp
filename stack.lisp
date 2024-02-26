(in-package claesp)

(defclass stack-trait ()
  ())

(defmethod value-peek ((type stack-trait) value)
  (peek-value (value-data value)))

(defmethod value-push ((type stack-trait) value it)
  (push-value (value-data value) it))

(defmethod value-pop ((type stack-trait) value)
  (pop-value (value-data value)))


(defmethod peek-value ((value value))
  (value-peek (value-type value) value))

(defmethod pop-value ((value value))
  (value-pop (value-type value) value))

(defmethod push-value ((value value) it)
  (value-push (value-type value) value it))


(defmethod peek-value ((value vector))
  (aref value (1- (length value))))

(defmethod pop-value ((value vector))
  (vector-pop value))

(defmethod push-value ((value vector) it)
  (vector-push-extend it value)
  value)  

(new-macro "peek"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(peek-value (progn ,@(emit-form (pop-front args)))) out)))

(new-macro "pop"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(pop-value (progn ,@(emit-form (pop-front args)))) out)))

(new-macro "push"
	   (lambda (location args out)
	     (declare (ignore location))
	     (cons `(push-value (progn ,@(emit-form (pop-front args)))
				(progn ,@(emit-form (pop-front args))))
		   out)))
