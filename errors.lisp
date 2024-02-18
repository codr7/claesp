(in-package claesp)

(define-condition location-error (error)
  ((location :initarg :location
             :initform (error "Missing :location")
             :reader location)
   (message :initarg :message
	    :initform (error "Missing :message")
	    :reader message)))

(define-condition syntax-error (location-error) ()
  (:report (lambda (condition stream)
	     (format stream
		     "Syntax error in ~a: ~a"
		     (location condition) (message condition)))))

(defun syntax-error (location spec &rest args)
  (error 'syntax-error :location location :message (apply #'format nil spec args)))
