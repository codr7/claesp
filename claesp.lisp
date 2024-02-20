(defpackage claesp
  (:use cl)
  (:import-from sort compare)
  (:export))

(in-package claesp)

(define-symbol-macro version
    (multiple-value-bind (v)
	(parse-integer (slot-value (asdf:find-system 'claesp) 'asdf:version))
      v))

(defvar debug-mode t)  

(defvar load-path "")

(defun eval-from (path)
  (let ((p (merge-pathnames path load-path)))
    (with-open-file (in p)
      (let ((load-path (merge-pathnames (directory-namestring path)
					load-path))
	    (forms (read-forms in (new-location p))))
	(format t "~a~%" forms)
	(let ((lisp (emit-forms forms)))
	  (format t "~a~%" lisp)
          (funcall (compile-lisp lisp)))))))
