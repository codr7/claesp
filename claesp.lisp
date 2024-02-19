(defpackage claesp
  (:use cl)
  (:import-from sort compare)
  (:export))

(in-package claesp)

(define-symbol-macro version
  (slot-value (asdf:find-system 'claesp) 'asdf:version))

(defvar debug-mode t)  

(defvar load-path "")

(defun eval-from (path)
  (let ((p (merge-pathnames path load-path)))
    (with-open-file (in p)
      (let* ((load-path (merge-pathnames (directory-namestring path)
					 load-path))
	     (forms (read-forms in (new-location p)))
             (imp (compile-forms forms)))
        (funcall imp)))))
