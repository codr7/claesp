(defpackage claesp-user
  (:export))

(in-package claesp)

(defun find-id (id)
  (symbol-value (find-symbol id 'claesp-user)))

(defun (setf find-id) (value id)
  (let* ((sid (intern id 'claesp-user))
	 (*package* (find-package 'claesp-user)))
    (setf (symbol-value sid) value)))

(defun bind-id (id value)
  (setf (find-id id) value))
