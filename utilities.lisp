(in-package claesp)

(defun char-digit (c)
  (- (char-int c) (char-int #\0)))

(defun neq (&rest args)
  (not (apply #'eq args)))
