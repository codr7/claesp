(defpackage claesp
  (:use cl)
  (:import-from sort compare)
  (:export))

(in-package claesp)

(define-symbol-macro version 1)

(defvar debug-mode t)  
