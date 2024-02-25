(in-package claesp)

(defun repl (&key (in *standard-input*) (out *standard-output*))
  (flet ((say (spec &rest args)
           (apply #'format out spec args)
           (finish-output out)))
    (let ((buffer (make-string-output-stream)))
      (tagbody
       next 
         (say "  ")
         (let ((line (read-lne in nil)))
           (when line
             (if (string= line "")
                 (progn
                   (setf line (get-output-stream-string buffer))
                   (restart-case
                       (let* ((forms (read-forms (make-string-input-stream line)
						 (new-location "repl")))
                              (result (funcall (compile-lisp (emit-forms forms)))))
			 (say "~a~%" (if result result #\_)))
                     (ignore ()
                       :report "Ignore condition.")))
                 (write-string line buffer))
             (go next)))))))


