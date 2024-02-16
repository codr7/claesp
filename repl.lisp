(in-package claesp)

(defun repl (&key (in *standard-input*) (out *standard-output*))
  (flet ((say (spec &rest args)
           (apply #'format out spec args)
           (finish-output out)))
    (let ((buffer (make-string-output-stream)))
      (tagbody
       next
         (say "  ")
         (let ((line (read-line in nil)))
           (when line
             (if (string= line "")
                 (progn
                   (setf line (get-output-stream-string buffer))
                   (restart-case
                       (let* ((forms (read-forms (make-string-input-stream line)
						 (new-location "repl")))
                              (imp (compile-forms forms))
                              (result (funcall imp)))
			 (say "~a~%" result))
                     (ignore ()
                       :report "Ignore condition.")))
                 (write-string line buffer))
             (go next)))))))


