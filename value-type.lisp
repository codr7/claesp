(in-package claesp)

(defclass meta-type (value-type)
  ())

(defvar meta-type (make-instance 'meta-type :name "Meta"))

(defmethod initialize-instance :after ((self value-type) &rest args)
  (declare (ignore args))
  (let ((name (name self)))
    (bind-id (name self) (new-value (if (string= name "Meta") self meta-type) self))))

(defclass bit-type (value-type)
  ())

(defvar bit-type (make-instance 'bit-type :name "Bit"))

(defmethod print-value ((type bit-type) value out)
  (write-char (if value #\T #\F) out))

(defclass number-type (value-type)
  ())

(defvar number-type (make-instance 'number-type :name "Number"))

(defclass pair-type (value-type)
  ())

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod print-value ((type pair-type) value out)
  (format out "~a:~a" (first value) (rest value)))

(defclass vector-type (value-type)
  ())

(defvar vector-type (make-instance 'pair-type :name "Vector"))

(defmethod print-value ((type vector-type) value out)
  (write-char #\[ out)
  (dotimes (i (length value))
    (unless (zerop i)
      (write-char #\space out))
    (print-object (aref value i) out))
  (write-char #\] out))

