(in-package claesp)

(defclass meta-type (value-type)
  ())

(defmethod initialize-instance :after ((self value-type) &rest args)
  (declare (ignore args))
  (let ((name (name self)))
    (bind-id (name self) (new-value (if (string= name "Meta") self meta-type) self))))

(defvar meta-type (make-instance 'meta-type :name "Meta"))

(defclass bit-type (value-type)
  ())

(defvar bit-type (make-instance 'bit-type :name "Bit"))

(defmethod print-value ((type bit-type) value out)
  (write-char (if (value-data value) #\T #\F) out))

(defvar true (new-value bit-type t))
(defvar false (new-value bit-type nil))

(bind-id "T" true)
(bind-id "F" false)

(defclass macro-type (value-type)
  ())

(defvar macro-type (make-instance 'macro-type :name "Macro"))

(defclass nil-type (value-type)
  ())

(defvar nil-type (make-instance 'nil-type :name "Nil"))

(defmethod print-value ((type nil-type) value out)
  (write-char #\_ out))

(defvar _ (new-value nil-type nil))

(bind-id "_" _)

(defclass number-type (value-type)
  ())

(defvar number-type (make-instance 'number-type :name "Number"))

(defclass pair-type (value-type)
  ())

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod print-value ((type pair-type) value out)
  (let ((d (value-data value)))
    (format out "~a:~a" (first d) (rest d))))

(defclass string-type (value-type)
  ())

(defvar string-type (make-instance 'string-type :name "String"))

(defmethod print-value ((type string-type) value out)
  (format out "\"~a\"" (value-data value)))

(defclass vector-type (value-type)
  ())

(defvar vector-type (make-instance 'vector-type :name "Vector"))

(defmethod print-value ((type vector-type) value out)
  (let ((v (value-data value)))
    (write-char #\[ out)
    (dotimes (i (length v))
      (unless (zerop i)
	(write-char #\space out))
      (print-object (aref v i) out))
    (write-char #\] out)))

