(in-package claesp)

(defclass meta-type (value-type)
  ())

(defmethod initialize-instance :after ((self value-type) &rest args)
  (declare (ignore args))
  (let ((name (name self)))
    (bind-id (name self) (new-value (if (string= name "Meta") self meta-type) self))))

(defmethod lisp-value-type ((type meta-type))
  'value-type)

(defvar meta-type (make-instance 'meta-type :name "Meta"))

(defclass bit-type (value-type)
  ())

(defvar bit-type (make-instance 'bit-type :name "Bit"))

(defmethod lisp-value-type ((type meta-type))
  'bit)

(defmethod print-value ((type bit-type) value out)
  (write-char (if (value-data value) #\T #\F) out))

(defvar true (new-value bit-type t))
(defvar false (new-value bit-type nil))

(bind-id "T" true)
(bind-id "F" false)

(defclass nil-type (value-type)
  ())

(defvar nil-type (make-instance 'nil-type :name "Nil"))

(defmethod lisp-value-type ((type nil-type))
  'null)

(defmethod print-value ((type nil-type) value out)
  (write-char #\_ out))

(defvar _ (new-value nil-type nil))

(bind-id "_" _)

(defclass number-type (value-type)
  ())

(defvar number-type (make-instance 'number-type :name "Number"))

(defmethod lisp-value-type ((type number-type))
  'number)

(defclass pair-type (stack-trait value-type)
  ())

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod lisp-value-type ((type pair-type))
  'pair)

(defmethod value-push ((type pair-type) value it)
  (new-value pair-type (push-value (value-data value) it)))

(defclass string-type (value-type)
  ())

(defvar string-type (make-instance 'string-type :name "String"))

(defmethod lisp-value-type ((type string-type))
  'string)

(defmethod print-value ((type string-type) value out)
  (format out "\"~a\"" (value-data value)))

(defmethod say-value ((type string-type) value out)
  (write-string (value-data value) out))

(defclass variable-type (value-type)
  ())

(defvar variable-type (make-instance 'variable-type :name "Variable"))

(defmethod lisp-value-type ((type variable-type))
  'variable)

(defmethod emit-value-lisp ((type variable-type) value args out)
  (cons (value-data value) out))

(defmethod print-value ((type variable-type) value out)
  (format out "(Variable ~a)" (value-data value)))

(defmethod say-value ((type variable-type) value out)
  (say (find-id (value-data value)) out))

(defclass vector-type (stack-trait value-type)
  ())

(defvar vector-type (make-instance 'vector-type :name "Vector"))

(defmethod lisp-value-type ((type vector-type))
  'vector)

(defmethod print-value ((type vector-type) value out)
  (let ((v (value-data value)))
    (write-char #\[ out)
    (dotimes (i (length v))
      (unless (zerop i)
	(write-char #\space out))
      (print-object (aref v i) out))
    (write-char #\] out)))

(defmethod value-push ((type vector-type) value it)
  (new-value vector-type (push-value (value-data value) it)))
