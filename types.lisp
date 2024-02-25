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

(defclass function-type (value-type)
  ())

(defvar function-type (make-instance 'function-type :name "Function"))

(defmethod emit-value-call-lisp ((type function-type) value location args out)
  (cons `(,(first (emit-form value))
	  ,@(emit-forms args))
	out))

(defmethod emit-value-lisp ((type function-type) value args out)
  (cons (intern (value-data value) 'claesp-user) out))

(defmethod lisp-value-type ((type function-type))
  'function)

(defmethod print-value ((type function-type) value out)
  (format out "(Function ~a)" (value-data value)))

(defmethod say-value ((type function-type) value out)
  (say (find-id (value-data value)) out))

(defclass macro-type (value-type)
  ())

(defvar macro-type (make-instance 'macro-type :name "Macro"))

(defmethod emit-call-value-lisp ((type macro-type) value location args out)
  (format t "macro-type emit-call-value-lisp ~a ~%" value)
  (let ((macro (value-data value)))
    (funcall (macro-body macro) location args out)))

(defmethod lisp-value-type ((type macro-type))
  'macro)

(defclass nil-type (value-type)
  ())

(defvar nil-type (make-instance 'nil-type :name "Nil"))

(defmethod lisp-value-type ((type nil-type))
  'null)

(defmethod print-value ((type nil-type) value out)
  (write-char #\_ out))
´
(defvar _ (new-value nil-type nil))

(bind-id "_" _)

(defclass number-type (value-type)
  ())

(defvar number-type (make-instance 'number-type :name "Number"))

(defmethod lisp-value-type ((type number-type))
  'number)

(defclass pair-type (value-type)
  ())

(defvar pair-type (make-instance 'pair-type :name "Pair"))

(defmethod lisp-value-type ((type pair-type))
  'pair)

(defmethod print-value ((type pair-type) value out)
  (let ((d (value-data value)))
    (format out "~a:~a" (first d) (rest d))))

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
  (cons (intern (value-data value)) out))

(defmethod print-value ((type variable-type) value out)
  (format out "(Variable ~a)" (value-data value)))

(defmethod say-value ((type variable-type) value out)
  (say (find-id (value-data value)) out))

(defclass vector-type (value-type)
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

