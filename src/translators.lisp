(in-package #:cl-gi)

(define-foreign-type g-string-array ()
  ()
  (:actual-type :pointer)
  (:simple-parser g-string-array))

(defmethod translate-from-foreign (value (type g-string-array))
  (collect-strings value))

(defun collect-strings (foreign-ptr)
  (unless (null-pointer-p foreign-ptr)
    (loop for i upfrom 0
       for s = (mem-aref foreign-ptr :string i)
       until (null s)
       collect s)))

(defun type-info-get-cffi-type (type-info)
  (cdr (assoc (gi::g-type-tag-to-string
	       (gi::g-type-info-get-tag type-info))
	      gi::+gtype-ctype+
	      :test 'string=)))
