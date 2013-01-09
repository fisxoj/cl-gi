(in-package :cl-gi)

(defmethod parse-info-entry (entry (type (eql :constant)) (library library))
  (parse-constant entry library))

(defmacro parse-constant (constant-info library)
  `(let* ((*package* (library-package ,library))
	  (name (lispify-gir-constant (g-base-info-get-name ,constant-info)))
	  (value (constant-value ,constant-info)))
     (export (intern name))
     (print-eval `(defconstant ,(intern name)
		    ,value))))

(defun constant-value (constant-info)
  (with-foreign-object (arg 'gi-argument)
    (g-constant-info-get-value constant-info arg)
    (let ((type (type-info-get-cffi-type (g-constant-info-get-type constant-info))))
      (print type)
      (mem-ref arg type))))
