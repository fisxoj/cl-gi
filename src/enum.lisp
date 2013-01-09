(in-package :cl-gi)

(defmethod parse-info-entry (entry (type (eql :enum)) (library library))
  (parse-enum entry library))

(defmacro parse-enum (entry library)
  `(let* ((*package* (library-package ,library))
	  (name (translate-camelcase-name (g-base-info-get-name ,entry)))
	  (slots (loop
		    for i from 0 below (g-enum-info-get-n-values ,entry)
		    for member = (g-enum-info-get-value ,entry i)
		    collect (list (c-name-to-lisp-keyword (g-base-info-get-name member))
				  (g-value-info-get-value member)))))
     (export  name)
     (print-eval `(defcenum ,name
		    ,@slots))))


