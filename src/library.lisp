(in-package :cl-gi)

(defclass library ()
  ((namespace :reader namespace :initarg :namespace)
   (version :reader version :initarg :version)
   (shared-libraries :reader shared-libraries :initarg :shared-libraries)
   (c-prefixes :reader c-prefixes :initarg :c-prefixes)
   (dependencies :reader dependencies :initarg :dependencies)
   (typelib :reader typelib :initarg :typelib)
   (package :reader library-package :initarg :package)))

(defgeneric parse-info-entry (entry type library))

(defun load-library (library)
  (with-slots (namespace c-prefixes shared-libraries package) library
    (setf package (make-package (string-upcase namespace))
	    c-prefixes (g-irepository-get-c-prefix *default-repository* namespace)
	    shared-libraries (g-irepository-get-shared-library *default-repository* namespace))
      (loop for i from 0 below (g-irepository-get-n-infos *default-repository* namespace)
	 for entry = (g-irepository-get-info *default-repository* namespace i)
	 do (parse-info-entry entry (g-base-info-get-type entry) library))))

(defmethod parse-info-entry (entry type library)
  (warn "parse-info-entry not implemented for type ~a~%" type))
