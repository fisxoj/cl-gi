(in-package #:cl-gi)

(defparameter *default-repository* nil)

(defun gir-init ()
  (foreign-funcall "g_type_init" :void)
  (setf *default-repository*
	(foreign-funcall "g_irepository_get_default" gi-repository)))

(defun load-typelib (namespace version &optional (flags 0))
  (let ((typelib (g-irepository-load-typelib *default-repository*
					     (g-irepository-require
					      *default-repository*
					      namespace version flags (null-pointer))
					     flags (null-pointer))))
    (load-library (make-instance 'library
				 :namespace namespace
				 :typelib typelib
				 :version version))))


