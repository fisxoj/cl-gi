(in-package :cl-gir)

(defgeneric read-repository (path format)
  (:documentation "Begins the parsing of .gir files into lisp"))

(defmethod read-repository ((path string) (format t))
  (read-repository (parse-namestring path)))

(defmethod read-repository ((path pathname) (format t))
  (with-open-file (s path)
    (parse-repository (parse s))))

#|

Main repository-parsing function

|#

(defun parse-repository (root)
  (loop for node in (node-children root)
     ;; FIXME: This looks wrong
     for namespace = nil

     ;; Find namespace node and save it
     when (string= (node-name node) "namespace")
     do (setf namespace  node)

     ;; Collect the includes and packages
     when (string= (node-name node) "include")
     collect node into includes
     when (string= (node-name node) "package")
     collect node into packages

     ;; Feed all of our new information into the repository maker
     finally (make-repository namespace includes packages)))

(defun make-repository (namespace includes packages)
  (if-let (attrs (node-attrs namespace))
    (when-let* ((name (attr-value "name" attrs))
		(version (attr-value "version" attrs))
		(shared-library (split-comma (attr-value "shared-library" attrs)))
		(t-prefixes (split-comma (attr-value "identifier-prefixes" attrs)))
		(f-prefixes (split-comma (attr-value "symbol-prefixes" attrs)))
		(repo-name (format nil "~a-~a" name version)))

      ;; Check if we've already loaded the repository

      (if (not (gethash repo-name *repository*))

	  ;; Make a new repository only if the required info exists
	  (let ((repo (make-instance 'repository
				     :name name
				     :version version
				     :so shared-library
				     :f-prefixes f-prefixes
				     :t-prefixes t-prefixes)))

	    ;; Add includes and packages information
	    (setf (repository-includes repo) (loop for node in includes
						for attrs = (node-attrs node)
						for name = (attr-value "name" attrs)
						for version = (attr-value "version" attrs)
						collect (list name version))

		  (repository-packages repo) (loop for node in packages
						for name = (attr-value "name" (node-attrs node))
						collect name))

	    (setf (gethash repo-name *repository*) repo)
	    )
	  (error "Repository ~a already is loaded!" repo-name))
      )

    ;; If we didn't get these necessary attributes, error and bail
    (error "Unable to load information about namespace ~a" name)
    )
  )


#|

Helpler functions

|#

(defun split-comma (string)
  (split-sequence #\, string))

;(declaim (inline attr-value))
(defun attr-value (name alist)
  (second (assoc name alist :test #'string=)))