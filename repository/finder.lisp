(in-package :cl-gir)

(defparameter *repositories* '((#p"/usr/share/gir-1.0/" :xml)))

(defun find-repository (name version)
  (loop for (repo format) in *repositories*
     ;; Look for repository entries of the form GLib-2.0, where
     ;; name is "GLib" and version is "2.0"
     for test-path = (make-pathname :defaults repo
				    :name (format nil "~a-~a" name version)
				    :type "gir")
     ;; When a file named (format nil "~a-~a.gir" name version) exists
     ;; return that
     when (probe-file test-path)
     return (values test-path format)

     ;; Error if we failed to find the repository
     finally (error "Unable to find repository ~a" (format nil "~a-~a.gir" name version)))
  )