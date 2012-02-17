(in-package :cl-gir)

(defparameter *repository* (make-hash-table :test 'equal))

(defclass repository ()
  ((name :accessor repository-name :initarg :name)
   (version :accessor repository-version :initarg :version)
   (function-prefixes :accessor repository-function-prefixes :initarg :f-prefixes)
   (type-prefixes :accessor repository-type-prefixes :initarg :t-prefixes)
   (shared-library :accessor repository-shared-library :initarg :so)
   (includes :accessor repository-includes :initform '())
   (packages :accessor repository-packages :initform '())))

(defun load-repository (name version)
  (multiple-value-bind (path format) (find-repository name version)
    (read-repository path format)))