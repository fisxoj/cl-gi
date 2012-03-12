;;; repository.lisp
;;
;; Copyright 2012 Matt Novenstern <fisxoj@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(in-package :cl-gir)

(defparameter *repository* (make-hash-table :test 'equal))

(defclass repository ()
  ((name :accessor repository-name :initarg :name)
   (version :accessor repository-version :initarg :version)
   (function-prefixes :accessor repository-function-prefixes :initarg :f-prefixes)
   (type-prefixes :accessor repository-type-prefixes :initarg :t-prefixes)
   (shared-library :accessor repository-shared-library :initarg :so)
   (includes :accessor repository-includes :initarg :includes)
   (packages :accessor repository-packages :initarg :packages)
   (package :accessor repository-package :initarg :package))
  (:documentation "Class for storing information about a repository, including what other repositories it depends on.
")
  )

(defun load-repository (name version)
  (multiple-value-bind (path format) (find-repository name version)
    (read-repository path format)))

(defgeneric repository-load-libraries (repo)
  (:documentation "Loads the library specified in the gir file into the ffi"))

(defmethod repository-load-libraries ((repo repository))
  (loop for library in (repository-shared-library repo)
     do (handler-case (load-foreign-library library)
	    (load-foreign-library-error ()
	      (warn "Unable to load library ~a" library)))))

(defmacro gir-foreign-library (name so-list)
  `(define-foreign-library ,name
     (:unix (:or ,@so-list))
     (t (:default ,(first (split-sequence "." (first so-list)))))))