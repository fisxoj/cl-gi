;;; reader.lisp
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

#|
(defun parse-repository (root)
  (loop for node in (node-children root)
       for node-name = (xmlrep-tag node)
     ;; FIXME: This looks wrong
     for namespace = nil

     ;; Find namespace node and save it
     when (string= node-name "namespace")
     do (setf namespace  node)

     ;; Collect the includes and packages
     when (let ((name (xmlrep-attrib-value "name" node)))
	    (and (string= node-name "include")
		 ;; Ignore header file includes, e.g. <c:include name="gee.h" />
		 (not (string= (subseq name (- (length name) 2) (length name)) ".h"))))
     collect node into includes
     when (string= (node-name node) "package")
     collect node into packages

     ;; Feed all of our new information into the repository maker
     finally (make-repository namespace includes packages)))
|#

(defun parse-repository (root)
  "Assemble packages and includes for the repository and find the namespace node for
further processing"

  (let ((namespace (xmlrep-find-child-tag "namespace" root))
	(includes (remove-if #'header-file-p (xmlrep-find-child-tags "include" root)))
	(packages (xmlrep-find-child-tags "package" root)))
    (make-repository namespace includes packages)))


(defun make-repository (namespace includes packages)
  (with-node-attributes namespace (name version identifier-prefixes symbol-prefixes shared-library)
			(let ((repo-name (name-repository name version))
			      (so-files (split-comma shared-library))
			      (t-prefixes (split-comma identifier-prefixes))
			      (f-prefixes (split-comma symbol-prefixes))
			      (include-list (loop for node in includes
					       for name = (xmlrep-attrib-value "name" node)
					       for version = (xmlrep-attrib-value "version" node)
					       collect (list name version)))
			      (package-list (loop for node in packages
					       for name = (xmlrep-attrib-value "name" node)
					       collect name))
			      )
			  (if (not (gethash repo-name *repository*))
			      (setf (gethash repo-name *repository*)
				    (make-instance
				     'repository
				     :name name
				     :version version
				     :so so-files
				     :t-prefixes t-prefixes
				     :f-prefixes f-prefixes
				     :packages package-list
				     :includes include-list))
			      (error "Repository ~a already is loaded!" repo-name))
			  )))

#|


(defun make-repository (namespace includes packages)
  "Generate the repository object that will store information about it and
what it requires to operate."

  (if-let ((name (xmlrep-attrib-value "name" namespace))
	   (version (xmlrep-attrib-value "version" namespace))
	   (repo-name (format nil "~a-~a" (xmlrep-attrib-value "name" namespace)
			      (xmlrep-attrib-value "version" namespace)))
	   (shared-library (split-comma (xmlrep-attrib-value "shared-library" namespace)))
	   ;; Prefixes are stored longest first in their lists, so that when
	   ;; we try to match prefixes later, we try the longer ones first,
	   ;; to avoid a partial match when we have "g" and "glib"
	   ;; where we could end up creating (glib:lib-init) instead of
	   ;; (glib:init)
	   (t-prefixes (attrib-value-split-sort "identifier-prefixes" namespace))
	   (f-prefixes (attrib-value-split-sort "symbol-prefixes" namespace)))

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
					      for name = (xmlrep-attrib-value "name" node)
					      for version = (xmlrep-attrib-value "version" node)
					      collect (list name version))

		(repository-packages repo) (loop for node in packages
					      for name = (xmlrep-attrib-value "name" node)
					      collect name)
		(repository-package repo) (make-package name))

	  (setf (gethash repo-name *repository*) repo)

	  ;; Set up some cffi things
	  ;; notably, there is only a :unix clause to this library, since
	  ;; GIR only exists on linux anyway.
	  (gir-foreign-library name shared-library)
	  )
	(error "Repository ~a already is loaded!" repo-name))

    ;; If we didn't get these necessary attributes, error and bail
    (error "Unable to load information about namespace ~a" name)))

|#

#|

Functions for parsing things inside the namespace

|#

(defun parse-namespace (node repo)
  (loop for child in (xmlrep-children node)
     do (switch ((xmlrep-tag child) :test 'string=)
	  ("alias" (parse-alias child repo))
	  ("constant" (parse-constant child repo))
	  ("function" (parse-function child repo))
	  (t (error "No parser for node type ~a" (xmlrep-tag child))))))


(defun parse-alias (node repo)
  (if-let ((name (xmlrep-attrib-value "name" node))
	   (type (get-type node)))
    `(defctype ,name ,type)
    (warn "Unable to parse alias ~a~%" name)))

(defun parse-constant (node repo)
  (let* ((name (xmlrep-attrib-value "name" node))
	 (type (gir-to-cffi (get-type node)))
	 (value (xmlrep-attrib-value "value"))
	 (*package* (repository-package repo)))
    (setf (export (intern (lispify-gir-constant name))) value)))

(defun parse-function (node repo)
  (let* ((name (xmlrep-attrib-value "name" node))
	 (type)
	 ))
  )
#|

Helpler functions

|#

(defun get-type (node)
  (xmlrep-attrib-value "type" (xmlrep-find-child-tag "type" node)))

(defun attrib-value-split-sort (name alist)
  (let ((vals (split-comma (xmlrep-attrib-value name alist))))
    (sort vals (lambda (a b) (> (length a) (length b))))))

(defun split-comma (string)
  (split-sequence #\, string))

(defun gobject->lisp (name prefixes)
    (loop for prefix in prefixes
       for l = (length prefix)
       when (string= (subseq name 0 l) prefix)
       return (subseq name l)))

(defun gsymbol->lisp (name repo)
  (coerce  (loop for c across (gobject->lisp name (repository-symbol-prefixes repo))
	      collect (if (upper-case-p c)
			  (progn (princ (char-downcase c))
				 (princ #\-))
			  (princ c)))
	   'string))

(defun gfunction->lisp (name repo)
  (coerce
   (let* ((name (gobject->lisp name (repository-function-prefixes repo)))
	  (fixed-name (if (eql #\_ (elt name 0))
			  (subseq name 1)
			  name)))
     (loop for c across fixed-name
	if (eql c #\_)
	collect #\-
	else collect c))
   'string))

(defun name-repository (name version)
  (format nil "~a-~a" name version))

#|
(defmacro with-node-attributes (node attributes &body body)
  "Fetches node attributes based on a short notation for getting to them from the current node
<tag1 name=\"hi\">
  <tag2 pizza=\"tasty\"/>
</tag1>

We could get at the 'pizza' attribute of tag2 and 'name' from tag1 by passing
\(with-node-attributes node \(name \(tag pizza\)\)
		      \(print pizza\)
		      \(print name\)\)\)
=> \"tasty\"
\"hi\"
"
  `(labels (
	    ;; For converting attribute names to strings
	    (name-from-symbol (sym) (string-downcase (symbol-name sym)))
	    ;; Traverses path lists
	    (node-down (node path)
	      (if (cdr path)
		  (node-down (xmlrep-find-child-tag (car path) node) (cdr path))
		  (xmlrep-attrib-value (car path) node))))
     (let ,(loop for attribute in attributes
	      collect (list (last attribute) `(node-down ,node ,attribute)))
       ,@body)))

|#

(defmacro with-node-attributes (node attributes &body body)
  `(let ,(loop for attribute in attributes
	      collect (list attribute
			    `(xmlrep-attrib-value ,(string-downcase (symbol-name attribute)) ,node)))
     ,@body))

(defun header-file-p (node)
  (with-node-attributes node (name)
		       (string= (subseq name (- (length name) 2) (length name)) ".h")))