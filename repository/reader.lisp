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
(defun parse-repository (root)
  "Assemble packages and includes for the repository and find the namespace node for
further processing"

  (let ((namespace (xmlrep-find-child-tag "namespace" root))
	(includes (remove-if #'header-file-p (xmlrep-find-child-tags "include" root)))
	(packages (xmlrep-find-child-tags "package" root)))
    (parse-namespace namespace (make-repository namespace includes packages))))


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
				     :includes include-list
				     :package (make-package repo-name
							    :nicknames (list name))
				     ))
			      (error "Repository ~a already is loaded!" repo-name))
			  )))

#|

Functions for parsing things inside the namespace

|#

(defun parse-namespace (node repo)
  (loop for child in (xmlrep-children node)
     do (switch ((xmlrep-tag child) :test 'string=)
	  ("alias" (parse-alias child repo))
	  ("constant" (parse-constant child repo))
	  ("function" (parse-function child repo))
	  (t (warn "No parser for node type ~a" (xmlrep-tag child))))))


(defun parse-alias (node repo)
  (declare (ignore repo))
  (if-let ((new-type (xmlrep-attrib-value "type" node))
	   (type (gir-to-cffi (get-type node))))
    (print-eval `(defctype ,(read-from-string new-type) ,type))
    (warn "Unable to parse alias ~a~%" new-type)))

(defun parse-constant (node repo)
  (let* ((name (lispify-gir-const (xmlrep-attrib-value "name" node)))
	 (type (gir-to-cffi (get-type node)))
	 (value (xmlrep-attrib-value "value" node))
	 (*package* (repository-package repo)))
    (export (intern name))
    (print-eval `(defconstant ,(read-from-string name) ,value))))

(defun parse-function (node repo)
  (when (xmlrep-attrib-value "moved-to" node nil)
    (warn "Function ~a has moved, skipping this entry" (xmlrep-attrib-value "identifier" node))
    (return-from parse-function))
  (let* ((name (xmlrep-attrib-value "identifier" node))
	 (lisp-name (gfunction->lisp name repo))
	 (return-type (gir-to-cffi (get-type (xmlrep-find-child-tag "return-value" node))))
	 (parameters (loop for parameter in
			  (xmlrep-find-child-tags "parameter"
						  (xmlrep-find-child-tag "parameters" node))
			for name = (read-from-string (c-name-to-lisp-name (xmlrep-attrib-value "name" parameter)))
			for type = (gir-to-cffi (get-type parameter))
			collect (list name type)))
	 (*package* (repository-package repo)))
    (export (intern lisp-name))
    (print-eval `(defcfun (,name ,lisp-name) ,return-type
		     ,@parameters))
    ))
#|

Helpler functions for reading from xmls nodes

|#

(defun get-type (node)
  (xmlrep-attrib-value "type" (xmlrep-find-child-tag "type" node)))

(defun attrib-value-split-sort (name alist)
  (let ((vals (split-comma (xmlrep-attrib-value name alist))))
    (sort vals (lambda (a b) (> (length a) (length b))))))

(defun split-comma (string)
  (split-sequence #\, string))

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
  "For filtering out <c:include>'s from the top level of repository files, which
xmls doesn't parse differently than <include>'s"

  (with-node-attributes node (name)
		       (string= (subseq name (- (length name) 2) (length name)) ".h")))