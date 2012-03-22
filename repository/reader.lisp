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

  (let* ((namespace (xmlrep-find-child-tag "namespace" root))
	(includes (remove-if #'header-file-p (xmlrep-find-child-tags "include" root)))
	(packages (xmlrep-find-child-tags "package" root))
	(repo (make-repository namespace includes packages))
	)
    (repository-load-libraries repo)
    (parse-namespace namespace repo)))


(defun make-repository (namespace includes packages)
  (with-node-attributes namespace (name version identifier-prefixes symbol-prefixes shared-library)
    (let ((repo-name (name-repository name version))
	  (so-files (split-comma shared-library))
	  (t-prefixes (split-comma identifier-prefixes))
	  (f-prefixes (split-comma symbol-prefixes))
	  (include-list (when includes
			  (loop for node in includes
			     for name = (xmlrep-attrib-value "name" node)
			     for i-version = (xmlrep-attrib-value "version" node nil)
			     collect (list name i-version))))
	  (package-list (when packages
			  (loop for node in packages
			     for name = (xmlrep-attrib-value "name" node)
			     collect name)))
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
		 :package (make-package (read-from-string repo-name)
					:nicknames (list (read-from-string name)))
		 ))
	  (error "Repository ~a already is loaded!" repo-name))
      )))

#|

Functions for parsing things inside the namespace

|#

(defun parse-namespace (node repo)
  (loop for child in (xmlrep-children node)
     do (funcall (switch ((xmlrep-tag child) :test 'string=)
		   ("alias" #'parse-alias)
		   ("constant" #'parse-constant)
		   ("function" #'parse-function)
;		   ("record" #'parse-record)
		   ("class" #'parse-class)
		   (t (lambda (n r) (declare (ignore n r))
			      (warn "No parser for node type ~a" (xmlrep-tag child)))))
		 child repo)))


(defun parse-alias (node repo)
  (declare (ignore repo))
  (if-let ((new-type (xmlrep-attrib-value "type" node))
	   (type (gir-to-cffi (get-type node))))
    (print-eval `(defctype ,(read-from-string new-type) ,type))
    (warn "Unable to parse alias ~a~%" new-type)))

(defun parse-constant (node repo)
  (let* ((name (lispify-gir-constant (xmlrep-attrib-value "name" node)))
	 (type (gir-to-cffi (get-type node)))
	 (value (xmlrep-attrib-value "value" node))
	 (*package* (repository-package repo)))
    (export (intern name))
    (print-eval `(defconstant ,(read-from-string name)
		   ,(if (eq type :string) value (read-from-string value))))))

(defun parse-function (node repo)
  (let ((*package* (repository-package repo)))
    (when (xmlrep-attrib-value "moved-to" node nil)
      (warn "Function ~a has moved, skipping this entry" (xmlrep-attrib-value "identifier" node))
      (return-from parse-function))
    (with-node-attributes node (name identifier)
      (let* ((lisp-name (c-name-to-lisp-symbol name))
	     (return-type (gir-to-cffi (get-type (xmlrep-find-child-tag "return-value" node))))
	     (parameters (collect-parameters node))
	     (*package* (repository-package repo)))
	(print-eval `(defcfun (,(coerce identifier 'simple-string) ,lisp-name) ,return-type
		       ,@(when parameters parameters)))
	(export lisp-name)
	))))


(defun parse-record (node repo)
  ;; FIXME: Stub.
  )

(defun make-constructor (node repo g-type)
  (with-node-attributes node (identifier)
    (let* ((lisp-name (c-name-to-lisp-name identifier))
	   (return-type (gir-to-cffi (get-type (xmlrep-find-child-tag "return-value" node))))
	   (parameters (collect-parameters node))
	   (parameter-names (loop for p in parameters
			       if (consp p)
			       collect (car p)
			       else
			       collect 'rest
				 ))
	   (*package* (repository-package repo))
	   (internal-name (read-from-string (format nil "%~a" lisp-name)))
	   )

      (print-eval `(defcfun (,(coerce identifier 'simple-string) ,internal-name)
		       ,return-type
		     ,@(when parameters parameters)))

      (print-eval `(defmethod initialize-instance ((o ,g-type)
						   &key ,@parameter-names)
		     (warn "Calling initializer for ~a" ,(symbol-name g-type))
		     (setf (slot-value o 'pointer)
			   (funcall (function ,internal-name) ,@(remove-if
								 (lambda (a) (eq a '&rest))
								 parameter-names)))))

;      (export (read-from-string lisp-name))
      )))


(defun parse-class (node repo)
  (with-node-attributes node (name parent get-type symbol-prefix)
    (let* ((class-type (c-name-to-lisp-symbol name))
	   (constructor (xmlrep-find-child-tags "constructor" node))
	   (*package* (repository-package repo))
	   (type-fn-name (read-from-string
			  (format nil "~a-get-type" (c-name-to-lisp-name symbol-prefix))))
;	   (class-name (read-from-string
;			(format nil "~a:~a" (package-name (repository-package repo)) class-type)))
	   )

      (export (intern (symbol-name class-type)))

      (print-eval `(defclass ,class-type
		       (cl-gir::gir-class) ; ,(list (resolve-c-object parent))
		     nil))

      (export (intern (symbol-name type-fn-name)))

      (print-eval `(defcfun (,(coerce get-type 'simple-string)
			      ,type-fn-name)
		       :int))

      (when constructor
	(make-constructor (first constructor) repo class-type)))))

(defun parse-method (node repo g-type)
  (with-node-attributes node (name identifier)
    (let* ((return-type (gir-to-cffi (get-type (xmlrep-find-child-tag "return-value" node))))
	   (lisp-name (c-name-to-lisp-name name))
	   (parameters (collect-parameters node))
	   (parameter-names (loop for p in parameters
			       collect (car p)))
	   (*package* (repository-package repo))
	   (internal-name (read-from-string (format nil "%~a" lisp-name)))
	   )

      `(defcfun (,(coerce identifier 'simple-string) ,internal-name)
	   ,return-type
	 ,@(when parameters parameters))

      `(defmethod ,(read-from-string lisp-name) ((o ,g-type)
					       &key ,@parameter-names)
	 (funcall (function ,internal-name) (object o) ,@parameter-names))

      (export (read-from-string lisp-name))
      )))


#|

Helpler functions for reading from xmls nodes

|#

(defun get-type (node)
  ;; Check for a subtag named type and get information from it,
  ;; in the event it doesn't exist, check to see if there is and <array>
  ;; tag before giving up and erroring.
  (let ((varargs (xmlrep-find-child-tag "varargs" node nil))
	(array (xmlrep-find-child-tag "array" node nil))
	(node (xmlrep-find-child-tag "type" node nil)))

    (if varargs
	:varargs
	(xmlrep-attrib-value "type" (or array node)))))

(defun collect-parameters (node)
  (when (xmlrep-find-child-tag "parameters" node nil)
    (loop for parameter in
	 (xmlrep-find-child-tags "parameter"
				 (xmlrep-find-child-tag "parameters" node nil))
       for type = (get-type parameter)

       if (eq type :varargs)
       collect '&rest
       else
       collect (list
		;; Name
		(read-from-string (c-name-to-lisp-name (xmlrep-attrib-value "name" parameter)))
		;; Cffi Type
		(gir-to-cffi type)))))

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
			    `(xmlrep-attrib-value ,(string-downcase (symbol-name attribute))
						  ,node
						  nil)))
     ,@body))

(defun header-file-p (node)
  "For filtering out <c:include>'s from the top level of repository files, which
xmls doesn't parse differently than <include>'s"

  (with-node-attributes node (name)
		       (string= (subseq name (- (length name) 2) (length name)) ".h")))

(defun print-eval (form)
  (format t "Evaluating ~a~%" form)
  (eval form))