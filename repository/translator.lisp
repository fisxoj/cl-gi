;;; translator.lisp
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



(defparameter +gtype-ctype+
  '(
    ;; Basic type aliases taken from glib/gtypes.h
    ("gchar" 	.	:char)
    ("gshort" 	.	:short)
    ("glong" 	.	:long)
    ("gint"	.	:int)
    ("gboolean" .	:int)
    ("gpointer" .	:pointer)

    ;; More types
    ("void"	.	:void)
    ("int"	.	:int)
    ("char"	.	:char)
    ("char*"	.	:string)
    ("gchar*"	.	:string)
    ;; NOTE: String won't be caught preogrammatically, but rather in (gir-to-cffi), which
    ;; has a special case for it

    ;; FIXME: This isn't right, but there doesn't seem to be a
    ;; long-double type in cffi
    ("long double".	:double)

    ("gsize"	.	:ulong)
    ("gssize"	.	:long)

    ("gboolean"	.	:boolean)

    ("gint"	.	:int)
    ("guint"	.	:uint)
    ("glong" 	.	:long)
    ("gulong"	.	:ulong)
    ("gchar"	.	:char)

    ("gint8"	.	:int8)
    ("guint8"	.	:uint8)
    ("gint16"	.	:int16)
    ("guint16"	.	:uint16)
    ("gint32"	.	:int32)
    ("guint32"	.	:uint32)
    ("gint64"	.	:int64)
    ("guint64"	.	:uint64)

    ("gfloat"	.	:float)
    ("gdouble"	.	:double)
    ))

(defun gir-to-cffi (typestring)
  (let ((length (length typestring)))
    (cond
      ((string= typestring "gchar*") :string)
      ((string= typestring "char*") :string)
      ((string= (subseq typestring (1- length)) "*")
       (if (eq (gir-to-cffi (subseq typestring 0 (1- length))) :pointer)
	   `(:pointer ,(gir-to-cffi (subseq typestring 0 (1- length))))
	   :pointer))  ;; If it's a pointer to something we don't understand,
      ;; it's probably a pointer to a GObject of some kind
      ((assoc typestring +gtype-ctype+ :test #'string=)
       (cdr (assoc typestring +gtype-ctype+ :test #'string=)))
      (t :pointer) ;; If all else fails, it's probably a pointer
      )))

(defun lispify-gir-constant (name)
  (format nil "+~a+" (coerce (loop for c across name
				for i upfrom 0
				if (eql c #\_)
				collect #\-
				else
				collect c) 'string)))


(defun gobject->lisp (name prefixes)
    (loop for prefix in prefixes
       for l = (length prefix)
       when (string= (subseq name 0 l) prefix)
       return (subseq name l)))

(defun gsymbol->lisp (name repo)
  (c-name-to-lis-name (gobject->lisp name (repository-symbol-prefixes repo))))

(defun gfunction->lisp (name repo)
  (coerce
   (let* ((name (gobject->lisp name (repository-function-prefixes repo)))
	  (fixed-name (if (eql #\_ (elt name 0))
			  (subseq name 1)
			  name)))
     (c-name-to-lisp-name fixed-name))
   'string))

(defun c-name-to-lisp-name (name)
  (coerce
   (loop for c across name
      if (eql c #\_)
      collect #\-
      else collect c)
   'string))
