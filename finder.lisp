;;; finder.lisp
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