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
   (includes :accessor repository-includes :initform '())
   (packages :accessor repository-packages :initform '())))

(defun load-repository (name version)
  (multiple-value-bind (path format) (find-repository name version)
    (read-repository path format)))