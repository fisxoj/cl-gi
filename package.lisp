(in-package :cl-user)

(defpackage :cl-gir
  (:use :cffi :cl :alexandria :xmls :split-sequence)

  ;; Repository functions
  (:export
   #:find-repository
   #:load-repository
   )
  )