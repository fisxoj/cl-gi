(in-package :cl-gi)

(defparameter *debug* nil)

(defun print-eval (form)
  (when *debug* (format t "Evaluating ~a~%" form))
  (eval form))
