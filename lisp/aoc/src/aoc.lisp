;;;; aoc.lisp

(in-package #:aoc)

(defparameter *solutions* (make-hash-table))

(defun add-solution (name fn)
  (setf (gethash name *solutions*) fn))

(defun solve (name)
  (if (gethash name *solutions*)
      (funcall (gethash name *solutions*))
      (format t "~&No solution found for ~a" name)))
