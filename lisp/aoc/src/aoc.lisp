;;;; aoc.lisp

(in-package #:aoc)

(defun read-input (day)
  (let ((path (format nil "../inputs/~a.txt" day)))
    (uiop:read-file-string path)))
