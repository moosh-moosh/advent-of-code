;;;; aoc.lisp

(in-package #:aoc)

(defun read-input (day &key (lines? nil))
  (let ((path (format nil "../inputs/~a.txt" day)))
    (if lines?
        (uiop:read-file-lines path)
        (uiop:read-file-string path))))
