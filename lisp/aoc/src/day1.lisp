(in-package #:aoc)

(defun day1-solve ()
  (let* ((input (read-input "day1" :lines? t))
         (number-bags (mapcar #'(lambda (b) (if (> (length b) 0) (parse-integer b) b)) input))
         (bags (split-sequence number-bags "" :test #'equal))
         (sorted-bags (sort (mapcar #'(lambda (b) (reduce #'+ b)) bags) #'>)))
    (values (car sorted-bags)
            (reduce #'+ sorted-bags :end 3))))
