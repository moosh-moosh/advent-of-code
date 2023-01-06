(defpackage #:aoc/2022/day4
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day4)

(defun line-to-pair (line)
  (let ((pair (uiop:split-string line :separator '(#\,))))
    (mapcar #'(lambda (p)
                (let ((r (uiop:split-string p :separator '(#\-))))
                  (range (parse-integer (car r))
                         (1+ (parse-integer (cadr r))))))
            pair)))

(defun part-one (pairs)
  (reduce #'(lambda (val pair)
              (if (or (subsetp (car pair) (cadr pair))
                      (subsetp (cadr pair) (car pair)))
                  (1+ val)
                  val))
          pairs
          :initial-value 0))

(defun part-two (pairs)
  (reduce #'(lambda (val pair)
              (if (intersection (car pair) (cadr pair))
                  (1+ val)
                  val))
          pairs
          :initial-value 0))

(defun solve ()
  (let* ((input (read-input "day4" :lines? t))
         (pairs (mapcar #'line-to-pair input)))
    (values (part-one pairs) (part-two pairs))))

(add-solution '202204 #'solve)
