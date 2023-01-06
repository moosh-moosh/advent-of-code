(defpackage #:aoc/2022/day2
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day2)

(defparameter *shape-scores*
  '(("A X" . 4) ("A Y" . 8) ("A Z" . 3)
    ("B X" . 1) ("B Y" . 5) ("B Z" . 9)
    ("C X" . 7) ("C Y" . 2) ("C Z" . 6)))

(defparameter *outcome-scores*
  '(("A X" . 3) ("A Y" . 4) ("A Z" . 8)
    ("B X" . 1) ("B Y" . 5) ("B Z" . 9)
    ("C X" . 2) ("C Y" . 6) ("C Z" . 7)))

(defun shapes->score (shapes)
  (cdr (assoc shapes *shape-scores* :test #'string=)))

(defun outcome->score (outcome)
  (cdr (assoc outcome *outcome-scores* :test #'string=)))

(defun solve ()
  (let ((input (read-input "day2" :lines? t)))
    (values (reduce #'+ (mapcar #'shapes->score input))
            (reduce #'+ (mapcar #'outcome->score input)))))

(add-solution '202202 #'solve)
