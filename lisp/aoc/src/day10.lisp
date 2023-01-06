(defpackage #:aoc/2022/day10
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day10)

(defun parse ()
  (let* ((input (read-input "day10" :lines? t))
         (instructions (make-array (length input) :fill-pointer 0 :adjustable t)))
    (loop :for ln :in input :do
          (loop :for n :in (parse-line ln) :do (vector-push-extend n instructions)))
    instructions))

(defun parse-line (line)
  (let ((ln (cl-ppcre:regex-replace "noop|addx" line "0")))
    (cl-ppcre:register-groups-bind ((#'parse-integer a b))
        ("([0-9-]+)\\s?([0-9-]+)?" ln)
      (if b (list a b) (list a)))))

(defun cycle (instructions)
  (loop :for i :across instructions :with x := 1
        :collect (+ i x) :do (setf x (+ i x))))

(defun part-one (cycles)
  (let ((interesting '(20 60 100 140 180 220)))
    (loop :for i :in interesting :sum (* i (elt cycles (1- i))))))

(defun part-two (cycles)
  (loop :for ln :in (chunks-of cycles 40)
        :do (print-line ln)))

(defun draw-pixel-p (pos x)
  (some #'(lambda (v) (= pos v))
        (list (1- x) x (1+ x))))

(defun print-line (ln)
  (loop :for p :from 0
        :and x :in ln
        :do (format t "~a" (if (draw-pixel-p p x) "#" ".")))
  (format t "~%"))

(defun solve ()
  (let* ((instructions (parse))
         (cycles (cons 1 (cycle instructions))))
    (values
     (part-one cycles)
     (part-two cycles))))

(add-solution '202210 #'solve)
