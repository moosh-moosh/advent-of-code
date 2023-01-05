(in-package #:aoc)

(defun day10/parse ()
  (let* ((input (read-input "day10" :lines? t))
         (instructions (make-array (length input) :fill-pointer 0 :adjustable t)))
    (loop :for ln :in input :do
          (loop :for n :in (day10/parse-line ln) :do (vector-push-extend n instructions)))
    instructions))

(defun day10/parse-line (line)
  (let ((ln (cl-ppcre:regex-replace "noop|addx" line "0")))
    (cl-ppcre:register-groups-bind ((#'parse-integer a b))
        ("([0-9-]+)\\s?([0-9-]+)?" ln)
      (if b (list a b) (list a)))))

(defun cycle (instructions)
  (loop :for i :across instructions :with x := 1
        :collect (+ i x) :do (setf x (+ i x))))

(defun day10/part-one (cycles)
  (let ((interesting '(20 60 100 140 180 220)))
    (loop :for i :in interesting :sum (* i (elt cycles (1- i))))))

(defun day10/solve ()
  (let ((instructions (day10/parse)))
    (day10/part-one (cons 1 (cycle instructions)))))
