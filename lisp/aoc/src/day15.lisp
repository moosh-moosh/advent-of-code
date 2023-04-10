(defpackage #:aoc/2022/day15
  (:use #:cl #:aoc))
(in-package #:aoc/2022/day15)

(defparameter *sensor-pattern*
  "Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)")

(defun parse ()
  (let ((input (read-input "day15" :lines? t)))
    (reduce #'(lambda (res line)
                (let ((pts (parse-line line)))
                  (cons (list (first pts) (second pts) (distance (first pts) (second pts)))
                        res)))
            input
            :initial-value '())))

(defun parse-line (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer x1 y1 x2 y2))
      (*sensor-pattern* ln)
    (list (make-point :x x1 :y y1)
          (make-point :x x2 :y y2))))

(defun point->key (pt)
  (format nil "~a,~a" (pt-x pt) (pt-y pt)))

(defun key->point (key)
  (let ((xy (uiop:split-string key ",")))
    (make-point :x (first xy) :y (second xy))))

(defun distance (p1 p2)
  "Calculate the 'manhattan distance' between P1 and P2."
  (+ (abs (- (pt-x p1) (pt-x p2)))
     (abs (- (pt-y p1) (pt-y p2)))))

(defun reaches-row-p (pt-dist row)
  (let ((target (make-point :x (pt-x (first pt-dist)) :y row))
        (dist (third pt-dist)))
    (<= (distance (first pt-dist) target) dist)))

(defun reaches-point-p (pt-dist pt)
  (let ((dist (third pt-dist))
        (p (first pt-dist)))
    (<= (distance p pt) dist)))

(defun check-points-left (blocked-points pt-dist row)
  (let* ((p (first pt-dist))
         (pt-start (make-point :x (pt-x p) :y row)))
    (loop :while (reaches-point-p pt-dist pt-start)
          :do (progn
                (setf (gethash (point->key pt-start) blocked-points) 1)
                (decf (pt-x pt-start))))
    blocked-points))

(defun check-points-right (blocked-points pt-dist row)
  (let* ((p (first pt-dist))
         (pt-start (make-point :x (pt-x p) :y row)))
    (loop :while (reaches-point-p pt-dist pt-start)
          :do (progn
                (setf (gethash (point->key pt-start) blocked-points) 1)
                (incf (pt-x pt-start))))
    blocked-points))

(defun row-blocked-points (pts row)
  (let ((blocked-points (make-hash-table :test 'equal)))
    (dolist (pt-dist pts)
      (when (reaches-row-p pt-dist row)
        (check-points-left blocked-points pt-dist row)
        (check-points-right blocked-points pt-dist row))
      (when (= (pt-y (second pt-dist)) row)
        (setf (gethash (point->key (second pt-dist)) blocked-points) 2)))
    blocked-points))

(defun count-row-blocked-points (pts row)
  (let ((blocked-points (row-blocked-points pts row)))
    (loop :for key :being :the :hash-keys :of blocked-points
          :with total := 0
          :when (= (gethash key blocked-points) 1)
            :do (incf total)
          :finally (return total))))

(defun print-blocked-points (blocked-points)
  (loop :for key :being :the :hash-keys :of blocked-points
        :do (cond ((= (gethash key blocked-points) 1)
                   (format t "point ~a CANNOT HAVE A BEACON ~&" key))
                  ((= (gethash key blocked-points) 2)
                   (format t "point ~a HAS A BEACON ~&" key)))))

(defun part-one (data)
  (count-row-blocked-points data 2000000))

(defun part-two () nil)

(defun solve ()
  (let ((data (parse)))
    (values (part-one data) nil)))
