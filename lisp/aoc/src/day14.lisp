(defpackage #:aoc/2022/day14
  (:use #:cl #:aoc))
(in-package #:aoc/2022/day14)

(defun parse-point (str)
  (cl-ppcre:register-groups-bind ((#'parse-integer x y))
      ("(\\d+),(\\d+).*" str)
    (make-point :x x :y y)))

(defun split-points (ln)
  (mapcar #'parse-point (cl-ppcre:split "\\s+->\\s+" ln)))

(defun place-rocks (points cave)
  ;; this doesn't feel too good! maybe think about this again afterwards
  (let ((pair (first points)))
    (cond ((null points) cave)
          ((and (= (pt-x (first pair))
                   (pt-x (second pair)))
                (< (pt-y (first pair))
                   (pt-y (second pair))))
           (loop :for i :from (pt-y (first pair))
                 :upto (pt-y (second pair))
                 :do (setf (gethash (format nil "~a,~a" (pt-x (first pair)) i) cave)
                           (make-point :x (pt-x (first pair)) :y i)))
           (place-rocks (rest points) cave))
          ((and (= (pt-x (first pair))
                   (pt-x (second pair)))
                (> (pt-y (first pair))
                   (pt-y (second pair))))
           (loop :for i :from (pt-y (first pair))
                 :downto (pt-y (second pair))
                 :do (setf (gethash (format nil "~a,~a" (pt-x (first pair)) i) cave)
                           (make-point :x (pt-x (first pair)) :y i)))
           (place-rocks (rest points) cave))
          ((and (= (pt-y (first pair))
                   (pt-y (second pair)))
                (< (pt-x (first pair))
                   (pt-x (second pair))))
           (loop :for i :from (pt-x (first pair))
                 :upto (pt-x (second pair))
                 :do (setf (gethash (format nil "~a,~a" i (pt-y (first pair))) cave)
                           (make-point :x i :y (pt-y (first pair)))))
           (place-rocks (rest points) cave))
          ((and (= (pt-y (first pair))
                   (pt-y (second pair)))
                (> (pt-x (first pair))
                   (pt-x (second pair))))
           (loop :for i :from (pt-x (first pair))
                 :downto (pt-x (second pair))
                 :do (setf (gethash (format nil "~a,~a" i (pt-y (first pair))) cave)
                           (make-point :x i :y (pt-y (first pair)))))
           (place-rocks (rest points) cave))
          (t cave))))

(defun sand-key (sand)
  (format nil "~a,~a" (pt-x sand) (pt-y sand)))

(defun fall (sand cave bottom &key (part-two nil))
  (cond ((> (pt-y sand) bottom) (values nil sand))
        ((rest-p sand cave (and part-two bottom))
         (setf (gethash (sand-key sand) cave) sand)
         (values cave sand))
        ((down-p sand cave (and part-two bottom))
         (fall (down sand) cave bottom :part-two part-two))
        ((down-left-p sand cave (and part-two bottom))
         (fall (down-left sand) cave bottom :part-two part-two))
        ((down-right-p sand cave (and part-two bottom))
         (fall (down-right sand) cave bottom :part-two part-two))))

(defun rest-p (sand cave &optional bottom)
  (and (not (down-p sand cave bottom))
       (not (down-left-p sand cave bottom))
       (not (down-right-p sand cave bottom))))

(defun floor-p (sand bottom)
  (>= (pt-y sand) bottom))

(defun down-p (sand cave &optional bottom)
  (and (not (gethash (sand-key (down sand)) cave))
       (not (and bottom (floor-p (down sand) bottom)))))

(defun down-left-p (sand cave &optional bottom)
  (and (not (gethash (sand-key (down-left sand)) cave))
       (not (and bottom (floor-p (down-left sand) bottom)))))

(defun down-right-p (sand cave &optional bottom)
  (and (not (gethash (sand-key (down-right sand)) cave))
       (not (and bottom (floor-p (down-right sand) bottom)))))

(defun down (sand)
  (make-point :x (pt-x sand) :y (1+ (pt-y sand))))

(defun down-left (sand)
  (make-point :x (1- (pt-x sand)) :y (1+ (pt-y sand))))

(defun down-right (sand)
  (make-point :x (1+ (pt-x sand)) :y (1+ (pt-y sand))))

(defun find-bottom (cave)
  (loop :for v :being :the :hash-values :of cave
        :with bottom := 0
        :when (> (pt-y v) bottom)
          :do (setf bottom (pt-y v))
        :finally (return bottom)))

(defun parse ()
  (let ((input (read-input "day14" :lines? t)))
    (reduce #'(lambda (acc val)
                (place-rocks val acc))
    (mapcar #'(lambda (pts) (partition pts 2 :step 1))
            (mapcar #'split-points input))
    :initial-value (make-hash-table :test 'equal))))

(defun part-one (cave bottom n)
  (let ((c (fall (make-point :x 500 :y 0) cave bottom)))
    (if c (part-one c bottom (1+ n)) n)))

(defun part-two (cave bottom n)
  (multiple-value-bind (c s) (fall (make-point :x 500 :y 0) cave bottom :part-two t)
    (cond ((null c) n)
          ((and (= (pt-y s) 0)
                (= (pt-x s) 500))
           n)
          (t (part-two c bottom (1+ n))))))


(defun solve ()
  ;; TODO: make this better
  (let ((cave (parse))
        (cave2 (parse)))
    (values
     (part-one cave (find-bottom cave) 0)
     (part-two cave2 (+ 2 (find-bottom cave2)) 1))))
