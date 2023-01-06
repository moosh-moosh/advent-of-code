(defpackage #:aoc/2022/day9
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day9)

(defparameter *tail-locs* '("(0,0)"))

(defun ln->moves (ln)
  (cl-ppcre:register-groups-bind (dir (#'parse-integer n))
      ("^(\\w)\\s(\\d+)" ln)
    (get-moves dir n)))

(defun get-moves (dir count)
  (cond
    ((string= dir "U")
     (loop :for i :from 0 :below count :collecting #'move-up))
    ((string= dir "R")
     (loop :for i :from 0 :below count :collecting #'move-right))
    ((string= dir "D")
     (loop :for i :from 0 :below count :collecting #'move-down))
    ((string= dir "L")
     (loop :for i :from 0 :below count :collecting #'move-left))))

(defun parse ()
  (let ((input (read-input "day9" :lines? t)))
    (mapcar #'ln->moves input)))

(defun make-rope (len &optional (start-x 0) (start-y 0))
  "A rope is a list of points (x,y). Create a list of length LEN of points."
  (loop :for i :from 1 :to len
        :collecting (make-point :x start-x :y start-y)))

(defun distance (p1 p2)
  "Return the distance between points P1 and P2."
  (floor (sqrt (+ (expt (- (pt-x p1) (pt-x p2)) 2)
                  (expt (- (pt-y p1) (pt-y p2)) 2)))))

(defun move-up (pt)
  (incf (pt-y pt))
  pt)

(defun move-down (pt)
  (decf (pt-y pt))
  pt)

(defun move-left (pt)
  (decf (pt-x pt))
  pt)

(defun move-right (pt)
  (incf (pt-x pt))
  pt)

(defun move-up-left (pt)
  (move-left
   (move-up pt))
  pt)

(defun move-up-right (pt)
  (move-right
   (move-up pt))
  pt)

(defun move-down-left (pt)
  (move-left
   (move-down pt))
  pt)

(defun move-down-right (pt)
  (move-right
   (move-down pt))
  pt)

(defun needs-to-move-p (p1 p2)
  (> (distance p1 p2) 1))

(defun get-move (p1 p2)
  (when (and (not (null p1))
             (not (null p2))
             (> (distance p1 p2) 1))
    (cond
      ((= (pt-x p1) (pt-x p2))
       (if (> (pt-y p1) (pt-y p2))
           #'move-up
           #'move-down))
      ((= (pt-y p1) (pt-y p2))
       (if (> (pt-x p1) (pt-x p2))
           #'move-right
           #'move-left))
      ((and (> (pt-y p1) (pt-y p2))
            (> (pt-x p1) (pt-x p2)))
       #'move-up-right)
      ((and (> (pt-y p1) (pt-y p2))
            (< (pt-x p1) (pt-x p2)))
       #'move-up-left)
      ((and (< (pt-y p1) (pt-y p2))
            (< (pt-x p1) (pt-x p2)))
       #'move-down-left)
      ((and (< (pt-y p1) (pt-y p2))
            (> (pt-x p1) (pt-x p2)))
       #'move-down-right))))

(defun move-rope (rope mv)
  (let ((rope-head (funcall mv (first rope)))
        (rope-tail (rest rope)))
    (unless (null rope-tail)
      (move-tail rope-head rope-tail))))

(defun move-tail (rhead rtail)
  (let ((mv (get-move rhead (first rtail))))
    (cond
      ((null mv) nil)
      (t
       (let ((new-pt (funcall mv (first rtail))))
         (when (= (length rtail) 1)
           (setf *tail-locs* (cons (format nil "(~A,~A)" (pt-x new-pt) (pt-y new-pt)) *tail-locs*)))
         (move-tail new-pt (rest rtail)))))))

(defun handle-moves (mv count rope)
  (loop :for i :from 0 :below count :do (move-rope rope mv)))

(defun count-tail-locs (locations)
  (length (remove-duplicates locations :test #'string=)))

(defun part-one (moves)
  (setf *tail-locs* '("(0,0)"))
  (let ((rope (make-rope 2)))
    (loop :for m :in moves :do
          (loop :for mv :in m :do (move-rope rope mv)))
    (count-tail-locs *tail-locs*)))

(defun part-two (moves)
  (setf *tail-locs* '("(0,0)"))
  (let ((rope (make-rope 10)))
    (loop :for m :in moves :do
      (loop :for mv :in m :do (move-rope rope mv)))
    (count-tail-locs *tail-locs*)))

(defun solve ()
  (let ((moves (parse)))
    (values
     (part-one moves)
     (part-two moves))))

(add-solution '202209 #'solve)
