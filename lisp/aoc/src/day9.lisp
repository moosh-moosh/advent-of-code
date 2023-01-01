(in-package #:aoc)

(defparameter *tail-locs* '("(START)"))

(defun ln->moves (ln)
  (cl-ppcre:register-groups-bind (dir (#'parse-integer n))
      ("^(\\w)\\s(\\d+)" ln)
    (get-moves dir n)))

(defun get-moves (dir count)
  (cond
    ((string= dir "U")
     (loop :for i :from 0 :below count :collecting #'move-pt-up))
    ((string= dir "R")
     (loop :for i :from 0 :below count :collecting #'move-pt-right))
    ((string= dir "D")
     (loop :for i :from 0 :below count :collecting #'move-pt-down))
    ((string= dir "L")
     (loop :for i :from 0 :below count :collecting #'move-pt-left))))

(defun d9-parse ()
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

(defun move-pt-up (pt)
  (incf (pt-y pt))
  pt)

(defun move-pt-down (pt)
  (decf (pt-y pt))
  pt)

(defun move-pt-left (pt)
  (decf (pt-x pt))
  pt)

(defun move-pt-right (pt)
  (incf (pt-x pt))
  pt)

(defun move-pt-up-left (pt)
  (move-pt-left
   (move-pt-up pt))
  pt)

(defun move-pt-up-right (pt)
  (move-pt-right
   (move-pt-up pt))
  pt)

(defun move-pt-down-left (pt)
  (move-pt-left
   (move-pt-down pt))
  pt)

(defun move-pt-down-right (pt)
  (move-pt-right
   (move-pt-down pt))
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
           #'move-pt-up
           #'move-pt-down))
      ((= (pt-y p1) (pt-y p2))
       (if (> (pt-x p1) (pt-x p2))
           #'move-pt-right
           #'move-pt-left))
      ((and (> (pt-y p1) (pt-y p2))
            (> (pt-x p1) (pt-x p2)))
       #'move-pt-up-right)
      ((and (> (pt-y p1) (pt-y p2))
            (< (pt-x p1) (pt-x p2)))
       #'move-pt-up-left)
      ((and (< (pt-y p1) (pt-y p2))
            (< (pt-x p1) (pt-x p2)))
       #'move-pt-down-left)
      ((and (< (pt-y p1) (pt-y p2))
            (> (pt-x p1) (pt-x p2)))
       #'move-pt-down-right))))

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
         (setf *tail-locs* (cons (format nil "(~A,~A)" (pt-x new-pt) (pt-y new-pt)) *tail-locs*))
         (move-tail new-pt (rest rtail)))))))

(defun handle-moves (mv count rope)
  (loop :for i :from 0 :below count :do (move-rope rope mv)))

(defun count-tail-locs (locations)
  (length (remove-duplicates locations :test #'string=)))

(defun d9-part-one (moves)
  (let ((rope (make-rope 2)))
    (loop :for m :in moves :do
          (loop :for mv :in m :do (move-rope rope mv)))
    (count-tail-locs *tail-locs*)))

(defun d9-solve ()
  (let ((moves (d9-parse)))
    (values
     (d9-part-one moves)
     nil)))
