(defpackage #:aoc/2022/day12
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day12)

(defstruct hill-climber (start nil) (end nil) (heightmap nil))
(defstruct hill (row 0) (col 0) (value 0))

(defparameter part-one-test
  (lambda (height-destination height-current)
    (<= height-destination (1+ height-current)))
  "The elevation of the destination square can be at most one higher
   than the elevation of your current square")
(defparameter part-two-test
  (lambda (height-destination height-current)
    (>= height-destination (1- height-current))))

(defun hill-key (hill)
  (format nil "~a,~a" (hill-row hill) (hill-col hill)))

(defun hill-position (hill)
  (list (hill-row hill) (hill-col hill)))

(defun init-hill-climber (lines)
  "LINES is a list of strings. This function loops over LINES and then over each character
   of the string. Collecting the character ints into a 2d array."
  (let ((heightmap (make-array (list (length lines) (length (first lines)))))
        (start nil)
        (end nil))
    (loop :for y :from 0 :below (length lines)
          :for line := (nth y lines)
          :do (loop :for x :from 0 :below (length line)
                    :do (progn
                          (let ((val (char-int (aref line x))))
                            (cond
                              ((= (char-int #\S) val)
                               (setf start (make-hill :row y :col x :value (char-int #\a)))
                               (setf (aref heightmap y x) (make-hill :row y :col x :value (char-int #\a))))
                              ((= (char-int #\E) val)
                               (setf end (make-hill :row y :col x :value (char-int #\z)))
                               (setf (aref heightmap y x) (make-hill :row y :col x :value (char-int #\z))))
                              (t (setf (aref heightmap y x) (make-hill :row y :col x :value val))))))))
    (make-hill-climber :start start :end end :heightmap heightmap)))

(defun parse ()
  (let ((input (read-input "day12" :lines? t)))
    (init-hill-climber input)))

(defun adjacent-points (pt)
  (list
   (make-point :x (1- (pt-x pt)) :y (pt-y pt))
   (make-point :x (1+ (pt-x pt)) :y (pt-y pt))
   (make-point :x (pt-x pt) :y (1+ (pt-y pt)))
   (make-point :x (pt-x pt) :y (1- (pt-y pt)))))

(defun valid-position-p (row col rows cols)
  (and (>= row 0)
       (>= col 0)
       (< row rows)
       (< col cols)))

(defun adjacent-hills (hill hills)
  (destructuring-bind (rows cols) (array-dimensions hills)
    (let ((row (hill-row hill))
          (col (hill-col hill))
          (adjacents nil))
      (when (valid-position-p (1- row) col rows cols)
        (setf adjacents (cons (aref hills (1- row) col) adjacents)))
      (when (valid-position-p (1+ row) col rows cols)
        (setf adjacents (cons (aref hills (1+ row) col) adjacents)))
      (when (valid-position-p row (1- col) rows cols)
        (setf adjacents (cons (aref hills row (1- col)) adjacents)))
      (when (valid-position-p row (1+ col) rows cols)
        (setf adjacents (cons (aref hills row (1+ col)) adjacents)))
      adjacents)))

(defun neighbours (hill hills &optional (test part-one-test))
  (remove-if-not #'(lambda (h)
                     (funcall test (hill-value h) (hill-value hill)))
                 (adjacent-hills hill hills)))

(defun BFS (start goals arr &optional (test part-one-test))
  (let ((queue (enqueue start (make-queue)))
        (visited (make-hash-table :test 'equal)))
    (setf (gethash (hill-key start) visited) 0)
    (loop :while (not (empty-queue? queue))
          :do (progn
                (let ((pos (dequeue queue)))
                  (when (some #'(lambda (h) (equalp h pos)) goals)
                    (return (gethash (hill-key pos) visited)))
                  (dolist (hill (neighbours pos arr test))
                    (unless (gethash (hill-key hill) visited)
                      (enqueue hill queue)
                      (setf (gethash (hill-key hill) visited)
                            (1+ (gethash (hill-key pos) visited))))))))))

(defun find-points (val arr)
  (destructuring-bind (rows cols) (array-dimensions arr)
    (let ((points nil))
      (dotimes (row rows)
        (dotimes (col cols)
          (when (= val (hill-value (aref arr row col)))
            (setf points (cons (aref arr row col) points)))))
      points)))


(defun part-one (ht) (BFS (hill-climber-start ht)
                          (list (hill-climber-end ht))
                          (hill-climber-heightmap ht)))

(defun part-two (ht)
  (let ((goals (find-points (char-int #\a) (hill-climber-heightmap ht))))
    (BFS (hill-climber-end ht)
         goals
         (hill-climber-heightmap ht)
         part-two-test)))

(defun solve ()
  (let ((ht (parse)))
    (values
     (part-one ht)
     (part-two ht))))

(add-solution '202212 #'solve)
