(defpackage #:aoc/2022/day8
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day8)

(defstruct (ftree (:conc-name tree-))
  value
  point)

;; ugly and unnecessary really! just my style
(defun create-forest (input)
  (let ((row -1))
    (mapcar #'(lambda (r)
                (incf row)
                (let ((col -1))
                  (mapcar #'(lambda (c)
                              (incf col)
                              (make-ftree :value (parse-integer c)
                                          :point (make-point :x col :y row)))
                          (cl-ppcre:split "" r))))
            input)))

(defun parse ()
  (let* ((input (read-input "day8" :lines? t))
         (lst (create-forest input))
         (forest (make-array (list (length lst) (length (first lst))) :initial-contents lst)))
    forest))


(defun scan-left (point array)
  (loop :for i :from (1- (pt-x point)) :downto 0
        :collecting (aref array (pt-y point) i)))

(defun scan-right (point array)
  (let ((cols (array-dimension array 1)))
    (loop :for i :from (1+ (pt-x point)) :below cols
          :collecting (aref array (pt-y point) i))))

(defun scan-up (point array)
  (loop :for i :from (1- (pt-y point)) :downto 0
        :collecting (aref array i (pt-x point))))

(defun scan-down (point array)
  (let ((rows (array-dimension array 0)))
    (loop :for i :from (1+ (pt-y point)) :below rows
          :collecting (aref array i (pt-x point)))))

(defun remove-shorter-trees (tree trees)
  (remove-if #'(lambda (tr)
                 (< (tree-value tr) (tree-value tree)))
             trees))

(defun tree-visible-p (tree forest)
  (or (null (remove-shorter-trees tree (scan-left (tree-point tree) forest)))
      (null (remove-shorter-trees tree (scan-right (tree-point tree) forest)))
      (null (remove-shorter-trees tree (scan-up (tree-point tree) forest)))
      (null (remove-shorter-trees tree (scan-down (tree-point tree) forest)))))

(defun num-edge-trees (forest)
  (let ((rows (array-dimension forest 0))
        (cols (array-dimension forest 1)))
    (+ (* 2 cols) (* 2 (- rows 2)))))

(defun viewing-distance (tree trees)
  (loop :for i :in trees
        :with n := 0 :do
        (cond
          ((= (tree-value i) (tree-value tree)) (return (incf n)))
          ((< (tree-value i) (tree-value tree)) (incf n))
          (t (return (incf n))))
        :finally (return n)))

(defun scenic-score (tree forest)
  (* (viewing-distance tree (scan-up (tree-point tree) forest))
     (viewing-distance tree (scan-right (tree-point tree) forest))
     (viewing-distance tree (scan-down (tree-point tree) forest))
     (viewing-distance tree (scan-left (tree-point tree) forest))))

(defun visible-trees (forest)
  (let ((rows (array-dimension forest 0))
        (cols (array-dimension forest 1)))
    (loop :for i :from 1 :below (1- rows) nconc
            (loop :for j :from 1 :below (1- cols)
                  :when (tree-visible-p (aref forest i j) forest)
                    :collect (aref forest i j)))))

(defun part-one (forest)
  (let ((edges (num-edge-trees forest))
        (visible-trees (visible-trees forest)))
    (+ (length visible-trees) edges)))

(defun part-two (forest)
  (destructuring-bind (rows cols) (array-dimensions forest)
    (loop :for i :from 0 :below (* rows cols)
          :with h := 0
          :do
             (let ((score (scenic-score (row-major-aref forest i) forest)))
               (when (> score h)
                 (setf h score)))
          :finally (return h))))

(defun solve ()
  (let ((forest (parse)))
    (values
     (part-one forest)
     (part-two forest))))

(add-solution '202208 #'solve)
