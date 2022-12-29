(in-package #:aoc)

(defun d8-parse ()
  (let* ((input (read-input "day8" :lines? t))
         (lst (mapcar #'(lambda (row)
                          (mapcar #'parse-integer (cl-ppcre:split "" row)))
                      input))
         (vec (make-array (list (length lst) (length (first lst))) :initial-contents lst)))
    vec))

(defun row-hidden? (x y width forest)
  (let ((tree (aref forest y x))
        (hidden nil))
    (loop :for i :from (1+ x) :below width
          :when (>= (aref forest y i) tree)
            :do (loop :for j :from (1- x) :downto 0
                      :when (>= (aref forest y j) tree)
                        :do (setf hidden t)))
    hidden))

(defun col-hidden? (x y height forest)
  (let ((tree (aref forest y x))
        (hidden nil))
    (loop :for i :from (1+ y) :below height
          :when (>= (aref forest i x) tree)
            :do (loop :for j :from (1- y) :downto 0
                      :when (>= (aref forest j x) tree)
                        :do (setf hidden t)))
    hidden))

(defun tree-hidden? (x y forest)
  (destructuring-bind (height width) (array-dimensions forest)
    (when (and (> x 0) (< x width)
               (> y 0) (< y height))
      (and (row-hidden? x y width forest)
           (col-hidden? x y height forest)))))

(defun get-row (array col)
  (let ((rows (array-dimension array 0)))
    (loop :for i :from 0 :below rows
          :collecting (aref array i col))))

(defun d8-part-one (forest)
  (let ((visible-trees nil))
  (destructuring-bind (height width) (array-dimensions forest)
    (loop :for i :from 0 :below height :do
          (loop :for j :from 0 :below width
                :when (not (tree-hidden? j i forest))
                  :do (setf visible-trees (cons (aref forest i j) visible-trees))))
    (length visible-trees))))

(defun d8-part-two () nil)
(defun d8-solve ()
  (let ((forest (d8-parse)))
    (values
     (d8-part-one forest)
     nil)))
