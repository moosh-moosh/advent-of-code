(defpackage #:aoc/2022/day13
  (:use #:cl #:aoc))
(in-package #:aoc/2022/day13)

(defun replace-left-bracket (str)
  (cl-ppcre:regex-replace-all "\\[" str "("))

(defun replace-right-bracket (str)
  (cl-ppcre:regex-replace-all "\\]" str ")"))

(defun replace-comma (str)
  (cl-ppcre:regex-replace-all "," str " "))

(defun make-lispy (str)
  (replace-left-bracket (replace-right-bracket (replace-comma str))))

(defun empty-str-p (str)
  (= 0 (length str)))

(defun compare-pair (left right)
  (unless (and (null left) (null right))
    (cond
      ((and (null left) (not (null right))) 'right)
      ((and (not (null left)) (null right)) 'left)
      ((and (null (first left)) (not (null (first right)))) 'right)
      ((and (not (null (first left))) (null (first right))) 'left)
      ((and (null (first left)) (null (first right))) (compare-pair (rest left) (rest right)))
      ((and (atom (first left))
            (atom (first right)))
       (or (compare-int (first left) (first right))
           (compare-pair (rest left) (rest right))))
      ((and (atom (first left))
            (listp (first right)))
       (compare-pair (cons (list (first left)) (rest left)) right))
      ((and (listp (first left))
            (atom (first right)))
       (compare-pair left (cons (list (first right)) (rest right))))
      ((and (listp (first left)) (listp (first right)))
       (or (compare-pair (first left) (first right))
            (compare-pair (rest left) (rest right))))
      (t nil))))

(defun compare-int (left right)
  (cond
    ((< left right) 'right)
    ((> left right) 'left)
    (t nil)))

(defun parse ()
  (let ((input (read-input "day13" :lines? nil)))
    (mapcar #'read-from-string
            (remove-if #'empty-str-p
                       (cl-ppcre:split "\\n" (make-lispy input))))))

(defun compare-pairs (pairs)
  (loop :for pair :in pairs
        :for i :from 1
        :when (eql 'right (compare-pair (first pair) (second pair)))
          :collect i))

(defun part-one (input)
  (let ((pairs (chunks-of input 2)))
    (reduce #'+ (compare-pairs pairs))))

(defun part-two (input)
  (let ((sorted-packets (sort (append '(((2)) ((6))) input) #'(lambda (a b) (eql (compare-pair a b) 'right)))))
    (* (1+ (position '((2)) sorted-packets :test 'equal))
       (1+ (position '((6)) sorted-packets :test 'equal)))))

(defun solve ()
  (let ((input (parse)))
    (values
     (part-one input)
     (part-two input))))

(add-solution '202213 #'solve)
