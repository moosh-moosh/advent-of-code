(in-package #:aoc)

(defun line-to-pair (line)
  (let ((pair (uiop:split-string line :separator '(#\,))))
    (mapcar #'(lambda (p)
                (let ((r (uiop:split-string p :separator '(#\-))))
                  (range (parse-integer (car r))
                         (1+ (parse-integer (cadr r))))))
            pair)))

(defun day4-p1 (pairs)
  (reduce #'(lambda (val pair)
              (if (or (subsetp (car pair) (cadr pair))
                      (subsetp (cadr pair) (car pair)))
                  (1+ val)
                  val))
          pairs
          :initial-value 0))

(defun day4-p2 (pairs)
  (reduce #'(lambda (val pair)
              (if (intersection (car pair) (cadr pair))
                  (1+ val)
                  val))
          pairs
          :initial-value 0))

(defun day4-solve ()
  (let* ((input (read-input "day4" :lines? t))
         (pairs (mapcar #'line-to-pair input)))
    (values (day4-p1 pairs) (day4-p2 pairs))))
