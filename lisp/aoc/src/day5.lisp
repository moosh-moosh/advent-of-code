(in-package #:aoc)

(defun d5-parse ()
  (let* ((data (read-input "day5" :lines? nil))
         (stacks-moves (cl-ppcre:split "\\n\\n" data))
         (stack-count (d5-parse-stack-count (first stacks-moves)))
         (stacks (mapcar #'(lambda (ln)
                             (fill-list (d5-parse-crate ln) stack-count))
                         (cl-ppcre:split "\\n" (remove-stack-count (first stacks-moves)))))
         (instructions (mapcar #'d5-parse-instruction (cl-ppcre:split "\\n" (second stacks-moves)))))
    (list (mapcar #'remove-empty (transpose stacks))
          instructions)))

(defun remove-empty (stack)
  (remove-if #'(lambda (crate)
                 (equal crate :empty))
             stack))

(defun remove-stack-count (stacks)
  (cl-ppcre:regex-replace "\\n.+\\d+" stacks ""))

(defun d5-parse-stack-count (stacks)
  (let* ((pos (cl-ppcre:scan "\\n.+\\d+" stacks))
         (count-ln (subseq stacks (1+ pos))))
    (cl-ppcre:register-groups-bind ((#'parse-integer n))
        ("^.+(\\d+)$" count-ln)
      n)))

(defun d5-parse-crate (ln)
  (every-nth (subseq ln 1) 4))

(defun d5-parse-instruction (inst)
  (cl-ppcre:register-groups-bind ((#'parse-integer num from to))
      ("move\\s(\\d+)\\sfrom\\s(\\d+)\\sto\\s(\\d+)" inst)
    (list num from to)))

(defun fill-list (lst sz)
  (labels ((fill-lst (lst sz cur)
             (cond
               ((>= cur sz) lst)
               (t (fill-lst (append lst (list :empty)) sz (1+ cur))))))
    (if (= (length lst) sz)
        (substitute :empty #\  lst)
        (fill-lst (substitute :empty #\  lst) sz (length lst)))))

(defun top-crates (stacks)
  (reduce #'(lambda (acc stack)
              (cons (first stack) acc))
          stacks
          :initial-value '()))

(defun d5-part-one (data)
  (let ((final-stacks (reduce #'(lambda (stacks inst)
                                  (d5-moves stacks inst))
                              (second data)
                              :initial-value (first data))))
    (concatenate 'string (nreverse (top-crates final-stacks)))))

(defun d5-part-two (data)
  (let ((final-stacks (reduce #'(lambda (stacks inst)
                                  (d5-moves stacks inst t))
                              (second data)
                              :initial-value (first data))))
    (concatenate 'string (nreverse (top-crates final-stacks)))))

(defun d5-moves (stacks instruction &optional (crate-mover? nil))
  (let ((num  (first instruction))
        (from (second instruction))
        (to   (third instruction)))
    (if crate-mover?
        (d5-move-multiple stacks num from to)
        (progn
          (dotimes (n num) (d5-move stacks from to))
          stacks))))

(defun d5-move (stacks from to)
  (let ((crate (pop (nth (1- from) stacks))))
    (push crate (nth (1- to) stacks))
    stacks))

(defun d5-move-multiple (stacks n from to)
  (let* ((crates   (take n (nth (1- from) stacks)))
         (new-from (nthcdr n (nth (1- from) stacks)))
         (new-to   (append crates (nth (1- to) stacks))))
    (setf (nth (1- from) stacks) new-from)
    (setf (nth (1- to) stacks) new-to)
    stacks))

(defun d5-solve ()
  (values
   (d5-part-one (d5-parse))
   (d5-part-two (d5-parse))))
