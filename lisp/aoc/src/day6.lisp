(defpackage #:aoc/2022/day6
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day6)

(defun parse ()
  (let ((input (read-input "day6")))
    input))

(defun find-start-of-packet (stream marker-count)
  (labels ((read-stream (stream start end)
             (when (>= (length stream) end)
               (let ((seq (subseq stream start end)))
                 (if (unique? seq)
                     end
                     (read-stream stream (1+ start) (1+ end)))))))
    (read-stream stream 0 marker-count)))

(defun unique? (str)
  (string= str (remove-duplicates str)))

(defun part-one (input)
  (find-start-of-packet input 4))

(defun part-two (input)
  (find-start-of-packet input 14))

(defun solve ()
  (let ((input (parse)))
    (values
     (part-one input)
     (part-two input))))

(add-solution '202206 #'solve)
