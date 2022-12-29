(in-package #:aoc)

(defun d6-parse ()
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

(defun d6-part-one (input)
  (find-start-of-packet input 4))

(defun d6-part-two (input)
  (find-start-of-packet input 14))

(defun d6-solve ()
  (let ((input (d6-parse)))
    (values
     (d6-part-one input)
     (d6-part-two input))))
