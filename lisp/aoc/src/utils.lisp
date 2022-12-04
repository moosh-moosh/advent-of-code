(in-package #:aoc)

(defun split-sequence (seq delimiter &key (test #'eq))
  "Split sequence SEQ by DELIMITER."
  (labels ((split-seq (seq del test res)
             (let ((pos (position del seq :test test)))
               (cond
                 ((null seq) res)
                 (pos
                  (split-seq (nthcdr (1+ pos) seq)
                             del
                             test
                             (append res (list (subseq seq 0 pos)))))
                 (t (split-seq nil del test (append res (list seq))))))))
    (split-seq seq delimiter test '())))
