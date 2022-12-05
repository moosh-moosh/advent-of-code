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

(defun chunks-of (lst n)
  (labels ((chunklist (lst n res)
             (cond
               ((null lst) (nreverse res))
               ((< (length lst) n) (if res (nreverse (cons lst res)) (list lst)))
               (t (chunklist (nthcdr n lst) n (cons (subseq lst 0 n) res))))))
    (chunklist lst n '())))

(defun str-to-set (str)
  (loop :for ch :across str
        :for res := (list ch) :then (if (member ch res)
                                  res
                                  (cons ch res))
        :finally (return res)))
