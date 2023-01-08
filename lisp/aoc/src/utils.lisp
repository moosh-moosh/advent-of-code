(in-package #:aoc)

(defstruct (point (:conc-name pt-)) x y)

(defun read-input (day &key (lines? nil))
  ;; TODO: Figure out how to make this more generic
  (let ((path (format nil "../../../inputs/~a.txt" day)))
    (if lines?
        (uiop:read-file-lines path)
        (uiop:read-file-string path))))


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

(defun range (start &optional end)
  (let ((st (if end start 0))
        (en (if end end start)))
    (do ((s st (1+ s))
         (res '() (cons s res)))
        ((>= s en) (nreverse res)))))

(defun every-nth (str n)
  (loop :for x :across str
        :for y :in (range (length str))
        :when (= (mod y n) 0)
          :collect x))

(defun transpose (grid)
  (apply 'mapcar 'list grid))

(defun take (n lst)
  (if (> n (length lst))
      lst
      (subseq lst 0 n)))

(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
             (format stream "(~{~S ~}... ~S)" (subseq list 0 3) (first tail)))
            (t (format stream "~:S" list))))))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
            (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)

(defmethod peek ((queue queue))
  (with-slots (list) queue
    (first list)))

(defmethod empty-queue? ((queue queue))
  (with-slots (list) queue
    (null list)))

(defun make-queue ()
  (make-instance 'queue))
