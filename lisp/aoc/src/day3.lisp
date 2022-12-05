(in-package #:aoc)

(defun char-priority (char)
  (let ((ch (char-int char)))
    (if (< ch 97) (- ch 38) (- ch 96))))


(defun rucksack-to-comps (rucksack)
  (let ((mid (/ (length rucksack) 2)))
    (list (subseq rucksack 0 mid)
          (subseq rucksack mid))))

(defun compartmentalize-rucksacks (rucksacks)
  (mapcar #'rucksack-to-comps rucksacks))

(defun find-common-item (comp1 comp2)
  (loop :named outer :for x :across comp1
        :do (loop :for y :across comp2
                  :do (if (eq x y) (return-from outer x)))))

(defun common-items-from-compartments (comp-rucksacks)
  (mapcar #'(lambda (compartments)
              (find-common-item (car compartments)
                                (cadr compartments)))
          comp-rucksacks))

(defun find-common-in-all (chunked-rucksack)
  (let ((s1 (str-to-set (car chunked-rucksack)))
        (s2 (str-to-set (cadr chunked-rucksack)))
        (s3 (str-to-set (caddr chunked-rucksack))))
    (intersection s3 (intersection s1 s2))))

(defun part-one (rucksacks)
  (let ((comp-rucksacks (compartmentalize-rucksacks rucksacks)))
    (reduce #'(lambda (val item) (+ val (char-priority item)))
            (common-items-from-compartments comp-rucksacks)
            :initial-value 0)))

(defun part-two (rucksacks)
  (let ((chunked-rucksacks (chunks-of rucksacks 3)))
    (reduce #'(lambda (val c)
                (if c (+ val (char-priority (car c))) val))
            (mapcar #'find-common-in-all chunked-rucksacks)
            :initial-value 0)))

(defun day3-solve ()
  (let ((input (read-input "day3" :lines? t)))
    (values (part-one input)
            (part-two input))))
