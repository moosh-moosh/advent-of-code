(defpackage #:aoc/2022/day11
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day11)

(defstruct monkey
  (items nil) (op nil) (test nil) (true nil) (false nil) (inspected 0))

(defparameter *lcm* nil)

(defun parse-op (ln)
  (cl-ppcre:register-groups-bind (o (#'parse-integer x))
      (".*Operation: new = old ([\\-*+]) ([\\-0-9]+)?" ln)
    (if x
        (lambda (i)
          (when i
            (funcall (read-from-string o) i x)))
        (lambda (i)
          (when i
            (funcall (read-from-string o) i i))))))

(defun parse-test (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer x))
      (".*Test: divisible by (\\d+)" ln)
    (if *lcm* (setf *lcm* (* *lcm* x)) (setf *lcm* x))
    (lambda (i) (= (mod i x) 0))))

(defun parse-success (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer x))
      (".*If true: throw to monkey (\\d+)" ln)
    x))

(defun parse-failure (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer x))
      (".*If false: throw to monkey (\\d+)" ln)
    x))

(defun parse-items (ln)
  ;; This looks pretty ugly, can I do threading in CL?
  (cl-ppcre:register-groups-bind (items)
      (".*Starting items: ([0-9\\-,\\s]+)" ln)
    (mapcar
     #'parse-integer
     (cl-ppcre:split
      ","
      (cl-ppcre:regex-replace-all
       "\\s"
       (string-trim '(#\Space #\Newline) items) "")))))

(defun parse-monkey-idx (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer x))
      ("Monkey (\\d+):" ln)
    x))

(defun parse-monkey (str monkeys)
  (let* ((data (cl-ppcre:split "\\n" str))
         (idx (parse-monkey-idx (nth 0 data)))
         (items (parse-items (nth 1 data)))
         (op (parse-op (nth 2 data)))
         (test (parse-test (nth 3 data)))
         (res-t (parse-success (nth 4 data)))
         (res-f (parse-failure (nth 5 data))))
    (setf (gethash idx monkeys)
          (make-monkey :items items :op op :test test :true res-t :false res-f))
    monkeys))

(defun parse ()
  (setf *lcm* nil)
  (let ((input (read-input "day11")))
    (reduce #'(lambda (acc m)
                (parse-monkey m acc))
            (cl-ppcre:split "\\n\\n" input) :initial-value (make-hash-table))))

(defun do-turns (monkeys turns &key relief lcm)
  (dotimes (i turns)
    (loop :for m :being :the :hash-values :of monkeys
          :do (inspect-items m monkeys :relief relief :lcm lcm)))
  monkeys)

(defun inspect-items (m monkeys &key relief lcm)
  (let ((item (funcall (monkey-op m) (pop (monkey-items m)))))
    (if item
        (progn
          (when relief (setf item (floor (/ item relief))))
          (when lcm (setf item (rem item lcm)))
          (incf (monkey-inspected m))
          (if (funcall (monkey-test m) item)
              (inspect-items m (throw-item item (monkey-true m) monkeys) :relief relief :lcm lcm)
              (inspect-items m (throw-item item (monkey-false m) monkeys) :relief relief :lcm lcm)))
        monkeys)))

(defun throw-item (item idx monkeys)
  (let ((target-monkey (gethash idx monkeys)))
    (setf (monkey-items target-monkey)
          (append (monkey-items target-monkey) (list item)))
    monkeys))

(defun inspect-counts (monkeys)
  (loop :for m :being :the :hash-values :of monkeys
        :collect (monkey-inspected m)))

(defun part-one (monkeys)
  (reduce
   #'*
   (take 2 (sort (inspect-counts (do-turns monkeys 20 :relief 3.0)) #'>))))

(defun part-two (monkeys)
  (reduce
   #'*
   (take 2 (sort (inspect-counts (do-turns monkeys 10000 :lcm *lcm*)) #'>))))

(defun solve ()
  (values
   (part-one (parse))
   (part-two (parse))))

(add-solution '202211 #'solve)
