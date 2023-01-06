(defpackage #:aoc/2022/day7
  (:use #:cl #:aoc)
  (:export :solve))
(in-package #:aoc/2022/day7)

(defparameter *total-size* 70000000)
(defparameter *required-size* 30000000)
(defparameter *path* '())
(defparameter *fs* (make-hash-table :test 'equal))
(defstruct dir name (files nil) (size 0))
(defstruct file name size)

(defun parse ()
  (let ((input (read-input "day7" :lines? t)))
    input))

(defun cmd? (ln)
  (cl-ppcre:scan "^\\$.+" ln))

(defun unused-space (fs)
  (let ((total-used (dir-size (gethash "/" fs))))
    (- *total-size* total-used)))

(defun min-to-free (fs)
  (let ((unused (unused-space fs)))
    (- *required-size* unused)))

(defun parse-cmd (ln)
  (rest (cl-ppcre:split "\\s" ln)))

(defun dir? (ln)
  (cl-ppcre:scan "^dir.+" ln))

(defun parse-dir (ln)
  (cl-ppcre:register-groups-bind (name)
      ("^dir\\s([A-Za-z0-9_.-]+)" ln)
    name))

(defun file? (ln)
  (cl-ppcre:scan "^\\d+\\s\\w*.+" ln))

(defun parse-file (ln)
  (cl-ppcre:register-groups-bind ((#'parse-integer size) name)
      ("^(\\d+)\\s([A-Za-z0-9_.-]+)" ln)
    (make-file :name name :size size)))

(defun handle-cmd (cmd-arg)
  (let ((cmd (first cmd-arg)))
    (cond
      ((string= "ls" cmd) nil)
      ((string= "cd" cmd) (cd (second cmd-arg))))))

(defun parse-line (ln)
  (cond
    ((cmd? ln) (handle-cmd (parse-cmd ln)))
    ((file? ln) (add-file (parse-file ln)))))

(defun join-path (path)
  (format nil "~{~A~}" path))

(defun cwd ()
  (gethash (join-path *path*) *fs*))

(defun propagate-size (size path)
  (cond
    ((null path) nil)
    (t
     (let ((d (gethash (join-path path) *fs*)))
       (when d
         (setf (dir-size d) (+ (dir-size d) size))
         (propagate-size size (rest path)))))))

(defun add-file (f)
  (let ((d (cwd)))
    (when d
      (push f (dir-files d))
      (propagate-size (file-size f) (rest *path*))
      (setf (dir-size d)
            (+ (dir-size d) (file-size f))))))

(defun move-up ()
  (cond
    ((> (length *path*) 1)
     (pop *path*)
     *path*)
    (t *path*)))

(defun mkdir (name)
  (setf (gethash (join-path *path*) *fs*) (make-dir :name name))
  *path*)

(defun cd (arg)
  (cond
    ((string= arg "..") (move-up))
    (t
     (push arg *path*)
     (let ((d (gethash (join-path *path*) *fs*)))
       (if d
           *path*
           (mkdir arg))))))

(defun sum-totals (fs max)
  (loop :for v :being :the :hash-values :of fs
        :when (<= (dir-size v) max)
          :sum (dir-size v) :into total
        :finally (return total)))

(defun find-smallest-delete (fs min)
  (loop :for v :being :the :hash-values :of fs
        :with s := (dir-size (gethash "/" fs))
        :when (and (>= (dir-size v) min)
                   (< (dir-size v) s))
          :do (setf s (dir-size v))
        :finally (return s)))

(defun part-one (input)
  (mapcar #'parse-line input)
  (sum-totals *fs* 100000))

(defun part-two (fs)
  (let ((min (min-to-free fs)))
    (find-smallest-delete fs min)))

(defun solve ()
  (let ((input (parse)))
    (values
     (part-one input)
     (part-two *fs*))))

(add-solution '202207 #'solve)
