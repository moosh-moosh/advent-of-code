;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export :read-input
           :add-solution
           :split-sequence
           :chunks-of
           :str-to-set
           :range
           :every-nth
           :transpose
           :take
           :make-point
           :pt-x
           :pt-y
           :make-queue
           :dequeue
           :enqueue
           :peek
           :empty-queue?
           :partition))
