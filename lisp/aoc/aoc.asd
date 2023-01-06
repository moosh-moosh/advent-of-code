;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Trying my best to work through Advent of Code 2022 in Common Lisp"
  :author "Ossi Pettersson <ossi@moosh.dev>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "aoc")
               (:file "utils")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "day6")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10"))
  :depends-on (:cl-ppcre))
