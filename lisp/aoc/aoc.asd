;;;; aoc.asd

(asdf:defsystem #:aoc
  :description "Trying my best to work through Advent of Code 2022 in Common Lisp"
  :author "Ossi Pettersson <ossi@moosh.dev>"
  :license  ""
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "aoc")))