(ns aoc.core
  (:require
   [aoc.day1 :as day1])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [day (first args)]
    (case day
      "1" (println (day1/solve))
      (println (str "no solution for day " day)))))
