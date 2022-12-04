(ns aoc.core
  (:require
   [aoc.day1 :as day1]
   [aoc.day2 :as day2]
   [aoc.day3 :as day3])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [day (first args)]
    (case day
      "1" (println (day1/solve))
      "2" (println (day2/solve))
      "3" (println (day3/solve))
      (println (str "no solution for day " day)))))
