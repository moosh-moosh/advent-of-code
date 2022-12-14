(ns aoc.core
  (:require
   [aoc.day1 :as day1]
   [aoc.day2 :as day2]
   [aoc.day3 :as day3]
   [aoc.day4 :as day4]
   [aoc.day5 :as day5]
   [aoc.day6 :as day6]
   [aoc.day7 :as day7]
   [aoc.day8 :as day8])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [day (first args)]
    (case day
      "1" (println (day1/solve))
      "2" (println (day2/solve))
      "3" (println (day3/solve))
      "4" (println (day4/solve))
      "5" (println (day5/solve))
      "6" (println (day6/solve))
      "7" (println (day7/solve))
      "8" (println (day8/solve))
      (println (str "no solution for day " day)))))
