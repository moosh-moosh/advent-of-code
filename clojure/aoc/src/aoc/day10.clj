(ns aoc.day10
  (:require
   [clojure.string :as string]
   [aoc.utils :as u]))

(defn parse-line [line]
  (-> (string/replace line #"noop|addx" "0")
      (string/split #"\s")
      (#(mapv u/str->long %))))

(defn parse-input [input]
  (->> (string/split-lines input)
       (mapv parse-line)
       (flatten)))

(defn part-one [data]
  (let [cycles [20 60 100 140 180 220]]
    (->> (mapv (fn [n] (reduce + 1 (take (dec n) data))) cycles)
         (mapv * cycles)
         (reduce +))))

(defn solve []
  (let [input (u/read-input "day10")
        data (parse-input input)]
    [(part-one data)]))
