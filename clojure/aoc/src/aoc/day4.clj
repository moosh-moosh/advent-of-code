(ns aoc.day4
  (:require
   [aoc.utils :as u]
   [clojure.set :as s]
   [clojure.string :as string]))

(defn section->range [section]
  (let [sect (string/split section #"-")]
    (set
     (range (Long/parseLong (first sect))
            (inc (Long/parseLong (second sect)))))))

(defn line->pair [line]
  (let [p (string/split line #",")]
    [(section->range (first p))
     (section->range (second p))]))

(defn part-one [input]
  (reduce (fn [val pair]
            (if (or (s/subset? (first pair) (second pair))
                    (s/subset? (second pair) (first pair)))
              (inc val) val))
          0 input))

(defn part-two [input]
  (reduce (fn [val pair]
            (if (> (count (s/intersection
                           (first pair)
                           (second pair))) 0)
              (inc val) val))
          0 input))

(defn parse-input []
  (->> (string/split-lines (u/read-input "day4"))
       (map (fn [line] (line->pair line)))))

(defn solve []
  (let [input (parse-input)]
    [(part-one input)
     (part-two input)]))
