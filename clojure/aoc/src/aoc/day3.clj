(ns aoc.day3
  (:require
   [aoc.utils :as u]
   [clojure.set :as s]
   [clojure.string :as string]))

(defn data->rucksacks [data]
  (string/split-lines data))

(defn char->priority [ch]
  (let [c (int ch)]
    (if (< c 91)
      (- c 38)
      (- c 96))))

(defn to-compartments [items]
  (let [mid (/ (count items) 2)]
    [(subs items 0 mid) (subs items mid)]))

(defn appears-in-both [[c1 c2]]
  ;; could get rid of a step if I just return the char here instead of the set
  (vec (s/intersection (set c1) (set c2))))

(defn appears-in-all [[x y z]]
  ;; could get rid of a step if I just return the char here instead of the set
  (vec (s/intersection (set x) (set y) (set z))))

(defn part-one [rucksacks]
  (->> rucksacks
       (map (fn [r] (to-compartments r)))
       (map (fn [c] (appears-in-both c)))
       (flatten)
       (map char->priority)
       (reduce +)))

(defn part-two [rucksacks]
  (->> rucksacks
       (partition-all 3)
       (map (fn [c] (appears-in-all c)))
       (flatten)
       (map char->priority)
       (reduce +)))

(defn solve []
  (let [rucksacks (data->rucksacks (u/read-input "day3"))]
    [(part-one rucksacks) (part-two rucksacks)]))
