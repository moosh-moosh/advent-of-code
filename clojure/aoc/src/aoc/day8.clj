(ns aoc.day8
  (:require
   [clojure.string :as string]
   [aoc.utils :as u]))

(defn line->row [line]
  (mapv u/char->long (char-array line)))

(defn inner-vec [vct]
  (subvec vct 1 (- (count vct) 1)))

(defn inner-coords [trees]
  (let [coords (to-coordinates trees)]
    (mapv inner-vec (inner-vec coords))))

(defn to-coordinates [trees]
  (vec (map-indexed (fn [rn row]
                 (vec (map-indexed (fn [cn tree] [rn cn]) row)))
               trees)))

(defn get-col [col trees]
  (mapv #(get % col) trees))

(defn upto [n vct]
  (subvec vct 0 n))

(defn upfrom [n vct]
  (subvec vct (inc n)))

(defn all-shorter? [tree trees]
  (every? #(< % tree) trees))

(defn visible? [row col trees]
  (let [tree (get-in trees [row col])
        tree-row (get trees row)
        tree-col (get-col col trees)]
    (or  (all-shorter? tree (upto col tree-row))
         (all-shorter? tree (upfrom col tree-row))
         (all-shorter? tree (upto row tree-col))
         (all-shorter? tree (upfrom row tree-col)))))

(defn count-edges [trees]
  (+ (* 2 (count (first trees)))
     (- (* 2 (count trees)) 4)))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (reduce conj [] (map line->row lines))))

(defn part-one [trees]
  (let [edges (count-edges trees)
        coords (inner-coords trees)]
    (reduce (fn [val row]
              (let [nvisible (count (filter (fn [[r c]] (visible? r c trees)) row))]
                (+ val nvisible)))
            edges
            coords)))

(defn solve []
  (let [trees (-> (u/read-input "day8" :sample? false)
                  (parse-input))]
    [(part-one trees)]))
