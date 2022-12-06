(ns aoc.day6
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(defn detect-start-of-packet-marker [input num-chars]
  (let [first-seq (take num-chars input)
        rest-seq (drop num-chars input)]
    (when (= (count (set first-seq)) num-chars) num-chars)
    (loop [tail (vec rest-seq)
           idx (inc num-chars)
           past-signal (vec first-seq)]
      (let [char (first tail)
            last-n (take-last (dec num-chars) past-signal)]
        (cond
          (= (count (set (conj last-n char))) num-chars) idx
          (empty? tail) nil
          :else (recur (rest tail) (inc idx) (conj past-signal char)))))))

(defn part-one [input]
  (detect-start-of-packet-marker input 4))

(defn part-two [input]
  (detect-start-of-packet-marker input 14))

(defn solve []
  (let [input (u/read-input "day6")]
    [(part-one input) (part-two input)]))
