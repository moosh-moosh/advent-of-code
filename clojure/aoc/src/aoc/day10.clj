(ns aoc.day10
  (:require
   [clojure.string :as string]
   [aoc.utils :as u]
   [clojure.string :as str]
   [clojure.pprint :as pprint]))

(defn parse-line [line]
  (-> (string/replace line #"noop|addx" "0")
      (string/split #"\s")
      (#(mapv u/str->long %))))

(defn parse-input [input]
  (->> (string/split-lines input)
       (mapv parse-line)
       (flatten)))

(defn part-one [cycles]
  (let [interesting [20 60 100 140 180 220]]
    (->> (map (fn [n] (nth cycles (dec n))) interesting)
         (map * interesting)
         (reduce +))))

(defn make-sprite [x]
  [(dec x) x (inc x)])

(defn currently-drawn? [sp pos]
  (some #(= pos %) sp))

(defn draw-pixel [pos sprite]
  (if (currently-drawn? sprite pos)
    "#"
    "."))

(defn part-two [cycles]
  (map-indexed (fn [i x]
                 (let [sprite (make-sprite x)
                       pixel  (draw-pixel (mod i 40) sprite)]
                   pixel))
               cycles))

(defn print-msg [cycles]
  (->> (part-two cycles)
       (partition 40)
       (map #(string/join #"" %))
       (pprint/pprint)))

(defn solve []
  (let [input (u/read-input "day10" :sample? false)
        data (parse-input input)
        cycles (reduce (fn [values op] (conj values (+ op (peek values)))) [1] data)]
    (print-msg cycles)
    (part-one cycles)))
