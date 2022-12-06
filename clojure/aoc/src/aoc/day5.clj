(ns aoc.day5
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(def day5-sample
  ['("N" "Z")
   '("D" "C" "M")
   '("P")])

;; Should figure out how to parse the input file properly, but not today
(def day5-data
  ['("M" "S" "J" "L" "V" "F" "N" "R")
   '("H" "W" "J" "F" "Z" "D" "N" "P")
   '("G" "D" "C" "R" "W")
   '("S" "B" "N")
   '("N" "F" "B" "C" "P" "W" "Z" "M")
   '("W" "M" "R" "P")
   '("W" "S" "L" "G" "N" "T" "R")
   '("V" "B" "N" "F" "H" "T" "Q")
   '("F" "N" "Z" "H" "M" "L")])

(defn str->int [str]
  (Integer/parseInt str))

(defn line->instructions [line]
  (let [[num from to] (map str->int (rest (re-matches #"^move (\d+) from (\d+) to (\d+)" line)))]
    [num (dec from) (dec to)]))

(defn parse-input []
  (let [input (string/split (u/read-input "day5") #"\n\n")
        raw-instructions (string/split-lines (second input))
        instructions (reduce (fn [coll line]
                               (conj coll (line->instructions line)))
                             []
                             raw-instructions)]
    instructions))

(defn move [crates [num from to] crate-mover?]
  (let [items (take num (nth crates from))
        from-crate (nthrest (nth crates from) num)
        to-crate (into (nth crates to) (if crate-mover?
                                         (reverse items)
                                         items))]
    (-> (assoc crates from from-crate)
        (assoc to to-crate))))

(defn move-crates [crates instructions crate-mover?]
  (loop [crates crates
         instructions instructions]
    (let [instruction (first instructions)]
      (if (empty? instructions)
        crates
        (recur (move crates instruction crate-mover?) (rest instructions))))))

(defn top-items [crates]
  (loop [crates crates
         s ""]
    (let [crate (first crates)]
      (if (empty? crates)
        s
        (recur (rest crates) (str s (first crate)))))))

(defn part-one [instructions crates]
  (-> (move-crates crates instructions nil)
      (top-items)))

(defn part-two [instructions crates]
  (-> (move-crates crates instructions true)
      (top-items)))

(defn solve []
  (let [instructions (parse-input)
        crates day5-data]
    [(part-one instructions crates) (part-two instructions crates)]))
