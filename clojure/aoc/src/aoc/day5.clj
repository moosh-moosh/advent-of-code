(ns aoc.day5
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(defn str->int [str]
  (Integer/parseInt str))

(defn line->instructions [line]
  (let [[num from to] (map str->int (rest (re-matches #"^move (\d+) from (\d+) to (\d+)" line)))]
    [num (dec from) (dec to)]))

(defn line->crates [line]
  (->> line (rest) (take-nth 4)))

(defn do-move [num from to & {:keys [crate-mover?] :or {crate-mover? false}}]
  (if crate-mover?
    (cond
      (empty? from) [from to]
      (>= num (count from)) [[] (into to from)]
      :else [(subvec from 0 (- (count from) num))
             (into to (take-last num from))])
    (loop [i 0
           from from
           to to]
      (if (or (>= i num) (empty? from))
        [from to]
        (recur (inc i) (pop from) (conj to (last from)))))))

(defn move [stacks [num from to] & {:keys [crate-mover?] :or {crate-mover? false}}]
  (let [from-stack (stacks from)
        to-stack (stacks to)
        [new-from new-to] (do-move num from-stack to-stack :crate-mover? crate-mover?)]
    (-> (assoc stacks from new-from)
        (assoc to new-to))))

(defn move-stacks [stacks instructions & {:keys [crate-mover?] :or {crate-mover? false}}]
  (reduce (fn [s i] (move s i :crate-mover? crate-mover?)) stacks instructions))

(defn build-stacks [n]
  (loop [i 0
         v []]
    (if (>= i n)
      v
      (recur (inc i) (conj v [])))))

(defn parse-stacks [input]
  (let [num-stacks (last (re-find #"\n\s\d+.*" input))]
    (build-stacks (Character/digit num-stacks 10))))

(defn crates-to-stacks [crates stacks]
  (loop [crates crates
         stacks stacks
         i 0]
    (let [crate (first crates)]
      (cond
        (empty? crates) stacks
        (= crate \space) (recur (rest crates) stacks (inc i))
        :else (recur (rest crates) (assoc stacks i (conj (stacks i) crate)) (inc i))))))

(defn parse-input []
  (let [input (string/split (u/read-input "day5") #"\n\n")
        stacks (parse-stacks (first input))
        crates (map line->crates (string/split-lines (string/replace (first input) #"\n\s\d+.*" "")))
        instructions (reduce (fn [coll line]
                               (conj coll (line->instructions line)))
                             []
                             (string/split-lines (second input)))]
    [(reduce (fn [s c]
              (vec (crates-to-stacks c s)))
            stacks
            (reverse crates))
     instructions]))

(defn top-items [stacks]
  (reduce (fn [v c] (str v (last c))) "" stacks))

(defn part-one [instructions crates]
  (-> (move-stacks crates instructions)
      (top-items)))

(defn part-two [instructions crates]
  (-> (move-stacks crates instructions :crate-mover? true)
      (top-items)))

(defn solve []
  (let [[stacks instructions] (parse-input)]
    [(part-one instructions stacks) (part-two instructions stacks)]))
