(ns aoc.day1
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(defn str->long [s]
  (Long/parseLong s))

(defn data->bags [data]
  (->> (string/split data #"\n\n")
       (map string/split-lines)
       (map (fn [coll] (map str->long coll)))))

(defn bags->counts [bags]
  (->> (map (fn [coll] (reduce + coll)) bags)
       (sort)))

(defn solve []
  (let [bags (data->bags (u/read-input "day1"))
        counts (bags->counts bags)]
    [(last counts) (reduce + (take-last 3 counts))]))
