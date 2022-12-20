(ns aoc.day12
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(defn ch->int [ch]
  (case ch
    \S (int \a)
    \E (int \z)
    (int ch)))

(defn line->row [line]
  (map identity line))

(defn input->grid [input]
  (reduce (fn [grid line]
            (conj grid (line->row line)))
          []
          (string/split-lines input)))

(defn find-coords [ch grid]
  (for [[x r] (map-indexed vector grid)
        [y v] (map-indexed vector r)
        :when (= ch v)]
    [x y]))

(defn find-start-end [grid]
  [(first (find-coords \S grid))
   (first (find-coords \E grid))])

(defn grid-to-values [grid]
  (mapv #(mapv ch->int %) grid))

(defn neighbors [[x y] grid]
  (let [nbors [[(dec x) y] [(inc x) y] [x (inc y)] [x (dec y)]]
        pos (get-in grid [x y])]
    (filter (fn [loc]
              (let [val (get-in grid loc)]
                (and (not (nil? val)) (<= val (inc pos)))))
            nbors)))

(defn copy-blank-grid [grid]
  (let [x (count grid)
        y (count (first grid))]
    (vec (replicate x (vec (replicate y nil))))))

;; god I suck at programming
(defn bfs [start dst grid]
  (let [routes (copy-blank-grid grid)
        queue (conj (clojure.lang.PersistentQueue/EMPTY) [0 start])]
    (loop [routes routes
           queue queue]
      (if (empty? queue)
        routes
        (let [[steps pos] (peek queue)]
          (cond
            (= pos dst) steps
            (not (nil? (get-in routes pos))) (recur routes (pop queue))
            :else (recur (assoc-in routes pos steps)
                         (pop (into queue (reduce (fn [paths path]
                                                    (conj paths [(inc steps) path]))
                                                  []
                                                  (neighbors pos grid)))))))))))

(defn parse-input [input]
  (let [char-grid (input->grid input)
        [start end] (find-start-end char-grid)]
    {:start start :end end :grid (grid-to-values char-grid)}))

(defn solve []
  (let [data (parse-input (u/read-input "day12" :sample? false))]
    (bfs (:start data) (:end data) (:grid data))))
