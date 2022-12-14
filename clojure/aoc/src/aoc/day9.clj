(ns aoc.day9
  (:require
   [clojure.string :as string]
   [aoc.utils :as u]))

(defn make-rope [len]
  (vec (repeat len [0 0])))

(defn distance [[x1 y1] [x2 y2]]
  (int
   (Math/sqrt (+ (int (Math/pow (- x2 x1) 2))
                 (int (Math/pow (- y2 y1) 2))))))

(defn is-above? [[_ y1] [_ y2]]
  (> y1 y2))

(defn is-below? [[_ y1] [_ y2]]
  (< y1 y2))

(defn is-left? [[x1 _] [x2 _]]
  (< x1 x2))

(defn is-right? [[x1 _] [x2 _]]
  (> x1 x2))

(defn col-aligned? [[x1 _] [x2 _]]
  (= x1 x2))

(defn row-aligned? [[_ y1] [_ y2]]
  (= y1 y2))

(defn move-right [[x y]]
  [(inc x) y])

(defn move-left [[x y]]
  [(dec x) y])

(defn move-up [[x y]]
  [x (inc y)])

(defn move-down [[x y]]
  [x (dec y)])

(defn move-up-right [loc]
  (-> (move-up loc)
      (move-right)))

(defn move-up-left [loc]
  (-> (move-up loc)
      (move-left)))

(defn move-down-right [loc]
  (-> (move-down loc)
      (move-right)))

(defn move-down-left [loc]
  (-> (move-down loc)
      (move-left)))

(defn move-diagonal [head [x2 y2 :as tail]]
  (cond
    (and (is-above? head tail) (is-right? head tail)) move-up-right
    (and (is-above? head tail) (is-left? head tail)) move-up-left
    (and (is-below? head tail) (is-right? head tail)) move-down-right
    (and (is-below? head tail) (is-left? head tail)) move-down-left
    :else tail))

(defn move-vertical [head tail]
  (cond
    (is-above? head tail) move-up
    (is-below? head tail) move-down))

(defn move-horizontal [head tail]
  (cond
    (is-right? head tail) move-right
    (is-left? head tail) move-left))

(defn get-move [head tail]
  (if (< (distance head tail) 2)
    nil
    (cond
      (row-aligned? head tail) (move-horizontal head tail)
      (col-aligned? head tail) (move-vertical head tail)
      :else (move-diagonal head tail))))

(defn move-rope [[head & tail] move]
  (let [new-head (move head)
        [new-rope _] (reduce (fn [[rope front] knot]
                               (let [m (get-move front knot)]
                                 (if m [(conj rope (m knot)) (m knot)] [(conj rope knot) knot])))
                             [[new-head] new-head]
                             tail)]
    new-rope))

(defn apply-moves [rope tail-locs moves]
  (reduce (fn [[r tail-locs] move]
            (let [new-rope (move-rope r move)]
              [new-rope (conj tail-locs (last new-rope))]))
          [rope tail-locs]
          moves))

(defn get-count [count-str]
  (u/str->int count-str))

(defn direction->move [direction]
  (case direction
    "R" move-right
    "L" move-left
    "U" move-up
    "D" move-down))

(defn line->moves [line]
  (let [direction-count (string/split line #"\s")
        d (first direction-count)
        c (get-count (second direction-count))]
    (repeat c (direction->move d))))

(defn lines->moves [lines]
  (mapv (fn [line] (line->moves line)) lines))

(defn parse-input [input]
  (let [lines (string/split-lines input)]
    (lines->moves lines)))

(defn part-one [moves]
  (let [[_ tail-locs] (reduce (fn [[rope tail-locs] moves]
                                (apply-moves rope tail-locs moves))
                              [(make-rope 2) #{[0 0]}]
                              moves)]
    (count tail-locs)))

(defn part-two [moves]
  (let [[_ tail-locs] (reduce (fn [[rope tail-locs] moves]
                                (apply-moves rope tail-locs moves))
                              [(make-rope 10) #{[0 0]}]
                              moves)]
    (count tail-locs)))

(defn solve []
  (let [input (u/read-input "day9" :sample? false)
        moves (parse-input input)]
    [(part-one moves) (part-two moves)]))
