(ns aoc.day2
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(def shape-scores {:rock 1
                   :paper 2
                   :scissors 3})

(defn code->shape [code]
  (case code
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn code->outcome [code]
  (case code
    "X" :loss
    "Y" :draw
    "Z" :win))

(defn outcome->shape [[opponent-shape outcome]]
  (case opponent-shape
    :rock (cond
            (= outcome :win) [opponent-shape :paper]
            (= outcome :draw) [opponent-shape :rock]
            (= outcome :loss) [opponent-shape :scissors])
    :paper (cond
             (= outcome :win) [opponent-shape :scissors]
             (= outcome :draw) [opponent-shape :paper]
             (= outcome :loss) [opponent-shape :rock])
    :scissors (cond
                (= outcome :win) [opponent-shape :rock]
                (= outcome :draw) [opponent-shape :scissors]
                (= outcome :loss) [opponent-shape :paper])))

(defn pair->shapes [pair & {:keys [outcomes] :or {outcomes false}}]
  (if outcomes
    (outcome->shape [(code->shape (first pair))
                     (code->outcome (second pair))])
    [(code->shape (first pair))
     (code->shape (second pair))]))

(defn shape->score [shape]
  (shape shape-scores))

(defn result->score [[x y]]
  (case y
    :rock (if (= x :rock) 3 (if (= x :scissors) 6 0))
    :paper (if (= x :paper) 3 (if (= x :rock) 6 0))
    :scissors (if (= x :scissors) 3 (if (= x :paper) 6 0))))

(defn score [[x y]]
  (+ (shape->score y)
     (result->score [x y])))

(defn data->pairs [data]
  (->> (string/split-lines data)
       (map (fn [x] (string/split x #" ")))))

(defn solve []
  (let [pairs (data->pairs (u/read-input "day2"))]
    [(->> pairs
          (map (fn [pair] (score (pair->shapes pair))))
          (reduce +))
     (->> pairs
          (map (fn [pair] (score (pair->shapes pair :outcomes true))))
          (reduce +))]))
