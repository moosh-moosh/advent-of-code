(ns aoc.day11
  (:require
   [aoc.utils :as u]
   [clojure.string :as string]))

(defn make-monkey [[monkey starting-items operation test t f]]
  {:id (parse-monkey-id monkey)
   :items (parse-items starting-items)
   :operation (parse-operation operation)
   :test (parse-test test)
   :true (parse-true t)
   :false (parse-false f)
   :inspect-count 0})

(defn parse-monkey-id [str]
  (u/str->long (last (re-matches #"Monkey (\d+):" str))))

(defn parse-items [str]
  (-> (re-matches #".+Starting\sitems:\s(.+).*" str)
      (last)
      (string/split #",")
      (#(mapv (fn [n] (u/str->long (string/trim n))) %))))

(defn parse-operation [str]
  (rest (re-matches #".+Operation:\snew\s=\sold\s([\*\+\-\/])\s(\d+|old|new)" str)))

(defn parse-test [str]
  (let [e (rest (re-matches #".+Test:\s(\w+)\s\w+\s(.+).*" str))
        test (first e)
        by (u/str->long (string/trim (second e)))]
    {:test test :by by}))

(defn parse-true [str]
  (u/str->long (second (re-matches #".+If true:.+(\d+)" str))))

(defn parse-false [str]
  (u/str->long (second (re-matches #".+If false:.+(\d+)" str))))

(defn parse-monkey [monkey]
  (->> (string/split-lines monkey)
       (make-monkey)))

(defn do-op [operation item]
  (let [op (resolve (symbol (str (first operation) "'")))
        operand (second operation)]
    (if (= operand "old")
      (apply op [(bigint item) (bigint item)])
      (apply op [(bigint item) (u/str->long operand)]))))

(defn do-test [test worry]
  (case (:test test)
    "divisible" (= (mod worry (:by test)) 0)))

(defn inspect [monkey item]
  (do-op (:operation monkey) item))

(defn relief [worry lcm]
  (if (nil? lcm)
    (int (/ worry 3))
    (rem worry lcm)))

(defn throw-item [item monkey monkeys]
  (if (do-test (:test monkey) item)
    (-> (update-in monkeys [(:true monkey) :items] #(conj % item))
        (update-in [(:id monkey) :items] #(vec (rest %)))
        (update-in [(:id monkey) :inspect-count] inc))
    (-> (update-in monkeys [(:false monkey) :items] #(conj % item))
        (update-in [(:id monkey) :items] #(vec (rest %)))
        (update-in [(:id monkey) :inspect-count] inc))))

(defn inspect-item [monkey monkeys item lcm]
  (let [i (do-op (:operation monkey) item)]
    (-> (relief i lcm)
        (throw-item monkey monkeys))))

(defn inspect-items [monkey monkeys lcm]
  (let [items (:items monkey)]
    (if (empty? items)
      monkeys
      (reduce (fn [monkeys item]
                (inspect-item monkey monkeys item lcm))
              monkeys
              items))))

;; one round is going through each monkey, and each item for each monkey
(defn do-round [monkeys lcm]
  (reduce (fn [new-monkeys monkey] (inspect-items (nth new-monkeys (:id monkey)) new-monkeys lcm))
          monkeys
          monkeys))

(defn do-rounds [monkeys rounds lcm]
  (reduce (fn [new-monkeys r] (do-round new-monkeys lcm))
          monkeys
          (range rounds)))

(defn part-one [monkeys]
  (->> (do-rounds monkeys 20 nil)
       (sort-by :inspect-count >)
       (take 2)
       (map :inspect-count)
       (reduce *)))

(defn part-two [monkeys]
  (->> (do-rounds monkeys 10000 (reduce * (map #(get-in % [:test :by]) monkeys)))
       (sort-by :inspect-count >)
       (take 2)
       (map :inspect-count)
       (reduce *')))

(defn parse-input [input]
  (mapv parse-monkey (string/split input #"\n\n")))

(defn solve []
  (let [input (u/read-input "day11" :sample? false)
        monkeys (parse-input input)]
    [(part-one monkeys)
     (part-two monkeys)]))
