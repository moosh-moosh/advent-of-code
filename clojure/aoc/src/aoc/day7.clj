(ns aoc.day7
  (:require
   [aoc.utils :as u]
   [clojure.walk :as walk]
   [clojure.string :as string]))

(def limit 100000)
(def total-disk 70000000)
(def min-free-space 30000000)

(defn push-path [path loc]
  (conj path loc))

(defn pop-path [path]
  "Pop a location from the path, make sure root location always remains."
  (if (= (count path) 1)
    path
    (pop path)))

(defn make-dir [name]
  {name {:dirs {}
         :files {}
         :total 0}})

(defn make-file [name size]
  {name (Long/parseLong size)})

(defn get-used-size [fs]
  (or (get-in fs ["/" :total]) 0))

(defn get-min-deleted [fs]
  (->> (- total-disk (get-used-size fs))
       (- min-free-space)))

(defn add-dir [[path fs] dir]
  "Add new directory to FS. Update new location to PATH."
  (let [d (make-dir dir)]
    (if (nil? d)
      [path fs]
      [(push-path path dir)
       (update-in fs (-> (interpose :dirs path) (concat [:dirs])) (fn [dirs] (into dirs d)))])))

(defn update-totals [fs path size]
  (if (= (count path) 1)
    (update-in fs (concat path [:total]) (fn [total] (+ total size)))
    (recur (update-in fs (-> (interpose :dirs path) (concat [:total])) (fn [total] (+ total size)))
           (pop-path path)
           size)))

(defn sum-below-limit [fs limit]
  "Walk over FS adding together totals less than or equal to LIMIT."
  (let [total (atom 0)]
    (walk/postwalk (fn [n]
                             (when (and (map-entry? n)
                                        (= :total (first n))
                                        (<= (second n) limit))
                               (swap! total + (second n)))
                             n)
                           fs)
    @total))

(defn size-of-dir-to-delete [fs min-size]
  "Walk over FS, add all totals greater than or equal to MIN-SIZE to a vector, return the smallest value."
  (let [sizes (atom [])]
    (walk/postwalk (fn [n]
                             (when (and (map-entry? n)
                                        (= :total (first n))
                                        (>= (second n) min-size))
                               (swap! sizes conj (second n)))
                             n)
                           fs)
    (reduce min @sizes)))

(defn add-file [[path fs] file]
  "Adds FILE to FS at current location in PATH."
  (if (nil? file)
    [path fs]
    [path
     (-> (update-in fs (-> (interpose :dirs path) (concat [:files]))
                    (fn [files] (into files file)))
         (update-totals path (second (first (seq file)))))]))

(defn line->action [line]
  (let [action (rest (re-matches #"^\$\s(\w+)[\s]?([\w.-]+)?.*$" line))]
    (if action
      {:action (first action)
       :target (second action)}
      nil)))

(defn line->dir [line]
  (let [dir (rest (re-matches #"^dir\s([\w.-]+).*$" line))]
    (if dir
      (make-dir dir)
      nil)))

(defn line->file [line]
  (let [f (rest (re-matches #"^(\d+)\s([\w.-]+).*$" line))]
    (if f
      (make-file (second f) (first f))
      nil)))

(defn cd [[path fs] action]
  (case (:target action)
    nil [path fs]
    ".." [(pop-path path) fs]
    (add-dir [path fs] (:target action))))

(defn handle-action [[path fs] action]
  (case (:action action)
    "ls" [path fs]
    "cd" (cd [path fs] action)
    [path fs]))

(defn parse-line [[path fs] line]
  (cond
    (nil? line) [path fs]
    (string/starts-with? line "dir") [path fs]
    (string/starts-with? line "$") (handle-action [path fs] (line->action line))
    :else (add-file [path fs] (line->file line))))

(defn parse-input [input]
  (let [tree {:name "/" :children [] :parent nil :total-size 0}
        lines (rest (string/split-lines input))]
    (reduce (fn [fs line]
              (parse-line fs line))
            [["/"] (make-dir "/")]
            lines)))

(defn part-one [fs]
  (sum-below-limit fs limit))

(defn part-two [fs]
  (let [min-deleted (get-min-deleted fs)]
    (size-of-dir-to-delete fs min-deleted)))

(defn solve []
  (let [input (u/read-input "day7" :sample? false)
        fs (parse-input input)]
    [(part-one fs) (part-two (second fs))]))
