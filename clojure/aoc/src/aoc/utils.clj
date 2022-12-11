(ns aoc.utils)

(defn read-input [day & {:keys [sample?] :or {sample? false}}]
  (slurp (str (System/getProperty "user.dir") "/../../inputs/" day (when sample? "_sample") ".txt")))

(defn str->long [str]
  (Long/parseLong str))

(defn str->int [str]
  (Integer/parseInt str))

(defn char->long [ch]
  (Character/digit ch 10))
