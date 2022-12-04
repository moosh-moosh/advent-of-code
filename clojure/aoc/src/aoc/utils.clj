(ns aoc.utils)

(defn read-input [day & {:keys [sample?] :or {sample? false}}]
  (slurp (str (System/getProperty "user.dir") "/../../inputs/" day (when sample? "_sample") ".txt")))
