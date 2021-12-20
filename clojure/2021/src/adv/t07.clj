(ns adv.t07
  (:require [adv.util :refer [split parse-int]]))

(def input
  (-> "data/t07.txt"
      (slurp)
      (split #",")
      (#(mapv parse-int %))))

(defn diff [ns n]
  (->> ns
       (map #(Math/abs (- n %)))
       (reduce +)))

(defn solve [ns]
  (let [sorted (vec (sort ns))
        mid-index (quot (count sorted) 2)
        mid (sorted mid-index)]
    (diff ns mid)))

(defn f[]
  (solve input))          


; Part 2 

(defn diff2 [ns n]
  (->> ns
       (map #(Math/abs (- n %)))
       (map #(/ (* % (inc %)) 2))
       (reduce +)))

(defn solve2 [ns]
  (let [avg (Math/round (double (/ (reduce + ns) (count ns))))
        diff0 (diff2 ns avg)]
    (reduce (fn [[m diff-prev] n]
               (let [d (diff2 ns m)]
                 (if (> d diff-prev)
                    (reduced [m diff-prev])
                    [n d])))
            [avg diff0]
            (range (dec avg) 0 -1))))        

(defn f2[]
  (solve2 input))          
