(ns adv.t02
  (:require [adv.util :refer [split-lines split parse-int]]))

(def input
  (->> "data/t02.txt"
       (slurp)
       (split-lines)
       (map (fn [s] (split s #"\t")))
       (map (partial mapv parse-int))))

(defn find-max-min-diff [v]
  (- (apply max v) (apply min v))) 

(defn find-evenly-divisible [v]
  (for [i (range (count v))
        j (range (count v))
        :when (and (not= i j) (zero? (mod (v i) (v j))))]
    (/ (v i) (v j))))
    
(defn solve [f vs]
  (->> vs
       (map f)
       (reduce +)))

(def solve1 (partial solve find-max-min-diff))

(def solve2 (partial solve (comp first find-evenly-divisible)))



