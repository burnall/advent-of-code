(ns adv.t03
  (:require [adv.util :refer [split-lines parse-int]]))

(def string-to-binary-array
  (partial mapv #(if (= % \0) 0 1))) 

(defn binary-array-to-number [bits]
  (Integer/parseInt (apply str bits) 2))


(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (mapv string-to-binary-array)))

(defn count-ones [xs]
  (->> (count (xs 0))
       (range)
       (map (fn [i] 
              (reduce (fn [sum row] (+ sum (row i)))
                      0
                      xs))))) 

(defn gamma [xs]
  (let [half (/ (count xs) 2)]
    (->> input
         (count-ones)
         (map (fn [cnt] (if (> cnt half) 1 0))))))


(defn solve [xs]
  (let [gamma (gamma xs)
        epsilon (map #(- 1 %) gamma)]
    (* (binary-array-to-number gamma)
       (binary-array-to-number epsilon))))      

(defn f []
  (solve input))


