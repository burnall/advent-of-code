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
    (->> xs
         (count-ones)
         (map (fn [cnt] (if (> cnt half) 1 0))))))


(defn solve [xs]
  (let [gamma (gamma xs)
        epsilon (map #(- 1 %) gamma)]
    (* (binary-array-to-number gamma)
       (binary-array-to-number epsilon))))      

(defn f []
  (solve input))

; Part 2
(defn bit-criteria [xs idx most-common?]
  (let [half (/ (count xs) 2)]
    (->> xs
         (map #(% idx))
         (reduce +)
         (#(if (or (and most-common? (>= % half))
                   (and (not most-common?) (< % half)))
              1
              0)))))

(defn filter-for-rating [xs idx most-common?]
  (let [b (bit-criteria xs idx most-common?)]
    (filter (fn [ys] (= (ys idx) b))
            xs)))

(defn find-rating [xs idx most-common?]
  (if (< (count xs) 2)
     (first xs)
     (recur (filter-for-rating xs idx most-common?)
            (inc idx)
            most-common?)))  

(defn find-oxygen-generator-rating [xs] 
  (find-rating xs 0 true))

(defn find-co2-scrubber-rating [xs]
  (find-rating xs 0 false))

(defn solve2 [xs]
  (->> [find-oxygen-generator-rating find-co2-scrubber-rating]
       (map #(% xs))
       (map binary-array-to-number)
       (reduce *)))

(defn f2 [] (solve2 input))
