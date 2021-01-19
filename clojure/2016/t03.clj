(ns adv.t03
  (:require [adv.util :refer [split split-lines parse-int]]))

(defn get-sides [line]
  (->> (split line #" +")
       (drop 1)
       (map parse-int)))

(def input 
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (map get-sides)))

(defn valid? [sides]
  (let [[a b c] (sort sides)]
    (< c (+ a b))))

(defn solve 
  ([triangles] 
    (->> triangles
         (filter valid?)
         (count)))
  ([] (solve input)))

; Part 2
(defn rows-to-cols [rows]
  (->> (concat (map first rows) 
               (map second rows)
               (map #(nth % 2) rows))
       (partition 3))) 

(defn solve2 
  ([rows] (solve (rows-to-cols rows)))
  ([] (solve2 input)))
