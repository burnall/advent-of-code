(ns adv.t06
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "data/t06.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(defn nth-s [lines n]
  (mapv #(% n) lines))

(defn most-frequent [xs]
  (->> xs
       (frequencies)
       (sort-by second)
       (first)
       (first)))

(defn solve
  ([lines] 
    (->> (lines 0)
         (count)
         (range) 
         (mapv (partial nth-s lines))
         (map most-frequent)
         (apply str))) 

  ([] (solve input)))
