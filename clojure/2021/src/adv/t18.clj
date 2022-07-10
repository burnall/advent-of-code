(ns adv.t18
   (:require [adv.util :refer [split-lines]]))

(def input 
  (->> "data/t18.txt"
       (slurp)
       (split-lines)
       (map read-string)))

(defn magnitude [v]
  (if (number? v)
    v
    (+ (* 3 (magnitude (first v)))
       (* 2 (magnitude (second v))))))   

