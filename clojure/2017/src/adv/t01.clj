(ns adv.t01
  (:require [adv.util :refer [split-lines zip]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (vec)))

(defn char-to-int [c]
  (- (int c) (int \0)))

(defn solve [s]
  (->> (concat (rest s) [(first s)])
       (zip s)
       (reduce (fn [agg [a b]] (if (= a b) (+ agg (char-to-int a)) agg)) 
               0)))

(defn solve2 [v]
  (let [cnt (count v) 
        nb (fn [n] (mod (+ n (/ cnt 2)) cnt))]
   (->> cnt
        (range) 
        (reduce (fn [agg i] 
                  (if (= (v i) (v (nb i)))
                    (+ agg (char-to-int (v i)))
                    agg))
                0))))      
