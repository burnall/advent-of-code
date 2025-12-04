(ns adv.t01
  (:require [adv.util :refer [split-lines parse-int]]))

(defn parse [[dir & ns]]
  (->> ns
       (apply str)
       (parse-int)
       (* (if (= dir \R) 1 -1))))

(def input
  (->> "resources/t01.txt"
       (slurp)
       (split-lines)
       (map parse)))

(defn mod+ [a b] 
  (mod (+ a b) 100))

(defn task1 []
  (->> input
       (reductions mod+ 50)
       (filter (partial = 0))
       (count)))

(defn add-turn [pos clicks]
  (if (neg? clicks)
    (recur (if (zero? pos) 0 (- 100 pos)) (- clicks))
    (quot (+ pos clicks) 100)))

(defn task2 []
  (letfn [(agg [[pos turns] clicks]
             [(mod+ pos clicks) 
              (+ turns (add-turn pos clicks))])]
  (->> input
       (reduce agg [50 0])
       (second))))
