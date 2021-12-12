(ns adv.t01
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)
       (mapv parse-int)))

(defn solve [xs]
  (->> xs
       (cons (first xs))
       (map - xs)
       (filter pos?)
       (count)))

(defn f [] (solve input))

(defn solve2 [xs]
  (->> (- (count xs) 2)
       (range)
       (map (fn [i] (+ (xs i) (xs (+ i 1)) (xs (+ i 2)))))
       (solve)))

(defn f2 [] (solve2 input))


