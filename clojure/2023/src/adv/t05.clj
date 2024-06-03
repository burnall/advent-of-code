(ns adv.t05
  (:require [adv.util :refer [split-lines]]))

(defn parse-numbers [s]
  (->> s
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-groups [lines]
  (->> lines
       (reduce #(if (= %2 "") 
                  (conj % [])
                  (assoc % 
                         (dec (count %)) 
                         (conj (peek %) %2)))
               [[]]))) 

(defn parse-input [lines]
  (let [[seeds & maps] (parse-groups lines)]
    {:seeds (parse-numbers (seeds 0))
     :maps (->> maps 
                (map (partial drop 1))
                (map (partial map parse-numbers)))})) 

(def input
  (->> "data/t05.txt"
       (slurp)
       (split-lines)
       (parse-input)))

(defn domap [n m]
  (->> m
       (some (fn [[dst src len]]
                 (when (<= src n  (+ src len -1)) (+ dst (- n src)))))
       (#(or % n))))

(defn domaps [ms n]
  (reduce domap n ms))

(defn part1 [{:keys [seeds maps]}]
  (->> seeds
       (map (partial domaps maps))
       (apply min)))

