(ns adv.t02
  (:require [adv.util :refer [split]]))

(defn parse-range [s]
  (->> (split s #"-")
       (map parse-long)
       ((fn [[a b]] (range a (inc b))))))

(def input
  (as-> "resources/t02.txt" $
    (slurp $)
    (split $ #",")
    (map parse-range $)))

(defn invalid2? [xs]
  (some #(apply = (partition-all % xs))
        (range 1 (inc (quot (count xs) 2)))))

(defn invalid? [xs]
  (let [cnt (count xs)]
    (when (even? cnt)
      (apply = (partition-all (quot (count xs) 2) xs)))))

(defn task1 [rs]
  (->> rs
       (mapcat (partial filter (comp invalid? str)))
       (apply +)))

(task1 [(range 1 100)])
(task1 input)

(defn task2 [rs]
  (->> rs
       (mapcat (partial filter (comp invalid2? str)))
       (apply +)))

(comment
  (task2 [(range 1 200)])
  (task2 input))