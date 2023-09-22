(ns adv.t03
  (:require [adv.util :refer [split-lines]]))

(defn parse-rucksack [s]
  (->> s
      (split-at (/ (count s) 2))
      (map (partial into #{}))))

(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (map parse-rucksack)))

(defn priority [ch]
  (if (> (int ch) (int \Z))
    (- (int ch) (int \a) -1)
    (- (int ch) (int \A) -27)))

(defn calc [a b]
  (-> a
      (clojure.set/intersection b)
      (first)
      (priority)))

(defn f1 [rucksacks]
  (->> rucksacks
       (map (partial apply calc))
       (apply +)))

(println (f1 input))

(defn f2 [rucksacks]
  (->> rucksacks
       (map (partial apply concat))
       (map (partial into #{}))
       (partition 3)
       (map (partial apply clojure.set/intersection))
       (map first)
       (map priority)
       (apply +)))

(println (f2 input))