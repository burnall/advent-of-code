(ns adv.t01
  (:require [adv.util :refer [split-lines split]]))

(defn parse-line [s]
  (->> (split s #"\s+")
       (map parse-long)))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)
       (map parse-line)))

(defn distance [pairs]
  (let [ll (sort (map first pairs))
        rl (sort (map second pairs))]
    (->> (map #(Math/abs (- %1 %2))
              ll
              rl)
         (apply +))))

(defn part1 [] 
  (distance input))

(defn similarity-score [pairs]
  (let [freq1 (frequencies (map first pairs))
        freq2 (frequencies (map second pairs))]
    (->> freq1
         (map (fn [[key cnt]] (* key cnt (freq2 key 0))))
         (apply +))))

(defn part2 [] 
  (similarity-score input))
