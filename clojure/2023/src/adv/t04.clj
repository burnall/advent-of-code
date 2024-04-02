(ns adv.t04
  (:require [adv.util :refer [split-lines]]))

(defn parse-numbers [s]
  (->> s
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-card [s] 
  (let [[[_ card-no winning-numbers numbers]] (re-seq #"Card\s+(\d+):(.*)\|(.*)" s)]
    {:no (parse-long card-no) 
     :winning-numbers (set (parse-numbers winning-numbers))
     :numbers (parse-numbers numbers)}))

(def input
  (->> "data/t04.txt"
       (slurp)
       (split-lines)
       (map parse-card)))

(defn worth [card]
  (let [{:keys [winning-numbers numbers]} card]
    (->> numbers
         (filter winning-numbers)
         (count)
         (#(if (zero? %) 0 (bit-shift-left 1 (dec %)))))))

(defn part1 [cards]
  (->> cards
       (map worth)
       (reduce +)))