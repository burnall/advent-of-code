(ns adv.t08
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-lines [s]
  (let [[[_ left right]] (re-seq #"(.+) \| (.+)" s)]
     {:left (split left #"\s+")
      :right  (split right #"\s+")}))

(def input
  (->> "data/t08.txt"
       (slurp)
       (split-lines)
       (map parse-lines)))

(defn solve [lines]
  (->> lines
       (mapcat :right)
       (filter (fn [s] (#{2 3 4 7} (count s))))
       (count)))

(defn f[]
  (solve input))