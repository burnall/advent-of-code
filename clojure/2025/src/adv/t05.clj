(ns adv.t05
  (:require [adv.util :refer [split-lines split]]))

(defn parse-range [s] 
  (as-> s $
       (split $ #"-")
       (map parse-long $) 
    )
  )

(defn parse-input [lines]
  (let [[part1 part2] (split-with (partial not= "") lines)]
    {:ranges (mapv parse-range part1)
     :ns (map parse-long (rest part2))
     }))

(def input
  (->> "resources/t05.txt"
       (slurp)
       (split-lines)
       (parse-input)))

(comment
  input)