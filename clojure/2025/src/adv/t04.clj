(ns adv.t04
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "resources/t04.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(def neighbors
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not= [dx dy] [0 0])]
    [dx dy]))

(defn count-nbs [m p]
  (->> neighbors
       (map (partial map + p))
       (filter #(= \@ (get-in m %)))
       (count)))

(defn task1 [m]
  (let [ps (for [x (range (count (m 0))) 
                 y (range (count m))]
               [y x])]
    (->> ps
         (filter #(= \@ (get-in m %)))
         (map (partial count-nbs m))
         (filter #(< % 4))
         (count)
         )))

(comment
  (task1 input)
  )

