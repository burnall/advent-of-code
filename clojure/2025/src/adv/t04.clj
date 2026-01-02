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

(defn get-accessible [m]
  (let [ps (for [x (range (count (m 0)))
                  y (range (count m))]
              [y x])]
     (->> ps
          (filter #(= \@ (get-in m %)))
          (filter #(> 4 (count-nbs m %))))))

(defn task1 [m]
  (->> m
       (get-accessible)
       (count)))

(defn access [m]
  (when-let [ps (seq (get-accessible m))] 
    (reduce #(assoc-in % %2 \. ) m ps)))

(defn access-all [m]
  (if-let [m' (access m)]
    (recur m')
    m))  

(defn count-rolls [m]
  (->> m 
       (mapcat identity)
       (filter (partial = \@))
       (count)))

(defn task2 [m]
  (- (count-rolls m)
     (count-rolls (access-all m))))

(comment
  (task1 input)
  (task2 input)
  )

