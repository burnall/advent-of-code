(ns adv.t07
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "resources/t07.txt"
       (slurp)
       (split-lines)))

(defn find-s [m]
  (for [y (range (count m))
        x (range (count (m y)))
        :let [p (get-in m [y x])]
        :when (= p \S)]
    [y x]))
    
(defn find-s' [m]
  (for [y (range (count m))
        :let [row (m y)]
        x (range (count row))
        :let [p (get row x)]
        :when (= p \S)]
    [y x]))

(defn map-indexes [row indexes]
  (map #(row %) indexes))

(defn handle-row [[cnt indexes] row] 
  [(+ cnt (count (filter #(= (row %) \^) indexes)))
   (map #(if (= (row %) \^)
              #{(dec x) (inc x)}
              #{x})
           indexes))
   ]
  )

(defn part1 [m s]
  (->> m
       (drop (first s))
       (reduce (fn [[cnt indexes] row] 
                 (mapcat 
                  (fn [x] 
                     (if (= (row x) \^)
                       [(dec x) (inc x)])         ) 
                         row)
                 )
               [0 #{(second s)}])
  ))

(comment
  input
  (find-s' input))