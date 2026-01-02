(ns adv.t07
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "resources/t07.txt"
       (slurp)
       (split-lines)
       (mapv vec)
       ))

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

(defn handle-row [[cnt indexes] row] 
  [(->> indexes
        (filter #(= (row %) \^))
        (count)
        (+ cnt))
   (->> indexes
        (map #(if (= (row %) \^) 
                #{(dec %) (inc %)} 
                #{%}))
        (apply clojure.set/union))])

(defn part1 [m]
  (let [[y x] (first (find-s' m))]
    (->> m
         (drop y)
         (reduce handle-row
                 [0 #{x}])
         (first)
    )))

(defn handle-row2 [counter row]
  (->> counter
       (keys)
       (map (fn [x] 
              (let [cnt (counter x)]
                (if (= (row x) \^)
                  {(dec x) cnt, (inc x) cnt}
                  {x cnt}))))
       (apply merge-with +)))

(defn part2 [m]
  (let [[y x] (first (find-s' m))]
     (->> m
          (drop y)
          (reduce handle-row2
                  {x 1})
          (vals)
          (apply +))))

(comment
  input
  (find-s' input)
  (part1 input)
  (part2 input)
  )