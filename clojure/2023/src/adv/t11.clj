(ns adv.t11
  (:require [adv.util :refer [split-lines split]]))

(def input
  (->> "data/t11.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(defn get-tile [m [x y]]
  (get-in m [y x]))

(defn find-empty [m]
  (let [mx (count (m 0))
        my (count m)
        rows (set (range my))
        columns (set (range mx))
        gals (for [x (range mx)
                   y (range my)
                   :when (= \# (get-tile m [x y]))]
                [x y])]
    {:rows (clojure.set/difference rows (set (map second gals)))
     :cols (clojure.set/difference columns (set (map first gals)))}))

(defn get-gals [m]
  (for [y (range (count m))
        x (range (count (m 0)))        
        :when (= \# (get-tile m [x y]))]
      [x y]))

(defn measure-distance-flat [z z' empty-z?]
  (->> (range (min z z') (max z z'))
       (map #(if (empty-z? %) 1000000 1))
       (apply +)))

(defn measure-distance [[x y] [x' y'] {empty-cols :cols, empty-rows :rows}] 
  (+ (measure-distance-flat x x' empty-cols)
     (measure-distance-flat y y' empty-rows)))

(defn get-distances [m]
  (let [empty-data (find-empty m)
        gals (vec (get-gals m))]
     (for [i (range (count gals))
           j (range (inc i) (count gals))]
       (measure-distance (gals i) (gals j) empty-data)))) 

(defn solve [m]
  (->> (get-distances m)
       (apply +))) 
