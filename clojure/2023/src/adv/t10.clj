(ns adv.t10
  (:require [adv.util :refer [split-lines lcm]]))

(def tile-type
  {\| #{:north :south}
   \- #{:east :west}
   \L #{:north :east}
   \J #{:north :west}
   \7 #{:south :west}
   \F #{:south :east}
   \. #{}
   \S #{}})

(def opposite
  {:north :south
   :south :north
   :east :west
   :west :east})

(defn connected? [tile dir]
  (contains? (tile-type tile) dir)) 

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(defn get-tile [m [x y]]
  (get-in m [y x]))

(defn find-animals [m]
  (for [y (range (count m)) 
        x (range (count (m y)))
        :when (= \S (get-tile m [x y]))]
    [x y]))

(defn get-coord [[x y] dir]
  (condp = dir
    :north [x (dec y)]
    :east [(inc x) y]
    :south [x (inc y)]
    :west [(dec x) y]))

(defn find-first-move [m [x y]]
  (cond
    (and (> y 0) (connected? (get-tile m (get-coord [x y] :north)) :south)) 
      :north
    (and (< (inc x) (count (m y))) (connected? (get-tile m (get-coord [x y] :east)) :west))
      :east   
    (and (< (inc y) (count y)) (connected? (get-tile m (get-coord [x y] :south)) :north)) 
      :south
    (and (> x 0) (connected? (get-tile m (get-coord [x y] :west)) :east))
      :west))

(defn get-loop [m]
  (let [start-pos (first (find-animals m))
        first-dir (find-first-move m start-pos) 
        second-pos (get-coord start-pos first-dir)
        find-loop (fn [p dir ps]
                    (let [p' (get-coord p dir)
                          dir' (->> (get-tile m p')
                                    (tile-type)
                                    (some #(when (not= % (opposite dir)) %)))]
                      (if (= p' start-pos)
                        ps
                        (recur p' dir' (conj ps p')))))]
    (find-loop start-pos first-dir [start-pos])))

(defn solve [m]
  (->> (get-loop m)
       (count)
       (#(/ % 2))))

;Part 2

(defn inside? [m ps [x y]]
  (when-not (contains? ps [x y])
    (->> (inc (min x y))
         (range 1)
         (mapcat (fn [d]
                   (let [p [(- x d) (- y d)]]
                     (when (contains? ps p)
                       (condp = (get-tile m p) 
                         \7 [2]
                         \L [2] 
                         [1])))))
         (apply +)
         (odd?))))

(defn get-inside-points [m]
  (let [ps (set (get-loop m))]
    (for [y (range (count m)) 
        x (range (count (m y)))
        :when (inside? m ps [x y])]
    [x y])))

(defn solve2 [m]
  (count (get-inside-points m)))

