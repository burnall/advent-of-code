(ns adv.t09
  (:require [adv.util :refer [split-lines split]]))

(defn parse-point [s]
  (map parse-long (split s #",")))

(def input
  (->> "resources/t09-2.txt"
       (slurp)
       (split-lines)
       (mapv parse-point)))

(defn area [[x1 y1] [x2 y2]]
  (* (inc (Math/abs (- x1 x2)))
     (inc (Math/abs (- y1 y2)))))

(defn pairs [n]
  (for [i (range n)
        j (range (inc i) n)]
    [i, j]))


(defn task1 []
  (->> input
       (count)
       (pairs)
       (map (fn [[i j]] (area (input i) (input j))))
       (apply max)))

(defn red-green-area [corner-a corner-b points]
  (->> points
       (filter))
  0)

(defn task2 []
  (let [closed-polygon (conj input (input 0))]
    (->> input
         (count)
         (pairs)
         (reduce (partial red-green-area closed-polygon) 0)
         (apply max))))

(comment
  input
  (task1)
  (task2))