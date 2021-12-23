(ns adv.t09
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-line [s]
  (->> s
       (mapv (comp parse-int str))))  

(def input
  (->> "data/t09.txt"
       (slurp)
       (split-lines)
       (mapv parse-line)))

(defn low-points [hmap]
  (let [ymax (count hmap)
        xmax (count (hmap 0))
        nb (fn [x y]
             [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]])

        higher? (fn [x y h]
                 (or (< x 0) (= x xmax) (< y 0) (= y ymax)
                      (> (get-in hmap [y x]) h)))]
    (for [x (range xmax)
          y (range ymax)
          :let [h (get-in hmap [y x])]
          :when (every? (fn [[xa ya]] (higher? xa ya h))
                        (nb x y))]
      h)))

(defn solve [hmap]
  (let [ps (vec (low-points hmap))]
    (->> ps
         (reduce +)
         (+ (count ps)))))

(defn f [] 
  (solve input))


