(ns adv.t05
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-line [s]
  (let [[[_ a b c d]] (re-seq #"(\d+),(\d+) -> (\d+),(\d+)" s)]
    {:x0 (parse-int a)
     :y0 (parse-int b)
     :x1 (parse-int c)
     :y1 (parse-int d)})) 

(def input
  (->> "data/t05.txt"
       (slurp)
       (split-lines)
       (map parse-line)))

(defn gen-dots [{:keys [x0 y0 x1 y1]}]
  (cond
    (= x0 x1) (map (fn [y] [x0 y])
                   (range (min y0 y1) (inc (max y0 y1))))
    (= y0 y1) (map (fn [x] [x y0])
                   (range (min x0 x1) (inc (max x0 x1))))
    :else []))

(defn create-diagram [generate-dots lines]
  (reduce (fn [diagram line]
            (reduce (fn [d dot]
                      (update d dot (fn [v] (inc (or v 0)))))
                    diagram
                    (generate-dots line)))
          {}
          lines))

(defn get-overlaps [diagram]
  (->> diagram
       (filter (fn [[p cnt]] (> cnt 1)))
       (count)))

(defn solve [generate-dots lines]
  (get-overlaps (create-diagram generate-dots input)))

(defn f []
  (solve gen-dots input))


; Part 2

(defn gen-dots2 [{:keys [x0 y0 x1 y1]}]
  (cond
    (= x0 x1) (map (fn [y] [x0 y])
                   (range (min y0 y1) (inc (max y0 y1))))
    (= y0 y1) (map (fn [x] [x y0])
                   (range (min x0 x1) (inc (max x0 x1))))
    (= (Math/abs (- x0 x1)) (Math/abs (- y0 y1)))
              (let [dx (if (> x1 x0) 1 -1)
                    dy (if (> y1 y0) 1 -1)]
                (map (fn [d] [(+ x0 (* dx d)) (+ y0 (* dy d))])
                     (range 0 (inc (Math/abs (- x0 x1))))))
    :else []))

(defn f2 []
  (solve gen-dots2 input))
