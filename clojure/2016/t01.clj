(ns adv.t01
  (:require [adv.util :refer [split parse-int]]))

(defn parse-cmd [cmd] 
  {:turn (keyword (subs cmd 0 1))
   :distance (parse-int (subs cmd 1))})

(def input 
  (-> "data/t01.txt"
      (slurp)
      (split #", |\n")
      (->> (map parse-cmd))))

(def gradients [[0 1] [1 0] [0 -1] [-1 0]])

(defn move [{:keys [gradient pos]}
            {:keys [turn distance]}]
  (let [gradient (mod (+ gradient (if (= turn :R) 1 -1)) 
                      (count gradients))
        pos (mapv (fn [z dz] (+ z (* distance dz))) 
                  pos
                  (gradients gradient))]
    {:gradient gradient, :pos pos}))              

(defn solve 
  ([directions] 
    (->> directions 
         (reduce move {:gradient 0, :pos [0 0]})
         (:pos)
         (map #(Math/abs %))
         (reduce +)))

  ([] (solve input)))

; Part 2
(defn get-points [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    (map (fn [y] [x1 y]) (range y1 y2 (if (< y1 y2) 1 -1)))
    (map (fn [x] [x y1]) (range x1 x2 (if (< x1 x2) 1 -1)))))      

(defn find-visited [points visited]
  (->> points
       (filter visited)
       (first)))

(defn move-and-check [{visited :visited, :as state} direction]
  (let [{:keys [gradient pos]} (move state direction)
        points (get-points (:pos state) pos) 
        found (find-visited points visited)]
    (if found
      (reduced found)
      {:gradient gradient, :pos pos, :visited (apply conj visited points)})))

(defn solve-two 
  ([directions] 
    (->> directions
         (reduce move-and-check {:gradient 0, :pos [0 0], :visited #{}})
         (map #(Math/abs %))
         (reduce +)))

  ([] (solve-two input)))

