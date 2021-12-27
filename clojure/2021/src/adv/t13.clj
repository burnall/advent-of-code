(ns adv.t13
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn read-point [s]
  (->> (split s #",")
       (map parse-int)))

(defn read-fold [s]
  (let [[[_ a n]] (re-seq #"(.)=(\d+)" s)]
    {:x? (= "x" a), :d (parse-int n)}))   

(defn parse-instruction [lines]
  (->> lines 
       (reduce (fn [{:keys [points folds read-points?] :as all} line]
                 (cond 
                   (not read-points?) (assoc all :folds (conj folds (read-fold line)))
                   (= line "") (assoc all :read-points? false)
                   :else (assoc all :points (conj points (read-point line)))))
               {:points [], :folds [], :read-points? true})
       (#(dissoc % :read-points?))))          

(def input
  (->> "data/t13.txt"
       (slurp)
       (split-lines)
       (parse-instruction)))

(defn foldx [points d]
  (->> points
       (reduce (fn [ps [x y]]
                 (conj ps [(if (< x d) x (+ d d (- x))) y]))
               #{})))

(defn solve [{:keys [points folds]}]
  (count (foldx points 
                (:d (first folds)))))

(defn f []
  (solve input))