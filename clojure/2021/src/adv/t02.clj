(ns adv.t02
  (:require [adv.util :refer [split-lines parse-int]]))

(def dirs 
  {"forward" :fw, "down" :dn, "up" :up}) 

(defn parse-сmds [s]
  (let [[[_ dir d]] (re-seq #"(\w+) (\d+)" s)]
     {:dir (dirs dir), :d (parse-int d)}))

(def input
  (->> "data/t02.txt"
       (slurp)
       (split-lines)
       (mapv parse-сmds)))

(def gradients
  {:fw (fn [[x y] d] [(+ x d) y])
   :dn (fn [[x y] d] [x (+ y d)])
   :up (fn [[x y] d] [x (- y d)])})

(defn solve [grad cmds start]
  (reduce
     (fn [p {:keys [dir d]}]
       ((grad dir) p d))
     start
     cmds))

(defn f [] 
  (let [[a b] (solve gradients input [0 0])]
    (* a b)))

(def gradients2
  {:fw (fn [[x y aim] d] [(+ x d) (+ y (* d aim)) aim])
   :dn (fn [[x y aim] d] [x y (+ aim d)])
   :up (fn [[x y aim] d] [x y (- aim d)])})

(defn f2 [] 
  (let [[a b] (solve gradients2 input [0 0 0])]
    (* a b)))

