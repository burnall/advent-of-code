(ns adv.t02
  (:require [adv.util :refer [split-lines split]]))

(defn read-set [s]
  (->> (split s #", ")
       (map (partial re-seq #"(\d+) (.+)"))
       (map (fn [[[_ cnt color]]] [(keyword color) (parse-long cnt)]))
       (into {})))

(defn read-game [s]
  (let [[[_ num sets]] (re-seq #"Game (\d+): (.*)" s)]
    {:game (parse-long num)
     :sets (map read-set (split sets #";"))}))

(def input
  (->> "data/t02.txt"
       (slurp)
       (split-lines)
       (map read-game)))

(defn possible? [bag game]
  (let [sets (:sets game)]
    (every? #(every? (fn [[color cnt]] (>= cnt (or (color %) 0)))
                     bag)
            sets)))

(defn f []
  (->> input 
       (filter (partial possible? {:red 12, :green 13, :blue 14}))
       (map :game)
       (reduce +)))

; Part 2
(defn power [game]
  (->> game
       (:sets)
       (apply merge-with max)
       (map second)
       (reduce *)))

(defn f2[] 
  (->> input
       (map power)
       (reduce +)))

