(ns adv.t09
  (:require [adv.util :refer [split-lines split]]))

(defn read-node [s]
  (let [[from l r] (re-seq #"\w+" s)]
    [from [l r]]))

(defn read-numbers [line]
  (->> (split line #" ")
       (mapv parse-long)))

(def input
  (->> "data/t09.txt"
       (slurp)
       (split-lines)
       (mapv read-numbers)))

(defn gen-next [v]
  (->> (count v)
       (range 1)
       (map #(- (v %) (v (dec %))))
       (vec)))

(defn guess [vs]
  (->> (range (dec (count vs)) -1 -1)
       (reduce (fn [agg i] (+ (peek (vs i)) agg))
               0)))

(defn extrapolate [guess-fn vs]
  (if (every? zero? (peek vs))
    (guess-fn vs)
    (recur guess-fn (conj vs (gen (peek vs))))))

(defn solve [guess-fn histories] 
  (->> histories
       (map #(extrapolate guess-fn [%]))
       (apply +)))

(defn guess' [vs]
  (->> (range (dec (count vs)) -1 -1)
       (reduce (fn [agg i] (- (first (vs i)) agg))
               0)))
