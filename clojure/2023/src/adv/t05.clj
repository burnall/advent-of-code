(ns adv.t05
  (:require [adv.util :refer [split-lines]]))

(defn parse-numbers [s]
  (->> s
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-groups [lines]
  (->> lines
       (reduce #(if (= %2 "") 
                  (conj % [])
                  (assoc % 
                         (dec (count %)) 
                         (conj (peek %) %2)))
               [[]]))) 

(defn parse-input [lines]
  (let [[seeds & maps] (parse-groups lines)]
    {:seeds (parse-numbers (seeds 0))
     :maps (->> maps 
                (map (partial drop 1))
                (map (partial map parse-numbers)))})) 

(def input
  (->> "data/t05.txt"
       (slurp)
       (split-lines)
       (parse-input)))

(defn domap [n m]
  (->> m
       (some (fn [[dst src len]]
                 (when (<= src n  (+ src len -1)) (+ dst (- n src)))))
       (#(or % n))))

(defn domaps [ms n]
  (reduce domap n ms))

(defn part1 [{:keys [seeds maps]}]
  (->> seeds
       (map (partial domaps maps))
       (apply min)))


; Part 2

; Does not work - ineffective for big ranges 
(defn part2 [{:keys [seeds maps]}]
  (->> seeds
       (partition 2)
       (mapcat (fn [[start len]] (range start (+ start len))))
       (map (partial domaps maps))
       (apply min)))

(def BIG 92233720368547758) ; But smaller than max long

(defn pad-mappings [m]
  (let [sorted (vec (sort-by second m))
        minv (second (first sorted))
        [_ max-start max-len] (peek sorted)
        maxv (+ max-start max-len)] 
    (->> sorted
         (#(if (<= minv 0) % (cons (list 0 0 minv) %)))
         (#(if (<= BIG maxv) % (concat % [(list maxv maxv (- BIG maxv))]))))))

(def input2
  {:seeds (->> (:seeds input)
               (partition 2)
               (map (fn [[start len]] [start (+ start len)])))
   :maps (map pad-mappings (:maps input))})

(defn intersect [leftA rightA leftB rightB]
  (cond (<= leftB leftA rightB) [leftA (min rightA rightB)]
        (<= leftA leftB rightA) [leftB (min rightA rightB)]))

(defn domaps2 [spans ms]
  (for [[dst src len] ms 
        [leftA rightA] spans
        :let [common (intersect leftA rightA src (+ src len))]
        :when common]
    (map (partial + dst (- src)) common)))

(defn part2' [{:keys [seeds maps]}]
  (->> maps
       (reduce domaps2 seeds)
       (map first)
       (apply min)))

