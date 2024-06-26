(ns adv.t03
  (:require [adv.util :refer [split-lines digit?]]))

(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(defn find-not-digit [v start] 
  (cond
    (>= start (count v)) start
    (digit? (v start)) (recur v (inc start))
    :else start))

(defn find-number [v start]
  (cond
    (>= start (count v)) nil
    (digit? (v start)) {:start start, :end (find-not-digit v (inc start))}
    :else (recur v (inc start))))

(defn find-number-idxs [v]
  (->> (find-number v 0)
       (iterate #(find-number v (:end %)))
       (take-while some?)))

(defn idx-to-number [v {start :start, end :end}]
  (->> (subvec v start end)
       (apply str )
       (parse-long)))

(defn get-neighbours [{:keys [start end row]} m]
  (let [maxx (count (m row))
        maxy (count m)]
    (concat (when (> row 0)  
              (map #(vector % (dec row)) 
                   (range (max 0 (dec start)) (min (inc end) maxx))))
            (when (> start 0)
              [[(dec start) row]])
            (when (< end maxx)
              [[end row]])
            (when (< (inc row) maxy)
              (map #(vector % (inc row)) 
                   (range (max 0 (dec start)) (min (inc end) maxx)))))))

(defn adjacent-symbol? [{:keys [start end row] :as idx} m]
  (some (fn [[x y]] 
          (not= ((m y) x) \.))
        (get-neighbours idx m)))


(defn find-numbers [m]
  (mapcat (fn [[row v]]
            (->> v
                 (find-number-idxs)
                 (filter #(adjacent-symbol? (assoc % :row row) m))
                 (map (partial idx-to-number v))))
          (map-indexed vector m)))

(defn f []
  (->> input
       (find-numbers)
       (apply +)))

; Part 2

(defn get-symbol-idxs [v]
  (->> v
       (map-indexed vector)
       (filter (fn [[idx ch]] (and (not (digit? ch)) (not= ch \.))))
       (map first)))


(def shifts [[-1 0] [-1 -1] [-1 1] 
             [0 -1] [0 1]
             [1 -1] [1 0] [1 1]])

(defn get-adjacent-idxs [m [x y]]
  (let [maxx (count (m 0))
        maxy (count m)]
    (->> shifts
         (map (partial map + [x y]))
         (filter (fn [[x' y']] 
                   (and (>= x' 0) (< x' maxx) (>= y' 0) (< y' maxy)))))))

(defn get-neighbour-numbers [m number-idxs [x y]]
  (->> [x y]
       (get-adjacent-idxs m)
       (filter (fn [[x' y']] 
                  (digit? ((m y') x'))))
       (filter (fn [[x' y]]
                 (some #(and )
                       number-idxs)

(defn get-gears [m]
  (let [number-idxs (mapcat find-number-idxs m)
  



