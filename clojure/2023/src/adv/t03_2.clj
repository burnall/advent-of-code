(ns adv.t03-2
  (:require [adv.util :refer [split-lines re-pos]]))

(def input
  (->> "data/t03.txt"
       (slurp)
       (split-lines)
       (mapv vec)))

(defn find-numbers [m]
  (for [x (range (count m)) 
        [y n] (re-pos #"\d+" (apply str (m x)))]
     {:x x, :ystart y, :val (parse-long n)}))

(defn find-symbols [m] 
  (for [x (range (count m)) 
        y (range (count (m x)))
        :let [ch (get-in m [x y])]
        :when (not (re-find #"\d|\." (str ch)))]
    {:x x, :y y, :ch ch}))

(defn adjacent? [number symbol]
  (and (<= (dec (:x number)) (:x symbol) (inc (:x number)))
       (<= (dec (:ystart number)) 
           (:y symbol) 
           (+ (:ystart number) (count (str (:val number)))))))

(defn part1 [m]
  (let [ns (find-numbers m)
        ss (find-symbols m)]
    (->> ns
         (filter #(some (partial adjacent? %) ss))
         (map :val)
         (reduce +))))

(defn part2 [m]
  (let [ns (find-numbers m)
        ss (find-symbols m)]
    (->> ss
         (mapcat (fn [symbol]
                    (let [numbers (filter #(adjacent? % symbol) ns)]
                      (when (= (count numbers) 2)
                         [(reduce * (map :val numbers))]))))
         (reduce +))))
