(ns adv.t11
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-line [s]
  (->> s
       (mapv (comp parse-int str))))  

(def input
  (->> "data/t11.txt"
       (slurp)
       (split-lines)
       (mapv parse-line)))

(def grads
  [[-1 -1] [-1 0] [-1 1]
   [0 -1] [0 1]
   [1 -1] [1 0] [1 1]])

(defn inc10 [n]
  (mod (inc n) 10))

(defn inc-lines [lines]
  (mapv (partial mapv inc10) lines))

(defn find-zeroes [lines]
  (->> (for [y (range (count lines))
             x (range (count (lines 0)))
             :when (zero? (get-in lines [y x]))]
          [x y])
       (set)))       

(defn make-step [octopuses]
  (let [octopuses (inc-lines octopuses)
        initial-flashes (find-zeroes octopuses) 
        ymax (count octopuses)
        xmax (count (octopuses 0))
        nb (fn [p octs] 
             (->> grads
                  (map (partial map + p))
                  (filter (fn [[x y]] 
                            (and (>= x 0) (>= y 0) (< x xmax) (< y ymax) 
                                 (> (get-in octs [y x]) 0))))))
        extend 
           (fn [octs flash]
             (reduce (fn [{:keys [flashes octs]} [x y]]
                       (let [v (get-in octs [y x])] 
                         {:flashes (if (= v 9) (conj flashes [x y]) flashes)
                          :octs (assoc-in octs [y x] (inc10 v))}))  
                     {:flashes #{}, :octs octs}
                     (nb flash octs)))
        extend-all 
           (fn [octs flashes]
             (if (empty? flashes)
               octs
               (let [flash (first flashes)
                     {new-octs :octs, new-flashes :flashes} (extend octs flash)]
                  (recur new-octs (clojure.set/union (disj flashes flash) new-flashes)))))]
    (extend-all octopuses initial-flashes)))


(defn solve [octopuses n]
  (->> {:octopuses octopuses, :flash-count 0}
       (iterate (fn [{:keys [octopuses flash-count]}]
                  (let [octs (make-step octopuses)]
                    {:octopuses octs
                      :flash-count (+ flash-count (count (find-zeroes octs)))})))
       (#(nth % n))
       (:flash-count)))                

(defn f []
  (solve input 100))  


; Part 2
(defn solve2 [octopuses]
  (let [area (* (count octopuses) (count (octopuses 0)))]
    (->> {:octopuses octopuses, :last-flash-count 0, :cnt 0}
         (iterate (fn [{:keys [octopuses cnt]}]
                    (let [octs (make-step octopuses)]
                      {:octopuses octs
                       :last-flash-count (count (find-zeroes octs))
                       :cnt (inc cnt)})))
         (drop-while (comp (partial > area) :last-flash-count))
         (first)
         (:cnt))))

(defn f2 []
  (solve2 input))          