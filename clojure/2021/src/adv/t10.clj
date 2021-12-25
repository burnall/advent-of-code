(ns adv.t10
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)))

(def OPENING #{\(, \[, \{, \<})

(def CLOSING {\) \(, \] \[, \} \{, \> \<})

(def COST {
  \) 3
  \] 57
  \} 1197
  \> 25137})

(defn parse-chunk [s]
  (->> s
       (reduce (fn [tags ch]
                 (cond 
                   (OPENING ch) (cons ch tags) 
                   (= (CLOSING ch) (first tags)) (rest tags)
                   :else (reduced {:err :wrong-closing, :ch ch})))
               '())
       ((fn [res]
          (cond 
            (map? res) res
            (empty? res) :ok
            :else {:err :incomplete, :tags res})))))

(defn solve [lines]
  (->> lines
       (map parse-chunk)
       (filter (comp (partial = :wrong-closing) :err))
       (map (comp COST :ch))
       (reduce +)))

(defn f []
  (solve input))  


; Part 2

(def COST2 {
  \( 1
  \[ 2
  \{ 3
  \< 4})

(defn get-cost [tags]
  (->> tags
       (reduce (fn [total tag]
                 (+ (* total 5) (COST2 tag)))   
               0)))

(defn solve2 [lines]
  (->> lines
       (map parse-chunk)
       (filter (comp (partial = :incomplete) :err))
       (map (comp get-cost :tags))
       (sort)
       (vec)
       ((fn [v] (v (quot (count v) 2))))))

(defn f2 []
  (solve2 input)) 