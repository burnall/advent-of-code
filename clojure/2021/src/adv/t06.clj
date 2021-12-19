(ns adv.t06
  (:require [adv.util :refer [split-lines split parse-int]]))

(def input
  (-> "data/t06.txt"
      (slurp)
      (split #",")
      (#(map parse-int %))
      (frequencies)))

(defn move [state]
  (->> state
       (reduce (fn [st [day freq]]
                 (if (= day 0)
                   (assoc (update st 
                                  6 
                                  (fn [v] (+ (or v 0) freq))) 
                          8 
                          freq)
                   (update st 
                           (dec day) 
                           (fn [v] (+ (or v 0) freq)))))
               {}))) 

(defn solve [state days]
  (->> state
       (iterate move)
       (#(nth % days))
       (map second)
       (reduce +)))

(defn f []
  (solve input 80))

(defn f2 []
  (solve input 256))

