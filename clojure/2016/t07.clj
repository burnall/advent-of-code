(ns adv.t07
  (:require [adv.util :refer [split-lines split]]))

(def input 
  (->> "data/t07.txt"
       (slurp)
       (split-lines)))

(defn abba? [s]
  (->> s
       (re-seq #"(.)(.)\2\1")
       (filter (fn [[_ a b]] (not= a b)))
       (seq)
       (boolean)))

(defn fit? [s]
  (let [xs (split s #"\[|\]")]
    (and (some abba? (take-nth 2 xs))
         (not-any? abba? (take-nth 2 (rest xs)))))) 

(defn solve
  ([lines] 
    (->> lines
         (filter fit2?)
         (count)))
  ([] (solve input)))

; Part 2
(defn aba [s]
  (->> s
       (re-seq #"(?=(.)(.)\1)")
       (filter (fn [[_ a b]] (not= a b)))
       (map rest)))

(defn fit2? [s]
  (let [xs (split s #"\[|\]")
        aba-s (mapcat aba (take-nth 2 xs))
        bab-s (mapcat aba (take-nth 2 (rest xs)))]
    (not (empty? (clojure.set/intersection (set aba-s)   
                                           (set (map reverse bab-s)))))))

  

