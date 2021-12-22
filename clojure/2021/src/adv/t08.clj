(ns adv.t08
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn to-set [s]
  (map set 
       (split s #"\s+")))

(defn parse-lines [s]
  (let [[[_ left right]] (re-seq #"(.+) \| (.+)" s)]
    {:left (to-set left)
     :right (to-set right)}))

(def input
  (->> "data/t08.txt"
       (slurp)
       (split-lines)
       (map parse-lines)))

(defn solve [lines]
  (->> lines
       (mapcat :right)
       (filter (fn [s] (#{2 3 4 7} (count s))))
       (count)))

(defn f[]
  (solve input))


; Part 2

(defn get-by-sub [xs subs]
  (some (fn [s] (when (clojure.set/superset? s subs) s))
        xs))

(defn decipher [xs] 
  (let [by-count (group-by count xs)
        one (first (by-count 2))
        four (first (by-count 4))
        seven (first (by-count 3))
        eight (first (by-count 7))
        three (get-by-sub (by-count 5) one)
        lfour (clojure.set/difference four one)
        five (get-by-sub (by-count 5) lfour)
        two (first (clojure.set/difference (set (by-count 5)) #{three five}))
        nine (get-by-sub (by-count 6) three)
        zero (get-by-sub (clojure.set/difference (set (by-count 6)) #{nine}) one)
        six (first (clojure.set/difference (set (by-count 6)) #{zero nine}))]
  {zero 0, one 1, two 2, three 3, four 4, five 5, six 6, seven 7, eight 8, nine 9})) 

(defn solve2 [lines]
  (->> lines 
       (map (fn [{:keys [left right]}]
              (->> right
                   (map (decipher left))
                   (apply str)
                   (parse-int))))
       (reduce +)))   

(defn f2[]
  (solve2 input))
