(ns adv.t05
  (:require [adv.util :refer [split-lines split]]))

(defn parse-range [s] 
  (as-> s $
       (split $ #"-")
       (map parse-long $)))

(defn parse-input [lines]
  (let [[part1 part2] (split-with (partial not= "") lines)]
    {:ranges (mapv parse-range part1)
     :ns (map parse-long (rest part2))}))

(def input
  (->> "resources/t05.txt"
       (slurp)
       (split-lines)
       (parse-input)))

(defn within? [ranges n]
  (some (fn [[a b]] (<= a n b)) ranges))

(defn part1 [{:keys [ranges ns]}]
  (->> ns
       (filter (partial within? ranges))
       (count)))

(defn intersect [ranges [a b]]
  (filter (fn [[a' b']] (not (or (> a b') (> a' b))))
          ranges))

(defn join [ranges]
  [(apply min (map first ranges))
   (apply max (map second ranges))])

(def join2 
  (juxt #(apply min (map first %)) 
        #(apply max (map second %))))

(defn add-range [ranges r]
  (let [intersections (intersect ranges r)
        joined (join (conj intersections r))]
    (conj (clojure.set/difference ranges intersections) joined)))

(defn trim [[r & other]]
  (reduce add-range #{r} other))

(defn part2 [ranges]
  (->> ranges
       (trim)
       (map (fn [[a b]] (- b a -1)))
       (reduce +)
       ))

(comment
  input
  (part1 input)
  (part2 (:ranges input)))