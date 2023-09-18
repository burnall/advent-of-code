(ns adv.t01
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)))

(defn parse-items [xs]
  (->> (conj xs "")
    (reduce (fn [{:keys [group obj]} s]
              (if (= s "")
                 {:group [], :obj (assoc obj (count obj) group)}
                 {:group (conj group (parse-long s)), :obj obj}))
              {:group [], :obj {}})
    (:obj)))

(defn f [xs]
  (->> xs
       (parse-items)
       (map (fn [[id group]] (apply + group)))
       (apply max)))

;(println (f input))

(defn f2 [xs]
  (->> xs
       (parse-items)
       (map (fn [[id group]] (apply + group)))
       (sort-by -)
       (take 3)
       (apply +)))

;(println (f2 input))






