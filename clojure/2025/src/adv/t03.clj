(ns adv.t03
  (:require [adv.util :refer [split-lines]]))

(def input
  (-> "resources/t03.txt"
      (slurp)
      (split-lines)))

(defn find-max-seq [n s]
  (let [len (count s)
        go (fn [chars i]
             (let [ch (get s i)
                   j (- n (min n (- len i)))
                   idx (first (filter #(> (int ch) (int (chars %))) (range j n)))]
               (if (nil? idx)
                 chars
                 (vec (take n (concat (subvec chars 0 idx)
                                      [ch]
                                      (repeat \0)))))))]
    (reduce go (vec (repeat n \0)) (range len))))

(defn part [n]
  (->> input
       (map (partial find-max-seq n))
       (map (partial apply str))
       (map parse-long)
       (reduce +)))

(comment
  (part 2)
  (part 12))
