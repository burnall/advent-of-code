(ns adv.t06
  (:require [adv.util :refer [split-lines split]]))

(defn form [xs]
  (let [[ns ops] (split-at (dec (count xs)) xs)]
    {:ns (map (partial mapv parse-long) ns)
     :ops (map {"+" +, "*" *} (first ops))}))

(def input
  (->> "resources/t06.txt"
       (slurp)
       (split-lines)
       (map clojure.string/trim)
       (mapv #(split % #"\s+"))
       (form)))

(defn part1 [{:keys [ns ops]}]
  (->> ns
       (apply mapv vector)
       (map #(apply % %2) ops)
       (apply +)))

(def input2
  (->> "resources/t06.txt"
       (slurp)
       (split-lines)))


(defn split-by0 [xs elem]
  (->> xs
      (reduce (fn [[splits current] x] 
                (if (= x elem)
                  [(conj splits current) []]
                  [splits (conj current x)]))
          [[] []])
      (apply conj)
      (remove empty?)))

(defn split-by [elem xs] 
  (->> (partition-by #(= elem %) xs) 
       (remove #(= elem (first %)))))

(defn part2 [matrix]
  (let [[ns last-line] (split-at (dec (count matrix)) matrix)
        ops (->> last-line
                 (first)
                 (filter (partial not= \space))
                 (map {\+ +, \* *}))]
    (->> ns
       (apply mapv str)
       (map clojure.string/trim)
       (split-by "")
       (map (partial map parse-long))  
       (map apply ops)  
       (apply +)  
         )))

(comment
  input
  (part1 input)
  input2
  (part2 input2)
  )