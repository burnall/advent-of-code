(ns adv.t01
  (:require [adv.util :refer [split-lines digit?]]))

(def input
  (->> "data/t01.txt"
       (slurp)
       (split-lines)))

(defn get-two-digits [s]
  (->> s
       (re-seq #"\d")
       ((juxt first last))
       (apply str)
       (parse-long)))

(defn f1 []
  (->> input
       (map get-two-digits)
       (apply +)))

; Part 2
(def digits {"one" 1, "two" 2, "three" 3, "four" 4, "five" 5, "six" 6, "seven" 7, "eight" 8, "nine" 9})
;(def sorted-digits (into (sorted-map-by (fn [a b] (compare (digits a) (digits b)))) digits))

(defn get-two-digits' [s]
  (->> (clojure.string/replace s (re-pattern (clojure.string/join "|" (keys digits))) #(str (digits %1)))
       (re-seq #"\d")
       ((juxt first last))
       (apply str)
       (parse-long)))

(defn check-pos [s {:keys [fst lst]} [i ch]]
   (let [curr (if (digit? ch) 
                 ch
                 (some (fn [[word v]] 
                         (when (.startsWith (subs s i) word) (str v)))
                       digits))]
     (if curr
       {:fst (or fst curr) :lst curr}
       {:fst fst, :lst lst})))   

(defn get-two-digits-another-way [s] 
  (let [{:keys [fst lst]} 
                 (reduce (partial check-pos s)
                         {}
                         (map-indexed vector s))]
    (parse-long (str fst lst))))

(defn f2 []
  (->> input
       (map get-two-digits-another-way)
       (apply +)))







