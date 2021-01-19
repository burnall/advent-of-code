(ns adv.t04
  (:require [adv.util :refer [split split-lines parse-int]]))

(defn parse-room [line]
  (->> line
       (re-seq #"(.+)-(\d+)\[(.+)\]")
       (first)
       ((fn [[_ encr-name sector-id checksum]] 
          {:encr-name encr-name 
           :sector-id (parse-int sector-id) 
           :checksum checksum }))))

(def input 
  (->> "data/t04.txt"
       (slurp)
       (split-lines)
       (map parse-room)))

(defn calc-checksum [s]
  (->> s
       (filter (partial not= \-))
       (frequencies)
       (sort-by (juxt (comp - second) first))
       (take 5)
       (map first)
       (apply str)))

(defn real? [{:keys [encr-name checksum]}]
  (= (calc-checksum encr-name) checksum))

(defn solve 
  ([rooms] 
    (->> rooms
         (filter real?)
         (map :sector-id)
         (reduce +)))
  ([] (solve input)))

; Part 2
(defn rotate [n ch]
  (if (= ch \-) 
    \s
    (char (+ (int \a) 
             (mod (+ n (int ch) (- (int \a))) 26)))))

(defn decrypt [n s]
  (->> s
       (map (partial rotate n))
       (apply str)))

(defn solve2
  ([rooms] 
    (->> rooms
         (map (fn [{:keys [encr-name sector-id] :as room}] 
                (assoc room :name (decrypt sector-id encr-name))))
         (filter (fn [{nm :name}] (clojure.string/includes? nm "north")))       
         ))
  ([] (solve2 input)))

