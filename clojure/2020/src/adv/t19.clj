(ns adv.t19
  (:require [adv.util :refer [split-lines parse-int zip]]
            [clojure.string :refer [split]]))

(defn parse-ints [s]
  (->> (split s #" ")
       (map parse-int)))
        
(defn parse-rule [s]
  (let [[_ n ending] (re-find #"(\d+): (.+)" s)]
    {:n (parse-int n)
     :rule (if (.startsWith ending "\"") 
              (re-find #"\w+" ending)
              (->> (split ending #" \| ")
                   (map parse-ints)))}))

(defn parse-input [s]
  (let [[rules-str msgs-str] (split s #"\n\n")]
    {:rules (->> rules-str
                 (split-lines)
                 (map parse-rule)
                 (map (juxt :n :rule))
                 (into {}))
    
     :msgs (split-lines msgs-str)}))  

(def input
  (->> "data/t19.txt"
       (slurp)
       (parse-input)))

(defn get-terms [rules]
  (->> rules
       (filter (fn [[_ v]] (string? v)))
       (into {})))

(defn can-be-trimmed [non-trimmed rule] 
  (prn 123 non-trimmed rule)
  (->> rule
       (flatten)
       (every? (comp not non-trimmed)))) 

(defn comb-two [xs ys]
  (for [x xs, y ys]
     (str x y)))

(defn combs [line]
  (prn "combs" line 33)
  (reduce comb-two
          [""]
          line))

(defn trim-line [trimmed line]
  (prn 333 trimmed line)
  (->> line
       (map trimmed)
       (combs)))

(defn trim-rule [trimmed rule]
  ;(prn 333 trimmed rule)
  (->> rule 
       (map (partial trim-line trimmed))
       (apply concat)))

(defn trim-rules [rules trimmed non-trimmed]
  (->> non-trimmed
       (filter (partial can-be-trimmed non-trimmed)) 
       (map rules)
       (map (partial trim-rule trimmed))))

(defn expand [rules trimmed non-trimmed i]
  (if (zero? i) 
    trimmed
    (let [new-trimmed (trim-rules rules trimmed non-trimmed)]
      ;(prn 444 new-trimmed) 
      (if (empty? new-trimmed)
        trimmed
        (recur rules 
               (merge trimmed new-trimmed)
               (clojure.set/union non-trimmed (keys new-trimmed))
               (dec i))))))

(defn solve []
  (let [rules (:rules adv.t19/input)
        terms (get-terms rules)]
    (expand rules 
            (into {} (map (fn [[k v]] [k [v]]) terms))
            (clojure.set/difference (set (keys rules)) (set (keys terms)))
            1)))
            
