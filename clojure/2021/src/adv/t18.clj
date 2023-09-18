(ns adv.t18
   (:require [adv.util :refer [split-lines digit? parse-int]]))


(defn parse-expr [s]
  (map #(if (digit? %) (parse-int (str %)) %) s))

(def input 
  (->> "data/t18.txt"
       (slurp)
       (split-lines)
       (map parse-expr)))


;(defn read-number [s] )

;(defn read-pair [s])

; [[6,[5,[4,[3,2]]]],1]
;(defn find-nested4 [[ch tail] lvl]
;  (if (and (> lvl 4) (digit? ch))
;     2
;     3))

(defn find-and-increase [v shift start-index n]
  


(defn transform-nested [l snd r] 
  (let [
    count (count l)
    fst (l (- cnt 2))] ))


(defn reduce-nested
  ([e] (reduce-nested [] e 0)) 
  ([l [a r] lvl open?]
    (cond 
      (nil? a) l
      (= \[ a) (recur (conj l a) r (inc lvl) false)
      (= \] a) (recur (conj l a) r (dec lvl) false)
      (= \, a) (recur (conj l a) r lvl open?)
      open? (transform-nested l a r)
      :default (recur (conj l a) r lvl true))))


    

(defn reduce-10 [s] s)

(defn reduce-sn [e] 
  (let [e1 (reduce-nested e)]
    (if (= e e1)
      (let [e2 (reduce-10 e)]
        (if (= e e2)
          e 
          (recur e2)))
      (recur e1))))  

(defn magnitude [v]
  (if (number? v)
    v
    (+ (* 3 (magnitude (first v)))
       (* 2 (magnitude (second v))))))   

