(ns adv.t08
  (:require [adv.util :refer [split-lines lcm]]))

(defn read-node [s]
  (let [[from l r] (re-seq #"\w+" s)]
    [from [l r]]))

(defn read-network [lines]
  (let [[route _ & nodes] lines]
    {:route (vec route)
     :nodes (into {} (map read-node nodes))}))

(def input
  (->> "data/t08.txt"
       (slurp)
       (split-lines)
       (read-network)))


(defn solve [{:keys [route nodes]}]
  (letfn [(left? [i] 
           (= \L (route (mod i (count route)))))
          (go [cnt key]
            (if (= key "ZZZ")
               cnt
               (recur (inc cnt)
                      ((get nodes key) (if (left? cnt) 0 1)))))]
    (go 0 "AAA")))

; Too slow
(defn solve2 [{:keys [route nodes]}]
  (let [left? (fn [i] 
            (= \L (route (mod i (count route)))))
        keys-by-suffix (group-by last (keys nodes))
        next-node (fn [cnt current] ((get nodes current) (if (left? cnt) 0 1)))
        go (fn [cnt xs]
            (when (zero? (mod cnt 10000)) (prn cnt xs))
            ;(prn cnt xs)
            (if (every? #(= (last %) \Z) xs)
               cnt
               (recur (inc cnt)
                      (map (partial next-node cnt) xs))))]
    (go 0 (keys-by-suffix \A))))

    
(defn get-z-loop [{:keys [route nodes]} key]
  (let [left? (fn [i] 
            (= \L (route (mod i (count route)))))
        next-node (fn [current cnt] ((get nodes current) (if (left? cnt) 0 1)))
        trip (fn [node cnt]
                (if (= (last node) \Z)
                  cnt
                  (recur (next-node node cnt) (inc cnt))))]
    (trip key 0)))

(defn hack [{:keys [route nodes] :as data}]
  (let [beginnings (filter #(= (last %) \A) (keys nodes))]
    (->> beginnings 
         (map (partial get-z-loop data))
         (reduce lcm))))


