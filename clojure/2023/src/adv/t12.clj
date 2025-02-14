(ns adv.t12
  (:require [adv.util :refer [split-lines split]]))

(defn read-record [s]
  (let [[condition descr] (split s #" ")]
    {:condition condition
     :descr (->> (split descr #",")
                 (map parse-long))})) 

(def input
  (->> "data/t12.txt"
       (slurp)
       (split-lines)
       (map read-record)))

(defn consume-damaged [[ch & rst] cnt] 
  (if (zero? cnt)
    (if (= ch \#) 
      nil
      (or rst '()))
    (if (or (= ch \.) (= ch nil))
      nil
      (recur rst (dec cnt)))))
  
; .??..??...?##. 1,1,3
(defn count-arrs [condition descr]
  (if (empty? descr)
    (if (some (partial = \#) condition)
      0
      1)

    (case (first condition) 
      \. (recur (rest condition) descr)
      \# (if-let [condition' (consume-damaged condition (first descr))]
           (recur condition' (rest descr))
           0)
      \? (if-let [condition' (consume-damaged condition (first descr))]
           (+ (count-arrs condition' (rest descr))
              (count-arrs (rest condition) descr))
           (recur (rest condition) descr))  
      0)))
    
(defn solve [records]
  (->> records
       (map #(count-arrs (:condition %) (:descr %)))
       (apply + )))

(defn convert-input [records]
  (map (fn [{:keys [condition descr]}]
         {:condition (clojure.string/join "?" (repeat 5 condition))
          :descr (mapcat identity (repeat 5 descr))})
       records))

; Dynamic programming
; Dynamic programming. f (pos, groups, len) = number of ways to:
;(defn move [ch descr {:keys [pos groups len ways]}]
;  (case ch
;     \. 
;     \#
;     \?
