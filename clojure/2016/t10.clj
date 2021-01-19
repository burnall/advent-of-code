(ns adv.t10
  (:require [adv.util :refer [parse-int split-lines split]]))

(defn parse-cmd [line]
  (if-let [[[_ value bot]] (re-seq #"value (\d+) goes to bot (\d+)" line)]
    {:cmd :init, :value (parse-int value), :bot (parse-int bot)}
    (let [[[_ from t1 v1 t2 v2]] (re-seq #"bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)" line)]
      {:cmd :trans, :from (parse-int from), :low-type (keyword t1) :low-id (parse-int v1), 
       :high-type (keyword t2), :high-id (parse-int v2)}))) 

(def input
  (->> "data/t10.txt"
       (slurp)
       (split-lines)
       (map parse-cmd)))

(defn get-initial-bots [cmds] 
  (->> cmds
       (filter #(= :init (:cmd %)))
       (reduce (fn [m {:keys [value bot]}] 
                 (update m bot #(conj (or % []) value)))
               {})))

(defn get-transitions [cmds]
  (->> cmds
       (filter #(= :trans (:cmd %)))
       (map (juxt :from identity))
       (into {})))

(defn get-complete-bots [bots]
  (mapcat (fn [[id bot]] (when (= 2 (count bot)) [id])) 
             bots))

(defn play [transitions [bots active-bots]]
  (->> active-bots
       (reduce (fn [[bots visited-bots] active-bot] 
                 (let [{:keys [low-type low-id high-type high-id]} (get transitions active-bot)]
                   (->> active-bot
                        (get bots) 
                        (sort)
                        (mapcat (fn [[_type id] v] (when (= _type :bot) [[id v]]))   
                                [[low-type low-id] [high-type high-id]])
                        (reduce (fn [visited-bots [id v]]
                                  (merge visited-bots
                                         {id (conj (or (get visited-bots id) (get bots id)) v)}))
                                visited-bots)
                        ((fn [visited-bots] 
                           [(merge bots visited-bots) visited-bots])))))
               [bots {}])
       ((fn [[bots visited-bots]]
         [bots (get-complete-bots visited-bots)]))))

(defn build [cmds]
  (let [transitions (get-transitions cmds)
        bots (get-initial-bots cmds)
        active-bots (get-complete-bots bots)]
    (->> [bots active-bots]
         (iterate (partial play transitions))
         (some (fn [[bots active-bots]] (when (empty? active-bots) bots))))))
 

(defn solve 
  ([cmds v1 v2]
    (->> cmds
         (build)  
         (filter (fn [[id bot]] (= (sort bot) (sort [v1 v2]))))))
  ([] (solve input 61 17)))

; Part 2

(defn find-output [transitions id]
  (some (fn [[from {:keys [low-type low-id high-type high-id]}]]
           (cond
             (and (= low-type :output) (= low-id id)) [from min]
             (and (= high-type :output) (= high-id id)) [from max]))
        transitions))

(defn solve2 []
  (let [transitions (get-transitions input)
        sol (build input)
        get-value (fn [[id f]] (apply f (sort (get sol id))))]
    (->> 3
         (range)
         (map (partial find-output transitions))
         (map (partial get-value))
         (reduce * 1))))
  
