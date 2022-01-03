(ns adv.t14
  (:require [adv.util :refer [split-lines split]]))

(defn parse-instruction [lines]
  (let [pol (vec (first lines))
        read-rule (fn [line] 
                    (split line #" -> "))
        rules (->> lines
                   (drop 2)
                   (map read-rule)
                   (into {}))]        
    {:pol pol, :rules rules}))      


(def input
  (->> "data/t14.txt"
       (slurp)
       (split-lines)
       (parse-instruction)))

(defn transform [rules pol]
  (->> (count pol)
       (range 1)
       (reduce (fn [new-pol i]
                 (let [ins (rules (str (pol (dec i)) (pol i)))]
                   (conj
                     (if ins 
                       (apply conj new-pol ins)
                       new-pol)
                      (pol i))))
               [(first pol)]))) 

(defn solve [{:keys [pol rules]} steps]
  (let [fr (->> pol
                (iterate (partial transform rules))
                (drop steps)
                (first)
                (frequencies)
                (sort-by second)
                (map second))]
    (- (last fr) (first fr))))

(defn f []
  (solve input 17))


; Part 2

(defn extend-rules [rules]
  (let [pairs (map-indexed (fn [idx key]
                             [key [key idx]])  
                           (keys rules))
        idxs (vec (map first pairs))
        key-to-idx (into {} (map second pairs))
        rules' (->> rules
                    (map (fn [[from to]]
                           [(key-to-idx from) [(key-to-idx (str (first from) to))
                                               (key-to-idx (str to (second from)))]]))
                    (into {}))]
    {:idxs idxs
     :key-to-idx key-to-idx
     :rules rules'})) 

(defn get-initial [pol key-to-idx]
  (->> (count pol)
       (dec)
       (range)
       (map (fn [i] (apply str (subvec pol i (+ i 2)))))
       (map key-to-idx)
       (frequencies)))

(defn transform-pairs [rules freqs]
  (letfn [(upd [m key v] 
            (update m key #(+ (or % 0) v)))]
    (reduce (fn [frs [rule cnt]]
              (let [[a b] (rules rule)]
                (upd (upd frs a cnt)
                     b
                     cnt)))
          {}
          freqs)))

(defn solve2 [{:keys [pol rules]} steps] 
  (let [{idxs :idxs, key-to-idx :key-to-idx rules' :rules} (extend-rules rules)
        init-freqs (get-initial pol key-to-idx)
        freqs (->> init-freqs
                   (iterate (partial transform-pairs rules'))
                   (drop steps)
                   (first))
        freqs' (->> freqs 
                    (map (fn [[key v]]
                           [(second (idxs key)) v]))
                    (reduce (fn [frs [ch v]]
                              (update frs ch #(+ (or % 0) v)))
                            {(first pol) 1})
                    (sort-by second)
                    (map second))]
         (- (last freqs') (first freqs'))))

(defn f2 []
  (solve2 input 40))



