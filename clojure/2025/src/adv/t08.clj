(ns adv.t08
  (:require [adv.util :refer [split-lines split]]))

(defn parse-numbers [s]
  (as-> s $
        (split $ #",")
        (mapv parse-long $)      
   ))

(def input
  (->> "resources/t08.txt"
       (slurp)
       (split-lines)
       (mapv parse-numbers)
       ))

(defn dist [a b]
  (->> b
       (map - a)
       (map #(* % %))
       (apply +)
       (Math/sqrt)
       ))

(defn all-distances [ps]
  (->> (for [i (range (count ps))
             j (range i)]
         {(dist (ps i) (ps j))  
          [[j i]]})
       (merge-with concat)
       (into (sorted-map))
))

(defn all-points-by-distance [ps]
  (->> ps 
       (all-distances)
       (vals)
       (mapcat identity)
       ))

(defn circuits [ps points-by-distance]
  (let [initial (->> (count ps)
                     (range)
                     (map (fn [i] [i #{i}]))
                     (into {}))
        connect (fn [circuits dist]
                  (let [[p1 p2] dist
                        circ1 (circuits p1)
                        circ2 (circuits p2)]
                      (if (= circ1 circ2)
                        circuits
                        (let [united (clojure.set/union circ1 circ2)]
                          (reduce #(assoc % %2 united)
                                  circuits
                                  united)
                        ))
                    )
                  )
        ]
       (reductions connect initial points-by-distance)
    )
  )

(defn task1 [ps]
  (->> ps
       (all-points-by-distance)
       (circuits ps)
       (drop 10)
       (first)
       (map second)
       (distinct)
       (map count)
       (sort-by -)
       (take 3)
       (apply *)
       ))

(defn task2 [ps]
  (let [points-by-distance (all-points-by-distance ps)
        circs (circuits ps points-by-distance) 
        len (count ps)
        iter (fn [idx circuit] 
               (if (= len (count (circuit 0)))
                 (reduced (dec idx))
                 (inc idx)
                 )
               )
        last-edge  
          (->> circs
           (reduce iter 0)
           (nth points-by-distance)  
           )]
      (->> last-edge
           (map ps)
           (map first)
           (apply *)
      )
    )
  )

(comment
  input
  (dist [0 1] [3 5])
  (take 10 (all-distances input))
  (all-points-by-distance input)
  (circuits input (all-points-by-distance input))
  (task1 input)
  (task2 input)
  )