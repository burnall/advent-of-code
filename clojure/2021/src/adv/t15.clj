(ns adv.t15
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t15.txt"
       (slurp)
       (split-lines)
       (mapv (partial mapv (comp parse-int str)))))

(defn get-infinities [maxy]
  (mapv (fn [_] (vec (repeat maxy Integer/MAX_VALUE)))
        (range maxy)))

(def grads [[-1 0] [0 -1] [0 1] [1 0]])

(defn solve [cmap] 
  (let [maxy (count cmap)
        initial-sol (assoc-in (get-infinities maxy) [0 0] 0)
        nb (fn [p blacklist] 
             (->> grads
                  (map (partial map + p))
                  (filter (fn [[x y]] 
                            (and (> maxy x -1) (> maxy y -1)
                                 (not (blacklist [x y])))))))
        f (fn [m x y] (get-in m [y x]))
        iter (fn [sol active-ps closed-ps] 
          (if (empty? active-ps)
            sol
            (let [[px py] (apply min-key (fn [[x y]] (f sol x y))
                                         active-ps)
                  links (nb [px py] closed-ps)
                  new-sol (reduce (fn [s [x y]]
                                    (let [v (+ (f s px py) (f cmap x y))] 
                                      (if (< v (f s x y)) 
                                        (assoc-in s [y x] v)
                                        s)))
                                  sol
                                  links)]
              (recur new-sol 
                     (disj (clojure.set/union active-ps (set links)) [px py])
                     (conj closed-ps [px py])))))
          ]
    (iter initial-sol #{[0 0]} #{})))    

(defn f []
  (solve input))

; Part 2
