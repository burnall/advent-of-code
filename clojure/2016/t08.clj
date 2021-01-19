(ns adv.t08
  (:require [adv.util :refer [split-lines parse-int]]))

(defn parse-cmd [line]
  (->> line 
       (re-seq #"rect (\d+)x(\d+)|rotate row y=(\d+) by (\d+)|rotate column x=(\d+) by (\d+)")
       ((fn [[[_ x y rot-y dx rot-x dy]]] 
         (cond
           x {:cmd :rect, :p [(parse-int x) (parse-int y)]}
           rot-y {:cmd :rotate-row, :y (parse-int rot-y), :dx (parse-int dx)}
           :else {:cmd :rotate-col, :x (parse-int rot-x), :dy (parse-int dy)})))))

(def input 
  (->> "data/t08.txt"
       (slurp)
       (split-lines)
       (map parse-cmd)))

(defn apply-rect-cmd [screen {[rx ry] :p}]
  (->> (for [x (range rx), y (range ry)] [y x])
       (reduce (fn [screen p] (assoc-in screen p 1)) 
               screen))) 

(defn apply-rotate-row-cmd [screen {ry :y, dx :dx}]
  (let [row (screen ry)
        dx (mod dx (count row))
        idx (- (count row) dx)
        r (concat (subvec row idx)
                  (subvec row 0 idx))]
    (assoc screen ry (vec r))))
               
(defn apply-rotate-col-cmd [screen {rx :x, dy :dy}]
  (let [row-count (count screen)]
    (->> row-count
         (range)
         (reduce (fn [scr y] 
                   (assoc-in scr 
                             [y rx] 
                             (get-in screen [(mod (- y dy) row-count) rx])))
                 screen))))

(defn apply-cmd [screen {cmd-type :cmd, :as cmd}]
  (let [f ({:rect apply-rect-cmd 
            :rotate-row apply-rotate-row-cmd
            :rotate-col apply-rotate-col-cmd} cmd-type)]
    (f screen cmd)))          

(defn zero-matrix [row-count col-count]
  (vec (repeat row-count 
               (vec (repeat col-count 0)))))
   

(defn get-image-line [line]
  (->> line
       (map #(if (zero? %) \. \#))
       (apply str)))

(defn get-image [screen]
  (map get-image-line screen)) 

(defn solve 
  ([commands row-count col-count]
    (->> commands
         (reduce apply-cmd
                 (zero-matrix row-count col-count))
         (get-image)
         (map prn)))
         ;(mapcat identity)
         ;(reduce +)))
  ([] (solve input 6 50)))

