(ns adv.t09
  (:require [adv.util :refer [split-lines split parse-int]]))

(defn parse-line [s]
  (->> s
       (mapv (comp parse-int str))))  

(def input
  (->> "data/t09.txt"
       (slurp)
       (split-lines)
       (mapv parse-line)))

(defn low-points [hmap]
  (let [ymax (count hmap)
        xmax (count (hmap 0))
        nb (fn [x y]
             [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]])

        higher? (fn [x y h]
                 (or (< x 0) (= x xmax) (< y 0) (= y ymax)
                      (> (get-in hmap [y x]) h)))]
    (for [x (range xmax)
          y (range ymax)
          :let [h (get-in hmap [y x])]
          :when (every? (fn [[xa ya]] (higher? xa ya h))
                        (nb x y))]
      h)))

(defn solve [hmap]
  (let [ps (vec (low-points hmap))]
    (->> ps
         (reduce +)
         (+ (count ps)))))

(defn f [] 
  (solve input))


; Part 2

(defn get-free-points [hmap]
  (->> (for [y (range (count hmap))
             x (range (count (hmap 0)))
             :when (not= 9 (get-in hmap [y x]))]
          [x y])
       (set)))

(defn explore-basin [hmap {:keys [points frontiers free-points]}]
  (let [ymax (count hmap)
        xmax (count (hmap 0))
        nb (fn [x y]
             (filter (fn [[x y]] (and (>= x 0) (< x xmax) (>= y 0) (< y ymax)))
                     [[x (dec y)] [x (inc y)] [(dec x) y] [(inc x) y]]))
        ;aa (prn points frontiers)
        {new-points :points, new-frontiers :frontiers, new-free-points :free-points} 
           (reduce (fn [{:keys [points frontiers]} [x0 y0]]
                    (let [ps (set (filter free-points (nb x0 y0)))]
                      {:points (clojure.set/union points ps)
                       :frontiers (clojure.set/union frontiers ps)
                       :free-points (clojure.set/difference free-points ps)})) 
                  {:points points, :frontiers #{}, :free-points free-points}
                  frontiers)] 
     (if (empty? new-frontiers)
       {:points new-points, :free-points new-free-points}
       (recur hmap  
              {:points new-points, :frontiers new-frontiers, :free-points new-free-points}))))

(defn explore [hmap free-points basins]
  (if (empty? free-points)
    basins
    (let [p (first free-points)
          {basin :points, new-free-points :free-points} 
            (explore-basin hmap {:points #{p}
                                 :frontiers [p]
                                 :free-points (disj free-points p)})]
      (recur hmap new-free-points (conj basins basin)))))          

(defn solve2 [hmap]
  (->> (explore hmap (get-free-points hmap) [])
       (map count)
       (sort-by -)
       (take 3)
       (reduce *) 
       ))

(defn f2 [] 
  (solve2 input))
