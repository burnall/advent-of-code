(ns adv.t02
  (:require [adv.util :refer [split-lines]]))

(def input 
  (->> "data/t02.txt"
       (slurp)
       (split-lines)))

(def field 
 [[1 2 3]
  [4 5 6]
  [7 8 9]])

(def gradients 
  {\L [0 -1]
   \U [-1 0]
   \R [0 1]
   \D [1 0]})

(defn add-lim [p delta]
  (if (or (and (= delta -1) (= p 0))
          (and (= delta 1) (= p 2)))
      p
      (+ p delta)))

(defn move [pos cmd]
  (map add-lim pos (gradients cmd)))

(defn apply-cmds [pos cmds]
  (reduce move pos cmds))
         
(defn solve
  ([cmds-list] 
    (->> cmds-list 
         (reductions apply-cmds [1 1])
         (drop 1)
         (map (partial get-in field))))
  ([] (solve input)))

; Part 2
(def transitions {
  1 {\D 3}
  2 {\R 3, \D 6}
  3 {\U 1, \L 2, \R 4, \D 7}
  4 {\L 3, \D 8}
  5 {\R 6}
  6 {\U 2, \L 5, \R 7, \D 10}
  7 {\L 6, \U 3, \R 8, \D 11}
  8 {\L 7, \U 4, \R 9, \D 12}
  9 {\L 8}
  10 {\U 6, \R 11}
  11 {\L 10, \U 7, \R 12, \D 13}
  12 {\L 11, \U 8}
  13 {\U 11}})

(defn make-moves [pos cmds]
  (reduce (fn [pos cmd] (get-in transitions [pos cmd] pos))
          pos 
          cmds))

(defn solve2 
  ([cmds-list] 
    (drop 1 
          (reductions make-moves 5 cmds-list)))
  ([] (solve2 input)))


