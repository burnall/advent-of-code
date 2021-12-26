(ns adv.t12
  (:require [adv.util :refer [split-lines split]]))

(defn parse-pair [s]
  (split s #"-"))   

(def input
  (->> "data/t12.txt"
       (slurp)
       (split-lines)
       (map parse-pair)))

(defn small? [cave]
  (= cave (clojure.string/lower-case cave)))

(defn build-cmap [pairs]
  (letfn [(upd [cmap a b]
            (update cmap a (fn [links] (conj (or links #{}) b))))] 
    (reduce (fn [cmap [a b]]
              (upd (upd cmap a b) b a))
          {}
          pairs)))

(defn walk [cmap loc path small-caves]
  (let [links (cmap loc)
        moves (clojure.set/difference links small-caves)
        end-move (if (links "end") [(conj path "end")] [])]
    (concat end-move
            (mapcat (fn [move] 
                      (walk cmap
                            move
                            (conj path move)
                            (if (small? move) (conj small-caves move) small-caves)))
                    moves))))       

(defn solve [pairs]
  (walk (build-cmap pairs) "start" ["start"] #{"start" "end"}))

(defn f [] 
  (solve input))


; Part 2

(defn walk2 [cmap loc path small-caves can-visit-twice?]
  (let [links (cmap loc)
        moves (clojure.set/difference links small-caves)
        end-move (if (links "end") [(conj path "end")] [])
        visit2 (if can-visit-twice?
                 (disj (clojure.set/intersection small-caves links) "start" "end")
                 [])]
    (concat end-move
            (mapcat (fn [move] 
                      (walk2 cmap
                             move
                             (conj path move)
                             (if (small? move) (conj small-caves move) small-caves)
                             can-visit-twice?))
                    moves)       
            (mapcat (fn [move] 
                      (walk2 cmap
                             move
                             (conj path move)
                             small-caves
                             false))
                    visit2))))

(defn solve2 [pairs]
  (walk2 (build-cmap pairs) "start" ["start"] #{"start" "end"} true))

(defn f2 [] 
  (solve2 input))  
