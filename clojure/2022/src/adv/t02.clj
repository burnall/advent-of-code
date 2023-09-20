(ns adv.t02
  (:require [adv.util :refer [split-lines split]]))

(defn parse-match [s]
  (split s #" "))

(def input
  (->> "data/t02.txt"
       (slurp)
       (split-lines)
       (map parse-match)))

(def move1-map {:rock "A", :paper "B", :scissors "C"})
(def move2-map {:rock "X" , :paper "Y", :scissors "Z"})

(def moves-to-outcome {[:rock :rock] :draw, [:rock :paper] :loss, [:rock :scissors] :win
              [:paper :paper] :draw, [:paper :rock] :win, [:paper :scissors] :loss
              [:scissors :scissors] :draw, [:scissors :paper] :win, [:scissors :rock] :loss})
(def outcome-to-score {:win 0, :draw 3, :loss 6})
(def shape-to-score {:rock 1, :paper 2, :scissors 3})

(defn cost [outcome move]
  (+ (outcome-to-score outcome) (shape-to-score move)))

(def encoded-outcome 
  (->> moves-to-outcome
       (map (fn [[[move1 move2] outcome]]
               [[(move1-map move1) (move2-map move2)] (cost outcome move2)]))
       (into {})))          
  
(defn f1 [moves]
  (->> moves
       (map encoded-outcome)
       (apply +)))

(println (f1 input))

(def outcome-map {:loss "Z", :draw "Y", :win "X"})

(def encoded-outcome2
  (->> moves-to-outcome
      (map (fn [[[move1 move2] outcome]]
              [[(move1-map move1) (outcome-map outcome)] (cost outcome move2)]))
      (into {})))

(defn f2 [moves]
  (->> moves 
      (map encoded-outcome2)
      (apply +)))

(println (f2 input))