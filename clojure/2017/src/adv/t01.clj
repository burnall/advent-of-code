(ns adv.t01
  (:require [adv.util :refer [split-lines parse-int]]))

(def input
  (->> "data/t01.txt"
       (slurp)))