(ns adv.t15
  (:require [adv.util :refer [split split-lines parse-int]]
            [clojure.string :refer [trim-newline]]
            [clojure.set :refer [difference]]
            ))

(def input 
  (->> "data/t15.txt"
       (slurp)
       (trim-newline)
       (#(split % #","))
       (map parse-int)
       )) 

