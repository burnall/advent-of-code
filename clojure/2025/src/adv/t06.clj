(ns adv.t06
  (:require [adv.util :refer [split-lines split]]))

(defn form [xs]
  (let [[ns ops] (split-at (dec (count xs)) xs)]
    {:ns (map (partial mapv parse-long) ns)
     :ops (first ops)}))

(def input
  (->> "resources/t06.txt"
       (slurp)
       (split-lines)
       (map clojure.string/trim)
       (mapv #(split % #"\s+"))
       (form)))

(comment
  (first (:ns input)))