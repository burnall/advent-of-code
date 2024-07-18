(ns adv.t07
  (:require [adv.util :refer [split-lines split zip]]))


(def card-ranks
  {\A 10, \K 20, \Q 30, \J 40, \T 50, \9 60, \8 70, \7 80,
   \6 90, \5 100, \4 110, \3 120, \2 130})

(def combo-ranks
  {:five 10, :four 20, :full-house 30, :three 40, :two-pairs 50, :one-pair 60, :high-card 70}) 

(defn get-combo [raw]
  (let [freq (vec (sort-by identity > (map second (frequencies raw))))
        fst (first freq)]
    (cond
       (= fst 5) :five
       (= fst 4) :four
       (and (= fst 3) (= (freq 1) 2)) :full-house
       (= fst 3) :three
       (and (= fst 2) (= (freq 1) 2)) :two-pairs
       (= fst 2) :one-pair
       :else :high-card)))

(defn parse-hand [raw]
  {:card-ranks (vec (map card-ranks raw))
   :combo (get-combo raw)
   :raw raw})

(defn parse-line [parser s] 
  (let [[hand bid] (split s #" ")]
    {:hand (parser hand)
     :bid (parse-long bid)}))

(defn get-input [parser]
  (->> "data/t07.txt"
       (slurp)
       (split-lines)
       (map parser)))

(def input (get-input (partial parse-line parse-hand)))

(defn solve [lines]
  (let [sorted (reverse (sort-by (fn [line] [(combo-ranks (get-in line [:hand :combo])) 
                                             (get-in line [:hand :card-ranks])]) lines))]
    (->> (zip sorted (range))
         (map (fn [[line rank]] (* (:bid line) (inc rank))))
         (apply +))))

; Part 2
(def card-ranks' (assoc card-ranks \J 140))

(defn get-combo' [raw]
  (let [freq (frequencies raw)
        jockers (get freq \J 0)
        freq' (->> (dissoc freq \J)
                   (map second)
                   (sort-by identity >)
                   (vec)
                   (conj )
                   (#(update % 0 (fn [v] (+ (or v 0) jockers)))))
        fst (first freq')]
    (cond
       (= fst 5) :five
       (= fst 4) :four
       (and (= fst 3) (= (freq' 1) 2)) :full-house
       (= fst 3) :three
       (and (= fst 2) (= (freq' 1) 2)) :two-pairs
       (= fst 2) :one-pair
       :else :high-card)))

(defn parse-hand' [raw]
  {:card-ranks (vec (map card-ranks' raw))
   :combo (get-combo' raw)
   :raw raw})

(def input' (get-input (partial parse-line parse-hand')))