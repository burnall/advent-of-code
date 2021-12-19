(ns adv.t04
  (:require [adv.util :refer [split-lines split parse-int]]))

(def dim 5)

(defn parse-draw [s]
  (map parse-int (split s #",")))

(defn parse-board [lines]
  (mapv (fn [s] 
          (mapv parse-int (split (clojure.string/triml s) #"\s+")))
        lines))

(defn parse-bingo [lines] 
  {:draw (parse-draw (lines 0))
   :boards (map (fn [idx] (parse-board (subvec lines idx (+ idx 5))))
                (range 2 (count lines) (inc dim)))})

(def input
  (->> "data/t04.txt"
       (slurp)
       (split-lines)
       (parse-bingo)))

(defn won? [draw board] 
  (or (some #(every? draw %)
            board)
      (some #(every? draw %)
            (map (fn [col] 
                   (map (fn [row] (get-in board [row col])) (range dim)))
                 (range dim)))))

(defn count-board [board draw last]
  (->> board
       (mapcat identity)
       (filter (complement draw))
       (reduce +)
       (* last)))

(defn play [boards draw]
  (let [{:keys [winner-board used-draw last]}
          (reduce (fn [used-draw current]
                    (let [new-used-draw (conj used-draw current)
                          winner-board (some (fn [b] (when (won? new-used-draw b) b)) boards)]
                      (if winner-board
                        (reduced {:winner-board winner-board, :used-draw new-used-draw, :last current})
                        new-used-draw))) 
                  #{}
                  draw)]
    (count-board winner-board used-draw last)))
    
(defn f []
  (play (:boards input)
        (:draw input)))

; Part 2
(defn play-to-lose [boards draw]
  (let [{:keys [loser-board used-draw last]}
          (reduce (fn [{:keys [used-draw not-winners]} current]
                    (let [new-used-draw (conj used-draw current)
                          new-not-winners (filter (complement (partial won? new-used-draw)) not-winners)]
                      (if (empty? new-not-winners)
                        (reduced {:loser-board (first not-winners), :used-draw new-used-draw, :last current})
                        {:used-draw new-used-draw, :not-winners new-not-winners})))
                  {:used-draw #{}, :not-winners boards}
                  draw)]
    (count-board loser-board used-draw last)))

(defn f2 []
  (play-to-lose (:boards input)
                (:draw input)))
