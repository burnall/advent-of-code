(ns adv.res100-ed2)

(defn add [number expr] [
   (conj expr number)
   (conj expr (- number))
   (let [top (peek expr) 
         rst (pop expr)]
     (conj rst (+ number (* 10 top))))])
          
(defn calc [expr]
  (reduce + expr))

(defn add-multiple [exprs number]
  (mapcat (partial add number) exprs)) 

(defn run [] 
  (->> (range 2 10)
       (reduce add-multiple [[1]]) 
       (filter #(= 100 (calc %)))
       (map (partial map-indexed (fn [idx n] (str (when (and (> idx 0) (> n 0)) \+) n)))) 
       (map (partial apply str))))

