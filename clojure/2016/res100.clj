(ns adv.res100)

(defn add [number expr]
  (let [lst (last expr)
        new-expr [(conj expr \+ number) 
                  (conj expr \- number)]]
    (if (number? lst)
      (conj new-expr (assoc expr 
                            (dec (count expr)) 
                            (+ number (* 10 lst))))
      new-expr)))                      
          
(defn calc [expr]
  (first (reduce (fn [[acc sign] el]
                   (if (number? el)
                     [(+ acc (* sign el)) +1]
                     [acc (if (= \+ el) 1 -1)]))
                 [0 +1] 
                 expr)))

(defn add-multiple [exprs number]
  (mapcat (partial add number) exprs)) 

(defn run [] 
  (->> (range 2 10)
       (reduce add-multiple [[1]]) 
       (filter #(= 100 (calc %)))
       (map (partial apply str))))

