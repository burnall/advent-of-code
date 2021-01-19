(ns adv.intcode)

(declare execute)

(defn throw-neg-exception [i]
  (throw (Exception. (str "Access by negative index " i))))

(defn storage 
  ([v] (storage v {}))
  ([v mem] (fn
    ([] mem)
    ([i]
      (cond
        (< i 0) (throw-neg-exception i)  
        (< i (count v)) (v i) 
        :else (get mem i 0)))
    ([i value]
      ;(println "writing to" i "value" value)
      (cond
        (< i 0) (throw-neg-exception i) 
        (< i (count v)) (storage (assoc v i value) mem)
        :else (storage v (assoc mem i value)))))))

(defn get-param [storage pos mode rel-base]
  (let [el (storage pos)]
    (condp = mode 
      0 (storage el)
      1 el
      2 (storage (+ rel-base el)))))

(defn get-two-params [storage pos modes rel-base]
  [(get-param storage (+ pos 1) (mod modes 10) rel-base)
   (get-param storage (+ pos 2) (mod (quot modes 10) 10) rel-base)]) 

(defn store [storage pos mode value rel-base] 
  (let [idx (condp = mode
              0 (storage pos)
              2 (+ rel-base (storage pos))
              (throw (Exception. (str "Incorrect mod for store call " mode))))]
    (storage idx value)))            
    
(defn run-binary-op [storage pos oper modes rel-base]
  (let [[a b] (get-two-params storage pos modes rel-base)]
    (store storage (+ pos 3) (quot modes 100) (oper a b) rel-base)))

(defn jump-if-next-pos [value storage pos modes rel-base] 
  (let [a (get-param storage (+ pos 1) (mod modes 10) rel-base)]
    (if (= value (not= a 0))
      (get-param storage (+ pos 2) (quot modes 10) rel-base)
      (+ pos 3))))        
        
(defn cond-store-data [pred storage pos modes rel-base]
  (let [[a b] (get-two-params storage pos modes rel-base)]
    (store storage 
           (+ pos 3) 
           (quot modes 100) 
           (if (pred a b) 1 0)
           rel-base)))

(defn execute [storage pos rel-base get-input]
  (let [op (mod (storage pos) 100)
        modes (quot (storage pos) 100)]
    ;(println "[execute] pos:" pos "op:" op "modes" modes "rel-base" rel-base)
    (condp = op
      99 {:halt true}
      1 (recur (run-binary-op storage pos + modes rel-base) 
               (+ pos 4) 
               rel-base
               get-input)       
      2 (recur (run-binary-op storage pos * modes rel-base) 
               (+ pos 4) 
               rel-base
               get-input)
      3 (recur (store storage (inc pos) modes (get-input) rel-base) 
               (+ pos 2) 
               rel-base
               get-input)
      4 {:storage storage
         :pos (+ pos 2)
         :out (get-param storage (inc pos) modes rel-base)
         :rel-base rel-base} 
      5 (recur storage 
               (jump-if-next-pos true storage pos modes rel-base)
               rel-base
               get-input)
      6 (recur storage
               (jump-if-next-pos false storage pos modes rel-base)
               rel-base
               get-input)
      7 (recur (cond-store-data < storage pos modes rel-base)
               (+ pos 4)
               rel-base
               get-input)
      8 (recur (cond-store-data = storage pos modes rel-base)
               (+ pos 4)
               rel-base
               get-input)
      9 (recur storage
               (+ pos 2)
               (+ rel-base (get-param storage (inc pos) modes rel-base))
               get-input)
      (throw (Exception. (str "unknown " op " "))))))

