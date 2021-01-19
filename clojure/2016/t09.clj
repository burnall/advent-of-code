(ns adv.t09
  (:require [adv.util :refer [parse-int]]))

(def input 
  (->> "data/t09.txt"
       (slurp)
       (clojure.string/trim-newline)))

(defn get-markers [s]
  (let [matcher (re-matcher #"\((\d+)x(\d+)\)" s)]
    (->> {:markers []}
         (iterate (fn [{markers :markers}]
                    (if-let [[_ a b] (re-find matcher)]
                      {:markers (conj markers 
                                      {:a (parse-int a)
                                       :b (parse-int b)
                                       :start (.start matcher)
                                       :end (.end matcher)})}
                      {:end markers})))                  
         (some :end))))        

(defn prune-markers [markers]
  (->> markers
       (reduce (fn [{:keys [markers prev-end]} marker] 
                 (if (>= (:start marker) prev-end)
                   {:markers (conj markers marker), :prev-end (+ (:end marker) (:a marker))}
                   {:markers markers, :prev-end prev-end}))
               {:markers [], :prev-end -1})    
       :markers))            

(defn solve 
  ([s] 
    (->> s
         (get-markers)
         (prune-markers)
         (map (fn [{:keys [a b start end]}] (- (* a (dec b)) (- end start))))
         (reduce +)
         (+ (count s))))

  ([] (solve input)))


; Part 2
(defn process [s markers start end q acc]
  (let [marker (first markers)]
    (if (and marker (< (:start marker) end))
      (let [before (- (:start marker) start)
            [v rest-markers] (process s 
                                      (rest markers) 
                                      (:end marker) 
                                      (+ (:end marker) (:a marker))
                                      (* q (:b marker))
                                      0)]
        (recur s 
               rest-markers
               (+ (:end marker) (:a marker))
               end
               q
               (+ before acc v)))
      [(+ acc (* q (- end start))) markers]))) 
 
 (defn solve2 
   ([s] (->> (process s 
                      (get-markers s) 
                      0 
                      (count s)
                      1
                      0)
             (first)))         
   ([] (solve2 input)))

