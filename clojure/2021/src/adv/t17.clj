(ns adv.t17)

(defn fire [[vx-start vy-start] [xmin xmax] [ymin ymax]]
  (let [can-make-it? 
          (fn [[x y]]
            (and (< x xmax) (>= y ymin)))
        in? 
          (fn [[x y]]
            (and (<= xmin x) (<= y ymax)))
        iter 
          (fn [{[x y] :pos, [vx vy] :v} ps]
            (let [x' (+ x vx)
                  y' (+ y vy)]
               (cond 
                 (not (can-make-it? [x' y'])) false
                 (in? [x' y']) (conj ps [x' y'])
                 :else (recur {:pos [x' y'], :v [(dec vx) (dec vy)]} 
                              (conj ps [x' y'])))))]     
    (iter {:pos [0 0], :v [vx-start vy-start]} [])))

(defn get-vx-range [xmin xmax]
  [(int (Math/ceil (/ (+ -1 (Math/sqrt (+ (* 8 xmin) 1))) 2)))
   xmax]) 

(defn get-xsteps [xmin xmax max-steps vx]
  (letfn [(f [v x steps step]
           (if (> step max-steps)
             steps
             (let [x' (+ x v)]
               (recur (if (zero? v) v (dec v)) 
                      x'
                      (if (<= xmin x' xmax)
                        (conj steps step)
                        steps)
                      (inc step)))))]
    (f vx 0 [] 1)))  

(defn get-all-xsteps [xmin xmax max-steps]
  (let [[vmin vmax] (get-vx-range xmin xmax)]
    (->> (range vmin (inc vmax))
         (mapcat (partial get-xsteps xmin xmax max-steps))
         (into #{})
         (sort-by -)))) 

(defn get-vy [ymin ymax xsteps]
  (let [f (fn [y]
            (+ (/ y xsteps)
               (/ (dec xsteps) 2)))
        v1 (f ymin)
        v2 (f ymax)
        vmin (Math/ceil (min v1 v2))
        vmax (int (Math/floor (max v1 v2)))]
    (when (>= vmax vmin) vmax)))

(defn solve [[xmin xmax] [ymin ymax]]
  (->> (get-all-xsteps xmin xmax (* 2 (- ymin)))
       (map (fn [xsteps]
              (when-let [vy (get-vy ymin ymax xsteps)]
                [xsteps vy])))
       (drop-while nil?)
       (first)))

(defn f []
  (solve [124 174] [-123 -86]))
