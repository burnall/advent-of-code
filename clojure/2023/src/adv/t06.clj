(ns adv.t06)

(def times [42 68 69 85])
(def distances [284 1005 1122 1341])

(defn number-of-ways [t d] 0
  (let [sqrt (Math/sqrt (- (* t t) (* 4 d)))
        m1 (/ (- t sqrt) 2)
        m2 (/ (+ t sqrt) 2)]
    (int (- (Math/floor m2) (Math/ceil m1) -1))))    

(defn part1 [ts ds]
  (->> (count ts)
       (range)
       (map #(number-of-ways (ts %) (ds %)))
       (apply *)))

(defn arr-to-number [xs]
  (parse-long (apply str xs)))

(defn part2 [ts ds]
  (part1 [(arr-to-number ts)] [(arr-to-number ds)]))
