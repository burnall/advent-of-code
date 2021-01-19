(ns adv.t11)

(def devices [
  {:dev :gen, :el :th, :id 0} 
  {:dev :chip, :el :th, :id 1} 
  {:dev :gen, :el :pl, :id 2} 
  {:dev :gen, :el :sr, :id 3}
  {:dev :chip, :el :pl, :id 4}
  {:dev :chip, :el :sr, :id 5}
  {:dev :gen, :el :pt, :id 6}
  {:dev :chip, :el :pt, :id 7}
  {:dev :gen, :el :rt, :id 8}
  {:dev :chip, :el :rt, :id 9}])

(def starting-node {
  :floor 0
  :plan {0 0, 1 0, 2 0, 3 0
         4 1, 5 1
         6 2, 7 2, 8 2, 9 2}
  :moves []})

(defn all-top? [node]
  (->> node
       :plan
       (every? (fn [[_ location]] (= 3 location)))))

; returns node reachable from the node:
; find devices on the floor
; get all 1-2 combinations
; check them
; check remaining on the floor
; get node up or down
(defn next-nodes [node]
  )

(defn breadth-first-search [nodes]
  (let [
    visit (fn [agg node]
            (if (all-top? node)
              (reduced node)
              (conj agg (next-nodes node))))
    result (reduce visit [] nodes)]
    (if (map? result)
      result
      (recur result))))

