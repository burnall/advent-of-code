(ns adv.matcher)


(defn match [[p0 p1 & ps] [s0 & ss]]
  (cond
    (not p0) (not s0)
    (not p1) (and (= p0 s0) (empty? ss))
    (not= p1 \*) (and (= p0 s0) (match (conj ps p1) ss))
    :else (or (match (conj ps p1 p0 p0) (conj ss s0))
              (match ps (conj ss s0)))))

(defn match2 [[p0 p1 & ps :as p] [s0 & ss :as s]]
  (if p0
    (let [eq? (= p0 s0)
          wc? (= p1 \*)]
      (cond
        (and eq? wc?)
        (or (match2 p ss) (match2 ps s))
        (and eq? (not wc?))
        (recur (rest p) ss)
        (and (not eq?) wc?)
        (recur ps s)
        :else false))

    (not s0)))

(defn match? [[p0 p1 & ps :as pat] [s0 & ss :as str]]
  (cond
    (empty? pat) (empty? str)
    (= p1 \*) (or (and (= p0 s0) (match? pat ss))
                  (match? ps str))
    (= p0 s0) (match? (rest pat) ss)
    :else false))

(comment
  (match? "a" "a")
  (match? "b" "bb")
  (match? "a*a" "a")
  (match? "b*c" "bbbbc")
  (match? "xy*x" "xx")
  (match? "z*z" "zz")
  (match? "m*n" "mmmnm")
  )