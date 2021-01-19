(ns adv.t05)

(import java.security.MessageDigest java.math.BigInteger)

(defn md5
  [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn find-zeroes-seq [door-id]
  (->> (range)
       (map (partial str door-id))
       (map md5)
       (filter (fn [s] (clojure.string/starts-with? s "00000")))))
 
(defn solve
  ([door-id] 
    (->> door-id
         (find-zeroes-seq)
         (take 8)
         (map (fn [s] (nth s 5)))
         (apply str)))
  ([] (solve "ugkcyxxp")))

; Part 2
(defn index5 [s] 
  (- (int (nth s 5)) (int \0)))

(defn fill-password [{:keys [cnt psw] :as res} s]
  (let [i (index5 s) 
        p (nth s 6) a (prn cnt psw i)]
    (if (< i (count psw))
      (if (nil? (psw i))
        (if (= cnt (dec (count psw)))
          (reduced {:cnt (inc cnt), :psw (assoc psw i p)})
          {:cnt (inc cnt), :psw (assoc psw i p)}) 
        res)
      res)))

(defn solve2
  ([door-id]
    (->> door-id
         (find-zeroes-seq)
         (reduce fill-password
                 {:cnt 0, :psw (vec (repeat 8 nil))})
         (:psw)
         (apply str)))
         
  ([] (solve2 "ugkcyxxp")))

