(ns adv.t16
  (:require [adv.util :refer [split-lines]]))

(def input
  (->> "data/t16.txt"
       (slurp)))

(defn parse-digit [hex] 
  (let [s (-> hex
              (str)
              (Integer/parseInt 16)
              (Integer/toBinaryString))]
    (map {\0 0, \1 1} s)))

(defn parse-digit [hex] 
  (let [s (-> hex
              (str)
              (Integer/parseInt 16)
              (Integer/toBinaryString))
        bits (map {\0 0, \1 1} s)]
    (concat (repeat (- 4 (count bits)) 0)
            bits)))      

(defn parse-input [s]
  (->> s
       (mapcat parse-digit)
       (vec)))

(defn to-number [digits]
  (reduce (fn [n d]
            (+ n n d))
          0
          digits))

(defn parse-literal [bits result size]
  (let [res (apply conj result (subvec bits 1 5))]
    (if (zero? (first bits))
      {:value res, :size (+ size 5)}
      (recur (subvec bits 5) res (+ size 5)))))

(declare parse-packet)

(defn parse-nested-with-subpackets-size [bits size packets] 
  (if (<= size 0)
    packets
    (let [packet (parse-packet bits)
          sz (:size packet)]
      (recur (subvec bits sz)
             (- size sz)
             (conj packets packet)))))  

(defn parse-nested-with-subpackets-count [bits count packets]
  (if (zero? count)
    packets
    (let [packet (parse-packet bits)
          sz (:size packet)]
      (recur (subvec bits sz)
             (dec count)
             (conj packets packet)))))  

(defn get-total-size [packets]
  (->> packets
       (map :size)
       (reduce +)))

(defn parse-nested [bits]
  (let [[prefix-size subpackets]
          (if (zero? (first bits))
            [16 (parse-nested-with-subpackets-size (subvec bits 16)
                                                   (to-number (subvec bits 1 16))
                                                   [])]
            [12 (parse-nested-with-subpackets-count (subvec bits 12)
                                                    (to-number (subvec bits 1 12))
                                                    [])])]
     {:subpackets subpackets
      :size (+ 6 prefix-size (get-total-size subpackets))}))


(defn parse-packet [bits]
  (let [packet {:version (to-number (subvec bits 0 3))
                :type-id (to-number (subvec bits 3 6))}]
    (if (= 4 (:type-id packet))
      (merge packet (parse-literal (subvec bits 6) [] 6))
      (merge packet (parse-nested (subvec bits 6))))))


(defn solve [s]
  (->> s
       (parse-input)
       (parse-packet)
       (tree-seq :subpackets :subpackets)
       (map :version)
       (reduce +)))

(defn f []
  (solve input))


; Part 2

(def operations 
  {0 +
   1 *
   2 min
   3 max
   5 (comp {false 0, true 1} >)
   6 (comp {false 0, true 1} <)
   7 (comp {false 0, true 1} =)})

(defn compute [{:keys [type-id subpackets value]}]
  (if (= 4 type-id)
    (to-number value)
    (apply (operations type-id) (map compute subpackets))))           


(defn solve2 [s]
  (->> s
       (parse-input)
       (parse-packet)
       (compute)))

(defn f2 []
  (solve2 input))
