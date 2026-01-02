(ns adv.morse)

(def codes
  {\A ".-",  \B "-...",  \C "-.-.",  \D "-..", \E ".",
   \F "..-.", \G "--.", \H "....", \I "..", \J ".---",
   \K "-.-",  \L ".-..", \M "--", \N "-.", \O "---", 
   \P ".--.", \Q "--.-", \R ".-.", \S "...", \T "-",
   \U "..-", \V "...-", \W ".--", \X "-..-", \Y "-.--",
   \Z "--..",

   \0 "-----", \1 ".----", \2 "..---", \3 "...--",
   \4 "....-", \5 ".....", \6 "-....", \7 "--...",
   \8 "---..", \9 "----."})

(def rev-codes 
  (zipmap (map second codes) (map first codes)))

(defn encode [s]
  (->> s
       (map codes)
       (apply str)))

(defn pos [base ch]
  (- (int ch) (int base)))

(defn checksum [s]
  (as-> s $
       (map (partial pos \a) $)
       (apply + $)
       (mod $ 10)))

(defn decode-digit [xs]
  (->> xs
       (apply str)
       (rev-codes)
       (pos \0)))

(defn decode-all [xs]
  (if (empty? xs)
    
    ))

(defn decode [msg] 
  (let [[prefix s] (split-at 5 msg)
        crc (decode-digit prefix)]
    (->> s
         (decode-all)
         (filter #(= % crc)))))

(comment
  (encode "Hey 123"))