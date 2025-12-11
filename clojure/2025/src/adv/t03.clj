(ns adv.t03
  (:require [adv.util :refer [split-lines]]))

(def input
  (-> "resources/t03.txt"
      (slurp)
      (split-lines)))

(defn find-max-seq [n s]
  (let [len (count s)
        go (fn [chars i]
          (let [idx (- n (min n (- len i)))]
          ))
        ]
    (reduce go [] (range len))))

        ;; for (int i = 0; i < len; i++) {
        ;;     char ch = s.charAt(i);
        ;;     boolean found = false;
        ;;     for  (int j = n - Integer.min(n, len - i); j < n; j++) {
        ;;         if (found) {
        ;;             v[j] = '\0';
        ;;         } else if (ch > v[j] ) {
        ;;             v[j] = ch;
        ;;             found = true;
        ;;         }
        ;;     }
        ;; }
