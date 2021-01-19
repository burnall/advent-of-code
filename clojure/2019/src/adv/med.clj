(def patient-medications [{:name "Blythenal"
                           :rxNorm "blyth"}
                          {:name "Masbutol"
                           :rxNorm "masbut"}
                          {:name "Welbutril"
                           :rxNorm "welb"}])

(def contraindication-pairs [["nr913ng" "blyth"]
                             ["masbut"  "87f2h139049f"]
                             ["nr913ng" "j1j81f0"]
                             ["blyth" "welb"]
                             ["masbut"  "welb"]])

(defn contraindications [meds pairs]
  (let [rx-norms (->> meds 
                      (map :rxNorm)
                      (set))]
    (->> pairs
         (filter (fn [[a b]] (and (rx-norms a) (rx-norms b)))))))

(defn t[] (contraindications patient-medications contraindication-pairs))


