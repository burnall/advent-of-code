; `(cons 1 ~(+ 2 3)) => (clojure.core/cons 1 5) 

; (let [x '(2 3)] `(1 ~@x)) => (1 2 3)

(defn contextual-eval [ctx expr]
  (eval 
    `(let [~@(mapcat (fn [[k v]] [k `~v]) ctx)]
       ~expr)))

;(contextual-eval {'a 1, 'b 10} '(- a b)) => -9

(defmacro print-els [coll]
  `(do (run! println ~coll) ~coll))

(defmacro print-els [coll]
  `(do (run! println ~coll) ~coll))

(defmacro print-els [coll]
  `(do ~@(map println coll) ~coll))