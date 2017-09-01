
(defn f1 [x] (* x x x))
(defn f2 [x] (+ 2 x))

(defn compose
  "Composes a new function f(g(x))"
  [f g]
  (fn [x]
    (f (g x))))

(def f3 (compose f1 f2))
(def f4 (compose f2 f2))
(def f5 (compose f3 f4))

(defn my-map
  "Our implementation of the map function."
  [fun lst]
  (cond
    (empty? lst) ()
    :else (cons (fun (first lst))
                (my-map fun (rest lst)))))
