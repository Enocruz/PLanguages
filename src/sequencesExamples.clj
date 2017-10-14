(defn remove-divisibles
  [lst]
  (lazy-seq
    (cons (first lst)
          (remove-divisibles
            (remove #(zero? (rem % (first lst))) lst)))))

(def primes (remove-divisibles (iterate inc 2)))