(defn remove-divisibles
  [lst]
  (lazy-seq
    (cons (first lst)
          (remove-divisibles
            (remove #(zero? (rem % (first lst))) lst)))))

(def primes (remove-divisibles (iterate inc 2)))

(fn [col1 col2]
  (->>
    (for [i col1]
      (some #{i} col2))
    (remove nil?)))