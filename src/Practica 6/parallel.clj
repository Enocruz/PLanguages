"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Using Parallel Map
October 26, 2017
"

(defn prime?  "Returns true if n is a prime number, otherwise returns false."
  [n]
  (if (< n 2)
    false
    (loop [i 2]
      (if (<= (* i i) n)
        (if (zero? (rem n i))
          false
          (recur (inc i)))
        true))))

(defn ranges
  "Returns a list of p vectors containing all available
  equal size ranges from 0 (inclusive) to n (exclusive)."
  [n p]
  (let [delta (/ n p)]
    (->>
      (range 0 n delta)
      (map (fn [x] [x (+ x delta)])))))


(defn compute-primes
  "Auxiliar fuction to calculate the sum of the primes number
   in the given chunk"
  [start end]
  (loop [i start sum 0]
    (cond
      (>= i end) sum
      (prime? i) (recur (inc i) (+ i sum))
      :else (recur (inc i) sum))))

(defn prime-sum
  "Calculates the sum of all the primes numbers between 0 - 5,000,000
  Takes as argument the number of chunks or in this case processors to use"
  [n]
  (->>
    (ranges 5000000 n)
    (pmap (fn [[start end]] (compute-primes start end)))
    (reduce +)))

