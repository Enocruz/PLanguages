;==========================================================
; Brandon Alain Cruz Ruiz A01375640
;==========================================================

(use 'clojure.test)

;==========================================================
(defn babylonian
  "Computes the square root of s using the Babylonian
  method with n iterations."
  [s n]
  (cond
    (zero? n) 1
    :else (* (/ 1 2) (+ (babylonian s (- n 1)) (/ s (babylonian s (- n 1)))))))


;==========================================================
(defn flip
  [lst]
  (loop [Lst lst nList (list)]
    (cond
      (empty? Lst)  nList
      (zero? (first Lst)) (recur (rest Lst) (conj nList 1))
      :else (recur (rest Lst) (conj nList 0)))))

(defn twos-complement
  "Retuns the two's complement of a list containing only
  ones and zeros."
  [lst]
  (loop [Lst (reverse lst) nList (list)]
    (cond
      (empty? Lst) nList
      (= (first Lst) 1) (concat (flip (rest Lst)) (cons (first Lst) nList))
      :else (recur (rest Lst) (cons (first Lst) nList)))))



;==========================================================
(use 'clojure.math.numeric-tower)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(deftest test-babylonian
  (is (aprox= 0.0001
              1
              (babylonian 1 0)))
  (is (aprox= 0.0001
              1.4142
              (babylonian 2 3)))
  (is (aprox= 0.0001
              4
              (babylonian 16 5)))
  (is (aprox= 0.00000000001
              31.62277660168
              (babylonian 1000 9))))

;==========================================================
(deftest test-twos-complement
  (is (= '(1)
         (twos-complement '(1))))
  (is (= '(1 1 1 1 1 1 1 1)
         (twos-complement '(0 0 0 0 0 0 0 1))))
  (is (= '(0 0 0 0 0 0 0 1)
         (twos-complement '(1 1 1 1 1 1 1 1))))
  (is (= '(1 0 0 0 0 0 0 0)
         (twos-complement '(1 0 0 0 0 0 0 0))))
  (is (= '(1 0 1 0 0 1 1 0 1 0)
         (twos-complement '(0 1 0 1 1 0 0 1 1 0))))
  (is (= '(0 1 0 1 0 1 1 0 1 1 1 1 0 0 0 0)
         (twos-complement '(1 0 1 0 1 0 0 1 0 0 0 1 0 0 0 0)))))

;==========================================================
(run-tests)
