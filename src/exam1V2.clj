;==========================================================
; Name: Oscar Allan Ruiz Toledo
; ID: A01376200
;==========================================================

(use 'clojure.test)

;==========================================================
(defn babylonian
  "Computes the square root of s using the Babylonian
  method with n iterations."
  [s n]
  (cond
    (= n 0) 1
    :else (* (/ 1 2) (+ (babylonian s (dec n)) (/ s (babylonian s (dec n)))))))


;==========================================================
(defn twos-complement
  "Retuns the two's complement of a list containing only
  ones and zeros."
  [lst]
  (loop [res ()
         ultimo (last lst)
         lst (butlast lst)
         flag 0]
    (cond
      (= flag 1) (cond
                   (empty? lst) (if (= ultimo 1) (cons 0 res)
                                               (cons 1 res))
                   (= ultimo 1) (recur (cons 0 res) (last lst) (butlast lst) 1)
                   :else (recur (cons 1 res) (last lst) (butlast lst) 1))
      :else (cond
              (empty? lst) (cons ultimo res)
              (= ultimo 1) (recur (cons ultimo res) (last lst) (butlast lst) 1)
              :else (recur (cons ultimo res) (last lst) (butlast lst) 0)))))





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

