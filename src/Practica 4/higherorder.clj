"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Problem Set: Higher-Order Functions
September 7, 2017
"

(use 'clojure.test)
(use 'clojure.math.numeric-tower)


(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

"1"
(defn  my-map-indexed
  "takes two arguments: a function f and a list lst. It returns
   a list consisting of the result of applying f to 0 and the first
   item of lst, followed by applying f to 1 and the second item
   in lst, and so on until lst is exhausted. Function f should
   accept 2 arguments: index and item."
  [f lst]
  (loop [count 0 Lst lst Elist '()]
    (cond
      (empty? Lst) (reverse Elist)
      :else (recur (inc count)
                   (rest Lst)
                   (conj Elist
                         (f count (first Lst)))))))

"2"
(defn my-drop-while
  "takes two arguments: a function f and a list lst. It returns a list
   of items from lst dropping the initial items that evaluate to true
   when passed to f. Once a false value is encountered, the rest
   of the list is returned. Function f should accept one argument."
  [f lst]
  (loop [nlist '() lst lst]
    (cond
      (empty? lst) nlist
      (true? (f (first lst))) (recur nlist (rest lst))
      :else (concat nlist lst))))

"3"
(defn bisection
  "that takes a, b, and f as arguments. It finds the corresponding
  root using the bisection method. The algorithm must stop when a value
   of c is found such that: |f(c)| < 1.0×10-15."
  [a b f]
  (loop [A a B b C (/ (+ a b) 2)]
   ;(if (number? A) Resultado)))
    (cond
      (< (abs (f C)) (* 1.0 (expt 10 (- 15)))) C
      (and (< (f C) 0) (> (f B) 0)) (recur C B (/ (+ C B) 2))
      (and (> (f C) 0) (< (f B) 0)) (recur C B (/ (+ C B) 2))
      ;(and (> (f c) 0) (> (f b) 0)) (recur (/ (+ A c) 2) A c)
      :else (recur A C (/ (+ A C) 2)))))



"4"
(defn deriv
  "that takes f and h as its arguments, and returns a new function
  that takes x as argument, and which represents the derivate
  of f given a certain value for h."
  [f h]
  (fn [x] (/
            (- (f (+ x h))
               (f x)) h)))


"5"
(defn integral
  "that takes as arguments a, b, n, and f. It returns the value
   of the integral, using Simpson's rule."
  [a b n f]
  (loop [sum 0 h (/ (- b a) n) yk (f a) count 0]
    (cond
      (= count n) (* (/ h 3)
                     (+ sum yk))
      (zero? count) (recur
                      (+ sum yk)
                      h
                      (f (+ a (* (inc count) h)))
                      (inc count))
      (odd? count) (recur
                     (+ sum (* 4 yk))
                     h
                     (f (+ a (* (inc count) h)))
                     (inc count))
      :else        (recur
                     (+ sum (* 2 yk))
                     h
                     (f (+ a (* (inc count) h)))
                     (inc count)))))

"TEST SECTION"

"1"
(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))

"2"
(deftest test-my-drop-while
  (is (= () (my-drop-while neg? ())))
  (is (= '(0 1 2 3 4)
         (my-drop-while
           neg?
           '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
  (is (= '(2 three 4 five)
         (my-drop-while
           symbol?
           '(zero one 2 three 4 five))))
  (is (= '(0 one 2 three 4 five)
         (my-drop-while
           symbol?
           '(0 one 2 three 4 five)))))

"3"
(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1))))))

"4"
(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

"5"
(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4 (integral 1 2 10 (fn [x]
                                 (integral 3 4 10
                                           (fn [y] (* x y))))))))

(run-tests)