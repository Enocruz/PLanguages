"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Problem Set: Higher-Order Functions
September 7, 2017
"

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (Math/abs (- x y)) epsilon))

"1"
(defn  my-map-indexed
  "takes two arguments: a function f and a list lst. It returns
   a list consisting of the result of applying f to 0 and the first
   item of lst, followed by applying f to 1 and the second item
   in lst, and so on until lst is exhausted. Function f should
   accept 2 arguments: index and item."
  [f lst])

"2"
(defn my-drop-while
  "takes two arguments: a function f and a list lst. It returns a list
   of items from lst dropping the initial items that evaluate to true
   when passed to f. Once a false value is encountered, the rest
   of the list is returned. Function f should accept one argument."
  [f lst])

"3"
(defn bisection
  "that takes a, b, and f as arguments. It finds the corresponding
  root using the bisection method. The algorithm must stop when a value
   of c is found such that: |f(c)| < 1.0×10-15."
  [a b f])

"4"
(defn deriv
  "that takes f and h as its arguments, and returns a new function
  that takes x as argument, and which represents the derivate
  of f given a certain value for h."
  [f h])

"5"
(defn integral
  "that takes as arguments a, b, n, and f. It returns the value
   of the integral, using Simpson's rule. The unit tests verify
    the following single and double integrals (with n = 10):"
  [a b n f])

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