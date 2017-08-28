; Brandon Alain Cruz Ruiz
; A01375640
; Simple Clojure  Exercises

; defn -> Create a function
; add1 -> Name of the function
; [x] -> Parameters
; (+ x 1) -> Body of the function

(use 'clojure.test)

(defn add1 [x]
  (+ x 1))

(defn add2 [y]
  (add1 (add1 y)))

(defn f2c [f]
  (* (- f 32) (/ 5 9)))

(defn sign [x]
  (if (= x 0) 0
              (if (< x 0) -1 1)))

(defn roots [a b c] (let [d (- b)
                          e (Math/sqrt (- (* b b) (* 4 a c)))
                          f (* 2 a)
                          x1 (/ (+ d e) f)
                          x2 (/ (- d e) f)]
                         [x1 x2]))

(defn bmi [weight heigth]
  (let [BMI (/ weight (* heigth heigth))]
    (if (< BMI 20)
      'underweight
      (if (< BMI 25)
        'normal
        (if (< BMI 30)
          'obese1
          (if (< BMI 35)
            'obese2
            'obese3))))))


(deftest test-add1
  (is (= 6 (add1 5)))
  (is (= -9 (add1 -10)))
  (is (= 0 (add1 -1))))

(deftest test-add2
  (is (= 12 (add2 10)))
  (is (= 0 (add2 -2)))
  (is (= 100 (add2 98))))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1.0 -1.0] (roots 2.0 4.0 2.0)))
  (is (= [0.0 0.0] (roots 1.0 0.0 0.0)))
  (is (= [-0.25 -1.0] (roots 4.0 5.0 1.0))))

(deftest test-bmi)
(is (= 'underweight (bmi 45 1.7)))
(is (= 'normal (bmi 55 1.5)))
(is (= 'obese1 (bmi 76 1.7)))
(is (= 'obese2 (bmi 81 1.6)))
(is (= 'obese3 (bmi 120 1.6)))

(run-tests)