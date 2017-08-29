(declare variable? same-variable? sum? addend augend make-sum
         product? multiplier multiplicand make-product)

(defn deriv [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var)
                      1
                      0)
    (sum? exp) (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var))
    (product? exp) (make-sum (make-product (multiplier exp)
                                           (deriv (multiplicand exp) var))
                             (make-product (multiplicand exp)
                                           (deriv (multiplier exp) var)))
    :else (throw (IllegalArgumentException. (str "unknown expresion type: " exp)))))

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn make-sum [a1 a2]
  (cond
    (zero? a1) a2
    (zero? a2) a1
    (and (number? a1)
         (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))


(defn make-product [m1 m2]
  (cond
    (or (zero? m1) (zero? m2)) 0
    (= 1 m1) m2
    (= 1 m2) m1
    (and (number? m1)
         (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn sum? [exp]
  (and (list? exp)
       (= '+ (first exp))))

(defn product? [exp]
  (and (list? exp)
       (= '* (first exp))))

(defn addend [exp]
  (nth exp 1))

(defn augend [exp]
  (nth exp 2))

(defn multiplier [exp]
  (nth exp 1))

(defn multiplicand [exp]
  (nth exp 2))