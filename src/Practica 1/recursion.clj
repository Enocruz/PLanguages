"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Problem Set: Recursive Functions I
August 24, 2017
"

(use 'clojure.test)

(defn my-count [lst]
  "Returns the number of elements contained in its input list."
  (if (empty? lst)
    0
    (+ 1 (my-count (rest lst)))))


(defn add-list [lst]
  "Returns the sum of all the elements of its input list"
  (if (empty? lst)
    0
    (+ (first lst) (add-list (rest lst)))))

(defn member? [x, lst]
  "takes two arguments, any data x and a list lst. Returns true
  if x is contained in lst, false otherwise."
  (cond
    (empty? lst) false
    (= (first lst) x) true
    :else (member? x (rest lst))))

(defn list-of-symbols? [lst]
  "Takes a list lst as its argument. It returns true if all the
  elements (possibly zero) contained in lst are symbols, or false
  otherwise."
  (cond
    (empty? lst) true
    (not (symbol? (first lst))) false
    :else (list-of-symbols? (rest lst))))

(defn my-last [lst]
  "Returns the last element of its input list, or nil of its empty."
  (cond
    (empty? lst) nil
    (= 1 (count lst)) (first lst)
    :else (my-last (rest lst))))

(defn cons-end [x lst]
  "Returns a list composed by the same elements of lst but
   with x at the end."
  (cond
    (empty? lst) [x]
    :else (cons-end x (rest lst))))

(defn cons-end
  "Returns a list composed by the same elements of lst but with x at the end."
  [x lst]
  (cond
    (empty? lst) (cons x ())
    :else (cons (first lst) (cons-end x (rest lst)))))

(defn my-reverse
  "Returns another list with the same elements as the input list, but in reverse order"
  [lst]
  (cond
    (empty? lst) ()
    :else (cons (last lst) (my-reverse (butlast lst)))))

(defn my-butlast
  "Returns a list with the same elements as its input list but excluding the last element, or nil of its empty"
  [lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) ()
    :else (cons (first lst) (my-butlast (rest lst)))))


(defn my-concat
  "Returns the resulting list of appending the two lists it takes as input"
  [lst1 lst2]
  (cond
    (empty? lst1)lst2
    (empty? lst2)lst1
    :else (cons (first lst1) (my-concat (rest lst1) lst2))))

(defn deep-reverse
  "Returns a list with the same elements as its input but in reverse order"
  [lst]
  (cond
    (empty? lst) ()
    (list? (last lst)) (cons (deep-reverse (last lst)) (deep-reverse (butlast lst)))
    :else (cons (last lst) (deep-reverse (butlast lst)))))


"TEST SECTION"

(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))


(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1)) (deep-reverse '((1 2) 3 (4 (5 6)))))))

