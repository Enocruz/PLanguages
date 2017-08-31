"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Problem Set: Recursive Functions II
August 31, 2017
"

(use 'clojure.test)

"1"
(defn my-repeat [n x]
  "The function my-repeat takes a number n and any data x as its
  arguments. It returns a list that contains n copies of x. Do not
  use the predefined repeat function"
  (cond
    (= n 0) ()
    :else (cons x (my-repeat (- n 1) x))))

"2"
(defn invert-pairs [lst]
  "The function invert-pairs takes as an argument a list of vectors
  containing two elements each. It returns a new list with every
  vector pair inverted."
  (cond
    (empty? lst) ()
    (vector? (first lst)) (cons (invert-pairs (first lst)) (invert-pairs (rest lst)))
    (empty? (rest lst)) [(first lst)]
    :else (conj (invert-pairs (rest lst))(first lst))))


"3"
(defn enlist [lst]
  "The function enlist surrounds in a list every upper-level element of
  the list it takes as input."
  (cond
    (empty? lst) ()
    :else (cons (cons (first lst) ()) (enlist (rest lst)))))

"4"
(defn my-interleave [lst1 lst2]
  "takes two arguments: the lists a and b. It returns a list
  containing the first element of a, followed by the first
  element of b, followed by the second element of a, followed by
  the second element of b, and so on. The lists a and b don't
  have to be of the same size. The interleaving of the elements
  stops when either a or b is exhausted."
  (cond
    (or (empty? lst1) (empty? lst2)) ()
    :else (cons (first lst1)
                (cons (first lst2)
                      (my-interleave (rest lst1) (rest lst2))))))

"5"
"The function my-flatten removes all the interior parenthesis
  of the list it takes as input. Do not use the predefined flatten
  function."
(defn my-flatten
  [lst]
  (cond
    (empty? lst) lst
    (list? (first lst)) (concat (my-flatten (first lst)) (my-flatten (rest lst)))
    :else (cons (first lst) (my-flatten (rest lst)))))

"6"
(defn exchange [x1 x2 lst]
  "The function exchange takes three arguments: two non-list values x1 and x2,
   and a list lst. It returns a list with the same elements as lst, except
   that all occurrences of x1 are replaced by x2 and vice versa, including
   any occurrences inside nested lists."
  (cond
    (empty? lst) ()
    (list? (first lst)) (cons (exchange x1 x2 (first lst)) (exchange x1 x2 (rest lst)))
    (= x1 (first lst)) (cons x2 (exchange x1 x2 (rest lst)))
    (= x2 (first lst )) (cons x1 (exchange x1 x2 (rest lst)))
    :else (cons (first lst) (exchange x1 x2 (rest lst)))))

"7"
(defn insert [n lst]
  "The function insert takes two arguments: a number n and a list of numbers
  lst in ascending order. It returns a new list with the same elements as
  lst but inserting n in its corresponding place."
  (cond
    (empty? lst) (cons n lst)
    (<= n (first lst)) (conj lst n)
    :else (cons (first lst) (insert n (rest lst)))))

"8"
(defn my-sort [lst]
  "The function my-sort takes an unordered list of numbers as an argument,
  and returns a new list with the same elements but in ascending order.
  You must use the insert function defined in the previous exercise
  to write the my-sort. Do not use the predefined sort function."
  (loop [Sort '() N (count lst) NList lst]
    (cond
      (zero? N) Sort
      :else (recur (insert (first NList) Sort) (dec N) (rest NList)))))

"9"
(defn binary [n]
  "The function binary takes an integer n as input (assume that n ≥ 0).
  If n is equal to zero, it returns an empty list. If n is greater than zero,
  it returns a list with a sequence of ones and zeros equivalent to the
  binary representation of n. "
  (cond
    (zero? n) ()
    :else  (concat (binary (quot n 2)) [(rem n 2)])))

"10"
(defn prime-factors [n]
  "The function prime-factors takes an integer n as input (assume that n > 0),
  and returns a list containing the prime factors of n in ascending order.
  The prime factors are the prime numbers that divide a number exactly.
  If you multiply all the prime factors you get the original number."
  (loop
    [N n div 2 lst '()]
    (cond
      (= N 1) ()
      (> (* div div) N) (concat lst [N])
      (zero? (rem N div)) (recur (quot N div) div (concat lst [div]))
      :else (cond
              (= div 2) (recur N 3 lst)
              :else (recur N (+ div 2) lst)))))

"11"
(defn compress [lst]
  "The function compress takes a list lst as its argument. If lst contains
  consecutive repeated elements, they should be replaced with a single
  copy of the element. The order of the elements should not be changed."
  (cond
    (empty? lst) ()
    (= (first lst) (first (rest lst))) (compress (rest lst))
    :else (cons (first lst) (compress (rest lst)))))

"12"
"The function pack takes a list lst as its argument. If lst contains
consecutive repeated elements they should be placed in separate sublists."
(defn pack
  [lst]
  (if (empty? lst)
    ()
    (loop [accum (conj () (last lst))
           lst (butlast lst)
           result ()]
      (cond
        (empty? lst) (conj result accum)
        (= (last accum) (last lst)) (recur (conj accum (last lst)) (butlast lst) result)
        :else (recur (conj () (last lst)) (butlast lst) (conj result accum))))))


"13"
(defn encode
  "Takes a list lst as its argument. Consecutive duplicates of
   elements in lst are encoded as vectors [n e], where n is the
   number of duplicates of the element e."
  [lst]
  (if (empty? lst)
    ()
    (loop [accum  [(first lst)]
           lst    (rest lst)
           result []]
      (cond

        (empty? lst)       (list* (conj result [(count accum)
                                                (first accum)]))

        (= (first accum)
           (first lst))    (recur (conj accum (first lst))
                                  (rest lst)
                                  result)

        :else              (recur [(first lst)]
                                  (rest lst)
                                  (conj result [(count accum)
                                                (first accum)]))))))


"14"
(defn encode-modified
  "The function encode-modified takes a list lst as its argument.
  It works the same as the previous problem, but if an element
  has no duplicates it is simply copied into the result list.
  Only elements with duplicates are converted to [n e] vectors."
  [lst]
  (if (empty? lst)
    ()
    (loop [accum  [(first lst)]
           lst    (rest lst)
           result []]
      (cond

        (empty? lst)       (if (= 1 (count accum))
                             (list* (conj result (first accum)))
                             (list* (conj result [(count accum)
                                                  (first accum)])))


        (= (first accum)
           (first lst))    (recur (conj accum (first lst))
                                  (rest lst)
                                  result)

        :else              (if (= 1 (count accum))
                             (recur [(first lst)]
                                    (rest lst)
                                    (conj result (first accum)))
                             (recur [(first lst)]
                                    (rest lst)
                                    (conj result [(count accum)
                                                  (first accum)])))))))







"15"
(defn decode
  "The function decode takes as its argument an encoded list lst
  that has the same structure as the resulting list from
  the previous problem."
  [lst]
  (if (empty? lst)
    ()
    (loop [elem (last lst)
           lst (butlast lst)
           result ()]
      (cond
        (empty? lst) (cond
                       (vector? elem) (concat (loop [con (first elem)
                                                     res ()]
                                                (cond
                                                  (= con 0) res
                                                  :else (recur (dec con) (cons (last elem) res)))) result)
                       :else (cons elem result))
        (vector? elem) (recur (last lst) (butlast lst) (concat (loop [con (first elem)
                                                                      res ()]
                                                                 (cond
                                                                   (= con 0) res
                                                                   :else (recur (dec con) (cons (last elem) res)))) result))

        :else (recur (last lst) (butlast lst) (cons elem result))))))



"1"
(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

"2"
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))(invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

"3"
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8)) (enlist '((1 2 3) 4 (5) 7 8)))))

"4"
(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5)
         (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4) (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

"5"
(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
         (my-flatten '(((one) ((two))) () (three (())) four)))))

"6"
(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
      (exchange true 42 '((true) 42 ((cool (42)) (true))))))

"7"
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

"8"
(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

"9"
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

"10"
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

"11"
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e) (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

"12"
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

"13"
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

"14"
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

"15"
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
(run-tests)