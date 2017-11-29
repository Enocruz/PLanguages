;==========================================================
; Final exam
; Date: November 28, 2017.
; Author: Oscar Allan Ruiz Toledo A01376200
;==========================================================

(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

;==========================================================
(defn consec-add
  [lst]
  (loop [fst (first lst)
         scd (second lst)
         rst (rest lst)
         result []]
    (cond
      (empty? rst) result
      :else (recur (first rst) (second rst) (rest rst) (conj result (+ fst scd))))))


;==========================================================
(defn when-aux
  [condition & exprs]
  (let [exprs (first exprs)]
    (if (empty? exprs)
      `(when-not ~condition (recur))
      `(when-not ~condition ~(first exprs) ~(when-aux condition (rest exprs))))))


(defmacro loop-and-exit-when
  [condition & exprs]
  (cond
    (empty? exprs) `(loop [] (recur))
    (== (count exprs) 1) `(loop [] ~@exprs (when-not ~condition (recur)))
    :else `(loop [] ~(first exprs) ~(when-aux condition (rest exprs)))))


;==========================================================
(defn powo
  [base expo result]
  (conde
    [(fd/== expo 0)
     (=== result 1)]
    [(fresh [temp new_exp]
            (fd/in temp (fd/interval 0 100))
            (fd/in new_exp (fd/interval 0 100))
            (fd/- expo 1 new_exp)
            (fd/* base temp result)
            (powo base new_exp temp))]))


;==========================================================
(defn neighbors
  [lst1 current lst3 j]
  (let [result ()
        prev-j (dec j)
        post-j (inc j)]
    (cond
      (empty? lst1) (conj result (nth current prev-j 0) (nth current post-j 0) (nth lst3 prev-j 0) (nth lst3 j) (nth lst3 post-j 0))
      (empty? lst3) (conj result (nth current prev-j 0) (nth current post-j 0) (nth lst1 prev-j 0) (nth lst1 j) (nth lst1 post-j 0))
      :else (conj result (nth current prev-j 0) (nth current post-j 0) (nth lst3 prev-j 0) (nth lst3 j) (nth lst3 post-j 0) (nth lst1 prev-j 0) (nth lst1 j) (nth lst1 post-j 0)))))


(defn alive-neighbors
  [lst1 current lst3 j]
  (reduce + (neighbors lst1 current lst3 j)))

(defn process-line
  [board current i]
  (let [size (count current)]
    (loop [cell (first current)
           lst (rest current)
           j 0
           result []]
      (if (<= size j)
        result
        (let [alive-neighbors-size (alive-neighbors (nth board (dec i) ()) current (nth board (inc i) ()) j)]
          (cond
            (== cell 0) (if (== alive-neighbors-size 3)
                          (recur (first lst) (rest lst) (inc j) (conj result 1))
                          (recur (first lst) (rest lst) (inc j) (conj result 0)))
            (== cell 1) (cond
                          (< alive-neighbors-size 2) (recur (first lst) (rest lst) (inc j) (conj result 0))
                          (> alive-neighbors-size 3) (recur (first lst) (rest lst) (inc j) (conj result 0))
                          :else (recur (first lst) (rest lst) (inc j) (conj result 1)))))))))


(defn game-of-life
  [board]
  (let [size (count board)]
    (loop [current (first board)
           rst (rest board)
           i 0
           result-board []]
      (cond
        (>= i size) result-board
        :else (recur (first rst) (rest rst) (inc i) (conj result-board (process-line board current i)))))))

(defn step
  [n board]
  (loop [i 0
         result board]
    (cond
      (<= n i) result
      :else (recur (inc i) (game-of-life result)))))


;==========================================================
(deftest test-consec-add
  (test-is (= [] (consec-add [])))
  (test-is (= [] (consec-add [42])))
  (test-is (= [12 23 31 39 65]
              (consec-add [4 8 15 16 23 42])))
  (test-is (= (repeat 100 0)
              (consec-add (take 101 (cycle [1 -1])))))
  (test-is (= (range 1 1998 2)
              (consec-add (range 1000)))))

;==========================================================
(deftest test-loop-and-exit-when
  (test-is (= '(clojure.core/loop [] (recur))
              (macroexpand-1 '(loop-and-exit-when true))))
  (test-is (= '(clojure.core/loop []
                 (println "*")
                 (clojure.core/when-not true
                   (recur)))
               (macroexpand-1
                 '(loop-and-exit-when true
                    (println "*")))))
  (test-is (= "*\n"
              (with-out-str
                (loop-and-exit-when true
                  (println "*")))))
  (test-is (= '(clojure.core/loop []
                 (println "Start")
                 (clojure.core/when-not (= (clojure.core/deref i) 3)
                   (println (clojure.core/deref i))
                   (clojure.core/when-not (= (clojure.core/deref i) 3)
                     (swap! i inc)
                     (clojure.core/when-not (= (clojure.core/deref i) 3)
                       (println "End")
                       (clojure.core/when-not (= (clojure.core/deref i) 3)
                         (recur))))))
              (macroexpand-1
                '(loop-and-exit-when (= @i 3)
                    (println "Start")
                    (println @i)
                    (swap! i inc)
                    (println "End")))))
  (test-is (= "Start\n0\nEnd\nStart\n1\nEnd\nStart\n2\n"
              (with-out-str
                (def i (atom 0))
                (loop-and-exit-when (= @i 3)
                  (println "Start")
                  (println @i)
                  (swap! i inc)
                  (println "End")))))
  (test-is (= '(clojure.core/loop []
                 (println "*")
                 (clojure.core/when-not (> (clojure.core/deref x) 10)
                   (swap! x inc)
                   (clojure.core/when-not (> (clojure.core/deref x) 10)
                     (println "**")
                     (clojure.core/when-not (> (clojure.core/deref x) 10)
                       (swap! x (partial + 2))
                       (clojure.core/when-not (> (clojure.core/deref x) 10)
                         (println "***")
                         (clojure.core/when-not (> (clojure.core/deref x) 10)
                           (recur)))))))
              (macroexpand-1
                '(loop-and-exit-when (> @x 10)
                   (println "*")
                   (swap! x inc)
                   (println "**")
                   (swap! x (partial + 2))
                   (println "***")))))
  (test-is (= "*\n**\n***\n*\n**\n***\n*\n**\n***\n*\n"
              (with-out-str
                (def x (atom 1))
                (loop-and-exit-when (> @x 10)
                  (println "*")
                  (swap! x inc)
                  (println "**")
                  (swap! x (partial + 2))
                  (println "***"))))))

;==========================================================
(deftest test-powo
  (test-is (= '(:yes)
              (run 1 [q] (powo 3 2 9) (=== q :yes))))
  (test-is (= '(32)
              (run 1 [q] (powo 2 5 q))))
  (test-is (= '(5)
              (run 1 [q] (powo q 2 25))))
  (test-is (= '(3)
              (run 1 [q] (powo 2 q 8))))
  (test-is (= '(1)
              (run 1 [q] (powo q q q))))
  (test-is (= #{[64 1] [8 2] [4 3] [2 6]}
              (set (run* [a b] (powo a b 64)))))
  (test-is (= '(_0)
              (run 1 [q] (powo q 0 1))))
  (test-is (= (set (range 101))
              (set (run* [q]
                     (fd/in q (fd/interval 0 100))
                     (powo q 1 q))))))

;==========================================================
(deftest test-step
  (test-is (= [[0 0 0]
               [0 1 1]
               [0 1 0]]
              (step 0 [[0 0 0]
                       [0 1 1]
                       [0 1 0]])))
  (test-is (= [[0 0 0 0 0 0]
               [0 1 1 0 0 0]
               [0 1 0 0 0 0]
               [0 0 0 0 1 0]
               [0 0 0 1 1 0]
               [0 0 0 0 0 0]]
              (step 1 [[0 0 0 0 0 0]
                       [0 1 1 0 0 0]
                       [0 1 1 0 0 0]
                       [0 0 0 1 1 0]
                       [0 0 0 1 1 0]
                       [0 0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 1 0 0]
               [0 0 1 0 0]
               [0 0 1 0 0]
               [0 0 0 0 0]]
              (step 1 [[0 0 0 0 0]
                       [0 0 0 0 0]
                       [0 1 1 1 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 0 0 0]
               [0 1 1 1 0]
               [0 0 0 0 0]
               [0 0 0 0 0]]
              (step 2 [[0 0 0 0 0]
                       [0 0 0 0 0]
                       [0 1 1 1 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0 0]
               [0 0 0 1 0 0]
               [0 1 0 0 1 0]
               [0 1 0 0 1 0]
               [0 0 1 0 0 0]]
              (step 1 [[0 0 0 0 0 0]
                       [0 0 0 0 0 0]
                       [0 0 1 1 1 0]
                       [0 1 1 1 0 0]
                       [0 0 0 0 0 0]])))
  (test-is (= [[1 0 0 1]
               [0 0 0 0]
               [0 0 0 0]
               [1 0 0 1]]
              (step 1 [[1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]])))
  (test-is (= [[0 0 0 0]
               [0 0 0 0]
               [0 0 0 0]
               [0 0 0 0]]
              (step 2 [[1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]
                       [1 1 1 1]])))
  (test-is (= [[0 0 0 0 0]
               [1 0 1 0 0]
               [0 1 1 0 0]
               [0 1 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]]
              (step 1 [[0 1 0 0 0]
                       [0 0 1 0 0]
                       [1 1 1 0 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 1 0]
               [0 0 0 0 1]
               [0 0 1 1 1]
               [0 0 0 0 0]]
              (step 8 [[0 1 0 0 0]
                       [0 0 1 0 0]
                       [1 1 1 0 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]
                       [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 1]
               [0 0 0 1 1]]
              (step 12 [[0 1 0 0 0]
                        [0 0 1 0 0]
                        [1 1 1 0 0]
                        [0 0 0 0 0]
                        [0 0 0 0 0]
                        [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 1 1]
               [0 0 0 1 1]]
              (step 13 [[0 1 0 0 0]
                        [0 0 1 0 0]
                        [1 1 1 0 0]
                        [0 0 0 0 0]
                        [0 0 0 0 0]
                        [0 0 0 0 0]])))
  (test-is (= [[0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 0 0]
               [0 0 0 1 1]
               [0 0 0 1 1]]
              (step 666 [[0 1 0 0 0]
                         [0 0 1 0 0]
                         [1 1 1 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]]))))

;==========================================================
(run-tests)

