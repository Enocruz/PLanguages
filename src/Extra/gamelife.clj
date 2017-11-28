;----------------------------------------------------------
; Conway’s Game of Life
; Date: November 28, 2017.
; Author: Oscar Allan Ruiz Toledo
;----------------------------------------------------------


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
