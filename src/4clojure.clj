(defn x [f lst]
  (loop [a lst result []]
    (cond
      (empty? a) result
      :else (recur (rest a) (conj result
                                  (f (first a)))))))