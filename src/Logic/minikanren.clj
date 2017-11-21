(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

(defn lasto
  [lst result]
  (conde
    [(=== lst [result])]
    [(fresh [head tail]
       (conso head tail lst)
       (lasto tail result))]))

(defn reverseo
  [lst result]
  (conde
    [(=== lst ())
     (=== result ())]


    [(fresh [head tail temp]
       (conso head tail lst)
       (reverseo tail temp)
       (appendo temp [head] result))]))

(defn dupo
  [lst result]
  (conde
    [(=== lst ())
     (=== result ())]

    [(fresh [head tail temp]
       (conso head tail lst)
       (appendo [head head] temp result)
       (dupo tail temp))]))

(defn addo
  [lst result]
  (conde
    [(=== lst [])
     (=== result 0)]

    [(fresh [head tail temp]
            (conso head tail lst)
            (addo tail temp)
            (fd/+ head temp result))]))


(defn ms
  []
  (run*  [q1 q2 q3
          q4 q5 q6
          q7 q8 q9]
       (fd/in q1 q2 q3 q4 q5 q6 q7 q8 q9 (fd/interval 1 9))
       (distincto [q1 q2 q3 q4 q5 q6 q7 q8 q9])
       (addo [q1 q2 q3] 15)
       (addo [q4 q5 q6] 15)
       (addo [q7 q8 q9] 15)
       (addo [q1 q4 q7] 15)
       (addo [q2 q5 q8] 15)
       (addo [q3 q6 q9] 15)
       (addo [q1 q5 q9] 15)
       (addo [q7 q5 q3] 15)))