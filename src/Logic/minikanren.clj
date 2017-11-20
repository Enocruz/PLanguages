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

