"
Brandon Alain Cruz Ruiz A01375640
Oscar Allan Ruiz Toledo A01376200
Problem Set: MiniKanren
November 26, 2017
"

(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

"1"
(defn removeo
  "This logic function succeeds if it’s able to remove the first
  occurrence of x from lst giving result."
  [x lst result]
  (conde
    [(fresh [head tail]
       (conso head tail lst)
       (=== head x)
       (=== result tail))]

    [(fresh [head tail temp]
       (conso head tail lst)
       (removeo x tail temp)
       (appendo [head] temp result))]))

"3"
(defn evensizeo
  "These two mutually recursive logic functions succeed if the
  number of elements in lst is even or odd, respectively."
  [lst]
  (conde
    [(=== lst ())]
    [(fresh [h1 h2 t1 t2]
            (conso h1 t1 lst)
            (conso h2 t2 t1)
            (evensizeo t2))]))
(defn oddsizeo
  "These two mutually recursive logic functions succeed if the
  number of elements in lst is even or odd, respectively."
  [lst]
  (conde
    [(fresh [x]
            (=== lst [x]))]
    [(fresh [h1 h2 t1 t2]
            (conso h1 t1 lst)
            (conso h2 t2 t1)
            (oddsizeo t2))]))

"5"
(defn converto
  "This logic function succeeds when digit d corresponds to the
  keyword k (for example digit 7 with keyword :seven)."
  [d k])

"7"
(defn splito
  "This logic function succeeds when splitting lst gives a and b.
   The first, third, fifth, etc. elements of lst go to a, while
   the second, fourth, sixth, etc. elements go to b."
  [lst a b]
  (conde
    [(=== lst ())
     (=== a ())
     (=== b ())]

    [(fresh [x]
            (=== lst [x])
            (=== a [x])
            (=== b ()))]

    [(fresh [h1 h2 t1 t2 temp temp2]
       (conso h1 t1 lst)
       (conso h2 t2 t1)
       (appendo [h1] temp a)
       (appendo [h2] temp2  b)
       (splito t2 temp temp2))]))

"9"
(defn counto
  "This logic function unifies result with the number of elements
  contained in lst."
  [lst result]
  (conde
    [(=== lst ())
     (=== result 0)]

    [(fresh [head tail temp]
            (conso head tail lst)
            (fd/+ temp 1 result)
            (counto tail temp))]))

"Tests 1"
(deftest tes-removeo
  (test-is (= [[:b :c :d :e]]
              (run 1 [q] (removeo :a [:a :b :c :d :e] q))))
  (test-is (= [[:a :b :d :e]]
              (run 1 [q] (removeo :c [:a :b :c :d :e] q))))
  (test-is (= [:d]
              (run 1 [q]
                   (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (test-is (= []
              (run 1 [q] (removeo :x [:a :b :c :d :e] q))))
  (test-is (= [[:x :a :b :c :d :e]
               [:a :x :b :c :d :e]
               [:a :b :x :c :d :e]
               [:a :b :c :x :d :e]
               [:a :b :c :d :x :e]
               [:a :b :c :d :e :x]]
              (run 6 [q] (removeo :x q [:a :b :c :d :e]))))
  (test-is (= [[:a [:b :c :d :e]]
               [:b [:a :c :d :e]]
               [:c [:a :b :d :e]]
               [:d [:a :b :c :e]]
               [:e [:a :b :c :d]]]
              (run* [q1 q2]
                    (removeo q1 [:a :b :c :d :e] q2)))))

"Tests 3"
(deftest test-evensizeo-oddsizeo
  (test-is (= [:yes]
              (run 1 [q] (evensizeo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (oddsizeo [:x]) (=== q :yes))))
  (test-is (= []
              (run 1 [q] (evensizeo [:x]) (=== q :yes))))
  (test-is (= []
              (run 1 [q] (oddsizeo []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (evensizeo [:a :b :c :d :e :f]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (oddsizeo [:a :b :c :d :e]) (=== q :yes))))
  (test-is (= '[[]
                [_0 _1]
                [_0 _1 _2 _3]
                [_0 _1 _2 _3 _4 _5]
                [_0 _1 _2 _3 _4 _5 _6 _7]]
              (run 5 [q] (evensizeo q))))
  (test-is (= '[[_0]
                [_0 _1 _2]
                [_0 _1 _2 _3 _4]
                [_0 _1 _2 _3 _4 _5 _6]
                [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
              (run 5 [q] (oddsizeo q)))))
"Test 5"
(deftest test-converto
  (test-is (= [:yes]
              (run 1 [q] (converto 0 :zero) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (converto 0 :zero)
                   (converto 1 :one)
                   (converto 2 :two)
                   (converto 3 :three)
                   (converto 4 :four)
                   (converto 5 :five)
                   (converto 6 :six)
                   (converto 7 :seven)
                   (converto 8 :eight)
                   (converto 9 :nine)
                   (=== q :yes))))
  (test-is (= []
              (run 1 [q] (converto 12 :twelve) (=== q :yes))))
  (test-is (= [7]
              (run 1 [q] (converto q :seven))))
  (test-is (= [:seven]
              (run 1 [q] (converto 7 q))))
  (test-is (= [[1 :two 3]]
              (run 1 [q1 q2 q3]
                   (converto q1 :one)
                   (converto 2 q2)
                   (converto q3 :three)))))

"7"
(deftest test-splito
  (test-is (= [:yes]
              (run 1 [q] (splito [] [] []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (splito [:a] [:a] []) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b] [:a] [:b]) (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b :c :d :e :f]
                           [:a :c :e]
                           [:b :d :f])
                   (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q]
                   (splito [:a :b :c :d :e :f :g]
                           [:a :c :e :g]
                           [:b :d :f])
                   (=== q :yes))))
  (test-is (= [[[:a :c :e] [:b :d :f]]]
              (run 1 [q1 q2]
                   (splito [:a :b :c :d :e :f] q1 q2))))
  (test-is (= [[:a :b :c :d :e :f :g]]
              (run 1 [q] (splito q [:a :c :e :g] [:b :d :f]))))
  (test-is (= '[[[] [] []]
                [[_0] [_0] []]
                [[_0 _1] [_0] [_1]]
                [[_0 _1 _2] [_0 _2] [_1]]
                [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
                [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
                [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
              (run 7 [q1 q2 q3] (splito q1 q2 q3)))))
