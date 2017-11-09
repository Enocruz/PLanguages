(defmacro my-comment
  [arg]
  nil)

(defmacro my-and
  ([] true)
  ([x] x)
  ([x & y]
   `(let [t# ~x]
      (if t#
        (my-and ~@y)
        t#))))


"
(list 'if x
 (list 'if y true false)
 ~x)))
))

`(if ~x (if ~y true false) false)))
(macroexpand-1 '(my-comment 4))"
