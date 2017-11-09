"Brandon Alain Cruz Ruiz A01375640
 Oscar Allan Ruiz Toledo A01376200
 Macros
 November 10, 2017"

(defmacro my-or
  "Evaluates its expressions one at a time, from left to right.
  If a form returns a logical true value, it returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & args]
   `(let [t# ~x]
      (if t#
        t#
        (my-or ~@args)))))

(defn between-keywords
  [start end lst]
  (->>
    (drop-while #(not= start %) lst)
    rest
    (take-while #(not= end %))))

(defmacro IF
  [condition & args]
  `(if ~condition
     (do ~@(between-keywords :THEN :ELSE args))
     (do ~@(between-keywords :ELSE :THEN args))))


