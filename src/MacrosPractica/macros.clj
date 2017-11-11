"Brandon Alain Cruz Ruiz A01375640
 Oscar Allan Ruiz Toledo A01376200
 Macros
 November 10, 2017"

"1"
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

"Auxiliary function"
"If the condition contains while returns true, otherwise return false"
(defn while?
  [x]
  (if (contains? (set x) :while) true false))

"If the condition contains a while, perform this macro"
(defmacro rep-while
  [cond & args]
  `(loop []
     (when (eval ~(last cond))
       (do ~@args)
       (recur))))

"If the condition contains a until, perform this macro"
(defmacro rep-until
  [cond & args]
  `(loop []
     (when (not (eval ~(last cond)))
       (do ~@args)
       (recur))))

"2"
(defmacro do-loop
  "Implements a post-test loop control statement. It must combine the
  functionality of C's do-while statement and Pascal's repeat-until statement."
  [& body]
  `(if (while? (quote ~(last body)))
    (rep-while ~(last body) ~@(butlast body))
    (rep-until ~(last body) ~@(butlast body))))

"3"
(defmacro def-pred
  "Takes a name, an arg vector, and a body of one or more expressions.
  The macro should define two predicate functions: a regular one and its
  negated version. The name of the negated predicate should be the same
  as name but with a 'not-' prefix, and its result should be negated using
  the not function"
  [name vector & args]
  `(do (defn ~name ~vector ~@args)
       (defn ~(symbol (str "not-" name)) ~vector (not (do ~@args)))))
"
(defmacro a
  [x]
  `(fn ~(vector (first x))))
"
"4"
(defmacro defn-curry
  [name args & body]
  `(defn ~name
     ~(vector (first args))))

"5"
(defn between-keywords
  [start end lst]
  (->>
    (drop-while #(not= start %) lst)
    rest
    (take-while #(not= end %))))

(defmacro IF
  "Write a macro called IF. Its purpose is to provide a conditional
  statement that is syntactically a bit more similar to those found
  in languages like Pascal or Fortran."
  [condition & args]
  `(if ~condition
     (do ~@(between-keywords :THEN :ELSE args))
     (do ~@(between-keywords :ELSE :THEN args))))


