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

"2"
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

"4"
"Creating functions (Auxiliar function)"
(defn functions
  [args body]
  (if (empty? args)
    `(do ~@body)
    `(fn ~(vector (first args))
       ~(functions (rest args) body))))

(defmacro defn-curry
  "Performs a currying transformation to a function definition.
  It takes as parameters a name, an args vector, and a body of one
  or more expressions. The macro defines a function called name
  that takes only the first argument from args and returns a function
  that takes the second argument from args and returns a function
  that takes the third argument from args, and so on. The last
  function returned takes the last argument from args and
  evaluates all the expressions in body using a do special form"
  [name args & body]
  (cond
    (> (count args) 1)
    `(defn ~name
         [~(first args)]
         ~(functions (rest args) body))
    (= 1 (count args))
    `(defn ~name
       [~(first args)]
       (do ~@body))
    :else
      `(defn ~name
           []
           (do ~@body))))

"5"
"Auxiliar function for macro IF"
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


