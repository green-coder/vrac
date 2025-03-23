(ns vrac.dsl.macro)

(defn thread-first [[_ x & forms]]
  (loop [x x
         forms (seq forms)]
    (if (nil? forms)
      x
      (let [form (first forms)]
        (recur (if (seq? form)
                 (-> `(~(first form) ~x ~@(next form))
                     (with-meta (meta form)))
                 `(~form ~x))
               (next forms))))))

#_ (thread-first `(-> x (+ 2) prn))

(defn thread-last [[_ x & forms]]
  (loop [x x
         forms (seq forms)]
    (if (nil? forms)
      x
      (let [form (first forms)]
        (recur (if (seq? form)
                 (-> `(~@form ~x)
                     (with-meta (meta form)))
                 `(~form ~x))
               (next forms))))))

#_ (thread-last `(->> x (+ 2) prn))

(defn let-do [[_ bindings & bodies :as original-form]]
  (if (> (count bodies) 1)
    `(let ~bindings
       (do ~@bodies))
    original-form))

#_ (let-do `(let [a 1] a))
#_ (let-do `(let [a 1] a a))

(defn when-do [[_ cond & bodies :as original-form]]
  (if (> (count bodies) 1)
    `(when ~cond
       (do ~@bodies))
    original-form))

#_ (when-do `(when true 1))
#_ (when-do `(when true 1 2))

(def macros
  {`-> thread-first
   `->> thread-last
   `let let-do
   `when when-do})
