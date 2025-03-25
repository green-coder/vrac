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

(defn thread-as [[_ expr name & forms]]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

#_ (thread-as `(as-> (+ 1 2) y
                 (+ y 3)
                 (+ y 4)))

(def macros
  {`-> thread-first
   `->> thread-last
   `as-> thread-as})
