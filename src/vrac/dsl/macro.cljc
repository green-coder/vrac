(ns vrac.dsl.macro
  (:require [clojure.walk :as walk]))

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

(defn expand-let [[_ bindings & bodies :as original-form]]
  ;; TODO: expand the destructurations in the let bindings.
  (if (> (count bodies) 1)
    `(let ~bindings
       (do ~@bodies))
    original-form))

#_ (let-do `(let [a 1] a))
#_ (let-do `(let [a 1] a a))

(defn expand-when [[_ cond & bodies :as original-form]]
  (if (> (count bodies) 1)
    `(when ~cond
       (do ~@bodies))
    original-form))

#_ (when-do `(when true 1))
#_ (when-do `(when true 1 2))

(defn expand-for [[:as original-form]]
  ;; TODO: expand the destructurations in the let bindings.
  ,)

(def macros
  {`-> thread-first
   `->> thread-last
   `let expand-let
   `when expand-when
   `for expand-for})

(defn dsl-expand [dsl-form]
  (walk/prewalk (fn [x]
                  (if (and (seq? x)
                           (seq x))
                    (let [macro-fn-symbol (first x)]
                      (if (contains? macros macro-fn-symbol)
                        (let [macro-fn (-> macro-fn-symbol macros)
                              expanded-x (macro-fn x)]
                          (if (identical? expanded-x x)
                            x
                            (recur expanded-x)))
                        x))
                    x))
                dsl-form))

#_ (dsl-expand `(-> x (+ 2) prn))
