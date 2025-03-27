(ns vrac.dsl.macro
  (:refer-clojure :exclude [destructure])
  (:require [mate.core :as mc]))

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

(defn- destructure [bindings]
  (let [pairs (partition 2 bindings)]
    (if (every? (mc/comp-> first symbol?) pairs)
      bindings
      (let [destruct (fn destruct [[k-form v-form]]
                       (cond
                         ;; Sequential Destructuring
                         (vector? k-form)
                         (let [[k-form v-symb] (if (= (-> k-form pop peek) :as)
                                                 [(-> k-form pop pop) (peek k-form)]
                                                 [k-form (gensym "vec__")])
                               [k-form &-symb] (if (= (-> k-form pop peek) '&)
                                                 [(-> k-form pop pop) (peek k-form)]
                                                 [k-form nil])]
                           (-> (into [v-symb v-form]
                                     (mc/mapcat-indexed (fn [index k]
                                                          (if (simple-symbol? k)
                                                            [k `(nth ~v-symb ~index)]
                                                            ;; map or vector
                                                            (destruct [k `(nth ~v-symb ~index)]))))
                                     k-form)
                               (cond->
                                 (some? &-symb)
                                 (conj &-symb `(drop ~(count k-form) ~v-symb)))))

                         ;; Associative Destructuring
                         (map? k-form)
                         (let [v-symb (or (:as k-form) (gensym "map__"))
                               or-hashmap (:or k-form)]
                           (into [v-symb v-form]
                                 (mapcat (fn [[k v]]
                                           (cond
                                             (contains? #{:as :or} k)
                                             nil

                                             (= :& k)
                                             (let [kws (->> k-form
                                                            (mapcat (fn [[k v]]
                                                                      (cond
                                                                        (simple-symbol? k)
                                                                        [v]

                                                                        (and (keyword? k)
                                                                             (= (name k) "keys"))
                                                                        (->> v
                                                                             (mapv (fn [s]
                                                                                     (keyword (or (namespace s) (namespace k))
                                                                                              (name s))))))))
                                                            distinct)]
                                               [v `(dissoc ~v-symb ~@kws)])

                                             (and (keyword? k)
                                                  (= (name k) "keys"))
                                             (let [k-ns (namespace k)
                                                   symbols v]
                                               (into []
                                                     (mapcat (fn [s]
                                                               (let [s-name (name s)
                                                                     s-simple (symbol s-name)
                                                                     s-ns (namespace s)
                                                                     s-keyword (keyword (or s-ns k-ns) s-name)
                                                                     default (when (contains? or-hashmap s-simple)
                                                                               [(or-hashmap s-simple)])]
                                                                 [s-simple `(~s-keyword ~v-symb ~@default)])))
                                                     symbols))

                                             (simple-symbol? k)
                                             (let [default (when (contains? or-hashmap k)
                                                             [(or-hashmap k)])]
                                               [k `(~v ~v-symb ~@default)])

                                             :else ;; map or vector
                                             (destruct [k `(~v ~v-symb)]))))
                                 k-form))

                         :else ;; No destructuring
                         [k-form v-form]))]
        (into [] (mapcat destruct) pairs)))))

#_ (destructure '[[a b c & rest :as d] x])

#_ (destructure '[{a         :aa
                   b         :bb
                   :keys     [i j foo/k foo/l]
                   :bar/keys [p q foo/r foo/s]
                   :or       {b :bb-default
                              j :jj-default
                              l :ll-default
                              q :qq-default
                              s :ss-default}
                   :as       xx} x])

#_ (destructure '[{a :a
                   :keys [b c]
                   :foo/keys [bar baz]
                   :& rest
                   :as x} y])

#_ (destructure '[{a        :aa
                   {c   :cc
                    d   :dd
                    :as bb} :bb
                   :as      xx} x])

#_ (destructure '[{[a1 a2]      :aa
                   {[c1 c2] :cc
                    :as     bb} :bb
                   :as          xx} x])

#_ (destructure '[[{a :a} {b :b} :as xx] x])


(defn expand-let-bindings [[_ bindings & bodies :as original-form]]
  (let [destructured-bindings (destructure bindings)]
    (if (identical? destructured-bindings bindings)
      original-form
      `(let ~destructured-bindings ~@bodies))))

#_ (expand-let-bindings `(let [~'{a :a} {:a 1}] ~'a))

(defn expand-for-bindings [[_ bindings body :as original-form]]
  (let [new-bindings (->> (partition 2 bindings)
                          (mapcat (fn [[k v]]
                                    (case k
                                      :let [:let (destructure v)]
                                      :when [:when v]
                                      :while [:while v]
                                      (if (simple-symbol? k)
                                        [k v]
                                        (let [item-symb (gensym "item__")]
                                          [item-symb v
                                           :let (destructure [k item-symb])])))))
                          vec)]
    (if (and (= (count new-bindings) (count bindings))
             (->> (map vector new-bindings bindings)
                  (every? (fn [[new-x x]] (identical? new-x x)))))
      original-form
      `(for ~new-bindings ~body))))

#_ (expand-for-bindings `(for [~'[a b] ~[[1 2] [3 4]]
                               :let [~'[c d] ~'[10 20]]]
                           ~'[a b c d]))

(def macros
  {`->   thread-first
   `->>  thread-last
   `as-> thread-as
   `let  expand-let-bindings
   `for  expand-for-bindings})
