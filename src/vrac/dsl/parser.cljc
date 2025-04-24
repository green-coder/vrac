(ns ^:no-doc vrac.dsl.parser
  #?(:cljs (:require-macros [vrac.dsl.parser]))
  (:require [mate.core :as mc]
            [vrac.dsl :as dsl]
            [vrac.dsl.macro :as macro]))

;; Those are not functions or vars, they can't be resolved, so
;; we treat them as special cases which resolve to themselves.
(def ^:private clj-reserved-symbols
  #{'do
    'if
    'when
    'quote})

(defn- rename-symb-ns [x rename-map]
  (if (symbol? x)
    (let [x-ns (namespace x)
          x-name (name x)
          x-name (rename-map x-name x-name)]
      (symbol x-ns x-name))
    x))

(defn symbol-resolver [ns env symb-ns-rename-map]
  (fn [x]
    #?(:clj
       (when-some [resolved-x-var (ns-resolve ns env x)]
         (cond-> (symbol resolved-x-var)
           (some? symb-ns-rename-map) (rename-symb-ns symb-ns-rename-map))))))

(defn resolve-and-macro-expand-dsl
  "Macro-expand and resolve the symbols in the DSL."
  [x resolve-symbol macros]
  (let [resolve-var (fn [x local-vars]
                      (or (when (contains? clj-reserved-symbols x) x)
                          (when (contains? macros x) x)
                          (when (contains? local-vars x) x)
                          (resolve-symbol x)
                          (symbol (namespace x) (str (name x) "-not-found"))))
        resolve-and-expand (fn resolve-and-expand [x local-vars]
                             (cond
                               (seq? x)
                               (if (zero? (count x))
                                 x
                                 (let [[f & args] x
                                       expanded-f (resolve-and-expand f local-vars)
                                       x (cons expanded-f args)
                                       macro-fn (get macros expanded-f)
                                       expanded-form (if (nil? macro-fn)
                                                       x
                                                       (macro-fn x))]
                                   (if-not (identical? x expanded-form)
                                     (recur expanded-form local-vars)
                                     (let [[f & args] x]
                                       ;;(prn [:expanded x])
                                       (cond
                                         ;; (defn ,,, [,,,] ,,,)
                                         (= f 'defn)
                                         (let [[fn-name params body] args
                                               local-vars (into local-vars params)
                                               body (resolve-and-expand body local-vars)]
                                           `(~'defn ~fn-name ~params ~body))

                                         ;; (let [,,,] ,,,)
                                         (= f 'let)
                                         (let [[bindings body] args
                                               [bindings local-vars] (reduce (fn [[bindings local-vars] [symbol-form value-form]]
                                                                               [(conj bindings symbol-form (resolve-and-expand value-form local-vars))
                                                                                (conj local-vars symbol-form)])
                                                                             [[] local-vars]
                                                                             (partition 2 bindings))
                                               body (resolve-and-expand body local-vars)]
                                           `(~'let ~bindings ~body))

                                         ;; (for [,,,] ,,,)
                                         (= f 'for)
                                         (let [[bindings body] args
                                               [bindings local-vars] (reduce (fn [[bindings local-vars] [symbol-form value-form]]
                                                                               (case symbol-form
                                                                                 :let (let [[let-bindings local-vars] (reduce (fn [[bindings local-vars] [symbol-form value-form]]
                                                                                                                                [(conj bindings symbol-form (resolve-and-expand value-form local-vars))
                                                                                                                                 (conj local-vars symbol-form)])
                                                                                                                              [[] local-vars]
                                                                                                                              (partition 2 value-form))]
                                                                                        [(conj bindings :let let-bindings)
                                                                                         local-vars])
                                                                                 (:when :while) [(conj bindings symbol-form (resolve-and-expand value-form local-vars))
                                                                                                 local-vars]
                                                                                 [(conj bindings symbol-form (resolve-and-expand value-form local-vars))
                                                                                  (conj local-vars symbol-form)]))
                                                                             [[] local-vars]
                                                                             (partition 2 bindings))
                                               body (resolve-and-expand body local-vars)]
                                           `(~'for ~bindings ~body))

                                         ;; (quote ,,,)
                                         (= f 'quote)
                                         x ; "You can't touch this"

                                         ;; generic (f a b c)
                                         :else
                                         `(~expanded-f ~@(map (mc/partial-> resolve-and-expand local-vars) args)))))))

                               (symbol? x)
                               (resolve-var x local-vars)

                               (vector? x)
                               (mapv (mc/partial-> resolve-and-expand local-vars) x)

                               (map? x)
                               (-> x
                                   (update-keys (mc/partial-> resolve-and-expand local-vars))
                                   (update-vals (mc/partial-> resolve-and-expand local-vars)))

                               (set? x)
                               (set (map (mc/partial-> resolve-and-expand local-vars) x))

                               :else
                               x))]
    (resolve-and-expand x #{})))

(defmacro expand-dsl [dsl-form]
  #?(:clj
     (let [ns *ns*
           env &env
           is-compiling-cljs-code (some? (:ns env))
           resolve-symbol (symbol-resolver ns env (when is-compiling-cljs-code {"cljs.core" "clojure.core"}))]
       `'~(resolve-and-macro-expand-dsl dsl-form
                                        resolve-symbol
                                        macro/default-macros))))

(defn dsl->ast [x]
  "Shallow transformation from a DSL expression to an AST."
  (cond

    (seq? x)
    (if (zero? (count x))
      {:node-type :clj/value
       :value ()}
      (let [[f & args] x]
        (cond
          ;; (defn fn-name params body)
          (= f 'defn)
          (let [[fn-name params body] args]
            {:node-type :clj/defn
             :fn-name fn-name
             :params (->> params
                          (mapv (fn [symbol]
                                  (let [metadata (meta symbol)]
                                    (-> {:node-type :clj/fn-param
                                         :symbol symbol}
                                        (cond-> (seq metadata) (assoc :metadata metadata)))))))
             :body (dsl->ast body)})

          ;; (let [,,,] ,,,)
          (= f 'let)
          (let [[bindings body] args]
            {:node-type :clj/let
             :bindings (->> bindings
                            (partition 2)
                            (mapv (fn [[symbol value]]
                                    {:node-type :clj/let-binding
                                     :symbol symbol
                                     :value (dsl->ast value)})))
             :body (dsl->ast body)})

          ;; (do ,,,)
          (= f 'do)
          (let [bodies args]
            {:node-type :clj/do
             :bodies (mapv dsl->ast bodies)})

          ;; (if cond then ?else)
          (= f 'if)
          (case (count args)
            2 (let [[cond then] args]
                {:node-type :clj/when
                 :cond (dsl->ast cond)
                 :body (dsl->ast then)})
            3 (let [[cond then else] args]
                {:node-type :clj/if
                 :cond (dsl->ast cond)
                 :then (dsl->ast then)
                 :else (dsl->ast else)}))

          ;; (quote x)
          (= f 'quote)
          {:node-type :clj/value
           :value x}

          ;; (when cond body)
          (= f 'when)
          (let [[cond body] args]
            {:node-type :clj/when
             :cond (dsl->ast cond)
             :body (dsl->ast body)})

          ;; (with-context ,,,)
          (= f `dsl/with-context)
          (let [[context body] args]
            {:node-type :dsl/with-context
             :context (dsl->ast context)
             :body (dsl->ast body)})

          ;; (for [,,,] ,,,)
          (= f 'for)
          (let [[bindings body] args]
            {:node-type :clj/for
             :bindings  (->> bindings
                             (partition 2)
                             (mapv (fn [[symbol value]]
                                     (case symbol
                                       :let {:node-type :clj/for-let
                                             :bindings  (->> value
                                                             (partition 2)
                                                             (mapv (fn [[symbol value]]
                                                                     {:node-type :clj/let-binding
                                                                      :symbol symbol
                                                                      :value (dsl->ast value)})))}
                                       :when {:node-type :clj/for-when
                                              :cond      (dsl->ast value)}
                                       :while {:node-type :clj/for-while
                                               :cond      (dsl->ast value)}
                                       {:node-type :clj/for-iteration
                                        :symbol    symbol
                                        :value     (dsl->ast value)}))))
             :body      (dsl->ast body)})

          ;; signal
          (= f `dsl/signal)
          (let [[body] args]
            {:node-type :dsl/signal
             :body (dsl->ast body)})

          ;; state
          (= f `dsl/state)
          (let [[body] args]
            {:node-type :dsl/state
             :body (dsl->ast body)})

          ;; memo
          (= f `dsl/memo)
          (let [[body] args]
            {:node-type :dsl/memo
             :body (dsl->ast body)})

          ;; once
          (= f `dsl/once)
          (let [[body] args]
            {:node-type :dsl/once
             :body (dsl->ast body)})

          ;; effect
          (= f `dsl/effect)
          (let [[body] args]
            {:node-type :dsl/effect
             :body (dsl->ast body)})

          ;; effect-on
          (= f `dsl/effect-on)
          (let [[triggers body] args]
            (assert (vector? triggers) "The triggers should be a vector literal.")
            {:node-type :dsl/effect-on
             :triggers (mapv dsl->ast triggers)
             :body (dsl->ast body)})

          :else
          {:node-type :clj/invocation
           :function (dsl->ast f)
           :args (mapv dsl->ast args)})))

    (set? x)
    {:node-type :clj/set
     :items (mapv dsl->ast x)}

    (vector? x)
    {:node-type :clj/vector
     :items (mapv dsl->ast x)}

    (map? x)
    {:node-type :clj/map
     :entries (->> x
                   (mapv (fn [[k v]]
                           {:node-type :clj/map-entry
                            :key (dsl->ast k)
                            :value (dsl->ast v)})))}

    (= x `dsl/global)
    {:node-type :dsl/global}

    (= x `dsl/context)
    {:node-type :dsl/context}

    (symbol? x)
    {:node-type :clj/var
     :symbol x}

    :else
    {:node-type :clj/value
     :value x}))

;; This is potentially useful for making the tests more humane.
(defn ast->dsl [x]
  (case (:node-type x)
    :clj/fn
    `(~'fn ~@(when-some [fn-name (:fn-name x)] [fn-name])
       ~(mapv ast->dsl (:params x))
       ~(ast->dsl (:body x)))

    :clj/defn
    `(~'defn ~(:fn-name x)
       ~(mapv ast->dsl (:params x))
       ~(ast->dsl (:body x)))

    :clj/fn-param
    (:symbol x) ;; TODO: add the metadata

    :clj/let
    `(~'let ~(into [] (mapcat ast->dsl) (:bindings x))
       ~(ast->dsl (:body x)))

    :clj/let-binding
    [(:symbol x) (ast->dsl (:value x))]

    :clj/do
    `(~'do ~@(mapv ast->dsl (:bodies x)))

    :clj/if
    `(~'if ~(ast->dsl (:cond x))
       ~(ast->dsl (:then x))
       ~(ast->dsl (:else x)))

    :clj/when
    `(~'when ~(ast->dsl (:cond x))
       ~(ast->dsl (:body x)))

    :clj/for
    `(~'for ~(into [] (mapcat ast->dsl) (:bindings x))
       ~(ast->dsl (:body x)))

    :clj/for-iteration
    [(:symbol x) (ast->dsl (:value x))]

    :clj/for-let
    [:let (into [] (mapcat ast->dsl) (:bindings x))]

    :clj/for-when
    [:when (ast->dsl (:cond x))]

    :clj/for-while
    [:while (ast->dsl (:cond x))]

    :clj/invocation
    (list* (ast->dsl (:function x))
           (map ast->dsl (:args x)))

    :clj/var
    (:symbol x)

    :clj/value
    (:value x)

    :clj/set
    (into #{} (map ast->dsl) (:items x))

    :clj/vector
    (into [] (map ast->dsl) (:items x))

    :clj/map
    (into {} (map ast->dsl) (:entries x))

    :clj/map-entry
    [(ast->dsl (:key x)) (ast->dsl (:value x))]

    :dsl/global
    `dsl/global

    :dsl/context
    `dsl/context

    :dsl/with-context
    :not-implemented-yet

    :dsl/once
    `(dsl/once ~(ast->dsl (:body x)))

    :dsl/signal
    `(dsl/signal ~(ast->dsl (:body x)))

    :dsl/state
    `(dsl/state ~(ast->dsl (:body x)))

    :dsl/memo
    `(dsl/memo ~(ast->dsl (:body x)))

    :dsl/effect
    `(dsl/effect ~(ast->dsl (:body x)))

    :dsl/effect-on
    `(dsl/effect-on ~(mapv ast->dsl (:triggers x))
                    ~(ast->dsl (:body x)))))
