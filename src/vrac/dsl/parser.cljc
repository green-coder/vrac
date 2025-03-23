(ns vrac.dsl.parser
  (:require [mate.core :as mc]
            [lambdaisland.deep-diff2 :refer [diff]]
            [vrac.dsl :as-alias dsl]))

;; Shallow transformation from a DSL expression to an AST.
(defn dsl->ast [x]
  (cond

    (seq? x)
    (if (zero? (count x))
      {:node-type :clj/value
       :value ()}
      (let [[f & args] x]
        (cond
          ;; (let [,,,] ,,,)
          (= f `let)
          (let [[bindings & bodies] args]
            {:node-type :clj/let
             :bindings (->> bindings
                            (partition 2)
                            (mapv (fn [[symbol value]]
                                    {:node-type :clj/let-binding
                                     :symbol symbol
                                     :value (dsl->ast value)})))
             :bodies (mapv dsl->ast bodies)})

          ;; (do ,,,)
          (= f `do)
          (let [bodies args]
            {:node-type :clj/do
             :bodies (mapv dsl->ast bodies)})

          ;; (if cond then ?else)
          (= f `if)
          (let [[cond then else] args]
            {:node-type :clj/if
             :cond (dsl->ast cond)
             :then (dsl->ast then)
             :else (dsl->ast else)})

          ;; (when cond & bodies)
          (= f `when)
          (let [[cond & bodies] args]
            {:node-type :clj/when
             :cond (dsl->ast cond)
             :bodies (mapv dsl->ast bodies)})

          ;; (with-context ,,,)
          (= f `dsl/with-context)
          (let [[context & bodies] args]
            {:node-type :dsl/with-context
             :context (dsl->ast context)
             :bodies (mapv dsl->ast bodies)})

          ;; (for [,,,] ,,,)
          (= f `for)
          (let [[bindings body] args]
            {:node-type     :clj/for
             :bindings (->> bindings
                            (partition 2)
                            (mapv (fn [[symbol value]]
                                    (case symbol
                                      :let {:node-type     :clj/for-let
                                            :bindings (->> value
                                                           (partition 2)
                                                           (mapv (fn [[symbol value]]
                                                                   {:node-type :clj/let-binding
                                                                    :symbol symbol
                                                                    :value (dsl->ast value)})))}
                                      :when {:node-type   :clj/for-when
                                             :symbol symbol
                                             :value  (dsl->ast value)}
                                      :while {:node-type   :clj/for-while
                                              :symbol symbol
                                              :value  (dsl->ast value)}
                                      {:node-type   :clj/for-iteration
                                       :symbol symbol
                                       :value  (dsl->ast value)}))))
             :body     (dsl->ast body)})

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

          ;; snap
          (= f `dsl/snap)
          (let [[body] args]
            {:node-type :dsl/snap
             :body (dsl->ast body)})

          ;; effect
          (= f `dsl/effect)
          (let [bodies args]
            {:node-type :dsl/effect
             :bodies (mapv dsl->ast bodies)})

          :else
          {:node-type :clj/invocation
           :function f
           :args (mapv dsl->ast args)})))

    (set? x)
    {:node-type :clj/set
     :items (set (mapv dsl->ast x))}

    (vector? x)
    {:node-type :clj/vector
     :items (mapv dsl->ast x)}

    (map? x)
    {:node-type :clj/map
     :entries (-> x
                  (update-keys dsl->ast)
                  (update-vals dsl->ast))}

    (= x `dsl/context)
    {:node-type :dsl/context}

    (= x `dsl/global)
    {:node-type :dsl/global}

    (symbol? x)
    {:node-type :clj/var
     :symbol x}

    :else
    {:node-type :clj/value
     :value x}))

#_
(dsl->ast `(let [a (dsl/signal 1)
                 b (dsl/state 2)
                 c (+ a b)
                 d (dsl/memo (+ a b))
                 e (dsl/snap (+ a b))]
             (do
               (for [i [1 2 3]
                     :let [j (inc i)]]
                 (+ i j 10))
               (if true 10 20)
               (when true 30)
               (dsl/effect
                 (prn (+ a b c d e))))))

;; node-type -> child-node -> #{:one :many}
(def node-type->walkable-children
  {:clj/let           [[:bindings :many]
                       [:bodies   :many]]
   :clj/let-binding   {:value :one}
   :clj/do            {:bodies :many}
   :clj/if            {:cond :one
                       :then :one
                       :else :one}
   :clj/when          {:cond   :one
                       :bodies :many}
   :clj/for           [[:bindings :many]
                       [:body     :one]]
   :clj/for-iteration {:value :one}
   :clj/for-when      {:cond :one}
   :clj/for-while     {:cond :one}
   :clj/for-let       {:value :one}
   :clj/invocation    {:function :one
                       :args     :many}
   :clj/var           {}
   :clj/value         {}
   :clj/set           {}
   :clj/vector        {}
   :clj/map           {}
   :dsl/with-context  {:context :one
                       :bodies  :many}
   :dsl/signal        {:body :one}
   :dsl/state         {:body :one}
   :dsl/memo          {:body :one}
   :dsl/snap          {:body :one}
   :dsl/effect        {:bodies :many}
   :dsl/context       {}
   :dsl/global        {}})

;; Walks the AST to process it.
(defn walk-ast [env pre-process post-process]
  (let [walk (fn walk [original-env]
               (let [{:keys [root-ast path] :as env} (pre-process original-env)
                     ast (get-in root-ast path)
                     field->cardinality (-> ast :node-type node-type->walkable-children)
                     env (reduce (fn [env [field cardinality]]
                                   (case cardinality
                                     :one (-> env
                                              (assoc :path (conj path field))
                                              walk)
                                     :many (reduce (fn [env index]
                                                     (-> env
                                                         (assoc :path (conj path field index))
                                                         walk))
                                                   env
                                                   (-> ast (get field) count range))))
                                 env
                                 field->cardinality)]
                 (-> env
                     (assoc :path path)
                     (assoc :original-env original-env)
                     post-process
                     (dissoc :original-env))))]
    (walk env)))

(defn make-env [ast]
  {:root-ast ast
   :path []
   :symbol->value-path {}})

(defn link-vars-pre-process [{:keys [root-ast path symbol->value-path] :as env}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [symbol (:symbol ast)
            value-path (symbol->value-path symbol)]
        (assoc-in env (cons :root-ast path)
          (if (nil? value-path)
            (assoc ast :error (str "Symbol " symbol " is unbound"))
            (assoc ast :value-path value-path))))

      ;; else
      env)))

(defn symbol->value-path-post-process [{:keys [root-ast path original-env] :as env}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      (:clj/let :clj/for)
      (-> env
          ;; Pop symbol->value-path back to its original state
          (assoc :symbol->value-path (:symbol->value-path original-env)))

      :clj/let-binding
      (let [symbol (:symbol ast)]
        (-> env
            ;; Curate symbol->value-path's content
            (update :symbol->value-path
                    assoc symbol (conj path :value))))

      ;; else
      env)))

;;#_
(-> (dsl->ast `(let [a (dsl/signal 1)
                     a (+ a 1)
                     b 2]
                 a))
    make-env
    (walk-ast link-vars-pre-process
              symbol->value-path-post-process))

#_(diff *2 *1)
