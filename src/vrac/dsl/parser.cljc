(ns vrac.dsl.parser
  #?(:cljs (:require-macros [vrac.dsl.parser]))
  (:require [mate.core :as mc]
            [lambdaisland.deep-diff2 :refer [diff]]
            [vrac.dsl :as dsl]
            [vrac.dsl.macro :as macro]))

;; Those are not functions or vars, they can't be resolved, so
;; we treat them as special cases which resolve to themselves.
#?(:clj
   (def clj-reserved-words
     '{do do
       if if
       quote quote}))

;; Macro-expand and resolve the symbols in the DSL.
;; var-resolver is a function which can be used for user-defined global variable resolution.
#?(:clj
   (defn resolve-and-macro-expand-dsl
     ([x] (resolve-and-macro-expand-dsl x
                                        nil
                                        clj-reserved-words
                                        macro/macros))
     ([x env var-resolver macros]
      (let [resolve-var (fn [x local-vars]
                          (if (contains? local-vars x)
                            x
                            (or (var-resolver x)
                                (let [resolved-x (resolve env x)]
                                  (assert (some? resolved-x) (str "Cannot resolve the symbol \"" x "\""))
                                  (symbol resolved-x)))))
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
                                           (cond
                                             ;; (let [,,,] ,,,)
                                             (= f `let)
                                             (let [[bindings & bodies] args
                                                   [bindings local-vars] (reduce (fn [[bindings local-vars] [symbol-form value-form]]
                                                                                   [(conj bindings symbol-form (resolve-and-expand value-form local-vars))
                                                                                    (conj local-vars symbol-form)])
                                                                                 [[] local-vars]
                                                                                 (partition 2 bindings))
                                                   bodies (map (mc/partial-> resolve-and-expand local-vars) bodies)]
                                               `(let ~bindings ~@bodies))

                                             ;; (for [,,,] ,,,)
                                             (= f `for)
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
                                               `(for ~bindings ~body)
                                               ,)

                                             ;; (f a b c)
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
        (resolve-and-expand x #{})))))

#?(:clj
   (defmacro expand-dsl [quoted-dsl-form]
     (let [env &env]
       (resolve-and-macro-expand-dsl quoted-dsl-form
                                     env
                                     clj-reserved-words
                                     macro/macros))))

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
                                              :symbol    symbol
                                              :value     (dsl->ast value)}
                                       :while {:node-type :clj/for-while
                                               :symbol    symbol
                                               :value     (dsl->ast value)}
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
(-> '(let [a (dsl/signal 1)
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
           (prn (+ a b c d e)))))
    resolve-and-macro-expand-dsl
    dsl->ast)

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
(defn walk-ast [context pre-process post-process]
  (let [walk (fn walk [original-context]
               (let [{:keys [root-ast path] :as context} (pre-process original-context)
                     ast (get-in root-ast path)
                     field->cardinality (-> ast :node-type node-type->walkable-children)
                     context (reduce (fn [context [field cardinality]]
                                       (case cardinality
                                         :one (-> context
                                                  (assoc :path (conj path field))
                                                  walk)
                                         :many (reduce (fn [context index]
                                                         (-> context
                                                             (assoc :path (conj path field index))
                                                             walk))
                                                       context
                                                       (-> ast (get field) count range))))
                                     context
                                     field->cardinality)]
                 (-> context
                     (assoc :path path)
                     (assoc :original-context original-context)
                     post-process
                     (dissoc :original-context))))]
    (walk context)))

(defn ast->context [ast]
  {:root-ast ast
   :path []})

(defn link-vars-pre-process [{:keys [root-ast path symbol->value-path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [symbol (:symbol ast)
            value-path (symbol->value-path symbol)]
        (assoc-in context (cons :root-ast path)
          (if (nil? value-path)
            (assoc ast :error (str "Symbol " symbol " is unbound"))
            (assoc ast :value-path value-path))))

      ;; else
      context)))

(defn symbol->value-path-post-process [{:keys [root-ast path original-context] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/let-binding
      (let [symbol (:symbol ast)]
        (-> context
            ;; Curate symbol->value-path's content
            (update :symbol->value-path
                    assoc symbol (conj path :value))))

      (:clj/let :clj/for)
      (-> context
          ;; Pop symbol->value-path back to its original state
          (assoc :symbol->value-path (:symbol->value-path original-context)))

      ;; else
      context)))

(defn find-bound-value-usages-pre-process [{:keys [root-ast path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [value-path (:value-path ast)]
        (-> context
            (update-in [:value-path->usage-paths value-path] (fnil conj []) path)))

      ;; else
      context)))

(defn find-bound-value-usages-post-process [{:keys [root-ast] :as context}]
  (let [value-path->usage-paths (:value-path->usage-paths context)]
    (-> context
        (assoc :root-ast (reduce (fn [root-ast [value-path usage-paths]]
                                   (-> root-ast
                                       (assoc-in (conj value-path :usage-paths) usage-paths)))
                                 root-ast
                                 value-path->usage-paths))
        (dissoc :value-path->usage-paths))))

#_
(-> '(let [a 1
           a (inc a)]
       (+ a a))
    resolve-and-macro-expand-dsl
    dsl->ast
    ast->context

    ;; Macro expansion & symbol resolution
    ((fn [context] (-> context (assoc :symbol->value-path {}))))
    (walk-ast link-vars-pre-process
              symbol->value-path-post-process)
    ((fn [context] (-> context (dissoc :symbol->value-path))))


    ;; Usage pass
    ;;(walk-ast find-bound-value-usages-pre-process
    ;;          identity)
    ;;find-bound-value-usages-post-process

    ,)

#_
(def task
  {:task-id :my-task-id
   :steps [{:step-id :my-step-id
            :deps [:other-step-id :other-task-id]
            :root-pre-process nil
            :pre-process nil
            :post-process nil
            :root-post-process nil}]})

;;(def add-vars-value-path
;;  {:task-id :task/add-vars-value-path
;;   :steps   [{:step-id           :step/add-vars-value-path
;;              :root-pre-process  (fn [context] (-> context (assoc :symbol->value-path {})))
;;              :pre-process       link-vars-pre-process
;;              :post-process      symbol->value-path-post-process
;;              :root-post-process (fn [context] (-> context (dissoc :symbol->value-path)))}]})
;;
;;(def add-bound-value-usage
;;  {:task-id :task/add-bound-value-usage
;;   :steps   [{:step-id           :step/add-bound-value-usage
;;              :pre-process       find-bound-value-usages-pre-process
;;              :root-post-process find-bound-value-usages-post-process}]})

#_(diff *2 *1)
