(ns vrac.dsl.ast
  (:require [lambdaisland.deep-diff2 :refer [diff]]
            [mate.core :as mc]))

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
   :clj/for-let       {:bindings :many}
   :clj/invocation    {:function :one
                       :args     :many}
   :clj/var           {}
   :clj/value         {}
   :clj/set           {}
   :clj/vector        {:items :many}
   :clj/map           {:entries :many}
   :clj/map-entry     {:key   :one
                       :value :one}
   :dsl/global        {}
   :dsl/context       {}
   :dsl/with-context  {:context :one
                       :bodies  :many}
   :dsl/once          {:body :one}
   :dsl/signal        {:body :one}
   :dsl/state         {:body :one}
   :dsl/memo          {:body :one}
   :dsl/effect        {:bodies :many}
   :dsl/effect-on     {:triggers :many
                       :bodies   :many}})

(defn walk-ast
  "Walks and transforms a context containing the AST."
  [context pre-process post-process]
  (let [walk (fn walk [original-context]
               (let [path (:path original-context)
                     {:keys [root-ast] :as context} (pre-process original-context)
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

(defn make-context
  "Returns an initial context from a given AST."
  [ast]
  {:root-ast ast
   :path []})

(defn push-ancestor-path [context]
  (-> context
      (update :ancestor-paths conj (:path context))))

(defn pop-ancestor-path [context]
  (-> context
      (update :ancestor-paths pop)))

;; -----------------------------------

(defn- link-vars-pre-walk
  "On each var node, assoc `:var.value/path` to point where its value is defined.
   Assoc :var/unbound true instead if the var is unbound."
  [{:keys [root-ast path symbol->value-path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [symbol (:symbol ast)
            value-path (symbol->value-path symbol)]
        (-> context
            (assoc-in (cons :root-ast path)
                      (-> ast
                          (mc/if-> (nil? value-path)
                            (assoc :var/unbound true)
                            (assoc :var.value/path value-path))))))

      ;; else
      context)))

(defn- symbol->value-path-post-walk
  "Updates an hashmap symbol->value-path as we walk the AST, to keep track of
   what vars are in the current scope and where they are defined."
  [{:keys [root-ast path original-context] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      ;; Add a var to the hashmap when we exit a let-binding, as it becomes available
      ;; in the next let-binding entries and the let's bodies.
      :clj/let-binding
      (let [symbol (:symbol ast)]
        (-> context
            ;; Curate symbol->value-path's content
            (update :symbol->value-path
                    assoc symbol (conj path :value))))

      ;; Restore the hashmap when we exit the body/ies of its parent let or for node.
      (:clj/let :clj/for)
      (-> context
          ;; Pop symbol->value-path back to its original state
          (assoc :symbol->value-path (:symbol->value-path original-context)))

      ;; else
      context)))

(defn link-vars-to-their-definition-pass
  "An AST pass which links the vars to their definition via `:var.value/path`."
  [context]
  (-> context
      (assoc :symbol->value-path {}) ;; pass setup
      (walk-ast link-vars-pre-walk symbol->value-path-post-walk)
      (dissoc :symbol->value-path))) ;; pass clean up

;; -----------------------------------

(defn- find-bound-value-usages-pre-walk
  "Collects all the var usages from the whole AST into
   a hashmap `:var.value/path` -> `:var.usage/paths`."
  [{:keys [root-ast path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [value-path (:var.value/path ast)]
        (-> context
            (cond-> (some? value-path)
                    (update-in [:value-path->usage-paths value-path] (fnil conj []) path))))

      ;; else
      context)))

(defn- find-bound-value-usages-pass-clean-up
  "From the hashmap, write down in the AST the usages of each bound value."
  [{:keys [root-ast] :as context}]
  (let [value-path->usage-paths (:value-path->usage-paths context)]
    (-> context
        (assoc :root-ast (reduce (fn [root-ast [value-path usage-paths]]
                                   (-> root-ast
                                       (assoc-in (conj value-path :var.usage/paths) usage-paths)))
                                 root-ast
                                 value-path->usage-paths))
        (dissoc :value-path->usage-paths))))

;; This pass works after `link-vars-to-their-definition-pass`
(defn add-var-usage-pass
  "An AST pass which annotates all the var usages bi-directionally,
   via `:var.value/path` and `:var.usage/paths`."
  [context]
  (-> context
      (walk-ast find-bound-value-usages-pre-walk identity)
      find-bound-value-usages-pass-clean-up))

;; -----------------------------------

(defn- lifespan-pre-walk
  [{:keys [root-ast path lifespan-path] :as context}]
  (let [ast (get-in root-ast path)
        ;; The lifespan defined on the node takes priority
        lifespan-path (:node.lifespan/path ast lifespan-path)
        ;; Saves the value in the node
        ast (-> ast
                (assoc :node.lifespan/path lifespan-path))
        ;;
        ast (case (:node-type ast)
              :clj/if
              (-> ast
                  ;; Sets a lifespan on the :then and :else nodes
                  (assoc-in [:then :node.lifespan/path] (conj path :then))
                  (assoc-in [:else :node.lifespan/path] (conj path :else)))

              :clj/when
              (-> ast
                  ;; Sets a lifespan on all the body nodes
                  (update :bodies (fn [bodies]
                                    (->> bodies
                                         (mapv (fn [body]
                                                 (-> body
                                                     (assoc :node.lifespan/path (conj path :bodies)))))))))

              :clj/for
              (let [[bindings lifespan-path] (reduce (fn [[bindings lifespan] [index binding]]
                                                       ;; Sets the lifespan on each binding
                                                       [(conj bindings (-> binding
                                                                           (assoc :node.lifespan/path lifespan)))
                                                        (if (= (:node-type binding) :clj/for-iteration)
                                                          (conj path :bindings index :symbol)
                                                          lifespan)])
                                                     [[] lifespan-path]
                                                     (mc/seq-indexed (:bindings ast)))]
                (-> ast
                    (assoc :bindings bindings)
                    ;; Sets a lifespan on the :body node
                    (assoc-in [:body :node.lifespan/path] lifespan-path)))

              ;; else
              ast)]
    (-> context
        (assoc :lifespan-path lifespan-path)
        (assoc-in (cons :root-ast path) ast))))

(defn- lifespan-post-walk
  [{:keys [original-context] :as context}]
  (-> context
      (assoc :lifespan-path (:lifespan-path original-context))))

(defn add-lifespan-pass
  "An AST pass which annotates all the nodes with a :node.lifespan/path
   pointing to the root of the scope which has the same lifespan."
  [context]
  (-> context
      (assoc :lifespan-path []) ; pass setup
      (walk-ast lifespan-pre-walk lifespan-post-walk)
      (dissoc :lifespan-path))) ; pass clean up

;; -----------------------------------

(defn add-reactivity-type-pre-walk [context]
  context)

(defn add-reactivity-type-post-walk [{:keys [root-ast path] :as context}]
  (let [ast (get-in root-ast path)
        ast (case (:node-type ast)
              :dsl/signal
              (-> ast
                  (assoc :reactivity/type :signal))

              :dsl/state
              (-> ast
                  (assoc :reactivity/type :memo))

              :dsl/memo
              (let [body-reactivity (:reactivity/type (:body ast))
                    ast-node-reactivity (if (contains? #{:signal :memo} body-reactivity)
                                          :memo
                                          :none)]
                (-> ast
                    (assoc :reactivity/type ast-node-reactivity)))

              (:dsl/once :clj/value)
              (-> ast
                  (assoc :reactivity/type :none))

              :clj/var
              (let [{:keys [var.value/path var/unbound]} ast
                    bound-value (get-in root-ast path)]
                (-> ast
                    (cond-> (not unbound)
                            (assoc :reactivity/type (:reactivity/type bound-value)))))

              :clj/invocation
              (let [args-reactivity-types (into #{} (map :reactivity/type) (:args ast))
                    ast-node-reactivity-type (cond
                                               (contains? args-reactivity-types :memo) :memo
                                               (contains? args-reactivity-types :signal) :signal
                                               (contains? args-reactivity-types :none) :none
                                               :else nil)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/let
              (let [last-body (last (:bodies ast))
                    ast-node-reactivity-type (:reactivity/type last-body)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              ;; else
              ast)]
    (-> context
        (assoc-in (cons :root-ast path) ast))))

(defn add-reactivity-type-pass
  ""
  [context]
  (-> context
      (walk-ast add-reactivity-type-pre-walk add-reactivity-type-post-walk)))

;; -----------------------------------

#_(diff *2 *1)
