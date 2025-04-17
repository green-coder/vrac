(ns vrac.dsl.ast
  (:require [lambdaisland.deep-diff2 :refer [diff]]
            [mate.core :as mc]))

;; node-type -> child-node -> #{:one :many}
(def node-type->walkable-children
  {:clj/fn            [[:params :many]
                       [:body :one]]
   :clj/defn          [[:params :many]
                       [:body :one]]
   :clj/fn-param      {}
   :clj/let           [[:bindings :many]
                       [:body :one]]
   :clj/let-binding   {:value :one}
   :clj/do            {:bodies :many}
   :clj/if            {:cond :one
                       :then :one
                       :else :one}
   :clj/when          {:cond :one
                       :body :one}
   :clj/for           [[:bindings :many]
                       [:body     :one]]
   :clj/for-iteration {:value :one}
   :clj/for-let       {:bindings :many}
   :clj/for-when      {:cond :one}
   :clj/for-while     {:cond :one}
   :clj/invocation    {:function :one
                       :args     :many}
   :clj/var           {}
   :clj/value         {}
   :clj/set           {:items :many}
   :clj/vector        {:items :many}
   :clj/map           {:entries :many}
   :clj/map-entry     {:key   :one
                       :value :one}
   :dsl/global        {}
   :dsl/context       {}
   :dsl/with-context  {:context :one
                       :body    :one}
   :dsl/once          {:body :one}
   :dsl/signal        {:body :one}
   :dsl/state         {:body :one}
   :dsl/memo          {:body :one}
   :dsl/effect        {:body :one}
   :dsl/effect-on     {:triggers :many
                       :body     :one}})

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

(defn- assoc-from [m-to m-from k]
  (-> m-to
      (assoc k (get m-from k))))

(defn- assoc-existing-from [m-to m-from k]
  (cond-> m-to
    (contains? m-from k)
    (assoc k (get m-from k))))

;; -----------------------------------

(defn- link-vars-pre-walk
  "On each var node, assoc `:var.definition/path` to point where its value is defined.
   Assoc :var/unbound true instead if the var is unbound."
  [{:keys [root-ast path symbol->definition-path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [symbol (:symbol ast)
            definition-path (symbol->definition-path symbol)]
        (-> context
            (assoc-in (cons :root-ast path)
                      (-> ast
                          (mc/if-> (nil? definition-path)
                            (assoc :var/unbound true)
                            (assoc :var.definition/path definition-path))))))

      ;; else
      context)))

(defn- link-vars-post-walk
  "Updates a hashmap symbol->value-path as we walk the AST, to keep track of
   what vars are in the current scope and where they are defined."
  [{:keys [root-ast path original-context] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      ;; Add a var to the hashmap when we exit a let-binding, as it becomes available
      ;; in the next let-binding entries and the let's body.
      (:clj/fn-param :clj/let-binding :clj/for-iteration)
      (let [symbol (:symbol ast)]
        (-> context
            ;; Curate symbol->definition-path's content
            (update :symbol->definition-path
                    assoc symbol path)))

      ;; Restore the hashmap when we exit the body/ies of its parent let or for node.
      (:clj/let :clj/for)
      (-> context
          ;; Pop symbol->definition-path back to its original state
          (assoc-from original-context :symbol->definition-path))

      ;; else
      context)))

(defn link-vars-to-their-definition-pass
  "An AST pass which links the vars to their definition via `:var.value/path`."
  [context]
  (-> context
      (assoc :symbol->definition-path {}) ;; pass setup
      (walk-ast link-vars-pre-walk link-vars-post-walk)
      (dissoc :symbol->definition-path))) ;; pass clean up

;; -----------------------------------

(defn- find-bound-value-usages-pre-walk
  "Collects all the var usages from the whole AST into
   a hashmap `:var.value/path` -> `:var.usage/paths`."
  [{:keys [root-ast path] :as context}]
  (let [ast (get-in root-ast path)]
    (case (:node-type ast)
      :clj/var
      (let [definition-path (:var.definition/path ast)]
        (-> context
            (cond-> (some? definition-path)
                    (update-in [:definition-path->usage-paths definition-path] (fnil conj []) path))))

      ;; else
      context)))

(defn- find-bound-value-usages-clean-up
  "From the hashmap, write down in the AST the usages of each bound value."
  [{:keys [root-ast] :as context}]
  (let [definition-path->usage-paths (:definition-path->usage-paths context)]
    (-> context
        (assoc :root-ast (reduce (fn [root-ast [value-path usage-paths]]
                                   (-> root-ast
                                       (assoc-in (conj value-path :var.usage/paths) usage-paths)))
                                 root-ast
                                 definition-path->usage-paths))
        (dissoc :definition-path->usage-paths))))

;; This pass works after `link-vars-to-their-definition-pass`
(defn add-var-usage-pass
  "An AST pass which annotates all the var usages bi-directionally,
   via `:var.value/path` and `:var.usage/paths`."
  [context]
  (-> context
      (walk-ast find-bound-value-usages-pre-walk identity)
      find-bound-value-usages-clean-up))

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
              :clj/defn
              (-> ast
                  ;; Sets a lifespan on all the body node
                  (assoc-in [:body :node.lifespan/path] (conj path :body)))

              :clj/if
              (-> ast
                  ;; Sets a lifespan on the :then and :else nodes
                  (assoc-in [:then :node.lifespan/path] (conj path :then))
                  (assoc-in [:else :node.lifespan/path] (conj path :else)))

              :clj/when
              (-> ast
                  ;; Sets a lifespan on all the body node
                  (assoc-in [:body :node.lifespan/path] (conj path :body)))

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

(defn- comp-reactivities [reactivity-set]
  (cond
    (contains? reactivity-set :signal) :signal
    (contains? reactivity-set :memo) :memo
    (contains? reactivity-set :value) :value
    :else nil))

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
                                          :value)]
                (-> ast
                    (assoc :reactivity/type ast-node-reactivity)))

              (:dsl/once :clj/value)
              (-> ast
                  (assoc :reactivity/type :value))

              :clj/var
              (let [{:keys [var.definition/path var/unbound]} ast
                    definition (get-in root-ast path)]
                (-> ast
                    (cond-> (and (not unbound)
                                 (contains? definition :reactivity/type))
                            (assoc :reactivity/type (:reactivity/type definition)))))

              :clj/invocation
              (let [ast-node-reactivity-type (comp-reactivities (into #{} (map :reactivity/type) (:args ast)))]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              (:clj/set :clj/vector)
              (let [ast-node-reactivity-type (comp-reactivities (into #{} (map :reactivity/type) (:items ast)))]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/map
              (let [ast-node-reactivity-type (comp-reactivities (into #{} (map :reactivity/type) (:entries ast)))]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/map-entry
              (let [ast-node-reactivity-type (comp-reactivities (into #{} (map :reactivity/type) [(:key ast) (:value ast)]))]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/let
              (let [ast-node-reactivity-type (-> ast :body :reactivity/type)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/let-binding
              (let [ast-node-reactivity-type (-> ast :value :reactivity/type)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/for
              (let [ast-node-reactivity-type (-> ast :body :reactivity/type)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))


              :clj/for-iteration
              (let [ast-node-reactivity-type (-> ast :value :reactivity/type)]
                (-> ast
                    (cond-> (some? ast-node-reactivity-type)
                            (assoc :reactivity/type ast-node-reactivity-type))))

              :clj/fn-param
              (let [ast-node-reactivity-type (-> ast :metadata :tag
                                                 {'value  :value
                                                  'signal :signal
                                                  'memo   :memo})]
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

;; This is a template for creating a new pass.
;; Copy/paste, rename those functions, then implement

(defn- xxx-pre-walk [context]
  context)

(defn- xxx-post-walk [{:keys [root-ast path] :as context}]
  (let [ast (get-in root-ast path)
        ast (case (:node-type ast)
              #_#_
              :clj/value ast

              ;; else
              ast)]
    (-> context
        (assoc-in (cons :root-ast path) ast))))

(defn xxx-pass
  [context]
  (-> context
      (walk-ast xxx-pre-walk xxx-post-walk)))

;; -----------------------------------

#_(diff *2 *1)
