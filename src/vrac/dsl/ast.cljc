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
   :clj/for-let       {:value :one}
   :clj/invocation    {:function :one
                       :args     :many}
   :clj/var           {}
   :clj/value         {}
   :clj/set           {}
   :clj/vector        {:items :many}
   :clj/map           {:entries :many}
   :clj/map-entry     {:key   :one
                       :value :one}
   :dsl/with-context  {:context :one
                       :bodies  :many}
   :dsl/signal        {:body :one}
   :dsl/state         {:body :one}
   :dsl/memo          {:body :one}
   :dsl/snap          {:body :one}
   :dsl/effect        {:bodies :many}
   :dsl/context       {}
   :dsl/global        {}})

(defn walk-ast
  "Walks and transforms a context containing the AST."
  [context pre-process post-process]
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

;; -----------------------------------

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
