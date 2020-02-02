(ns vrac.core
  (:require [clojure.spec.alpha :as s]
            [vrac.component :as vc]
            [vrac.util :refer [map-vals tag-id-class]])
  #?(:cljs (:require-macros vrac.core)))

;; A spec for the vrac components.
(s/def ::component
  (s/keys :req-un [::vc/id]
          :opt-un [::vc/name
                   ::vc/description
                   ::vc/props
                   ::vc/template]))

;; A spec for the defc macro's arguments.
(s/def ::defc-args
  (s/cat :var symbol?
         :props ::vc/props
         :options (s/? (s/keys :opt-un [::vc/id
                                        ::vc/name
                                        ::vc/description]))
         :template any?))

;; TODO: The process of the parsed-template should be easier if the tree is
;; enriched with all the information that one can grab.
;; Consider using a functional zipper for multi-directional navigation and node enrichment.
;; Consider using rules instead of functions when enriching the tree.
(defn template->ast [template]
  (s/conform ::vc/template template))

(defn ast->template [[kw val :as parsed-template]]
  ;; I am waiting for Clojure spec2 to support recursion to simply use s/unform.
  ;(s/unform ::vc/template parsed-template)
  (case kw
    (:boolean :number :string :keyword :symbol :nil) val
    :map (into {} (map (juxt ast->template ast->template)) val)
    :get-kw (let [{:keys [keyword valuable]} val]
              (list keyword (ast->template valuable)))
    :if (let [{:keys [cond then else]} val]
          (list 'if (ast->template cond)
            (ast->template then)
            (ast->template else)))
    :when (let [{:keys [cond then]} val]
            (list 'when (ast->template cond)
              (ast->template then)))
    (:let :for) (let [{:keys [directive bindings body]} val]
                  (list directive
                        (into []
                              (mapcat (juxt :symbol (comp ast->template :valuable)))
                              bindings)
                        (ast->template body)))
    :comp (let [{:keys [keyword props children]} val]
            (into (cond-> [keyword]
                          props (conj (ast->template props)))
                  (map ast->template) children))
    (:dsl :valuable) (ast->template val)))

;; TODO: Instead of visiting nodes in each different function and collecting the children,
;; use a visitor or maybe a functional zipper.

(defn- get-template-props* [context [kw val :as parsed-template]]
  (case kw
    (:nil :boolean :number :string :keyword) #{}
    :symbol (if (contains? context val) #{} #{val})
    :map (into #{}
               (comp cat ; sequence k0 v0 k1 v1 ...
                     (mapcat (partial get-template-props* context)))
               val)
    :get-kw (let [{:keys [valuable]} val]
              (get-template-props* context valuable))
    (:if :when) (let [{:keys [cond then else]} val]
                  (into #{}
                        (comp (remove nil?)
                              (mapcat (partial get-template-props* context)))
                        [cond then else]))
    (:let :for) (let [{:keys [bindings body]} val
                      [inner-context props] (reduce (fn [[context props] {:keys [symbol valuable]}]
                                                      [(conj context symbol)
                                                       (into props (get-template-props* context valuable))])
                                                    [context #{}]
                                                    bindings)]
                  (into props (get-template-props* inner-context body)))
    :comp (let [{:keys [children]} val]
            (into #{} (mapcat (partial get-template-props* context)) children))
    (:dsl :valuable) (get-template-props* context val)))

(defn get-template-props
  "Returns the set of unbound symbols used in the template."
  [parsed-template]
  (get-template-props* #{} parsed-template))

(declare get-comp-deps)

(defn- get-deps
  "Finds the dependencies in a parsed template."
  ; context is a map of: bound-var -> [(kw (kw (... (kw unbound-var)))) ...]
  [env context [kw val :as parsed-template]]
  (case kw
    (:boolean :number :string :keyword) []
    :nil [[]]
    :symbol (context val [[val]])
    :map (->> (mapcat identity val) ; sequence k0 v0 k1 v1 ...
              (mapcat (partial get-deps env context))
              (into []))
    :get-kw (let [{:keys [keyword valuable]} val]
              (mapv #(conj % keyword) (get-deps env context valuable)))
    (:if :when) (let [{:keys [cond then else]} val]
                  (->> [cond then else]
                       (remove nil?)
                       (mapcat (partial get-deps env context))
                       (into [])))
    (:let :for) (let [{:keys [bindings body]} val
                      inner-context (reduce (fn [context {:keys [symbol valuable]}]
                                              (assoc context
                                                symbol (get-deps env context valuable)))
                                            context
                                            bindings)]
                  (get-deps env inner-context body))
    :comp (let [{:keys [keyword props children]} val
                {:keys [tag]} (tag-id-class keyword)
                component-id tag]
            (if (simple-keyword? keyword)
              ; an html node
              (->> (concat (vals props) children)
                   (mapcat (partial get-deps env context))
                   (into []))
              ; a component
              (let [comp-context (into {}
                                       (map (fn [[k v]]
                                              [(symbol k) (get-deps env context v)]))
                                       props)]
                (get-comp-deps env comp-context component-id))))
    (:dsl :valuable) (get-deps env context val)))

(defn- conflict-free-symbol
  "Returns a symbol similar to symb and which is not in the existing-symb set."
  [symb existing-symbs]
  (let [symb-ns (namespace symb)]
    (loop [n 0, candidate symb]
      (if-not (contains? existing-symbs candidate)
        candidate
        (recur (inc n) (symbol symb-ns (str (name symb) "_" n)))))))

(defn- renamed-template
  "Returns a template resulting from renaming all the bound variables
   whose symbol appear in the parent-symbs set."
  ([parent-symbs parsed-template]
   (renamed-template parent-symbs #{} {} parsed-template))
  ([parent-symbs local-symbs renames [kw val :as parsed-template]]
   (case kw
     (:nil :boolean :number :string :keyword) parsed-template
     :symbol [kw (renames val val)]
     :map [kw (into {}
                    (map (fn [[k v]]
                           [(renamed-template parent-symbs local-symbs renames k)
                            (renamed-template parent-symbs local-symbs renames v)]))
                    val)]
     :get-kw [kw (update val :valuable (partial renamed-template parent-symbs local-symbs renames))]
     :when (let [{:keys [cond then]} val]
             [kw {:cond (renamed-template parent-symbs local-symbs renames cond)
                  :then (renamed-template parent-symbs local-symbs renames then)}])
     :if (let [{:keys [cond then else]} val]
           [kw {:cond (renamed-template parent-symbs local-symbs renames cond)
                :then (renamed-template parent-symbs local-symbs renames then)
                :else (renamed-template parent-symbs local-symbs renames else)}])
     (:let :for) (let [{:keys [bindings body]} val
                       [parent-symbs local-symbs renames bindings]
                       (reduce (fn [[parent-symbs local-symbs renames bindings] {:keys [symbol valuable]}]
                                 (let [[new-parent-symbs new-renames] (if (and (contains? parent-symbs symbol)
                                                                               (not (contains? renames symbol)))
                                                                        (let [new-symb (conflict-free-symbol symbol (into parent-symbs local-symbs))]
                                                                          [(conj parent-symbs new-symb)
                                                                           (assoc renames symbol new-symb)])
                                                                        [parent-symbs renames])
                                       new-symbol (new-renames symbol symbol)
                                       new-local-symbs (conj local-symbs new-symbol)]
                                   [new-parent-symbs
                                    new-local-symbs
                                    new-renames
                                    (conj bindings
                                          {:symbol new-symbol
                                           :valuable (renamed-template parent-symbs local-symbs renames valuable)})]))
                               [parent-symbs local-symbs renames []]
                               bindings)]
                   [kw (assoc val
                         :bindings bindings
                         :body (renamed-template parent-symbs local-symbs renames body))])
     :comp (let [{:keys [keyword props children]} val]
             [kw (cond-> val
                         props (assoc :props (into {}
                                                   (map (fn [[k v]]
                                                          [k (renamed-template parent-symbs local-symbs renames v)]))
                                                   props))
                         children (assoc :children (mapv (partial renamed-template parent-symbs local-symbs renames) children)))])
     (:dsl :valuable) [kw (renamed-template parent-symbs local-symbs renames val)])))

(defn- expanded-template
  "Returns a template resulting from inlining all the component occurrences."
  ([env component-id]
   (expanded-template env {} #{} (-> env :components component-id :parsed-template)))
  ([env context local-symbs [kw val :as parsed-template]]
   (case kw
     (:nil :boolean :number :string :keyword) parsed-template
     :symbol (context val parsed-template)
     :map [kw (into {}
                    (map (fn [[k v]]
                           [(expanded-template env context local-symbs k)
                            (expanded-template env context local-symbs v)]))
                    val)]
     :get-kw [kw (update val :valuable (partial expanded-template env context local-symbs))]
     :when (let [{:keys [cond then]} val]
             [kw {:cond (expanded-template env context local-symbs cond)
                  :then (expanded-template env context local-symbs then)}])
     :if (let [{:keys [cond then else]} val]
           [kw {:cond (expanded-template env context local-symbs cond)
                :then (expanded-template env context local-symbs then)
                :else (expanded-template env context local-symbs else)}])
     (:let :for) (let [{:keys [bindings body]} val
                       local-symbs (into local-symbs (map :symbol) bindings)]
                   [kw (assoc val :body (expanded-template env context local-symbs body))])
     :comp (let [{:keys [keyword props children]} val
                 {:keys [tag]} (tag-id-class keyword)
                 component-id tag]
             (if (or (simple-keyword? keyword)
                     (nil? (-> env :components component-id :parsed-template)))
               ; an html node or an external component
               [kw (cond-> val
                           props (assoc :props (expanded-template env context local-symbs props))
                           children (assoc :children (mapv (partial expanded-template env context local-symbs) children)))]
               ; a vrac component (inline it)
               (let [comp-context (into {}
                                        (map (fn [[k v]]
                                               [(symbol k) (expanded-template env context local-symbs v)]))
                                        props)]
                 (->> (-> env :components component-id :parsed-template)
                      (renamed-template local-symbs)
                      (expanded-template env comp-context local-symbs)))))
     (:dsl :valuable) [kw (expanded-template env context local-symbs val)])))

(defn- get-comp-deps
  "Finds the dependencies in a component."
  [env context component-id]
  (let [deps (get-deps env context (-> env :components component-id :parsed-template))]
    (->> (cond-> deps
           (contains? context 'id) (conj (context 'id))
           (contains? context 'class) (conj (context 'class)))
         (into []))))

(defn- deps->eql
  "Transform the flatten deps into eql query trees."
  [deps]
  (->> (filter seq deps)
       (group-by first)
       (mapv (fn [[ctx deps]]
               (let [fields (deps->eql (keep next deps))]
                 (if (empty? fields)
                   ctx
                   {ctx fields}))))))

(defn get-queries
  "Returns the data usage (eql and values together) for this component."
  [env component-id]
  (->> (get-comp-deps env {} component-id)
       (deps->eql)))

(defn render
  "Renders a parsed template and props into hiccup syntax."
  [context [kw val :as parsed-template]]
  (case kw
    (:nil :boolean :number :string :keyword) val
    :symbol (context val)
    :map (into {} (map (fn [[k v]]
                         [(render context k)
                          (render context v)])) val)
    :get-kw (let [{:keys [keyword valuable]} val]
              (keyword (if (= valuable [:nil nil])
                         (context nil)
                         (render context valuable))))
    :if (let [{:keys [cond then else]} val]
          (render context
                  (if (render context cond)
                    then
                    else)))
    :when (let [{:keys [cond then]} val]
            (when (render context cond)
              (render context then)))
    :let (let [{:keys [bindings body]} val
               inner-context (reduce (fn [context {:keys [symbol valuable]}]
                                       (assoc context
                                         symbol (render context valuable)))
                                     context
                                     bindings)]
           (render inner-context body))
    :for (let [{:keys [bindings body]} val
               for (fn for [context [binding & next-bindings]]
                     (if (nil? binding)
                       [(render context body)]
                       (let [{:keys [symbol valuable]} binding
                             coll (render context valuable)]
                         (mapcat (fn [val]
                                   (for (assoc context symbol val) next-bindings))
                                 coll))))]
           (-> (for context bindings)
               (with-meta {:inline true})))
    :comp (let [{:keys [keyword children]} val]
            (into [keyword]
                  (mapcat (fn [child]
                            (let [rendered (render context child)]
                              (if (some-> (meta rendered) :inline)
                                rendered
                                [rendered]))))
                  children))
    (:dsl :valuable) (render context val)))

;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defn- defc* [var props options template]
  (let [ns-str (name (ns-name *ns*))
        var-str (name var)
        default-id (keyword ns-str var-str)
        parsed-template (s/conform ::vc/template template)
        template-props (when-not (= parsed-template :clojure.spec.alpha/invalid)
                         (get-template-props parsed-template))
        missing-props (clojure.set/difference (set template-props)
                                              (set props))]
    (when (= parsed-template :clojure.spec.alpha/invalid)
      (throw (ex-info (str "Invalid template: " (s/explain-str ::vc/template template))
                      (s/explain-str ::vc/template template))))

    (when (seq missing-props)
      (throw (ex-info (str "Props missing from the declaration: " missing-props)
                      missing-props)))

    (merge {:id default-id}
           options
           `{:props '~props
             :template '~template

             ;; Those fields will go away in the long term, once they are used *only* at compilation phase.
             :template-props '~template-props
             :parsed-template '~parsed-template})))

;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defmacro defc [& args]
  (let [{:keys [var props options template]} (s/conform ::defc-args args)]
    `(def ~var ~(defc* var props options template))))

(defn with-components [env components]
  (assoc env
    :components (into {} (map (juxt :id identity)) components)))
