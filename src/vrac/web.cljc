(ns vrac.web
  #?(:cljs (:require-macros [vrac.web :refer [if-fragment
                                              case-fragment
                                              cond-fragment]]))
  (:require [clojure.string :as str]
            #?(:cljs [goog.object :as gobj])
            [signaali.reactive :as sr]))

(def xmlns-math-ml "http://www.w3.org/1998/Math/MathML")
(def xmlns-html    "http://www.w3.org/1999/xhtml")
(def xmlns-svg     "http://www.w3.org/2000/svg")

#_
(def xmlns-by-kw
  {:math xmlns-math-ml
   :html xmlns-html
   :svg  xmlns-svg})

;; ----------------------------------------------

(defrecord VcupNode [node-type children])
(defrecord ReactiveFragment [reactive-node])
(defrecord AttributeEffect [reactive-attributes])
(defrecord ComponentResult [effects elements])

;; ----------------------------------------------

#?(:cljs
   (defn- dom-node? [x]
     (instance? js/Node x)))

(defn- vcup-fragment? [x]
  (and (instance? VcupNode x)
       (= (:node-type x) :<>)))

#?(:cljs
   (defn- vcup-element? [x]
     (and (instance? VcupNode x)
          (not= (:node-type x) :<>)
          (or (simple-keyword? (:node-type x))
              (instance? js/Element (:node-type x))))))

(defn- component-invocation? [x]
  (and (instance? VcupNode x)
       (fn? (:node-type x))))

(defn- reactive-fragment? [x]
  (instance? ReactiveFragment x))

(defn- attribute-effect? [x]
  (instance? AttributeEffect x))

(defn- component-result? [x]
  (instance? ComponentResult x))

(defn- reactive-node? [x]
  (instance? signaali.reactive.ReactiveNode x))

(defn- attribute-map? [x]
  (and (map? x)
       (not (record? x))))

(defn- attributes? [x]
  (or (attribute-map? x)
      (attribute-effect? x)))

;; ----------------------------------------------

;;(defn component-result [& sub-results]
;;  (apply merge-with into sub-results))

;; ----------------------------------------------

(defn- ensure-coll [x]
  (cond-> x (not (coll? x)) vector))

(defn- parse-element-tag [s]
  (reduce (fn [acc part]
            (case (subs part 0 1)
              "." (update acc :classes conj (subs part 1))
              "#" (assoc acc :id (subs part 1))
              (assoc acc :tag-name part)))
          {:tag-name "div"
           :id nil
           :classes []}
          (re-seq #"[#.]?[^#.]+" s)))

(defn- style->str [x]
  (cond
    (map? x) (->> x
                  (map (fn [[k v]] (str (name k) ": " v)))
                  (str/join "; ")
                  (not-empty))
    :else x))

(defn- class->str [x]
  (when (some? x)
    (->> x
         (ensure-coll)
         (flatten)
         (remove nil?)
         (map name)
         (str/join " ")
         (not-empty))))

(defn- compose-attribute-maps [base new]
  (let [style (into (or (:style base) {}) (:style new))
        class (into (or (:class base) []) (some-> (:class new) ensure-coll))]
    (-> base
        (into (dissoc new :style :class))
        (cond-> (seq style) (assoc :style style))
        (cond-> (seq class) (assoc :class class)))))

;; ----------------------------------------------

#?(:cljs
   (defn- set-element-attribute [xmlns-kw ^js/Element element attribute-kw attribute-value]
     (cond
       ;; TODO: see if we could use `classList` on the element
       (= attribute-kw :class)
       (-> element (gobj/set "className" (class->str attribute-value)))

       (= attribute-kw :style)
       (-> element (gobj/set "style" (style->str attribute-value)))

       :else
       (let [attribute-name (name attribute-kw)]
         (if (str/starts-with? attribute-name "on-")
           ;; Add an event listener
           (-> element (.addEventListener (-> attribute-name
                                              (subs (count "on-"))
                                              str/lower-case)
                                          attribute-value))
           ;; Set a general element attribute
           (let [attribute-value (when-not (false? attribute-value) attribute-value)]
             (if (= xmlns-kw :html)
               (-> element (gobj/set attribute-name attribute-value))
               (-> element (.setAttribute attribute-name attribute-value)))))))))

#?(:cljs
   (defn- unset-element-attribute [xmlns-kw ^js/Element element attribute-kw attribute-value]
     ;; TODO: We might not need to unset all the attributes all the time.
     (cond
       (= attribute-kw :class)
       (-> element (.removeAttribute "className"))

       (= attribute-kw :style)
       (-> element (.removeAttribute "style"))

       :else
       (let [attribute-name (name attribute-kw)]
         (if (str/starts-with? attribute-name "on-")
           ;; Event listener
           (-> element (.removeEventListener (-> attribute-name
                                                 (subs (count "on-"))
                                                 str/lower-case)
                                             attribute-value))
           (if (= xmlns-kw :html)
             (-> element (gobj/set attribute-name nil))
             (-> element (.removeAttribute attribute-name nil))))))))

(defn- deref+ [x]
  (cond
    (instance? signaali.reactive.ReactiveNode x) @x
    (fn? x) (x)
    :else x))

#?(:cljs
   (defn- dynamic-attributes-effect [xmlns-kw ^js/Element element attrs]
     (let [attrs (->> attrs
                      ;; Combine the consecutive attribute-maps together, and
                      ;; unwrap the reactive-attributes in AttributeEffect values.
                      (into []
                            (comp
                              (partition-by attribute-effect?)
                              (mapcat (fn [attribute-group]
                                        (if (attribute-map? (first attribute-group))
                                          [(reduce compose-attribute-maps {} attribute-group)]
                                          (mapv :reactive-attributes attribute-group)))))))]
       (sr/create-effect (fn []
                           (let [attributes (transduce (map deref+) compose-attribute-maps {} attrs)]
                             (doseq [[attribute-kw attribute-value] attributes]
                               (set-element-attribute xmlns-kw element attribute-kw attribute-value))
                             (sr/on-clean-up (fn []
                                               (doseq [[attribute-kw attribute-value] attributes]
                                                 (unset-element-attribute xmlns-kw element attribute-kw attribute-value))))))))))

#?(:cljs
   (defn dynamic-children-effect
     "Dynamically update the DOM node so that its children keep representing the elements array.
   The elements are either js/Element or a reactive node whose value is a sequence of js/Element."
     [^js/Element parent-element elements]
     ;; TODO: This algorithm could be improved to only replace children where it is needed.
     ;;       Would it be faster? less CPU-intensive?
     ;;       maybe different algorithms depending on the size?
     ;;       measurements needed.
     (sr/create-effect (fn []
                         (let [new-children (make-array 0)]
                           (doseq [element elements]
                             (if (reactive-fragment? element)
                               (doseq [sub-element @(:reactive-node element)]
                                 (.push new-children sub-element))
                               (.push new-children element)))
                           (-> parent-element .-replaceChildren (.apply parent-element new-children)))))))

;; ----------------------------------------------

#?(:cljs
   (defn html-text-to-dom [html-text]
     (let [^js/Element element (js/document.createElement "div")]
       (set! (.-innerHTML element) html-text)
       (.-firstElementChild element))))

(def ^:private inline-seq-children-xf
  (mapcat (fn [child] ;; Inline when child is a seq.
            (if (seq? child)
              child
              [child]))))

(defn $ [node-type & children]
  (VcupNode. node-type children))

(def ^:private ^:dynamic *xmlns-kw* :html)

#?(:cljs
   (defn process-vcup [vcup]
     (let [all-effects (atom [])
           to-dom-elements (fn to-dom-elements [vcup]
                             (cond
                               (nil? vcup)
                               []

                               (dom-node? vcup)
                               [vcup]

                               ;; Reactive fragment (i.e. if-fragment and for-fragment)
                               (reactive-fragment? vcup)
                               [vcup]

                               (attributes? vcup)
                               (throw (js/Error. "Attributes cannot be at the root of a scope."))

                               ;; Component result (when the component is directly invoked)
                               (component-result? vcup)
                               (let [{:keys [effects elements]} vcup]
                                 (swap! all-effects into effects)
                                 elements)

                               ;; Component invocation
                               (component-invocation? vcup)
                               (let [{component-fn :node-type
                                      args         :children} vcup]
                                 (recur (apply component-fn args)))

                               ;; Vcup fragment
                               (vcup-fragment? vcup)
                               (into []
                                     (comp inline-seq-children-xf
                                           (mapcat to-dom-elements))
                                     (:children vcup))

                               ;; ($ :div ,,,)
                               (vcup-element? vcup)
                               (let [node-type (:node-type vcup)
                                     [xmlns-kw ^js/Element element id classes] (if (instance? js/Element node-type)
                                                                                 ;; DOM element
                                                                                 (let [tag-name (str/lower-case (.-tagName node-type))
                                                                                       xmlns-kw (case tag-name
                                                                                                  "svg" :svg
                                                                                                  "math" :math
                                                                                                  :html)]
                                                                                   [xmlns-kw node-type nil nil])
                                                                                 ;; keywords like :div and :div#id.class1.class2
                                                                                 (let [{:keys [tag-name id classes]} (parse-element-tag (name node-type))
                                                                                       xmlns-kw (case tag-name
                                                                                                  "svg" :svg
                                                                                                  "math" :math
                                                                                                  *xmlns-kw*)
                                                                                       element (case xmlns-kw
                                                                                                 :svg  (js/document.createElementNS xmlns-svg tag-name)
                                                                                                 :math (js/document.createElementNS xmlns-math-ml tag-name)
                                                                                                 :html (js/document.createElement tag-name))]
                                                                                   [xmlns-kw element id classes]))
                                     children (:children vcup)

                                     ;; Collect all the attributes.
                                     attributes (cons (cond-> {}
                                                              (some? id) (assoc :id id)
                                                              (seq classes) (assoc :class classes))
                                                      (filterv attributes? children))

                                     ;; Convert the children into elements.
                                     child-elements (binding [*xmlns-kw* xmlns-kw]
                                                      (into []
                                                            (comp (remove attributes?)
                                                                  inline-seq-children-xf
                                                                  (mapcat to-dom-elements))
                                                            children))]
                                 ;; Set the element's attributes
                                 (if (every? attribute-map? attributes)
                                   (let [composed-attribute-maps (reduce compose-attribute-maps {} attributes)]
                                     (doseq [[attribute-kw attribute-value] composed-attribute-maps]
                                       (set-element-attribute xmlns-kw element attribute-kw attribute-value)))
                                   (swap! all-effects conj (dynamic-attributes-effect xmlns-kw element attributes)))

                                 ;; Set the element's children
                                 (if (every? dom-node? child-elements)
                                   (doseq [child-element child-elements]
                                     (-> element (.appendChild child-element)))
                                   (swap! all-effects conj (dynamic-children-effect element child-elements)))

                                 ;; Result
                                 [element])

                               (reactive-node? vcup)
                               (let [^js/Text text-node (js/document.createTextNode "")
                                     effect (sr/create-effect (fn []
                                                                (set! (.-textContent text-node) @vcup)))]
                                 (swap! all-effects conj effect)
                                 [text-node])

                               :else
                               [(js/document.createTextNode vcup)]))

           elements (to-dom-elements vcup)]
       (ComponentResult. @all-effects elements))))

;; ----------------------------------------------

(defn use-effect
  ([run-fn]
   (use-effect run-fn nil))
  ([run-fn options]
   (ComponentResult. [(sr/create-effect run-fn options)] nil)))

(defn attributes-effect [reactive-attributes]
  (AttributeEffect. reactive-attributes))

;; ----------------------------------------------

;; Q: Why was it an effect in the first place?
;; A: It is an effect because it owns effects, not computations.
;;    The effect of the scope-effect is to trigger those effects when it is run,
;;    which makes it effectful, so it needs to be an effect too.
(defn scope-effect
  "This effects manages a static collection of effect's lifecycle so that they are
   first-run and disposed when this effect is run and cleaned up."
  ([owned-effects]
   (scope-effect owned-effects nil))
  ([owned-effects options]
   (when-some [owned-effects (seq (remove nil? owned-effects))]
     (let [scope (sr/create-effect (fn []
                                     (run! sr/run-if-needed owned-effects)
                                     (sr/on-clean-up (fn []
                                                       (run! sr/dispose owned-effects))))
                                   options)]
       (doseq [owned-effect owned-effects]
         (sr/run-after owned-effect scope))
       scope))))

(defn if-fragment* [vcup-fn]
  #?(:cljs
     (ReactiveFragment.
       (sr/create-derived (fn []
                            (let [{:keys [effects elements]} (process-vcup (vcup-fn))]
                              ;; scope-effect is inside the ReactiveFragment's effect because
                              ;; we want its lifetime to depend on the `(boolean if-cond)` value.
                              (some-> (scope-effect effects)
                                      deref)
                              elements))
                          {:metadata {:name "active-if-branch-vcup"}}))))

(defmacro if-fragment [reactive+-condition then-vcup-expr else-vcup-expr]
  `(let [reactive+-condition# ~reactive+-condition
         boolean-condition# (sr/create-memo (fn []
                                              (boolean (deref+ reactive+-condition#))))
         vcup-fn# (fn []
                    (if @boolean-condition#
                      ~then-vcup-expr
                      ~else-vcup-expr))]
     (if-fragment* vcup-fn#)))

(defn- indexed-fragment [reactive-matched-index-or-nil
                         clause-index->clause-vcup
                         default-clause]
  #?(:cljs
     (ReactiveFragment.
       (sr/create-derived (fn []
                            (let [matched-index @reactive-matched-index-or-nil
                                  vcup-clause (cond
                                                (some? matched-index)
                                                (-> matched-index clause-index->clause-vcup)

                                                (= default-clause ::undefined)
                                                (throw (js/Error. "Missing default clause in indexed-fragment."))

                                                :else
                                                default-clause)
                                  {:keys [effects elements]} (process-vcup vcup-clause)]
                              (some-> (scope-effect effects)
                                      deref)
                              elements))
                          {:metadata {:name "index-fragment"}}))))

(defn case-fragment* [reactive+-value-expr
                      clause-value->clause-index
                      clause-index->clause-vcup
                      default-clause]
  (let [reactive-matched-index-or-nil (sr/create-memo (fn [] (-> (deref+ reactive+-value-expr)
                                                                 clause-value->clause-index)))]
    (indexed-fragment reactive-matched-index-or-nil
                      clause-index->clause-vcup
                      default-clause)))

(defmacro case-fragment [reactive+-value-expr & clauses]
  (let [[even-number-of-exprs default-clause] (if (even? (count clauses))
                                                [clauses ::undefined]
                                                [(butlast clauses) (last clauses)])
        clauses (partitionv 2 even-number-of-exprs)
        clause-value->clause-index (into {}
                                         (comp (map-indexed (fn [index [clause-value _clause-vcup]]
                                                              (if (seq? clause-value)
                                                                (->> clause-value
                                                                     (mapv (fn [clause-value-item]
                                                                             [clause-value-item index])))
                                                                [[clause-value index]])))
                                               cat)
                                         clauses)
        clause-index->clause-vcup (mapv second clauses)]
    `(case-fragment* ~reactive+-value-expr
                     ~clause-value->clause-index
                     ~clause-index->clause-vcup
                     ~default-clause)))

(defn cond-fragment* [reactive-index-fn
                      clause-index->clause-vcup]
  (indexed-fragment (sr/create-memo reactive-index-fn)
                    clause-index->clause-vcup
                    nil))

(defmacro cond-fragment [& clauses]
  (assert (even? (count clauses)) "cond-fragment requires an even number of forms")
  (let [clauses (partitionv 2 clauses)
        clause-index->clause-vcup (mapv second clauses)]
    `(cond-fragment* (fn []
                       (cond
                         ~@(into []
                                 (comp (map-indexed (fn [index [clause-condition _clause-vcup]]
                                                      [`~clause-condition index]))
                                       cat)
                                 clauses)))
                     ~clause-index->clause-vcup)))

#?(:cljs
   (defn for-fragment*
     ([reactive+-coll item-component]
      (for-fragment* reactive+-coll identity item-component))
     ([reactive+-coll key-fn item-component]
      (let [item-cache-atom (atom {})] ;; mapping: item -> [scope elements]
        (ReactiveFragment.
          (sr/create-derived (fn []
                               (let [coll (deref+ reactive+-coll)

                                     ;; Update the cache:
                                     ;; - only keep the current item keys,
                                     ;; - compute values for things not already cached.
                                     old-item-cache @item-cache-atom
                                     new-item-cache (into {}
                                                          (map (fn [item]
                                                                 (let [k (key-fn item)]
                                                                   [k
                                                                    (if (contains? old-item-cache k)
                                                                      (get old-item-cache k)
                                                                      (let [{:keys [effects elements]} (process-vcup ($ item-component item))]
                                                                        [(scope-effect effects) elements]))])))
                                                          coll)]
                                 (reset! item-cache-atom new-item-cache)

                                 ;; Return the aggregated elements
                                 (into []
                                       (mapcat (fn [item]
                                                 (let [[scope elements] (get new-item-cache (key-fn item))]
                                                   ;; Makes this signal depend on the scope.
                                                   (some-> scope
                                                           deref)

                                                   ;; Return the elements
                                                   elements)))
                                       coll)))
                             {:metadata {:name "for-fragment"}}))))))

;; ----------------------------------------------

(def ^:private ^:dynamic *context*)

(defn get-context []
  *context*)

(defmacro with-context [new-context vcup]
   `(binding [*context* ~new-context]
      (process-vcup ~vcup)))

(defmacro with-context-update [context-fn vcup]
  `(let [parent-context# *context*
         new-context# (sr/create-derived (fn [] (~context-fn parent-context#)))]
     (with-context new-context# ~vcup)))

;; ----------------------------------------------

#?(:cljs
   (defn- re-run-stale-effectful-nodes-at-next-frame []
     (js/requestAnimationFrame (fn []
                                 (sr/re-run-stale-effectful-nodes)
                                 (re-run-stale-effectful-nodes-at-next-frame)))))

#?(:cljs
   (defn render [^js/Element parent-element vcup]
     (let [{:keys [effects elements]} (process-vcup vcup)]
       ;; Set all the elements as children of parent-element
       (.apply (.-replaceChildren parent-element) parent-element (to-array elements))
       ;; Run all the effects once
       (run! sr/run-if-needed effects)

       ;; Automatically refresh the DOM by re-running the effects which need a re-run.
       (re-run-stale-effectful-nodes-at-next-frame))))

#?(:cljs
   (defn dispose-render-effects []
     ;; TODO: dispose all the effects used for the rendering and the DOM updates.
     ;; In this article, we will skip this step.
     ,))
