(ns vrac.web
  (:require [clojure.string :as str]
            #?(:cljs [goog.object :as gobj])
            [signaali.reactive :as sr]))

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

(defn- vcup-element? [x]
  (and (instance? VcupNode x)
       (not= (:node-type x) :<>)
       (simple-keyword? (:node-type x))))

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
  (instance? sr/ReactiveNode x))

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
              (assoc acc :element-tag part)))
          {:element-tag "div"
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
   (defn- set-element-attribute [^js/Element element attribute-kw attribute-value]
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
             (-> element (gobj/set attribute-name attribute-value))))))))

#?(:cljs
   (defn- unset-element-attribute [^js/Element element attribute-kw attribute-value]
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
           (-> element (gobj/set attribute-name nil)))))))

#?(:cljs
   (defn- dynamic-attributes-effect [^js/Element element attrs]
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
                           (let [attributes (transduce (map deref) compose-attribute-maps {} attrs)]
                             (doseq [[attribute-kw attribute-value] attributes]
                               (set-element-attribute element attribute-kw attribute-value))
                             (sr/on-clean-up (fn []
                                               (doseq [[attribute-kw attribute-value] attributes]
                                                 (unset-element-attribute element attribute-kw attribute-value))))))))))

#?(:cljs
   (defn dynamic-children-effect
     "Dynamically update the DOM node so that its children keep representing the elements array.
   The elements are either js/Element or a reactive node whose value is a sequence of js/Element."
     [^js/Element parent-element elements]
     (sr/create-effect (fn []
                         (let [new-children (make-array 0)]
                           (doseq [element elements]
                             (if (reactive-fragment? element)
                               (doseq [sub-element @(:reactive-node element)]
                                 (.push new-children sub-element))
                               (.push new-children element)))
                           (-> parent-element .-replaceChildren (.apply parent-element new-children)))))))

;; ----------------------------------------------

(def ^:private inline-seq-children-xf
  (mapcat (fn [child] ;; Inline when child is a seq.
            (if (seq? child)
              child
              [child]))))

(defn $ [node-type & children]
  (VcupNode. node-type children))

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
                               (let [{:keys [element-tag id classes]} (parse-element-tag (name (:node-type vcup)))
                                     ^js/Element element (js/document.createElement element-tag)
                                     children (:children vcup)

                                     ;; Collect all the attributes.
                                     attributes (cons (cond-> {}
                                                              (some? id) (assoc :id id)
                                                              (seq classes) (assoc :class classes))
                                                      (filterv attributes? children))

                                     ;; Convert the children into elements.
                                     child-elements (into []
                                                          (comp (remove attributes?)
                                                                inline-seq-children-xf
                                                                (mapcat to-dom-elements))
                                                          children)]
                                 ;; Set the element's attributes
                                 (if (every? attribute-map? attributes)
                                   (let [composed-attribute-maps (reduce compose-attribute-maps {} attributes)]
                                     (doseq [[attribute-kw attribute-value] composed-attribute-maps]
                                       (set-element-attribute element attribute-kw attribute-value)))
                                   (swap! all-effects conj (dynamic-attributes-effect element attributes)))

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
