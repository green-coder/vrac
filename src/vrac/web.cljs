(ns vrac.web
  (:require [clojure.string :as str]
            [signaali.reactive :as sr]))

(defrecord VcupNode [node-type children])
(defrecord ReactiveFragment [reactive-node])
(defrecord AttributeEffect [reactive-attributes])
(defrecord ComponentResult [effects elements])

;; ----------------------------------------------

(defn- dom-node? [x]
  (instance? js/Node x))

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

(defn- set-element-attribute [^js/Element element attribute-name attribute-value]
  (cond
    ;; Event listener
    (str/starts-with? attribute-name "on-")
    (.addEventListener element
                       (-> attribute-name (subs (count "on-")))
                       attribute-value)

    :else
    :to-be-defined-in-next-articles))

;; ----------------------------------------------

(def ^:private inline-seq-children-xf
  (mapcat (fn [child] ;; Inline when child is a seq.
            (if (seq? child)
              child
              [child]))))

(defn $ [node-type & children]
  (VcupNode. node-type children))

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

                            ;; Hiccup fragment
                            (vcup-fragment? vcup)
                            (into []
                                  (comp inline-seq-children-xf
                                        (mapcat to-dom-elements))
                                  (:children vcup))

                            ;; ($ :div ,,,)
                            (vcup-element? vcup)
                            (let [^js/Element element (js/document.createElement (name (:node-type vcup)))
                                  [attributes children] (let [children (:children vcup)
                                                              x (first children)]
                                                          (if (and (map? x)
                                                                   (not (record? x))) ; because map? returns true on records
                                                            [x (next children)]
                                                            [nil children]))
                                  ;; Convert the children into elements.
                                  child-elements (into []
                                                       (comp (remove nil?)
                                                             ;; Inline elements when child is a seq.
                                                             (mapcat (fn [child]
                                                                      (if (seq? child)
                                                                          child
                                                                          [child])))
                                                             (mapcat to-dom-elements))
                                                       children)]
                              ;; Set the attributes on the created element.
                              (doseq [[attribute-name attribute-value] attributes]
                               (set-element-attribute element (name attribute-name) attribute-value))
                              ;; Set the element's children
                              (doseq [child-element child-elements]
                                (-> element (.appendChild child-element)))
                              [element])

                            (reactive-node? vcup)
                            (let [^js element (js/document.createTextNode "")
                                  effect (sr/create-effect (fn []
                                                             (set! (.-textContent element) @vcup)))]
                              (swap! all-effects conj effect)
                              [element])

                            :else
                            [(js/document.createTextNode vcup)]))

        elements (to-dom-elements vcup)]
    (ComponentResult. @all-effects elements)))

(defn- re-run-stale-effectful-nodes-at-next-frame []
  (js/requestAnimationFrame (fn []
                              (sr/re-run-stale-effectful-nodes)
                              (re-run-stale-effectful-nodes-at-next-frame))))

(defn render [^js/Element parent-element vcup]
  (let [{:keys [effects elements]} (process-vcup vcup)]
    ;; Set all the elements as children of parent-element
    (.apply (.-replaceChildren parent-element) parent-element (to-array elements))
    ;; Run all the effects once
    (run! sr/run-if-needed effects)

    ;; Automatically refresh the DOM by re-running the effects which need a re-run.
    (re-run-stale-effectful-nodes-at-next-frame)))

(defn dispose-render-effects []
  ;; TODO: dispose all the effects used for the rendering and the DOM updates.
  ;; In this article, we will skip this step.
  ,)
