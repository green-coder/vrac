(ns vrac.web
  (:require [clojure.string :as str]
            [signaali.reactive :as sr]))

(defrecord VcupNode [node-type children])

(defn $ [node-type & children]
  (VcupNode. node-type children))

(defn- component-invocation? [x]
  (and (instance? VcupNode x)
       (fn? (:node-type x))))

(defn- set-element-attribute [^js/Element element attribute-name attribute-value]
  (cond
    ;; Event listener
    (str/starts-with? attribute-name "on-")
    (.addEventListener element
                       (-> attribute-name (subs (count "on-")))
                       attribute-value)

    :else
    :to-be-defined-in-next-articles))

(defn process-vcup [vcup]
  (let [all-effects (atom [])
        to-dom-elements (fn to-dom-elements [vcup]
                          (cond
                            ;; Component invocation
                            (component-invocation? vcup)
                            (let [{component-fn :node-type
                                   args         :children} vcup]
                              (recur (apply component-fn args)))

                            (string? vcup)
                            [(js/document.createTextNode vcup)]

                            (instance? VcupNode vcup)
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

                            (instance? sr/ReactiveNode vcup)
                            (let [element (js/document.createTextNode "")
                                  effect (sr/create-effect (fn []
                                                             (set! (.-textContent element) @vcup)))]
                              (swap! all-effects conj effect)
                              [element])

                            :else
                            :to-be-defined-in-next-articles))
        elements (to-dom-elements vcup)]
    {:effects @all-effects
     :elements elements}))

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
