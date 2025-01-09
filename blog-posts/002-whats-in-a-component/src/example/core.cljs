(ns example.core
  (:require [clojure.string :as str]
            [goog.object :as gobj]
            [signaali.reactive :as sr]))

(defrecord VcupNode [element-tag children])

(defn $ [element-tag & children]
  (VcupNode. element-tag children))

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
                            (string? vcup)
                            [(js/document.createTextNode vcup)]

                            (instance? VcupNode vcup)
                            (let [^js/Element element (js/document.createElement (name (:element-tag vcup)))
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

(defn render [^js/Element parent-element vcup]
  (let [{:keys [effects elements]} (process-vcup vcup)]
    ;; Set all the elements as children of parent-element
    (.apply (.-replaceChildren parent-element) parent-element (to-array elements))
    ;; Run all the effects
    (run! sr/run-if-needed effects)))

(defn dispose-render-effects []
  ;; TODO: dispose all the effects used for the rendering and the DOM updates.
  ;; In this article, we will skip this step.
  ,)

;; Above this line is Vrac, below this line is the user land

(defn my-counter [counter-state]
  ($ :div "Counter value:" counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "Increment")
        ($ :button {:on-click #(reset! counter-state 0)} "Reset"))))

(defn my-html [nb-counters]
  ($ :main
     ($ :h1 "Developing Vrac from scratch - components")
     ($ :p "This demonstrates how Vrac is implemented.")
     ($ :h2 "Reactive counters")
     (for [i (range nb-counters)]
       (let [counter-state (sr/create-state (* i 100))]
         (my-counter counter-state)))
     ($ :h2 "Lazy effects")
     ($ :div "The effects are lazy, the user decides when to re-run them. Click the button to update the DOM."
        ($ :div
           ($ :button {:on-click #(sr/re-run-stale-effectful-nodes)} "Run effects")))))

;; Shadow-CLJS hooks: start & reload the app

(defn ^:dev/after-load setup! []
  (render (js/document.getElementById "app")
          (my-html 3)))

(defn ^:dev/before-load shutdown! []
  (dispose-render-effects))

(defn start-app []
  (setup!))
