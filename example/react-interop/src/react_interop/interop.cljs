(ns react-interop.interop
  (:require [signaali.reactive :as sr]
            [reagent.core :as r]
            [vrac.web :as vw :refer [$]]
            [uix.core :as uix]
            [uix.dom :as dom]))

;; ---------------------------------------------------
;; Interop utility function

;; Signaali effect which mounts some React vdom on a dom node.
(defn mount-react-root-effect [dom-node-ref react-vdom-fn]
  (let [create-root-effect (sr/create-effect
                             (fn []
                               (when-some [dom-node @dom-node-ref]
                                 (let [^js react-root (dom/create-root dom-node)]
                                   (sr/on-clean-up (fn [] (dom/unmount-root react-root)))
                                   ;; This value is returned when this effect is deref'd
                                   react-root))))
        re-render-effect (sr/create-effect
                           (fn []
                             (when-some [react-root @create-root-effect]
                               (dom/render-root (react-vdom-fn) react-root))))]
    (vw/use-effects [create-root-effect re-render-effect])))


;; ---------------------------------------------------
;; Siagent

;; React component implemented using Siagent
;; It demonstrates the interop with the reactive node from Signaali:
;; Basically no interop is needed, Siagent is taking care of it.
(defn my-reagent-component [text text-in-reactive-node]
  [:div
   [:div "React function's input: \"" text "\""]
   [:div "Signaali reactive node: \"" @text-in-reactive-node "\""]])

;; Vrac component
(defn interop-with-reagent-component-article [text1 text2]
  ($ :article
    ($ :h2 "React component implemented via Siagent")
    (let [dom-node-ref (sr/create-signal nil)]
      ($ :div {:ref dom-node-ref}
        (mount-react-root-effect dom-node-ref
          (fn []
            (r/as-element [my-reagent-component @text1 text2])))))))

;; ---------------------------------------------------
;; UIx

;; React component implemented using UIx
;; It demonstrates the interop with the reactive node from Signaali:
;; You need to use the `use-reactive` React hook.
(uix/defui my-react-component [{:keys [text text-in-reactive-node]}]
  (uix/$ :div
    (uix/$ :div "React function's input: \"" text "\"")
    (uix/$ :div "Signaali reactive node: \"" (r/use-reactive text-in-reactive-node) "\"")))

;; Vrac component
(defn interop-with-uix-component-article [text1 text2]
  ($ :article
    ($ :h2 "React component implemented via UIx")
    (let [dom-node-ref (sr/create-signal nil)]
      ($ :div {:ref dom-node-ref}
        (mount-react-root-effect dom-node-ref
          (fn []
            (uix/$ my-react-component {:text @text1
                                       :text-in-reactive-node text2})))))))

;; ---------------------------------------------------

(defn- text-input [label text-signal]
  ($ :div
     label ": "
     ($ :input
        (vw/attributes-effect (fn [] {:value @text-signal}))
        {:on-input (fn [^js event]
                     (reset! text-signal (-> event .-target .-value)))})))

;; Vrac component
(defn interop-demo []
  ($ :main
     ($ :h1 "Vrac + React interop demo")
     (let [text1 (sr/create-signal "text1")
           text2 (sr/create-signal "text2")]
       ($ :div
          ($ text-input "Text1" text1)
          ($ text-input "Text2" text2)

          ($ interop-with-reagent-component-article text1 text2)
          ($ interop-with-uix-component-article text1 text2)))))
