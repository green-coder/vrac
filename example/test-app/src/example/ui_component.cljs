(ns example.ui-component
  (:require [clojure.string :as str]
            [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

;; ----------------------------------------------
;; UI library's API

(defn- component-from-context [component-key]
  (fn [props & children]
    (let [context (vw/get-context)
          component-resolver (sr/create-memo
                               (fn []
                                 (get @context component-key)))]
      (vw/reactive-fragment
        (fn []
          ($ @component-resolver props children))))))

(def button
  (component-from-context :ui.component/button))

;; ----------------------------------------------
;; UI library's implementation

;; Components' implementation
(defn default-button-component [props children]
  ($ :button props children))

;; Bindings
(def component-registry
  {:ui.component/button default-button-component})

;; ----------------------------------------------

(defn my-custom-button [props children]
  ($ :div {:style {:display "inline-block"
                   :background-color "lightblue"
                   :padding "1em"
                   :border-radius "0.5em"}}
     ($ :button
        props
        {:style {:color "white"
                 :border-radius "0.5em"}}
        ($ :strong
           (map str/upper-case children)))))

(defn overridable-ui-component []
  ($ :article
     ($ :h2 "Overridable UI component")

     (vw/with-context (sr/create-signal component-registry)
       ($ :div
          ;; Default button
          ($ button {:style {:background-color "pink"}} "Button")

          (vw/with-context-update (fn [context]
                                    (-> @context
                                        (assoc :ui.component/button my-custom-button)))

             ;; Will resolve to the custom button component,
             ;; different behavior, structure and style.
             ($ button {:style {:background-color "pink"}} "Button"))))))

(defn ui-component-root []
  ($ :div
    ($ overridable-ui-component)))
