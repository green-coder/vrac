(ns example.reactive-fragment.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div ($ :button {:on-click #(swap! counter-state inc)} "Increment"))))

(defn- if-fragment-component []
  (let [counter-state (sr/create-state 0)]
    ($ :article
       ($ :h3 "If fragment")
       ($ counter-component counter-state)
       (vw/if-fragment (sr/create-memo
                         (fn []
                           (even? @counter-state)))
                       ($ :div "The value is even.")
                       ($ :div "The value is odd.")))))

(defn reactive-fragment-root []
  ($ :div
     ($ if-fragment-component)))
