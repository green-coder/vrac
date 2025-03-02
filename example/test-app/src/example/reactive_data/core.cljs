(ns example.reactive-data.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

;; Updating the state makes the DOM update
(defn counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "Increment")
        ($ :button {:on-click #(reset! counter-state 0)} "Reset"))))

(defn reactive-data-root []
  ($ :article
     ($ :h2 "Reactive counters")
     (for [i (range 3)]
       (let [counter-state (sr/create-state (* i 100))]
         ($ counter-component counter-state)))))
