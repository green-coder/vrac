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

(defn counters-article []
  ($ :article
     ($ :h2 "Reactive counters")
     (for [i (range 3)]
       (let [counter-state (sr/create-state (* i 100))]
         ($ counter-component counter-state)))))

(defn controlled-input-article []
  ($ :article
     ($ :h2 "Controlled input")
     (let [text (sr/create-state "short text")]
       ($ :input
          (vw/attributes-effect (fn [] {:value @text}))
          {:on-input (fn [^js event]
                       (let [s (-> event .-target .-value)]
                         (reset! text (subs s (max (- (count s) 10) 0)))))}))))

(defn reactive-data-root []
  ($ :div
     ($ counters-article)
     ($ controlled-input-article)))
