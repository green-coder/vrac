(ns example.reactive-data
  (:require [clojure.string :as str]
            [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

;; Updating the state makes the DOM update
(defn- counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div
        ($ :button {:on/click #(swap! counter-state inc)} "Increment")
        ($ :button {:on/click #(reset! counter-state 0)} "Reset"))))

(defn- counters-article []
  ($ :article
     ($ :h2 "Reactive counters")
     (for [i (range 3)]
       (let [counter-state (sr/create-state (* i 100))]
         ($ counter-component counter-state)))))

(defn- controlled-input-article []
  ($ :article
     ($ :h2 "Controlled input")
     ($ :div "This input prevents its content from containing \"foobar\".")
     (let [text-signal (sr/create-signal "foo")]
       ($ :input
          (vw/props-effect (fn [] {:value @text-signal}))
          {:on/input (fn [^js event]
                       (let [text (-> event .-target .-value)]
                         (swap! text-signal (fn [previous-text]
                                              (if (str/includes? text "foobar")
                                                previous-text
                                                text)))))}))))

(defn reactive-data-root []
  ($ :div
     ($ counters-article)
     ($ controlled-input-article)))
