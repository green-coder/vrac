(ns example.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn my-counter [counter-state]
  ($ :div "Counter value:" counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "Increment")
        ($ :button {:on-click #(reset! counter-state 0)} "Reset"))))

(defn root-component [nb-counters]
  ($ :main
     ($ :h2 "Reactive counters")
     (for [i (range nb-counters)]
       (let [counter-state (sr/create-state (* i 100))]
         (my-counter counter-state)))))

;; Shadow-CLJS hooks: start & reload the app

(defn ^:dev/after-load setup! []
  (vw/render (js/document.getElementById "app")
             (root-component 3)))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn start-app []
  (setup!))
