(ns example.reactive-fragment.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "+1")
        ($ :button {:on-click #(swap! counter-state (fn [n] (+ n 2)))} "+2"))))

(defn- if-fragment-component []
  (let [counter-state (sr/create-state 0)]
    ($ :article
       ($ :h3 "If fragment")
       ($ counter-component counter-state)
       (vw/if-fragment (sr/create-derived
                         (fn []
                           (even? @counter-state))
                         {:metadata {:name "if-cond"}})
         ($ :div "The value is even.")
         ($ :div "The value is odd.")))))

(defn- case-fragment-component []
  (let [current-route (sr/create-state :route/homepage)]
    ($ :article
       ($ :h3 "Case fragment")
       ($ :button {:on-click #(reset! current-route :route/homepage)} "Home")
       ($ :button {:on-click #(reset! current-route :route/blog)} "Blog")
       ($ :button {:on-click #(reset! current-route :route/about)} "About")
       ($ :button {:on-click #(reset! current-route :route/bookmark)} "Non-existing route")
       (vw/case-fragment current-route
          :route/homepage ($ :div "This is the homepage.")
          :route/blog ($ :div "This is the blog.")
          :route/about ($ :div "This is the about page.")
          ($ :div "This is the 'not found' page, for any other route.")))))

(defn reactive-fragment-root []
  ($ :div
     ($ if-fragment-component)
     ($ case-fragment-component)
     ,))
