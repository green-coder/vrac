(ns example.reactive-fragment.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "+1")
        ($ :button {:on-click #(swap! counter-state (fn [n] (+ n 2)))} "+2"))))

(defn- if-fragment-article []
  (let [counter-state (sr/create-state 0)]
    ($ :article
       ($ :h3 "If fragment")
       ($ counter-component counter-state)
       (vw/if-fragment (fn [] (even? @counter-state))
         ($ :div "The value is even.")
         ($ :div "The value is odd.")))))

(defn- case-fragment-article []
  (let [state (sr/create-state {:current-route :route/homepage})]
    ($ :article
       ($ :h3 "Case fragment")
       ($ :button {:on-click #(swap! state assoc :current-route :route/homepage)} "Home")
       ($ :button {:on-click #(swap! state assoc :current-route :route/blog)}     "Blog")
       ($ :button {:on-click #(swap! state assoc :current-route :route/about)}    "About")
       ($ :button {:on-click #(swap! state assoc :current-route :route/bookmark)} "Non-existing route")
       (vw/case-fragment (fn [] (:current-route @state))
          :route/homepage ($ :div "This is the homepage.")
          :route/blog     ($ :div "This is the blog.")
          :route/about    ($ :div "This is the about page.")
          ($ :div "This is the 'not found' page, for any other route.")))))

(defn- cond-fragment-article []
  (let [current-route (sr/create-state :route/homepage)]
    ($ :article
       ($ :h3 "Cond fragment")
       ($ :button {:on-click #(reset! current-route :route/homepage)} "Home")
       ($ :button {:on-click #(reset! current-route :route/blog)}     "Blog")
       ($ :button {:on-click #(reset! current-route :route/about)}    "About")
       ($ :button {:on-click #(reset! current-route :route/bookmark)} "Non-existing route")
       ;; The vw/cond-fragment macro groups the clauses' conditions and wraps them into a fn.
       (vw/cond-fragment
         (= @current-route :route/homepage) ($ :div "This is the homepage.")
         (= @current-route :route/blog)     ($ :div "This is the blog.")
         (= @current-route :route/about)    ($ :div "This is the about page.")
         :else ($ :div "This is the 'not found' page, for any other route.")))))

(defn- for-fragment-article []
  (let [state (sr/create-state {:persons [{:id 0 :name "Alice"}
                                          {:id 1 :name "Barbara"}
                                          {:id 2 :name "Cedric"}]})]
    ($ :article
       ($ :h3 "For fragment")

       (let [new-person-name (sr/create-state "Louise")]
         ($ :div
            ($ :input
               (vw/attributes-effect (fn [] {:value @new-person-name}))
               {:on-input (fn [event]
                            (reset! new-person-name (-> event .-target .-value)))})
            ($ :button {:on-click (fn []
                                    (swap! state update :person
                                           (fn [persons]
                                             (-> persons
                                                 (conj {:id (count persons)
                                                        :name @new-person-name}))))
                                    (reset! new-person-name ""))}
               "Add")))

       ($ :div
          (vw/for-fragment (fn [] (:persons @state))
                           :id
                           (fn [{:keys [id name]}]
                             ($ :div "[" id "] " name)))))))

(defn reactive-fragment-root []
  ($ :div
     ($ if-fragment-article)
     ($ case-fragment-article)
     ($ cond-fragment-article)
     ($ for-fragment-article)
     ,))
