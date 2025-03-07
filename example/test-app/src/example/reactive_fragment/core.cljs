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
       (vw/if-fragment (sr/create-derived
                         (fn []
                           (even? @counter-state))
                         {:metadata {:name "if-cond"}})
         ($ :div "The value is even.")
         ($ :div "The value is odd.")))))

  (let [current-route (sr/create-state :route/homepage)]
(defn- case-fragment-article []
    ($ :article
       ($ :h3 "Case fragment")
       ($ :button {:on-click #(reset! current-route :route/homepage)} "Home")
       ($ :button {:on-click #(reset! current-route :route/blog)}     "Blog")
       ($ :button {:on-click #(reset! current-route :route/about)}    "About")
       ($ :button {:on-click #(reset! current-route :route/bookmark)} "Non-existing route")
       (vw/case-fragment @current-route
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
  (let [persons (sr/create-state [{:id 0 :name "Alice"}
                                  {:id 1 :name "Barbara"}
                                  {:id 2 :name "Cedric"}])]
    ($ :article
       ($ :h3 "For fragment")

       (let [new-person-name (sr/create-state "Louise")]
         ($ :div
            ($ :input
               (vw/attributes-effect (fn [] {:value @new-person-name}))
               {:on-change (fn [event]
                             (reset! new-person-name (-> event .-target .-value)))})
            ($ :button {:on-click (fn []
                                    (swap! persons conj {:id (count @persons)
                                                         :name @new-person-name})
                                    (reset! new-person-name ""))}
               "Add")))

       ($ :div
          (vw/for-fragment (fn [] @persons) :id
             (fn [{:keys [id name]}]
               ($ :div "[" id "] " name)))))))

(defn reactive-fragment-root []
  ($ :div
     ($ if-fragment-article)
     ($ case-fragment-article)
     ($ cond-fragment-article)
     ($ for-fragment-article)
     ,))
