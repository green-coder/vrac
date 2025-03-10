(ns example.reactive-fragment
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn- counter-component [counter-state]
  ($ :div
     "Counter value: " counter-state
     ($ :div
        ($ :button {:on-click #(swap! counter-state inc)} "+1")
        ($ :button {:on-click #(swap! counter-state (fn [n] (+ n 2)))} "+2"))))

(defn- if-fragment-article []
  (let [counter-state (sr/create-state 0)]
    ($ :article
       ($ :h2 "If fragment")
       ($ counter-component counter-state)
       (vw/if-fragment (fn [] (even? @counter-state))
         ($ :div "The value is even.")
         ($ :div "The value is odd.")))))

(defn- case-fragment-article []
  (let [state (sr/create-state {:current-route :route/homepage})]
    ($ :article
       ($ :h2 "Case fragment")
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
       ($ :h2 "Cond fragment")
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
       ($ :h2 "For fragment")

       (let [new-person-name (sr/create-state "Louise")]
         ($ :div
            ($ :input
               (vw/attributes-effect (fn [] {:value @new-person-name}))
               {:on-input (fn [event]
                            (reset! new-person-name (-> event .-target .-value)))})
            ($ :button {:on-click (fn []
                                    (swap! state update :persons
                                           (fn [persons]
                                             (-> persons
                                                 (conj {:id (count persons)
                                                        :name @new-person-name}))))
                                    (reset! new-person-name ""))}
               "Add")))

       ($ :div
          (vw/for-fragment* (fn [] (:persons @state))    ;; coll-fn
                            :id                          ;; key-fn
                            (fn [{:keys [id name]}]      ;; item-component
                              ($ :div "[" id "] " name)))
          #_ ;; ideally
          (vw/for-fragment [{:keys [id name]} (:persons @state)]
             ^{:key id} ($ :div "[" id "] " name))

          #_ ;; nice to have: supports the Clojure for syntax
          (vw/for-fragment [person (:persons @state)
                            :let [{:keys [id name]} person]
                            :when (even? id)
                            :while (< id 10)]
             ^{:key id} ($ :div "[" id "] " name))
          #_ ;; That would translate to this:
          (vw/for-fragment* (fn []
                              (for [person (:persons @state)
                                    :let [{:keys [id name]} person]
                                    :when (even? id)
                                    :while (< id 10)]
                                ;; Collects everything which is used in the body of the `for`,
                                ;; either for the key-fn or the item-component
                                {:id id
                                 :name name}))
                            ;; Collects everything used in the key expression, here only `id`.
                            (fn [{:keys [id]}]
                              id)
                            ;; Collects everything used in the item, here `id` and `name`.
                            (fn [{:keys [id name]}]
                              ($ :div "[" id "] " name)))

          ;; Idea: write some unit tests for the for-fragment macro
          ,))))

(defn reactive-fragment-root []
  ($ :div
     ($ if-fragment-article)
     ($ case-fragment-article)
     ($ cond-fragment-article)
     ($ for-fragment-article)
     ,))
