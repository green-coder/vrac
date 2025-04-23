(ns example.context
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(def ^:dynamic *diy-context* (sr/create-signal {:a 10}))

(defn- display-diy-context [label]
  (let [context *diy-context*]
    ($ :div label ": " context)))

(defn- diy-context-article []
  (let [counter (sr/create-signal 0)]
    ($ :article
       ($ :h2 "DIY context using dynamic variables")
       ($ :button {:on/click #(swap! counter inc)} "counter = " counter)
       (display-diy-context "global context")
       (let [context *diy-context* ;; The current context, expectedly a reactive node.
             local-context (sr/create-derived (fn [] (-> @context
                                                         (assoc :counter @counter)
                                                         (update :a + @counter))))]
         ;; Create a derived inner context, and set it as the new current one within this scope.
         (binding [*diy-context* local-context]
           ;; Read the new current context from somewhere in the scope.
           (display-diy-context "local context")))
       (display-diy-context "back to the global context")

       ($ :div
          ($ :strong "Warning: ")
          "This won't work if we call the inner components using "
          ($ :code "($ display-diy-context ,,,)")))))

(defn- display-built-in-context [label]
  ($ :div label ": " (vw/get-context)))

(defn- builtin-context-article []
  (let [root-context (sr/create-signal {:a 10})
        counter (sr/create-signal 0)]
    (vw/with-context root-context
      ($ :article
         ($ :h2 "Built-in context")
         ($ :button {:on/click #(swap! counter inc)} "counter = " counter)
         ($ display-built-in-context "global context")
         (vw/with-context-update (fn [parent-context]
                                   (-> @parent-context
                                       (assoc :counter @counter)
                                       (update :a + @counter)))
           ($ display-built-in-context "local context"))
         ($ display-built-in-context "back to the global context")))))

(defn- person-component [{:keys [name]}]
  ($ :<>
     name
     (if-some [context (vw/get-context)]
       (sr/create-derived (fn []
                            (when (:birthday-party @context)
                              " says HAPPY BIRTHDAY !!! \uD83E\uDD73")))
       " is there, where is the context ?!")))

(defn- context-on-reactive-fragment []
  (let [root-context (sr/create-signal {:birthday-party true})
        person-count (sr/create-signal 2)
        persons (sr/create-derived (fn []
                                     (->> (cycle ["Alice" "Bob" "Connie" "Diva" "Elric" "Fred" "Giana"])
                                          (take @person-count)
                                          (map-indexed (fn [index name]
                                                         {:id index
                                                          :name name})))))]
    ($ :article
       ($ :h2 "Context on a reactive fragment")
       ($ :button {:on/click #(swap! person-count inc)} "Add 1 person to the party")
       ($ :button {:on/click #(swap! root-context update :birthday-party not)}
          (sr/create-memo (fn []
                            (if (-> @root-context :birthday-party)
                              "Turn the party OFF"
                              "Turn the party ON"))))
       (vw/with-context root-context
         ($ :ul
            (vw/for-fragment* persons :id
               (fn [person] ($ :li
                               ($ person-component person)))))))))

(defn context-root []
  ($ :div
     ($ diy-context-article)
     ($ builtin-context-article)
     ($ context-on-reactive-fragment)
     ,))
