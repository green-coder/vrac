(ns example.context.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(def ^:dynamic *diy-context* (sr/create-signal {:a 10}))

(defn- display-diy-context [label]
  (let [context *diy-context*]
    ($ :div label ": " context)))

(defn- diy-context-article []
  (let [counter (sr/create-signal 0)]
    ($ :article
       ($ :h3 "DIY context using dynamic variables")
       ($ :button {:on-click #(swap! counter inc)} "counter = " counter)
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
          "This sample won't work if we call the inner components using "
          ($ :code "($ display-diy-context ,,,)")))))

(defn context-root []
  ($ :div
     ($ diy-context-article)
     ,))
