(ns example.context.core
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(def ^:dynamic *context* (sr/create-signal {:a 1}))

(defn- diy-context-article []
  (let [counter (sr/create-signal 20)]
    ($ :article
       ($ :h3 "Context")

       ($ :button {:on-click #(swap! counter inc)} counter)

       (let [;; Read the current context, which is expectedly a reactive node.
             context *context*
             ;; Create a derived reactive node, for an inner context.
             inner-context (sr/create-derived (fn [] (-> @context (assoc :b @counter))))]
         ;; Set it as the new current context within this scope.
         (binding [*context* inner-context]

           ;; Read the new current context from somewhere in the scope.
           (let [context *context*]
             ($ :div "inner context: " context))))

       ;; Read the new current context from somewhere outside the scope.
       (let [context *context*]
         ($ :div "outer context: " context)))))

(defn context-root []
  ($ :div
     ($ diy-context-article)
     ,))
