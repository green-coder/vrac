(ns vrac.event
  (:require [vrac.util :as vu]))

(defn interceptors->fns
  "Returns a vector of the functions from the interceptors queued in the right order."
  [interceptors]
  (into []
        (remove nil?)
        (concat (map :before interceptors)
                (reverse (map :after interceptors)))))

(defn handle-event
  "Returns a context associated to an event, transformed by a chain of functions."
  [event fns]
  (reduce (fn [context f] (f context))
          {:coeffects {:event event}
           :effects []}
          fns))

(defn effects-processor
  "Returns a function that will process effects via the effect handlers
   registered in type->effect-handlers."
  [type->effect-handler]
  (fn process-effects! [context]
    (doseq [[type value] (:effects context)]
      (let [handler (type->effect-handler type)]
        (handler value)))))

(defn event-dispatcher
  "Routes an event to the right handler, process its context through a chain of function,
   then process the corresponding effects.

   event-type->interceptors has the form:
   ```clojure
   {:my-event [interceptor-1 ... interceptor-n]
    :other-event ...}
   ```

  effect-type->effect-handler has the form:
  ```clojure
  {:my-effect (fn [value] (do-side-effects!))
   :other-effect ...}
  ```
   "
  [event-type->interceptors effect-type->effect-handler]
  (let [event-type->fns (vu/map-vals event-type->interceptors interceptors->fns)
        process-effects! (effects-processor effect-type->effect-handler)]
    (fn dispatch-event! [event]
      (let [event-type (first event)
            fns (event-type->fns event-type)
            context (handle-event event fns)]
        (process-effects! context)))))

;; Some common interceptors

(defn inject-db-interceptor
  "An interceptor which inserts the current value of the db into the :coeffects hash-map."
  [db-atom]
  {:before (fn [context]
             (update context :coeffects assoc :db @db-atom))})

(defn event-handler-interceptor
  "An interceptor which passes the :coeffects hash-map to a handler function,
   which returns a sequence of effects,
   which are appended into the :effects vector."
  [handler]
  {:before (fn [context]
             (update context :effects
                     into (handler (:coeffects context))))})

(defn db-handler-interceptor
  "An interceptor which passes the :db and :event coeffect values to a handler function,
   which returns a new db value,
   which is appended as a :db effect to the :effects vector."
  [handler]
  {:before (fn [context]
             (let [{db :db, event :event} (:coeffects context)]
               (update context :effects
                       conj [:db (handler db event)])))})

(defn diff-handler-interceptor
  "An interceptor which passes the :db and :event coeffect values to a handler function,
   which returns a diff structure,
   which is appended as a :diff effect to the :effects vector."
  [handler]
  {:before (fn [context]
             (let [{db :db, event :event} (:coeffects context)]
               (update context :effects
                       conj [:diff (handler db event)])))})

;; Some common effect handlers

(defn db-effect-handler
  "An effect handler which is setting the new value of an atom"
  [db-atom]
  (fn [value]
    (reset! db-atom value)))
