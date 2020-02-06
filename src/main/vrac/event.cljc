(ns vrac.event)

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
  [type->effect-handlers]
  (fn process-effects! [context]
    (doseq [[type & values] (:effects context)]
      (let [handler (type->effect-handlers type)]
        (apply handler values)))))

(defn event-dispatcher
  "Routes an event to the right handler, process its context through a chain of function,
   then process the corresponding effects."
  [event-type->interceptors effect-type->effect-handlers]
  (let [event-type->fns (into {} (map (juxt identity interceptors->fns)) event-type->interceptors)
        process-effects! (effects-processor effect-type->effect-handlers)]
    (fn dispatch-event! [event]
      (let [event-type (first event)
            fns (event-type->fns event-type)
            context (handle-event event fns)]
        (process-effects! context)))))

;; Some common interceptors

(defn inject-db-interceptor [db-atom]
  {:before (fn [context]
             (update context :coeffects assoc :db @db-atom))})

(defn event-handler-interceptor [handler]
  {:before (fn [context]
             (update context :effects
                     into (handler (:coeffects context))))})

(defn db-handler-interceptor [handler]
  {:before (fn [context]
             (let [{db :db, event :event} (:coeffects context)]
               (update context :effects
                       conj [:db (handler db event)])))})

;; Some common effect handlers

(defn db-effect-handler [db-atom]
  {:effect-type :db
   :handler (fn [value]
              (reset! db-atom value))})
