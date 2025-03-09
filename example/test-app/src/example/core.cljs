(ns example.core
  (:require [example.context.core :refer [context-root]]
            [example.reactive-data.core :refer [reactive-data-root]]
            [example.reactive-fragment.core :refer [reactive-fragment-root]]
            [example.svg.core :refer [svg-root]]
            [example.vcup.core :refer [vcup-root]]
            [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn- debug-prn [reactive-node event-type]
  (when-some [name (-> reactive-node meta :name)]
    (prn name event-type)))

;; Print the debug info in the Browser's console.
;;(set! sr/*notify-lifecycle-event* debug-prn)

(defn root-component []
  ($ :main
     ($ vcup-root)
     ($ reactive-data-root)
     ($ reactive-fragment-root)
     ($ context-root)
     ($ svg-root)
     ,))

;; Shadow-CLJS hooks: start & reload the app

(defn ^:dev/after-load setup! []
  (vw/render (js/document.getElementById "app")
             ($ root-component)))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn start-app []
  (setup!))
