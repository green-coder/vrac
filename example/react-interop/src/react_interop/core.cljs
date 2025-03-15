(ns react-interop.core
  (:require [vrac.web :as vw :refer [$]]
            [react-interop.interop :refer [interop-demo]]))

;; Shadow-CLJS hooks: start & reload the app

(defn ^:dev/after-load setup! []
  (vw/render (js/document.getElementById "app")
             ($ interop-demo)))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn start-app []
  (setup!))
