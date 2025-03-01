(ns example.core
  (:require [example.counter.core :refer [counter-section]]
            [vrac.web :as vw :refer [$]]))

(defn root-component []
  ($ :main
     ($ counter-section)))

;; Shadow-CLJS hooks: start & reload the app

(defn ^:dev/after-load setup! []
  (vw/render (js/document.getElementById "app")
             ($ root-component)))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn start-app []
  (setup!))
