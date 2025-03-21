(ns todomvc.core
  (:require [re-frame.core :as rf]
            [todomvc.cofx]
            [todomvc.fx]
            [todomvc.interceptor]
            [todomvc.event]
            [todomvc.sub]
            [todomvc.router :as router]
            [todomvc.view :as view]
            [vrac.web :as vw :refer [$]]))

(defn mount-ui []
  (vw/render (js/document.getElementById "app")
             ($ view/main-page)))

(defn ^:dev/after-load clear-cache-and-render! []
  (rf/clear-subscription-cache!)
  (router/start-router!)
  (mount-ui))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn start []
  (rf/dispatch-sync [:initialize-db])
  (rf/dispatch-sync [:local-storage/load-todo-items])
  (router/start-router!)
  (mount-ui))
