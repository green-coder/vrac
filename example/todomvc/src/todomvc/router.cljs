(ns todomvc.router
  (:require [re-frame.core :as rf]
            [reitit.frontend :as rtf]
            [reitit.frontend.easy :as rtfe]))

(def routes
  ["/"
   ["" {:name :page/all-todo-items}]
   ["active" {:name :page/active-todo-items}]
   ["completed" {:name :page/completed-todo-items}]])

(defn start-router! []
  (rtfe/start!
    (rtf/router routes)
    (fn [m]
      (rf/dispatch [:router/set-route-match m]))
    {:use-fragment true}))
