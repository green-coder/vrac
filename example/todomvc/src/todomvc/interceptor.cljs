(ns todomvc.interceptor
  (:require [re-frame.core :as rf]
            [todomvc.constant :as const]))

(def save-todo-items-in-local-storage
  (rf/->interceptor
    :id :save-todo-items-in-local-storage
    :after (fn [context]
             (let [db (or (get-in context [:effects :db])
                          (get-in context [:coeffects :db]))]
               (update-in context [:effects :fx] conj
                          [:save-in-local-store {:key const/todo-item-local-storage-key
                                                 :value (str (:todo-items db))}])))))
