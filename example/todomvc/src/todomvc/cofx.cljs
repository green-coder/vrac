(ns todomvc.cofx
  (:require [cljs.reader :as reader]
            [re-frame.core :as rf]
            [todomvc.constant :as const]))

(rf/reg-cofx
  :todo-items-from-local-store
  (fn [cofx _]
    (assoc cofx
           :todo-items-from-local-store
           (some-> (js/localStorage.getItem const/todo-item-local-storage-key)
                   reader/read-string))))
