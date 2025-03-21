(ns todomvc.fx
  (:require [re-frame.core :as rf]))

(rf/reg-fx
  :save-in-local-store
  (fn [{:keys [key value]}]
    (js/localStorage.setItem key value)))
