(ns example.core
  (:require [signaali.reactive :as sr]))

;; Get the references to the DOM elements we want to modify
(def ^js counter-element      (js/document.getElementById "counter-element"))
(def ^js inc-counter-button   (js/document.getElementById "inc-counter-button"))
(def ^js reset-counter-button (js/document.getElementById "reset-counter-button"))
(def ^js run-effects-button   (js/document.getElementById "run-effects-button"))

(def counter-state
  (sr/create-state nil
                   ;; Optional param, useful for debugging
                   {:metadata {:name "counter state"}}))

(def counter-text-updater
  (sr/create-effect #(set! (.-textContent counter-element) (str @counter-state))
                    ;; Optional param, useful for debugging
                    {:metadata {:name "counter text updater"}}))

(defn on-inc-counter-button-clicked []
  (swap! counter-state inc))

(defn on-reset-counter-button-clicked []
  (reset! counter-state 0))

(defn on-run-effects-button-clicked []
  (sr/re-run-stale-effectful-nodes))

(defn setup! []
  (.addEventListener inc-counter-button   "click" on-inc-counter-button-clicked)
  (.addEventListener reset-counter-button "click" on-reset-counter-button-clicked)
  (.addEventListener run-effects-button   "click" on-run-effects-button-clicked)

  (reset! counter-state 0)
  (sr/add-on-dispose-callback counter-text-updater #(set! (.-textContent counter-element) "n/a"))
  @counter-text-updater)

(defn shutdown! []
  (sr/dispose counter-text-updater)

  (.removeEventListener inc-counter-button   "click" on-inc-counter-button-clicked)
  (.removeEventListener reset-counter-button "click" on-reset-counter-button-clicked)
  (.removeEventListener run-effects-button   "click" on-run-effects-button-clicked)
  ,)


;; Shadow-CLJS hooks: start & reload the app

(defn start-app []
  (set! sr/*notify-lifecycle-event* sr/debug-prn) ; Optional, useful for debugging
  (setup!))

(defn ^:dev/before-load stop-app []
  (shutdown!))

(defn ^:dev/after-load restart-app []
  (setup!))
