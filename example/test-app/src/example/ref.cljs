(ns example.ref
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn- ref-signal-article [element-ref]
  ($ :article
     ($ :h2 "Capture an element in a reactive signal via the special prop `ref`")
     (let [is-existing (sr/create-signal false)]
       ($ :div
          ($ :button {:on/click (fn [] (swap! is-existing not))}
             (sr/create-derived (fn [] (if @is-existing "Destroy element" "Create element"))))
          " "
          (vw/when-fragment is-existing
            ($ :span {:ref element-ref} "Referred element"))))))

(defn- using-ref-somewhere-else-article1 [element-ref]
  ($ :article
     ($ :h2 "Use the ref's innerText via a derived data.")
     ($ :div (sr/create-derived
               (fn []
                 (if-some [^js element @element-ref]
                   (.-innerText element)
                   "<ref is nil>"))))))

(defn- using-ref-somewhere-else-article2 [element-ref]
  ($ :article
     ($ :h2 "Use the ref to change a color reactively.")
     "The square is:"
     ($ :ul
        ($ :li ($ :strong "red")  " when the ref is nil, and")
        ($ :li ($ :strong "green")  " when the ref is set to an element."))
     ($ :div
        {:style {:width "50px"
                 :height "50px"}}
        (vw/props-effect
          (fn []
            {:style {:background-color (if (nil? @element-ref)
                                         "red"
                                         "green")}})))))

(defn ref-root []
  (let [element-ref (sr/create-signal nil)]
    ($ :div
       ($ ref-signal-article element-ref)
       ($ using-ref-somewhere-else-article1 element-ref)
       ($ using-ref-somewhere-else-article2 element-ref))))
