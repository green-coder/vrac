(ns example.ref-and-use-effects
  (:require [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn- ref-signal-article [element-ref]
  ($ :article
     ($ :h2 "Capture an element in a reactive signal via the special attribute `ref`")
     ($ :div {:ref element-ref} "Referred element")))

(defn- using-ref-somewhere-else-article [element-ref]
  ($ :article
     ($ :h2 "Display the ref's innerText via a derived signal")
     ($ :div (sr/create-derived
               (fn []
                 (if-some [^js element @element-ref]
                   (.-innerText element)
                   "<ref is nil>"))))))

(defn- use-effects-article [element-ref]
  ($ :article
     ($ :h2 "Use `use-effect` on a ref")
     "The square is:"
     ($ :ul
        ($ :li ($ :strong "grey") " before use-effect is run,")
        ($ :li ($ :strong "red")  " when the ref is nil, and")
        ($ :li ($ :strong "green")  " when the ref is set to an element."))
     (let [color-signal (sr/create-signal "grey")]
        ($ :div
           {:style {:width "50px"
                    :height "50px"}}
           (vw/attributes-effect (fn [] {:style {:background-color @color-signal}}))
           (vw/use-effects
             [(sr/create-effect
                (fn []
                  (reset! color-signal
                          (if (some? @element-ref)
                            "green"
                            "red"))))])))))

(defn ref-and-use-effects-root []
  (let [element-ref (sr/create-signal nil)]
    ($ :div
       ($ ref-signal-article element-ref)
       ($ using-ref-somewhere-else-article element-ref)
       ($ use-effects-article element-ref))))
