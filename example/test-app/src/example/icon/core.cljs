(ns example.icon.core
  (:require [shadow.resource :as res]
            [vrac.web :as vw :refer [$]]))

(defn fox-origami []
  ;; TODO: Should I cache the dom and provide a clone on each usage?
  ;; Is there another way to reference the same SVG from multiple places?
  ;; Is the DOM in fact a directed graph? What happens if I add the same DOM element in multiple places?
  (vw/html-text-to-dom (res/inline "svg/fox-origami.svg")))

(defn icon-root []
  ($ :div
     ($ :article
        ($ :h2 "SVG icon as a customized element")
        ($ (fox-origami))
        (for [i (range 4)]
          ($ (fox-origami) {:width 200
                            :height 200})))))
