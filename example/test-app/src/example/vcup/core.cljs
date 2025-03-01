(ns example.vcup.core
  (:require [vrac.web :as vw :refer [$]]))

(defn vcup-root []
  ($ :div
     ($ :section
        ($ :h2 "Text nodes")

        ($ :h3 "Text")
        "Hello, " "world!"

        ($ :h3 "Numbers")
        1 2 3

        ($ :h3 "Booleans")
        true false

        ;; TODO: we want this to work
        ;;($ :h3 "Maps")
        ;;{:a 1
        ;; :b 2}

        ($ :h3 "Vectors")
        [:div {:style {:color "lime"}} 1 2])

     ($ :section
        ($ :h2 "Fragments")
        ($ :<>
           ($ :<>
              "1a"
              "1b")
           ($ :<>
              "2a"
              "2b")))))
