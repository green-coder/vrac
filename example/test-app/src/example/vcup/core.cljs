(ns example.vcup.core
  (:require [vrac.web :as vw :refer [$]]))

(defn- text-node-component []
  ($ :section
     ($ :h2 "Text nodes")

     ($ :h3 "Text")
     "Hello, " "world!"

     ($ :h3 "Numbers")
     1 2 3

     ($ :h3 "Booleans")
     false true

     ;; TODO: we want this to work
     ;;($ :h3 "Maps")
     ;;{:a 1
     ;; :b 2}

     ($ :h3 "Vectors")
     ($ :pre
        ($ :code
           [:div {:style {:color "lime"}} 1 2]))

     ($ :h3 "`nil` values")
     nil
     nil

     ,))

(defn fragment-component []
   ($ :section
      ($ :h2 "Fragments")
      ($ :<>
         ($ :<>
            "1a"
            "1b")
         ($ :<>
            "2a"
            "2b"))))

(defn component-with-arguments [a b c]
   ($ :section
      ($ :h2 "Component with arguments")
      ($ :pre
         ($ :code
            "(is " (+ a b c) " (+ a b c))"))))

(defn vcup-root []
  ($ :div
     ($ text-node-component)
     ($ fragment-component)
     ($ component-with-arguments 100 20 3)))
