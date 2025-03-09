(ns example.svg.core
  (:require [shadow.resource :as rc]
            [vrac.web :as vw :refer [$]]))

(defn- fox-origami []
  ;; TODO: Should I cache the dom and provide a clone on each usage?
  ;; Is there another way to reference the same SVG from multiple places?
  ;; Is the DOM in fact a directed graph? What happens if I add the same DOM element in multiple places?
  (vw/html-text-to-dom (rc/inline "svg/fox-origami.svg")))

(defn- string-to-svg-element []
  ($ :article
     ($ :h2 "Using an SVG element built from a string")
     (fox-origami)))

(defn- changing-an-existing-svg-element []
  ($ :article
     ($ :h2 "Changing an existing SVG element")

     ($ :h3 "Custom width and height")
     ($ (fox-origami)
        {:width "20rem"
         :height "20rem"})

     ($ :h3 "Then add children")
     ($ (fox-origami)
        {:width "20rem"
         :height "20rem"}
        ;; Right eye
        ($ :circle {:cx 14.5
                    :cy 20
                    :r 3.5
                    :fill "white"
                    :stroke "black"
                    :stroke-width 1})
        ;; Left eye
        ($ :circle {:cx 24.5
                    :cy 20
                    :r 3.5
                    :fill "white"
                    :stroke "black"
                    :stroke-width 1}))))

(defn- svg-element-using-vcup []
  ($ :article
     ($ :h2 "SVG element using Vcup")

     ;; from https://www.svgrepo.com/svg/423821/fox-origami-paper
     ;; Author: Lima Studio
     ;; License: CC Attribution License
     ($ :svg {:width "800px"
              :height "800px"
              :viewBox "0 0 64 64"
              :xmlns "http://www.w3.org/2000/svg"}
        ($ :path {:d "M7.45,21.19l9.65-5.88L7.56,5.16c-.16,.12-.27,.31-.27,.54v14.64c0,.29,.06,.58,.16,.85h0Z"
                  :fill "#fca65c"})
        ($ :path {:d "M31.17,5.16l-9.54,10.15,9.81,6.04V5.69c0-.22-.11-.41-.27-.53Z"
                  :fill "#fca65c"})
        ($ :path {:d "M7.56,5.16l9.21,9.8-1.95,1.75-3.16,1.94L7.56,5.16Z"
                  :fill "#f5934a"})
        ($ :path {:d "M31.17,5.16l-9.21,9.8,1.95,1.75,3.16,1.94,4.1-13.5Z"
                  :fill "#f5934a"})
        ($ :path {:d "M8.05,5.01c-.18-.02-.35,.04-.49,.15l5.77,10.15h12.07l5.77-10.15c-.18-.15-.43-.21-.67-.11l-11.13,4.22L8.23,5.05c-.06-.02-.12-.04-.18-.04h0Z"
                  :fill "#feb87e"})
        ($ :path {:d "M12.78,26.84v26.44c0,.26,.05,.52,.14,.77l13.81-10.57,4.71-22.13-18.65,5.49Z"
                  :fill "#f0f4f6"})
        ($ :path {:d "M31.44,21.35L12.78,44.91V26.84l18.65-5.49Z"
                  :fill "#e2eef2"})
        ($ :path {:d "M31.44,21.34L12.92,54.04c.12,.35,.31,.68,.58,.95l3.3,3.3c.46,.46,1.07,.71,1.72,.71h13.49l19.14-7.05,1.15-16.09c-.23-.27-.49-.51-.78-.71l-20.07-13.81Z"
                  :fill "#fca65c"})
        ($ :path {:d "M49.88,34.03l-5.27,14.03-18.3,10.94h5.69l19.14-7.05,1.15-16.09c-.23-.27-.49-.51-.78-.71l-1.63-1.12Z"
                  :fill "#f5934a"})
        ($ :path {:d "M52.29,35.86l-4.72,13.71,6.76-.99,2.27-4.48c-.08-.33-.2-.65-.36-.96l-3.44-6.53c-.14-.27-.32-.52-.52-.75Z"
                  :fill "#feb87e"})
        ($ :path {:d "M25.4,15.31l-6.04-6.03-6.03,6.03h0l-5.88,5.88c.12,.32,.3,.62,.55,.87l4.78,4.78,1.23,1.23,5.16,1.6,5.54-1.6h0l6.72-6.72-6.04-6.04Z"
                  :fill "#fdc99c"})
        ($ :path {:d "M14.01,28.07l3.74,3.74c.89,.89,2.33,.89,3.22,0l3.74-3.74H14.01Z"
                  :fill "#9dacb9"})
        ($ :path {:d "M56.6,44.1l-9.03,5.47h0l-15.57,9.43h23.7c.56,0,1.01-.45,1.01-1.01v-12.96c0-.32-.04-.63-.11-.94h0Z"
                  :fill "#fdc99c"})
        ($ :path {:d "M19.36,9.27l-7.16,4.07,1.12,1.97,6.03-6.04Z"
                  :fill "#fca65c"})
        ($ :path {:d "M19.36,9.27l7.16,4.07-1.12,1.97-6.03-6.04Z"
                  :fill "#fca65c"})
        ($ :path {:d "M56.6,44.1l-7.64,1.43-1.39,4.04,9.03-5.47Z"
                  :fill "#fca65c"})
        ($ :path {:d "M12.79,55.7l3.3,3.3c.65,.65,1.51,1,2.42,1H55.7c1.11,0,2.01-.9,2.01-2.01v-12.96c0-.82-.2-1.63-.58-2.36l-3.43-6.53c-.38-.72-.94-1.35-1.62-1.82l-19.64-13.51V5.69c0-.56-.27-1.08-.73-1.39-.46-.32-1.04-.39-1.56-.19l-10.78,4.09L8.58,4.11c-.52-.2-1.1-.13-1.56,.19s-.73,.84-.73,1.39v14.64c0,.92,.36,1.78,1,2.42l4.49,4.49v26.02c0,.92,.36,1.78,1,2.42Zm1-26.45l3.26,3.26c1.28,1.28,3.36,1.28,4.63,0l5.26-5.26-13.16,23.23V29.25Zm2.65-.18h5.87l-2.03,2.03c-.5,.5-1.31,.5-1.81,0l-2.03-2.03Zm19.15,28.93l20.13-12.19v12.19h-20.13Zm19.77-14.39s0,.05,.02,.07l-5.96,3.61,3.11-9.05,2.83,5.37Zm-4.41-7.63c.08,.05,.11,.14,.18,.2l-4.38,12.72-15.02,9.1h-13.21c-.38,0-.74-.15-1.01-.42l-3.3-3.3c-.07-.07-.06-.19-.11-.28L31.77,22.79l19.17,13.19ZM12.08,15.14l-3.79,3.79V8.47l3.79,6.67Zm-2.55-8.53l8.03,3.05-4.02,4.02-4.02-7.06Zm20.9,1.86v10.46l-3.79-3.79,3.79-6.67Zm-5.26,5.2l-4.02-4.02,8.03-3.05-4.02,7.07Zm-16.47,7.67h0l10.66-10.66,10.66,10.66-5.72,5.72H14.43l-5.72-5.72Z"}))))

(defn- foreign-object-inside-svg []
   ($ :article
      ($ :h2 "<foreignObject> element inside SVG")
      ($ :svg {:xmlns "http://www.w3.org/2000/svg"
               :width "40rem"
               :height "40rem"
               :viewBox "0 0 200 200"}
         ($ :polygon {:points "5,5 195,10 185,185 10,195"})
         ($ :foreignObject {:x "20"
                            :y "20"
                            :width "160"
                            :height "160"}
            ($ :div {:xmlns "http://www.w3.org/1999/xhtml"
                     :style {:background-color "purple"
                             :color "pink"
                             :height "100%"}}
               "This is an HTML div inside a SVG element. "
               "It is wrapped inside a 'foreignObject' SVG element.")))))

(defn svg-root []
  ($ :div
     ($ string-to-svg-element)
     ($ changing-an-existing-svg-element)
     ($ svg-element-using-vcup)
     ($ foreign-object-inside-svg)
     ,))
