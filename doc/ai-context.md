# Vrac + Signaali + Si-Frame Code Generation Guide

## Dependencies
```clojure
taipei.404.vrac/vrac {:mvn/version "0.1.2"}
```

## Namespace Setup
```clojure
(ns example.app
  (:require [vrac.web :as vw :refer [$]]
            [signaali.reactive :as sr]
            [re-frame.core :as rf]))  ; Optional, for Si-Frame integration
```

## Element Creation with `$`
- **HTML elements**: `($ :div ...)` `($ :span#id.class1.class2 ...)`
- **Fragments**: `($ :<> child1 child2 ...)`
- **Components**: `($ component-fn arg1 arg2)`
- **Children**: strings, numbers, booleans, keywords, vectors, nil (renders nothing), reactive nodes

## Properties (Props)

### Static Props
```clojure
($ :div.class1 {:style {:color "red" :padding "1em"}
                :class [:class2]
                :data-testid "my-div"}
   "content")
```

### Reactive Props
Use `vw/props-effect` with a function returning a prop map:
```clojure
($ :input
   (vw/props-effect (fn [] {:value @text-signal}))
   {:type "text"
    :on/input (fn [e] (reset! text-signal (.. e -target -value)))})
```

### Prop Namespaces
- `:a/...` - HTML attributes (e.g., `:a/for`, `:a/readonly`)
- `:p/...` - DOM properties (e.g., `:p/htmlFor`, `:p/innerHTML`)
- `:on/...` - Event handlers (e.g., `:on/click`, `:on/input`)
- No namespace - Vrac special props (`:style`, `:class`, `:ref`, `:data-*`) or DOM properties

### Multiple Prop Maps
Props compose together; later values override earlier ones (except `:style` and `:class` which merge):
```clojure
($ :div {:class :base-class}
        {:class :extra-class}  ; Results in both classes
        {:style {:color "red"}}
        {:style {:padding "1em"}})  ; Both styles applied
```

## Reactive State (Signaali)

### Creating State
```clojure
(let [counter (sr/create-state 0)           ; Mutable state
      text (sr/create-signal "hello")       ; Immutable signal
      doubled (sr/create-derived (fn [] (* 2 @counter)))  ; Computed
      memoized (sr/create-memo (fn [] (expensive-calc @counter)))]  ; Cached computed
  ...)
```

### Using State
- Read: `@signal-or-state`
- Write: `(reset! signal-or-state new-value)` or `(swap! signal-or-state update-fn)`
- Reactive nodes auto-update UI when dereferenced in render

## Reactive Fragments

Don't use reactive fragment on static data

### Conditional Rendering
```clojure
;; If/else
(vw/if-fragment condition-fn
  ($ :div "then branch")
  ($ :div "else branch"))

;; When (no else)
(vw/when-fragment condition-fn
  ($ :div "shown when true"))

;; Case
(vw/case-fragment value-fn
  :route/home ($ :div "Home")
  :route/blog ($ :div "Blog")
  ($ :div "Default"))

;; Cond
(vw/cond-fragment
  (= @route :home) ($ :div "Home")
  (= @route :blog) ($ :div "Blog")
  :else ($ :div "Default"))
```

**Important**: Pass a **function** to `vw/if-fragment`, `vw/when-fragment`, and `vw/case-fragment`:
```clojure
;; Correct:
(vw/if-fragment (fn [] (even? @counter)) ...)
(vw/if-fragment counter ...)  ; OK if counter is already a reactive node

;; Wrong:
(vw/if-fragment (even? @counter) ...)  ; Don't evaluate directly
```

### List Rendering
```clojure
(vw/for-fragment*
  coll-fn          ; (fn [] @items) or just items-signal
  key-fn           ; :id or (fn [item] (:id item)), optional
  item-component)  ; (fn [item] ($ :div (:name item)))
```

## Components
Define as functions returning vcup:
```clojure
(defn person-card [person]
  ($ :div.card
     ($ :h3 (:name person))
     ($ :p (:bio person))))

;; Use with $
($ person-card {:name "Alice" :bio "Developer"})
```

## Context
```clojure
;; Set context
(vw/with-context (sr/create-signal {:theme "dark"})
  ($ child-component))

;; Update based on parent context
(vw/with-context-update (fn [parent-ctx]
                          (assoc @parent-ctx :nested true))
  ($ child-component))

;; Read context
(defn child-component []
  (let [ctx (vw/get-context)]
    ($ :div "Theme: " (sr/create-derived (fn [] (:theme @ctx))))))
```

## Refs
Capture DOM elements:
```clojure
(let [element-ref (sr/create-signal nil)]
  ($ :div
     ($ :input {:ref element-ref})
     ($ :button {:on/click (fn [] (.focus @element-ref))} "Focus input")))
```

## Re-frame Integration
```clojure
;; Subscriptions
(rf/reg-sub :counter (fn [db _] (:counter db)))

;; In components - wrap subscriptions for derived values
(defn counter-display []
  (let [counter (rf/subscribe [:counter])
        doubled (sr/create-derived (fn [] (* 2 @counter)))]
    ($ :div "Counter: " counter " Doubled: " doubled)))

;; Dispatch events
($ :button {:on/click #(rf/dispatch [:increment])} "+")
```

## Application Entry Point
```clojure
(defn mount-ui []
  (vw/render (js/document.getElementById "app")
             ($ root-component)))

(defn ^:dev/after-load reload! []
  (mount-ui))

(defn ^:dev/before-load shutdown! []
  (vw/dispose-render-effects))

(defn init []
  (mount-ui))
```

## SVG and MathML
Use standard element names:
```clojure
;; SVG
($ :svg {:width "100" :height "100" :xmlns "http://www.w3.org/2000/svg"}
   ($ :circle {:cx 50 :cy 50 :r 40 :fill "blue"}))

;; MathML
($ :math {:display "block"}
   ($ :mrow
      ($ :mi "x")
      ($ :mo "+")
      ($ :mn "1")))

;; Convert HTML/SVG string to DOM
(vw/html-text-to-dom "<svg>...</svg>")
```

## Common Patterns

### Counter
```clojure
(defn counter []
  (let [count (sr/create-state 0)]
    ($ :div
       "Count: " count
       ($ :button {:on/click #(swap! count inc)} "+"))))
```

### Controlled Input
```clojure
(defn text-input []
  (let [value (sr/create-signal "")]
    ($ :input
       (vw/props-effect (fn [] {:value @value}))
       {:on/input (fn [e] (reset! value (.. e -target -value)))})))
```

### Toggle Visibility
```clojure
(defn collapsible [title content]
  (let [open? (sr/create-state false)]
    ($ :div
       ($ :button {:on/click #(swap! open? not)} title)
       (vw/when-fragment open?
         ($ :div content)))))
```