(ns vrac.model
  (:require [clojure.walk :as w]
            [minimallist.core :as m]
            [minimallist.helper :as h]))

;; Note 1:
;; It won't be possible to generate templates so easily, because there
;; are properties which can only be enforced on a not-so-local scale.
;; For instance, symbols have to be defined in let blocks.

;; Note 2:
;; The model cannot be precise enough to validate some properties which
;; are not-so-local as well.
;; For instance, that symbols are defined in let blocks.

;; What this model brings is a way to put a name on each part of the template
;; for templates which are valid. More validation has to be made manually.

(def template-model
  (h/let ['kw-deref (-> (h/cat [:keyword (h/fn keyword?)]
                               [:value (h/not-inlined (h/ref 'clj-value))]
                               [:default (h/? (h/not-inlined (h/ref 'clj-value)))])
                        h/in-list)

          'fn-call (-> (h/cat [:fn-symb (-> (h/fn symbol?)
                                            (h/with-condition (h/fn (complement #{'if 'when 'let 'for
                                                                                  'val 'attrs
                                                                                  'clojure.core/unquote-splicing}))))]
                              [:args (h/* (h/not-inlined (h/ref 'clj-value)))])
                       h/in-list)

          'if (h/list (h/val 'if)
                      [:condition (h/ref 'clj-value)]
                      [:then (h/ref 'clj-value)]
                      [:else (h/ref 'clj-value)])

          'when (h/list (h/val 'when)
                        [:condition (h/ref 'clj-value)]
                        [:then (h/ref 'clj-value)])

          'bindings (-> (h/* (h/cat [:symbol (h/fn simple-symbol?)]
                                    [:clj-value (h/not-inlined (h/ref 'clj-value))]))
                        h/in-vector)

          'let (h/list (h/val 'let)
                       [:bindings (h/ref 'bindings)]
                       [:body (h/ref 'clj-value)])

          'for (h/list (h/val 'for)
                       [:bindings (h/ref 'bindings)]
                       [:body (h/ref 'clj-value)])

          'val-wrap (h/list (h/val 'val)
                            [:clj-value (h/ref 'clj-value)])

          'clj-value (h/alt [:clj/nil (h/val nil)]
                            [:clj/boolean (h/fn boolean?)]
                            [:clj/number (h/fn number?)]
                            [:clj/string (h/fn string?)]
                            [:clj/keyword (h/fn keyword?)]
                            [:clj/symbol (h/fn symbol?)]
                            [:clj/vector (h/vector-of (h/ref 'clj-value))]
                            [:clj/set (h/set-of (h/ref 'clj-value))]
                            [:clj/hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))]
                            [:clj/if (h/ref 'if)]
                            [:clj/when (h/ref 'when)]
                            [:clj/let (h/ref 'let)]
                            [:clj/for (h/ref 'for)]
                            [:clj/kw-deref (h/ref 'kw-deref)]
                            [:clj/val-wrap (h/ref 'val-wrap)]
                            [:clj/fn-call (h/ref 'fn-call)])

          ; Will be converted into text or nil, (or html for debug logging).
          'html/dom-leaf (h/alt [:clj/nil (h/val nil)]
                                [:clj/boolean (h/fn boolean?)]
                                [:clj/number (h/fn number?)]
                                [:clj/string (h/fn string?)]
                                [:clj/keyword (h/fn keyword?)]
                                [:clj/symbol (h/fn symbol?)]
                                ;[:clj/vector (h/vector-of (h/ref 'clj-value))]
                                [:clj/set (h/set-of (h/ref 'clj-value))]
                                [:clj/hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))]
                                [:clj/kw-deref (h/ref 'kw-deref)]
                                [:clj/val-wrap (h/ref 'val-wrap)]
                                [:clj/fn-call (h/ref 'fn-call)])

          'html/if (h/list (h/val 'if)
                           [:condition (h/ref 'clj-value)]
                           [:then (h/ref 'html/something)]
                           [:else (h/ref 'html/something)])

          'html/when (h/list (h/val 'when)
                             [:condition (h/ref 'clj-value)]
                             [:then (h/ref 'html/something)])

          'html/let (h/list (h/val 'let)
                            [:bindings (h/ref 'bindings)]
                            [:body (h/ref 'html/something)])

          'html/for (h/list (h/val 'for)
                            [:bindings (h/ref 'bindings)]
                            [:body (h/ref 'html/something)])

          'unquote-splicing-wrap (h/list [:unquote-splicing-symb (h/val 'clojure.core/unquote-splicing)]
                                         ;; TODO: this should be a collection of html/something
                                         [:clj-value (h/ref 'clj-value)])

          'attrs-wrap (h/list (h/val 'attrs)
                              [:clj-value (h/ref 'clj-value)])

          'html/props (h/alt [:html.props/nil (h/val nil)]
                             [:html.props/hashmap (h/map-of (h/fn keyword?) (h/ref 'clj-value))]
                             [:html.props/attrs (h/ref 'attrs-wrap)])

          ; div, h1 ..
          'html/dom-element (-> (h/cat [:type (h/fn simple-keyword?)]
                                       [:props (h/? (h/not-inlined (h/ref 'html/props)))]
                                       [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                                h/in-vector)

          ; my-comp, foo/bar ..
          'html/component (-> (h/cat [:component (h/fn qualified-keyword?)]
                                     [:props (h/? (h/not-inlined (h/ref 'html/props)))]
                                     [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                              h/in-vector)

          ;; In a context assumed to be html, represents something.
          'html/something (h/alt [:html/dom-leaf (h/ref 'html/dom-leaf)]
                                 [:html/dom-element (h/ref 'html/dom-element)]
                                 [:html/component (h/ref 'html/component)]
                                 [:html/if (h/ref 'html/if)]
                                 [:html/when (h/ref 'html/when)]
                                 [:html/let (h/ref 'html/let)]
                                 [:html/for (h/ref 'html/for)]
                                 [:html/unquote-splicing-wrap (h/ref 'unquote-splicing-wrap)])]
    (h/ref 'html/something)))

(defn- v-pair? [x]
  (and (vector? x)
       (= 2 (count x))))

(defn- simpler-ast [tree]
  (if (v-pair? tree)
    (let [[key val] tree]
      (case key
        ; Unwrap things - they were wrapped just for context clarification during parsing.
        :clj/val-wrap (:clj-value val)
        :html.props/attrs (:clj-value val)

        ; optional values h/?, [] / [x] -> nil / x
        :html/dom-element [key (update val :props first)]
        :html/component [key (update val :props first)]
        :clj/kw-deref [key (update val :default first)]
        tree))
    tree))

(defn template->ast [template]
  (->> template
       (m/describe template-model)
       (w/postwalk simpler-ast)))

(comment

  ;; html/if
  (template->ast '[:div (if condition [:h1 "big title"] [:h5 "small title"])])
  (template->ast '[:div (if condition "big title" "small title")])

  ;; if
  (template->ast '[:div (val (if condition "big title" "small title"))])

  ;; html/when
  (template->ast '[:div (when condition [:h1 "big title"])])
  (template->ast '[:div (when condition "big title")])

  ;; when
  (template->ast '[:div (val (when condition "big title"))])

  ;; html/let
  (template->ast '(let [a 1, b 2] [:div a b]))

  ;; let
  (template->ast '(val (let [a 1, b 2] {a b})))

  ;; html/for
  (template->ast '[:ul (for [a [1 2 3], b [:a :b :c]] [:li a b])])

  ;; for
  (template->ast '(val (for [a [1 2 3], b [:a :b :c]] {a b})))


  (template->ast [:div "hello, " nil "world!"])
  (template->ast [:div nil "hello, " true "world!"])
  (template->ast [:div {} {} "hello"])
  (template->ast [:div {:id :theme-name
                        :style {:color "pink"}} "Green"])
  (template->ast [:div {:id :theme-name
                        :class [:color "button"]
                        :style {:color "pink"}} "Green"])
  (template->ast '[:div "debug info: " {:id user-id, :user user-name}])
  (template->ast '[:div.debug-info nil {:id user-id, :user user-name}])
  (template->ast '[:div.debug-info (val {:id user-id, :user user-name})])
  (template->ast '[:div.debug-info (val [:a :b :c :d])])
  (template->ast '[:div.debug-info (attrs my-attributes)])

  ;; Including a component
  (template->ast '[:my-ns/my-component "hello"])

  ;; Deprecated construct - invalid
  ;(template->ast '[my-component "hello"])
  ;(template->ast '[(if my-cond ::my-component ::other-comp) "hello"])
  ;(template->ast '[(if my-cond ::my-component my-alias/other-comp) "hello"])

  ;; Deprecated construct - invalid
  ;; A component can have any identifier except unqualified keywords which are reserved for html elements.
  ;(template->ast '[true "This is not a pipe"])
  ;(template->ast '[false "This is a pipe"])
  ;(template->ast '[42 "This could be anything"])
  ;(template->ast '[foobar "hello"])
  ;(template->ast '[[:experiment 7] "hello"])

  ;; Arguments passing
  (template->ast '[::my-component true false nil "foo"])           ;; any value can be passed, including nil
  (template->ast '[::my-component (val [:a :b]) (val [c d])])      ;; vector literals should be marked as values
  (template->ast '[::my-component (str "peace and " my-love-var)]) ;; any s-expression can be passed
  (template->ast '[::my-component :a :b ~@my-kw-sequence :z])      ;; ~@ slices an expression in the arg list

  ;; Attributes applied on components
  (template->ast '[::my-component.foo {:class "bar"} arg1 arg2])    ;; my-component only has 2 args
  (template->ast '[::my-component (attrs my-attributes) arg1 arg2]) ;; my-component only has 2 args

  ;; Function calls
  (template->ast '[::my-component (str "peace and " my-love-var)])
  (template->ast '(str "peace and " my-love-var))

  ;; Keyword deref
  (template->ast '(:a global))
  (template->ast '(:a global 5))

  #__)
