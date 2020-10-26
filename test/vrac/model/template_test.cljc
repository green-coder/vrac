(ns vrac.model.template-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.model.template :as vt]
            [minimallist.core :as m]
            [minimallist.helper :as h]))

;(def valuable-model  (h/let vt/model-definitions (h/ref 'valuable)))
;(def props-model     (h/let vt/model-definitions (h/ref 'props)))
;(def bindings-model  (h/let vt/model-definitions (h/ref 'bindings)))
;(def directive-model (h/let vt/model-definitions (h/ref 'directive)))
;
;(deftest template-model-test
;  (are [x]
;    (m/valid? valuable-model x)
;    true
;    false
;    3
;    "text"
;    'var
;    :foobar
;    '(:foobar var)
;    '(:foobar nil)
;    {:foobar "text"}
;    [3 "text"]
;    {3 "text"}
;    #{3 "text"})
;
;  (are [x]
;    (m/valid? props-model x)
;    {:style {:foobar "text"}}
;    {:style {3 "text"}}
;    {:class 3}) ; to be improved later
;
;  (are [x]
;    (not (m/valid? props-model x))
;    {3 "text"}
;    '(:foobar var)
;    '[:foobar var])
;
;  (are [x]
;    (m/valid? bindings-model x)
;    '[var context]
;    '[var 3]
;    '[var "text"])
;
;  (are [x]
;    (not (m/valid? bindings-model x))
;    '(var context)
;    '(var 3)
;    '(var "text"))
;
;  (are [x]
;    (m/valid? directive-model x)
;    '(if (:kw var) 3 "text")
;    '(if (:kw var) [:p 3] [:p "text"])
;    '(when (:kw var) "text")
;    '(when (:kw var) [:p 3])
;    '(let [var context] 3)
;    '(for [var context] 3))
;
;  (are [x]
;    (not (m/valid? directive-model x))
;    '[let [var context] 3]
;    '(let (var context) 3)
;    '[for [var context] 3]
;    '(for (var context) 3)))






;There are things I know for sure I can have in the template:
;
;1. Representing html tags with properties and printable values inside.
;2. Referring to another component for inclusion (and possibly using recursion via a cycle in the references), passing s-expressions as argument (possibly including variables).
;3. Accepting input parameters in Vrac templates, use them as if they were described locally.
;4. Use those params and s-expressions in html attributes, if conditions, source data in let and for, and printable values.
;5. Destructuring in let and for expressions, as well as in the input param declaration.)

(comment
  ;; Examples for 1.

  [:div "hello, " nil "world!"] ;; nils values are ignored when used as argument on html tags

  [:div true " implies truth, and " false " implies anything."] ;; true and false are printed

  [:div :foobar] ;; displays ":foobar"

  [:div {:style {:color "pink"}, :class [:color "button"], :id :theme-name} "Green"] ;; attributes as in hiccup, keywords used as strings

  [:div "debug info: " {:id user-id, :user user-name}] ;; hash-maps can be used as printable values if not in second position in the hiccup vector

  ;; Ways to lift the ambiguity of the "type" of element inside a hiccup vector:
  [:div.debug-info nil {:id user-id, :user user-name}]   ;; Using nil at the attribute position
  [:div.debug-info (val {:id user-id, :user user-name})] ;; Using the val function
  [:div.debug-info (val [:a :b :c :d])]                  ;; Here, to avoid confusion with hiccup vectors
  [:div.debug-info (attrs my-attributes)]                ;; Using the attrs function

  ;; Those are equivalent:
  [:div {:class ["foo-1" "foo-2" "foo-3" "foo-4"], :id "bar"}]
  [:div.foo-1#bar.foo-2 {:class [:foo-3 "foo-4"]}]

  [:div {:class "foobar"}]) ;; Shortcut syntax when only 1 class, no vector needed.



(comment
  ;; Examples for 2.

  ;; Multiple ways to refer to vrac components for inclusion.
  [:my-ns/my-component "hello"]                             ;; using a qualified keyword
  [my-component "hello"]                                    ;; using a symbol which can be resolved in the current namespace
  [(if my-cond ::my-component ::other-comp) "hello"]        ;; using a s-expression
  [(if my-cond ::my-component my-alias/other-comp) "hello"] ;; using all of the above

  ;; A component can have any identifier except unqualified keywords which are reserved for html elements.
  [true "This is not a pipe"]
  [false "This is a pipe"]
  [42 "This could be anything"]
  ['foobar "hello"]
  [[:experiment 7] "hello"]

  ;; Arguments passing
  [::my-component true false nil "foo"]           ;; any value can be passed, including nil
  [::my-component (val [:a :b]) (val [c d])]      ;; vector literals should be marked as values
  [::my-component (str "peace and " my-love-var)] ;; any s-expression can be passed
  [::my-component :a :b ~@my-kw-sequence :z]      ;; ~@ slices an expression in the arg list

  ;; Attributes applied on components
  [::my-component.foo {:class "bar"} arg1 arg2]    ;; my-component only has 2 args
  [::my-component (attrs my-attributes) arg1 arg2]) ;; my-component only has 2 args

  ;; Included components are always returning html. If you need to return a value, call a function instead.


(comment
  ;; Examples for 3.

  ;; Similar to a function
  (defc my-component [myself friend]
    [:div (:name myself) " is friend with " (:name friend)
     [:img {:href (:picture-url friend)}]])

  ;; Supports variable arities
  (defc my-component [& kws]
    [:div "List of keywords:"
     [:ul (for [kw kws] [:li kw])]]) ;; The for block is implicitly sliced into its parent

  ;; Supports multiple arities
  (defc my-component
    ([user-name] [:div user-name])
    ([] [:div "Anonymous"])))


(comment
  ;; Example for 4.

  ;; Input params can be used in if conditions, let and for blocks
  (defc my-component [person]
    (let [name (:name person)]
      (if (zero? (count (:friends person)))
        [:div name " has no friend."]
        [:div name "'s " (count (:friends person)) " friends:"
         [:ul (for [friend (:friends person)] [:li (:name friend)])]])))

  ;; Can also be used in place of a component, for dynamic component resolution.
  ;; The value has to evaluate to a component's identifier, usually a qualified keyword.
  (defc my-component [user friend-comp]
    [:div "My friends:"
     [:ul (for [friend (:friends user)]
            [friend-comp friend])]]))



(comment
  ;; Example for 5.

  ;; Destructuring in input parameters
  (defc my-component [{my-name :name} {:key [name picture-url] :as friend}]
    [:div my-name " is friend with " name
     [:img {:href picture-url}]])

  ;; In let blocks
  (let [[first second & rest] my-list] ...)

  ;; In for blocks
  (for [[first second & rest] my-lists] ...))
