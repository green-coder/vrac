(ns vrac.model-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.model :as vm]
            [minimallist.core :as m]
            [minimallist.helper :as h]))

(def valuable-model  (h/let vm/model-definitions (h/ref 'valuable)))
(def props-model     (h/let vm/model-definitions (h/ref 'props)))
(def bindings-model  (h/let vm/model-definitions (h/ref 'bindings)))
(def directive-model (h/let vm/model-definitions (h/ref 'directive)))

(deftest template-model-test
  (are [x]
    (m/valid? valuable-model x)
    true
    false
    3
    "text"
    'var
    :foobar
    '(:foobar var)
    '(:foobar nil)
    {:foobar "text"}
    [3 "text"]
    {3 "text"}
    #{3 "text"})

  (are [x]
    (m/valid? props-model x)
    {:style {:foobar "text"}}
    {:style {3 "text"}}
    {:class 3}) ; to be improved later

  (are [x]
    (not (m/valid? props-model x))
    {3 "text"}
    '(:foobar var)
    '[:foobar var])

  (are [x]
    (m/valid? bindings-model x)
    '[var context]
    '[var 3]
    '[var "text"])

  (are [x]
    (not (m/valid? bindings-model x))
    '(var context)
    '(var 3)
    '(var "text"))

  (are [x]
    (m/valid? directive-model x)
    '(if (:kw var) 3 "text")
    '(if (:kw var) [:p 3] [:p "text"])
    '(when (:kw var) "text")
    '(when (:kw var) [:p 3])
    '(let [var context] 3)
    '(for [var context] 3))

  (are [x]
    (not (m/valid? directive-model x))
    '[let [var context] 3]
    '(let (var context) 3)
    '[for [var context] 3]
    '(for (var context) 3)))
