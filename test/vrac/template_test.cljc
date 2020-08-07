(ns vrac.template-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.template :as vt]
            [clojure.spec.alpha :as s]))

(deftest template-spec-test
  (are [template]
    (s/valid? ::vt/valuable template)
    true
    false
    3
    "text"
    'var
    :keyword
    '(:keyword var)
    '(:keyword nil)
    ;^:val [3 "text"]
    {3 "text"})

  (are [template]
    (s/valid? ::vt/props template)
    {:style {3 "text"}
     :class 3}) ; to be improved later

  (are [template]
    (not (s/valid? ::vt/props template))
    {3 "text"}
    '(:keyword var)
    '[:keyword var])

  (are [template]
    (s/valid? ::vt/bindings template)
    '[var context]
    '[var 3]
    '[var "text"])

  (are [template]
    (not (s/valid? ::vt/bindings template))
    '(var context)
    '(var 3)
    '(var "text"))

  (are [template]
    (s/valid? ::vt/dsl template)
    '(if (:kw var) 3 "text")
    '(if (:kw var) [:p 3] [:p "text"])
    '(when (:kw var) "text")
    '(when (:kw var) [:p 3])
    '(let [var context] 3)
    '(for [var context] 3))

  (are [template]
    (not (s/valid? ::vt/dsl template))
    '[let [var context] 3]
    '(let (var context) 3)
    '[for [var context] 3]
    '(for (var context) 3)))
