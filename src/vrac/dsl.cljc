(ns vrac.dsl)

;; Keywords used in Vrac's DSL which are already defined in clojure.core/cljs.core:
;; let, when, for

(def reserved-symbols '[$
                        do if quote ;; <- keywords of the Clojure languages, not defined in clojure.core
                        context with-context
                        global snap signal state memo
                        effect on-clean-up])

;; Those declarations help making the IDEs happy about the user's DSL expressions.
(declare $
         context with-context
         global snap signal state memo
         effect on-clean-up)
