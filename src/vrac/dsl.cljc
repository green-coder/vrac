(ns ^:no-doc vrac.dsl)

;; Keywords used in Vrac's DSL which are already defined in clojure.core/cljs.core:
;; defn, let, when, for

(def reserved-symbols '[$
                        do if quote ;; <- keywords of the Clojure languages, not defined in clojure.core
                        global context with-context
                        once signal state memo
                        effect effect-on on-clean-up])

;; Those declarations help making the IDEs happy about the user's DSL expressions.
(declare $
         global context with-context
         once signal state memo
         effect effect-on on-clean-up)
