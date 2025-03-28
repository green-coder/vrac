(ns vrac.dsl.ast-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl.parser :as parser]
            [vrac.dsl.ast :as sut]))

#?(:clj
   (deftest walk-ast-test
     (let [pre-process (fn tag-all-vars [{:keys [root-ast path] :as context}]
                         (let [ast (get-in root-ast path)]
                           (case (:node-type ast)
                             :clj/var
                             (-> context
                                 (update-in (cons :root-ast path) assoc :tagged true))

                             ;; else
                             context)))
           post-process (fn identity [context] context)]
       (is (= {:path     []
               :root-ast {:bindings  [{:node-type :clj/let-binding
                                       :symbol    'a
                                       :value     {:node-type :clj/value
                                                   :value     0}}]
                          :bodies    [{:node-type :clj/invocation
                                       :function  `vec
                                       :args      [{:node-type :clj/for
                                                    :bindings  [{:node-type :clj/for-iteration
                                                                 :symbol    'x
                                                                 :value     {:items     [{:node-type :clj/value
                                                                                          :value     1}
                                                                                         {:node-type :clj/value
                                                                                          :value     2}
                                                                                         {:node-type :clj/value
                                                                                          :value     3}]
                                                                             :node-type :clj/vector}}]
                                                    :body      {:node-type :clj/invocation
                                                                :function  `str
                                                                :args      [{:node-type :clj/var
                                                                             :symbol    'a
                                                                             :tagged    true}
                                                                            {:node-type :clj/var
                                                                             :symbol    'x
                                                                             :tagged    true}]}}]}]
                          :node-type :clj/let}}
              (-> '(let [a 0]
                     (-> (for [x [1 2 3]]
                           (str a x))
                         vec))
                  parser/resolve-and-macro-expand-dsl
                  parser/dsl->ast
                  sut/make-context
                  (sut/walk-ast pre-process post-process)))))))

#?(:clj
   (deftest link-vars-to-their-definition-pass-test
     (is (= {:root-ast {:node-type :clj/let
                        :bindings  [{:node-type :clj/let-binding
                                     :symbol    'a
                                     :value     {:node-type :clj/value
                                                 :value     1}}
                                    {:node-type :clj/let-binding
                                     :symbol    'b
                                     :value     {:node-type :clj/value
                                                 :value     2}}]
                        :bodies    [{:node-type :clj/vector
                                     :items     [{:node-type      :clj/var
                                                  :symbol         'a
                                                  :var.value/path [:bindings 0 :value]}]}
                                    {:node-type :clj/map
                                     :entries   [{:node-type :clj/map-entry
                                                  :key {:node-type      :clj/var
                                                        :symbol         'a
                                                        :var.value/path [:bindings 0 :value]}
                                                  :value {:node-type      :clj/var
                                                          :symbol         'b
                                                          :var.value/path [:bindings 1 :value]}}]}
                                    {:node-type      :clj/var
                                     :symbol         'a
                                     :var.value/path [:bindings 0 :value]}]}
             :path     []}
            (-> '(let [a 1
                       b 2]
                   [a]
                   {a b}
                   a)
                parser/resolve-and-macro-expand-dsl
                parser/dsl->ast
                sut/make-context
                sut/link-vars-to-their-definition-pass)))))
