(ns vrac.dsl.ast-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl.parser :as parser]
            [vrac.dsl.ast :as sut]))

#?(:clj
   (defn dsl->context [dsl]
     (-> dsl
         parser/resolve-and-macro-expand-dsl
         parser/dsl->ast
         sut/make-context)))

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
                                       :function  {:node-type :clj/var
                                                   :symbol    `vec
                                                   :tagged    true}
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
                                                                :function  {:node-type :clj/var
                                                                            :symbol    `str
                                                                            :tagged    true}
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
                  dsl->context
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
                dsl->context
                sut/link-vars-to-their-definition-pass)))))

#?(:clj
   (deftest add-var-usage-pass-test
     (is (= {:root-ast {:node-type :clj/let
                        :bindings [{:node-type :clj/let-binding
                                    :symbol 'a
                                    :value {:node-type :clj/value
                                            :value 1
                                            :var.usage/paths [[:bindings 1 :value :args 0]]}}
                                   {:node-type :clj/let-binding
                                    :symbol 'a
                                    :value {:node-type :clj/invocation
                                            :function {:node-type   :clj/var
                                                       :symbol      `inc
                                                       :var/unbound true}
                                            :args [{:node-type :clj/var
                                                    :symbol 'a
                                                    :var.value/path [:bindings 0 :value]}]
                                            :var.usage/paths [[:bodies 0 :args 0]
                                                              [:bodies 0 :args 1]]}}]
                        :bodies [{:node-type :clj/invocation
                                  :function {:node-type   :clj/var
                                             :symbol      `+
                                             :var/unbound true}
                                  :args [{:node-type :clj/var
                                          :symbol 'a
                                          :var.value/path [:bindings 1 :value]}
                                         {:node-type :clj/var
                                          :symbol 'a
                                          :var.value/path [:bindings 1 :value]}]}]}
             :path []}
            (-> '(let [a 1
                       a (inc a)]
                   (+ a a))
                dsl->context
                sut/link-vars-to-their-definition-pass
                sut/add-var-usage-pass)))))

#?(:clj
   (deftest add-lifespan-pass-test
     (testing "when the lifespan is the same for whole DSL"
       (is (= {:root-ast {:node-type :clj/let
                          :bindings [{:node-type :clj/let-binding
                                      :symbol 'a
                                      :value {:node-type :clj/value
                                              :value 1
                                              :node.lifespan/path []}
                                      :node.lifespan/path []}]
                          :bodies [{:node-type :clj/let
                                    :bindings [{:node-type :clj/let-binding
                                                :symbol 'b
                                                :value {:node-type :clj/value
                                                        :value 2
                                                        :node.lifespan/path []}
                                                :node.lifespan/path []}]
                                    :bodies []
                                    :node.lifespan/path []}
                                   {:node-type :clj/do,
                                    :bodies [{:node-type :clj/var
                                              :symbol 'a
                                              :node.lifespan/path []}]
                                    :node.lifespan/path []}
                                   {:node-type :clj/map
                                    :entries [{:node-type :clj/map-entry
                                               :key {:node-type :clj/var
                                                     :symbol 'a
                                                     :node.lifespan/path []}
                                               :value {:node-type :clj/var
                                                       :symbol 'a
                                                       :node.lifespan/path []}
                                               :node.lifespan/path []}]
                                    :node.lifespan/path []}
                                   {:node-type :clj/vector
                                    :items [{:node-type :clj/var
                                             :symbol 'a
                                             :node.lifespan/path []}]
                                    :node.lifespan/path []}
                                   {:node-type :clj/invocation
                                    :function {:node-type :clj/var
                                               :symbol `inc
                                               :node.lifespan/path []},
                                    :args [{:node-type :clj/var
                                            :symbol 'a
                                            :node.lifespan/path []}]
                                    :node.lifespan/path []}
                                   {:node-type :clj/var
                                    :symbol 'a
                                    :node.lifespan/path []}]
                          :node.lifespan/path []}
               :path []}
              (-> '(let [a 1]
                     (let [b 2])
                     (do a)
                     {a a}
                     [a]
                     (inc a)
                     a)
                  dsl->context
                  sut/add-lifespan-pass))))

     (testing "the if form"
       (is (= {:root-ast {:node-type :clj/if
                          :cond {:node-type :clj/invocation
                                 :function {:node-type :clj/var
                                            :symbol `=
                                            :node.lifespan/path []}
                                 :args [{:node-type :clj/value
                                         :value 1
                                         :node.lifespan/path []}
                                        {:node-type :clj/value
                                         :value 1
                                         :node.lifespan/path []}]
                                 :node.lifespan/path []}
                          :then {:node-type :clj/value
                                 :value :then
                                 :node.lifespan/path [:then]}
                          :else {:node-type :clj/value
                                 :value ''else
                                 :node.lifespan/path [:else]}
                          :node.lifespan/path []}
               :path []}
              (-> '(if (= 1 1)
                     :then
                     'else)
                  dsl->context
                  sut/add-lifespan-pass))))

     (testing "the when form"
       (is (= {:root-ast {:node-type :clj/when
                          :cond {:node-type :clj/value
                                 :value true
                                 :node.lifespan/path []}
                          :bodies [{:node-type :clj/value
                                    :value 1
                                    :node.lifespan/path [:bodies]}]
                          :node.lifespan/path []}
               :path []}
              (-> '(when true
                     1)
                  dsl->context
                  sut/add-lifespan-pass))))

     (testing "the for form"
       (is (= {:root-ast {:node-type :clj/for
                          :bindings [{:node-type :clj/for-iteration
                                      :symbol 'x
                                      :value {:node-type :clj/vector
                                              :items [{:node-type :clj/value
                                                       :value 1
                                                       :node.lifespan/path []}
                                                      {:node-type :clj/value
                                                       :value 2
                                                       :node.lifespan/path []}]
                                              :node.lifespan/path []}
                                      :node.lifespan/path []}

                                     {:node-type :clj/for-let
                                      :bindings [{:node-type :clj/let-binding
                                                  :symbol 'a
                                                  :value {:node-type :clj/invocation
                                                          :function {:node-type :clj/var
                                                                     :symbol `inc
                                                                     :node.lifespan/path [:bindings 0 :symbol]}
                                                          :args [{:symbol 'x
                                                                  :node-type :clj/var
                                                                  :node.lifespan/path [:bindings 0 :symbol]}]
                                                          :node.lifespan/path [:bindings 0 :symbol]}
                                                  :node.lifespan/path [:bindings 0 :symbol]}]
                                      :node.lifespan/path [:bindings 0 :symbol]}

                                     {:node-type :clj/for-when
                                      :cond {:node-type :clj/invocation
                                             :function {:node-type :clj/var
                                                        :symbol `even?
                                                        :node.lifespan/path [:bindings 0 :symbol]}
                                             :args [{:node-type :clj/var
                                                     :symbol 'a
                                                     :node.lifespan/path [:bindings 0 :symbol]}]
                                             :node.lifespan/path [:bindings 0 :symbol]}
                                      :node.lifespan/path [:bindings 0 :symbol]}

                                     {:node-type :clj/for-while
                                      :cond {:node-type :clj/invocation
                                             :function {:node-type :clj/var
                                                        :symbol `<
                                                        :node.lifespan/path [:bindings 0 :symbol]}
                                             :args [{:node-type :clj/var
                                                     :symbol 'a
                                                     :node.lifespan/path [:bindings 0 :symbol]}
                                                    {:node-type :clj/value
                                                     :value 10
                                                     :node.lifespan/path [:bindings 0 :symbol]}]
                                             :node.lifespan/path [:bindings 0 :symbol]}
                                      :node.lifespan/path [:bindings 0 :symbol]}

                                     {:node-type :clj/for-iteration
                                      :symbol 'y
                                      :value {:node-type :clj/vector
                                              :items [{:node-type :clj/value
                                                       :value 10
                                                       :node.lifespan/path [:bindings 0 :symbol]}
                                                      {:node-type :clj/value
                                                       :value 20
                                                       :node.lifespan/path [:bindings 0 :symbol]}]
                                              :node.lifespan/path [:bindings 0 :symbol]}
                                      :node.lifespan/path [:bindings 0 :symbol]}]

                          :body {:node-type :clj/invocation
                                 :function {:node-type :clj/var
                                            :symbol `+
                                            :node.lifespan/path [:bindings 4 :symbol]}
                                 :args [{:node-type :clj/var
                                         :symbol 'x
                                         :node.lifespan/path [:bindings 4 :symbol]}
                                        {:node-type :clj/var
                                         :symbol 'y
                                         :node.lifespan/path [:bindings 4 :symbol]}]
                                 :node.lifespan/path [:bindings 4 :symbol]}
                          :node.lifespan/path []}
               :path []}
              (-> '(for [x [1 2]
                         :let [a (inc x)]
                         :when (even? a)
                         :while (< a 10)
                         y [10 20]]
                     (+ x y))
                  dsl->context
                  sut/add-lifespan-pass))))))

