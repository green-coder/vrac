(ns vrac.dsl.ast-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl :as dsl]
            [vrac.dsl.parser :as parser :refer [expand-dsl]]
            [vrac.dsl.ast :as sut]))

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
            :root-ast {:node-type :clj/let
                       :bindings  [{:node-type :clj/let-binding
                                    :symbol    'a
                                    :value     {:node-type :clj/value
                                                :value     0}}]
                       :body      {:node-type :clj/invocation
                                   :function  {:node-type :clj/var
                                               :symbol    'clojure.core/vec
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
                                                                        :symbol    'clojure.core/str
                                                                        :tagged    true}
                                                            :args      [{:node-type :clj/var
                                                                         :symbol    'a
                                                                         :tagged    true}
                                                                        {:node-type :clj/var
                                                                         :symbol    'x
                                                                         :tagged    true}]}}]}}}
           (-> (expand-dsl (let [a 0]
                             (-> (for [x [1 2 3]]
                                   (str a x))
                                 vec)))
               parser/dsl->ast
               sut/make-context
               (sut/walk-ast pre-process post-process))))))

(deftest link-vars-to-their-definition-pass-test
  (testing "let"
    (is (= {:node-type :clj/let
            :bindings  [{:node-type :clj/let-binding
                         :symbol    'a
                         :value     {:node-type :clj/value
                                     :value     1}}
                        {:node-type :clj/let-binding
                         :symbol    'b
                         :value     {:node-type :clj/value
                                     :value     2}}]
            :body      {:node-type :clj/do
                        :bodies [{:node-type :clj/vector
                                  :items     [{:node-type      :clj/var
                                               :symbol         'a
                                               :var.definition/path [:bindings 0]}]}
                                 {:node-type :clj/map
                                  :entries   [{:node-type :clj/map-entry
                                               :key {:node-type      :clj/var
                                                     :symbol         'a
                                                     :var.definition/path [:bindings 0]}
                                               :value {:node-type      :clj/var
                                                       :symbol         'b
                                                       :var.definition/path [:bindings 1]}}]}
                                 {:node-type      :clj/var
                                  :symbol         'a
                                  :var.definition/path [:bindings 0]}]}}
           (-> (expand-dsl (let [a 1
                                 b 2]
                             [a]
                             {a b}
                             a))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               :root-ast))))

  (testing "for"
    (is (= {:node-type :clj/for
            :bindings  [{:node-type :clj/for-iteration
                         :symbol    'a
                         :value     {:node-type :clj/value
                                     :value     1}}]
            :body      {:node-type      :clj/var
                        :symbol         'a
                        :var.definition/path [:bindings 0]}}
           (-> (expand-dsl (for [a 1]
                             a))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               :root-ast)))

    (is (= {:node-type :clj/for
            :bindings [{:node-type :clj/for-iteration
                        :symbol 'a
                        :value {:node-type :clj/value
                                :value 1}}
                       {:node-type :clj/for-let
                        :bindings [{:node-type :clj/let-binding
                                    :symbol 'b
                                    :value {:node-type :clj/value
                                            :value 2}}]}]
            :body {:node-type :clj/var
                   :symbol 'b
                   :var.definition/path [:bindings 1 :bindings 0]}}
           (-> (expand-dsl (for [a 1
                                 :let [b 2]]
                             b))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               :root-ast))))

  (testing "defn"
    (is (= {:node-type :clj/defn
            :fn-name 'foo
            :params [{:node-type :clj/fn-param
                      :symbol 'a}
                     {:node-type :clj/fn-param
                      :symbol 'b
                      :metadata {:tag 'bar}}]
            :body {:node-type :clj/do
                   :bodies [{:node-type :clj/var
                             :symbol 'a
                             :var.definition/path [:params 0]}
                            {:node-type :clj/var
                             :symbol 'b
                             :var.definition/path [:params 1]}]}}
           (-> (expand-dsl (defn foo [a ^bar b]
                             a
                             b))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               :root-ast)))))

(deftest add-var-usage-pass-test
  (is (= {:node-type :clj/let
          :bindings [{:node-type :clj/let-binding
                      :symbol 'a
                      :value {:node-type :clj/value
                              :value 1}
                      :var.usage/paths [[:bindings 1 :value :args 0]]}
                     {:node-type :clj/let-binding
                      :symbol 'a
                      :value {:node-type :clj/invocation
                              :function {:node-type   :clj/var
                                         :symbol      'clojure.core/inc
                                         :var/unbound true}
                              :args [{:node-type :clj/var
                                      :symbol 'a
                                      :var.definition/path [:bindings 0]}]}
                      :var.usage/paths [[:body :args 0]
                                        [:body :args 1]]}]
          :body {:node-type :clj/invocation
                 :function {:node-type   :clj/var
                            :symbol      'clojure.core/+
                            :var/unbound true}
                 :args [{:node-type :clj/var
                         :symbol 'a
                         :var.definition/path [:bindings 1]}
                        {:node-type :clj/var
                         :symbol 'a
                         :var.definition/path [:bindings 1]}]}}
         (-> (expand-dsl (let [a 1
                               a (inc a)]
                           (+ a a)))
             parser/dsl->ast
             sut/make-context
             sut/link-vars-to-their-definition-pass
             sut/add-var-usage-pass
             :root-ast))))

(deftest add-lifespan-pass-test
  (testing "when the lifespan is the same for whole DSL"
    (is (= {:node-type :clj/let
            :bindings [{:node-type :clj/let-binding
                        :symbol 'a
                        :value {:node-type :clj/value
                                :value 1
                                :node.lifespan/path []}
                        :node.lifespan/path []}]
            :body {:node-type :clj/do
                   :bodies [{:node-type :clj/let
                             :bindings [{:node-type :clj/let-binding
                                         :symbol 'b
                                         :value {:node-type :clj/value
                                                 :value 2
                                                 :node.lifespan/path []}
                                         :node.lifespan/path []}]
                             :body {:node-type :clj/value
                                    :value nil
                                    :node.lifespan/path []}
                             :node.lifespan/path []}
                            {:node-type :clj/var
                             :symbol 'a
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
                                        :symbol 'clojure.core/inc
                                        :node.lifespan/path []}
                             :args [{:node-type :clj/var
                                     :symbol 'a
                                     :node.lifespan/path []}]
                             :node.lifespan/path []}
                            {:node-type :clj/var
                             :symbol 'a
                             :node.lifespan/path []}]
                   :node.lifespan/path []}
            :node.lifespan/path []}
           (-> (expand-dsl (let [a 1]
                             (let [b 2])
                             a
                             {a a}
                             [a]
                             (inc a)
                             a))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "if"
    (is (= {:node-type :clj/if
            :cond {:node-type :clj/invocation
                   :function {:node-type :clj/var
                              :symbol 'clojure.core/=
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
           (-> (expand-dsl (if (= 1 1)
                             :then
                             'else))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "when"
    (is (= {:node-type :clj/when
            :cond {:node-type :clj/value
                   :value true
                   :node.lifespan/path []}
            :body {:node-type :clj/value
                   :value 1
                   :node.lifespan/path [:body]}
            :node.lifespan/path []}
           (-> (expand-dsl (when true
                             1))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "for"
    (is (= {:node-type :clj/for
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
                                                       :symbol 'clojure.core/inc
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
                                          :symbol 'clojure.core/even?
                                          :node.lifespan/path [:bindings 0 :symbol]}
                               :args [{:node-type :clj/var
                                       :symbol 'a
                                       :node.lifespan/path [:bindings 0 :symbol]}]
                               :node.lifespan/path [:bindings 0 :symbol]}
                        :node.lifespan/path [:bindings 0 :symbol]}

                       {:node-type :clj/for-while
                        :cond {:node-type :clj/invocation
                               :function {:node-type :clj/var
                                          :symbol 'clojure.core/<
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
                              :symbol 'clojure.core/+
                              :node.lifespan/path [:bindings 4 :symbol]}
                   :args [{:node-type :clj/var
                           :symbol 'x
                           :node.lifespan/path [:bindings 4 :symbol]}
                          {:node-type :clj/var
                           :symbol 'y
                           :node.lifespan/path [:bindings 4 :symbol]}]
                   :node.lifespan/path [:bindings 4 :symbol]}
            :node.lifespan/path []}
           (-> (expand-dsl (for [x [1 2]
                                 :let [a (inc x)]
                                 :when (even? a)
                                 :while (< a 10)
                                 y [10 20]]
                             (+ x y)))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "effect"
    (is (= {:node-type :clj/let
            :bindings [{:node-type :clj/let-binding
                        :symbol 'a
                        :value {:node-type :dsl/signal
                                :body {:node-type :clj/value
                                       :value 1
                                       :node.lifespan/path []}
                                :node.lifespan/path []}
                        :node.lifespan/path []}]
            :body {:node-type :dsl/effect
                   ;; Should we parse the content of the effect, or treat it as a CLJC escape hatch?
                   :body {:node-type :clj/invocation
                          :function {:node-type :clj/var
                                     :symbol 'clojure.core/prn
                                     :node.lifespan/path []}
                          :args [{:node-type :clj/var
                                  :symbol 'a
                                  :node.lifespan/path []}]
                          :node.lifespan/path []}
                   :node.lifespan/path []}
            :node.lifespan/path []}
           (-> (expand-dsl (let [a (dsl/signal 1)]
                             (dsl/effect
                               (prn a))))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "effect-on"
    (is (= {:node-type :clj/let
            :bindings [{:node-type :clj/let-binding
                        :symbol 'a
                        :value {:node-type :dsl/signal
                                :body {:node-type :clj/value
                                       :value 1
                                       :node.lifespan/path []}
                                :node.lifespan/path []}
                        :node.lifespan/path []}]
            :body {:node-type :dsl/effect-on
                   :triggers [{:node-type :clj/var
                               :symbol 'a
                               :node.lifespan/path []}]
                   ;; Should we parse the content of the effect, or treat it as a CLJC escape hatch?
                   :body {:node-type :clj/invocation
                          :function {:node-type :clj/var
                                     :symbol 'clojure.core/prn
                                     :node.lifespan/path []}
                          :args [{:node-type :clj/var
                                  :symbol 'a
                                  :node.lifespan/path []}]
                          :node.lifespan/path []}
                   :node.lifespan/path []}
            :node.lifespan/path []}
           (-> (expand-dsl (let [a (dsl/signal 1)]
                             (dsl/effect-on [a]
                               (prn a))))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast))))

  (testing "defn"
    (is (= {:node-type :clj/let,
            :bindings [{:node-type :clj/let-binding
                        :symbol 'x
                        :value {:node-type :clj/value
                                :value 1
                                :node.lifespan/path []}
                        :node.lifespan/path []}]
            :body {:node-type :clj/defn
                   :fn-name 'foo
                   :params [{:node-type :clj/fn-param
                             :symbol 'a
                             :node.lifespan/path []}
                            {:node-type :clj/fn-param
                             :symbol 'b
                             :metadata {:tag 'bar}
                             :node.lifespan/path []}]
                   :body {:node-type :clj/do
                          :bodies [{:node-type :clj/var
                                    :symbol 'a
                                    :node.lifespan/path [:body :body]}
                                   {:node-type :clj/var
                                    :symbol 'b
                                    :node.lifespan/path [:body :body]}]
                          :node.lifespan/path [:body :body]}
                   :node.lifespan/path []}
            :node.lifespan/path []}
           (-> (expand-dsl (let [x 1]
                             (defn foo [a ^bar b]
                               a
                               b)))
               parser/dsl->ast
               sut/make-context
               sut/add-lifespan-pass
               :root-ast)))))

(deftest add-reactivity-type-pass-test
  (testing "reactivity 'none' for values"
    (is (= {:node-type :clj/value
            :value 1
            :reactivity/type :value}
           (-> (expand-dsl 1)
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "reactivity 'signal' for signals"
    (is (= {:node-type :dsl/signal
            :body {:node-type :clj/value
                   :value 1
                   :reactivity/type :value}
            :reactivity/type :signal}
           (-> (expand-dsl (dsl/signal 1))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "reactivity 'memo' for states"
    (is (= {:node-type :dsl/state
            :body {:node-type :clj/value
                   :value 1
                   :reactivity/type :value}
            :reactivity/type :memo}
           (-> (expand-dsl (dsl/state 1))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "function invocations"
    (is (= {:node-type :clj/invocation
            :function {:node-type :clj/var
                       :symbol 'clojure.core/+
                       :var/unbound true}
            :args [{:node-type :clj/value
                    :value 1
                    :reactivity/type :value}
                   {:node-type :clj/value
                    :value 1
                    :reactivity/type :value}]
            :reactivity/type :value}
           (-> (expand-dsl (+ 1 1)) ;; reactivity none & none
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (is (= {:node-type :clj/invocation
            :function {:node-type :clj/var
                       :symbol 'clojure.core/+
                       :var/unbound true}
            :args [{:node-type :clj/value
                    :value 1
                    :reactivity/type :value}
                   {:node-type :dsl/signal
                    :body {:node-type :clj/value
                           :value 1
                           :reactivity/type :value}
                    :reactivity/type :signal}]
            :reactivity/type :signal}
           (-> (expand-dsl (+ 1 (dsl/signal 1))) ;; reactivity none & signal
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (is (= {:node-type :clj/invocation
            :function {:node-type :clj/var
                       :symbol 'clojure.core/+
                       :var/unbound true}
            :args [{:node-type :clj/value
                    :value 1
                    :reactivity/type :value}
                   {:node-type :dsl/state
                    :body {:node-type :clj/value
                           :value 1
                           :reactivity/type :value}
                    :reactivity/type :memo}]
            :reactivity/type :memo}
           (-> (expand-dsl (+ 1 (dsl/state 1))) ;; reactivity none & memo
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (testing "mixing signals and memo"
      (is (= {:node-type :clj/invocation
              :function {:node-type :clj/var
                         :symbol 'clojure.core/+
                         :var/unbound true}
              :args [{:node-type :dsl/signal
                      :body {:node-type :clj/value
                             :value 1
                             :reactivity/type :value}
                      :reactivity/type :signal}
                     {:node-type :dsl/state
                      :body {:node-type :clj/value
                             :value 1
                             :reactivity/type :value}
                      :reactivity/type :memo}]
              :reactivity/type :signal}
             (-> (expand-dsl (+ (dsl/signal 1) (dsl/state 1))) ;; reactivity signal & memo
                 parser/dsl->ast
                 sut/make-context
                 sut/link-vars-to-their-definition-pass
                 sut/add-reactivity-type-pass
                 :root-ast)))))

  (testing "things wrapped in memo"
    (is (= {:node-type :dsl/memo
            :body {:node-type :clj/value
                   :value 1
                   :reactivity/type :value}
            :reactivity/type :value}
           (-> (expand-dsl (dsl/memo 1))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (is (= {:node-type :dsl/memo
            :body {:node-type :dsl/signal
                   :body {:node-type :clj/value
                          :value 1
                          :reactivity/type :value}
                   :reactivity/type :signal}
            :reactivity/type :memo}
           (-> (expand-dsl (dsl/memo (dsl/signal 1)))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (is (= {:node-type :dsl/memo
            :body {:node-type :dsl/memo
                   :body {:node-type :dsl/signal
                          :body {:node-type :clj/value
                                 :value 1
                                 :reactivity/type :value}
                          :reactivity/type :signal}
                   :reactivity/type :memo}
            :reactivity/type :memo}
           (-> (expand-dsl (dsl/memo (dsl/memo (dsl/signal 1))))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "set literal"
    (is (= {:node-type :clj/set
            ;; warning: the items ordering is undefined
            :items [{:node-type :clj/value
                     :value 1
                     :reactivity/type :value}
                    {:node-type :dsl/signal
                     :body {:node-type :clj/value
                            :value 4
                            :reactivity/type :value}
                     :reactivity/type :signal}
                    {:node-type :clj/set
                     :items [{:node-type :clj/value
                              :value 3
                              :reactivity/type :value}
                             {:node-type :clj/value
                              :value 2
                              :reactivity/type :value}]
                     :reactivity/type :value}]
            :reactivity/type :signal}
           (-> (expand-dsl #{1 #{2 3} (dsl/signal 4)})
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "vector literal"
    (is (= {:node-type :clj/vector
            :items [{:node-type :clj/value
                     :value 1
                     :reactivity/type :value}
                    {:node-type :clj/vector
                     :items [{:node-type :clj/value
                              :value 2
                              :reactivity/type :value}
                             {:node-type :clj/value
                              :value 3
                              :reactivity/type :value}]
                     :reactivity/type :value}
                    {:node-type :dsl/signal
                     :body {:node-type :clj/value
                            :value 4
                            :reactivity/type :value}
                     :reactivity/type :signal}]
            :reactivity/type :signal}
           (-> (expand-dsl [1 [2 3] (dsl/signal 4)])
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))


  (testing "hashmap literal"
    (is (= {:node-type :clj/vector
            :items [{:node-type :clj/map
                     :entries [{:node-type :clj/map-entry
                                :key {:node-type :clj/value
                                      :value :a
                                      :reactivity/type :value}
                                :value {:node-type :clj/value
                                        :value 1
                                        :reactivity/type :value}
                                :reactivity/type :value}]
                     :reactivity/type :value}
                    {:node-type :clj/map
                     :entries [{:node-type :clj/map-entry
                                :key {:node-type :clj/value
                                      :value :b
                                      :reactivity/type :value}
                                :value {:node-type :dsl/signal
                                        :body {:node-type :clj/value
                                               :value 2
                                               :reactivity/type :value}
                                        :reactivity/type :signal}
                                :reactivity/type :signal}]
                     :reactivity/type :signal}]
            :reactivity/type :signal}
           (-> (expand-dsl [{:a 1}
                            {:b (dsl/signal 2)}])
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "let"
    (is (= {:node-type :clj/let
            :bindings [{:node-type :clj/let-binding
                        :symbol 's
                        :value {:node-type :dsl/signal
                                :body {:node-type :clj/value
                                       :value 1
                                       :reactivity/type :value}
                                :reactivity/type :signal}
                        :reactivity/type :signal}]
            :body {:node-type :clj/var
                   :symbol 's
                   :var.definition/path [:bindings 0]
                   :reactivity/type :signal}
            :reactivity/type :signal}
           (-> (expand-dsl (let [s (dsl/signal 1)]
                             s))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "for"
    (is (= {:node-type :clj/for
            :bindings [{:node-type :clj/for-iteration
                        :symbol 'x
                        :value {:node-type :clj/vector
                                :items [{:node-type :clj/value
                                         :value 1
                                         :reactivity/type :value}
                                        {:node-type :clj/value
                                         :value 2
                                         :reactivity/type :value}]
                                :reactivity/type :value}
                        :reactivity/type :value}]
            :body {:node-type :clj/var
                   :symbol 'x
                   :var.definition/path [:bindings 0]
                   :reactivity/type :value}
            :reactivity/type :value}
           (-> (expand-dsl (for [x [1 2]]
                             x))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))

    (is (= {:node-type :clj/for
            :bindings [{:node-type :clj/for-iteration
                        :symbol 'x
                        :value {:node-type :clj/vector
                                :items [{:node-type :clj/value
                                         :value 1
                                         :reactivity/type :value}
                                        {:node-type :dsl/signal
                                         :body {:node-type :clj/value
                                                :value 2
                                                :reactivity/type :value}
                                         :reactivity/type :signal}]
                                :reactivity/type :signal}
                        :reactivity/type :signal}]
            :body {:node-type :clj/var
                   :symbol 'x
                   :var.definition/path [:bindings 0]
                   :reactivity/type :signal}
            :reactivity/type :signal}
           (-> (expand-dsl (for [x [1 (dsl/signal 2)]]
                             x))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast))))

  (testing "defn"
    (is (= {:node-type :clj/defn,
            :fn-name 'foo
            :params [{:node-type :clj/fn-param
                      :symbol 'a}
                     {:node-type :clj/fn-param
                      :symbol 'b
                      :metadata {:tag 'value}
                      :reactivity/type :value}
                     {:node-type :clj/fn-param
                      :symbol 'c
                      :metadata {:tag 'signal}
                      :reactivity/type :signal}
                     {:node-type :clj/fn-param
                      :symbol 'd
                      :metadata {:tag 'memo}
                      :reactivity/type :memo}]
            :body {:node-type :clj/do
                   :bodies [{:node-type :clj/var
                             :symbol 'a
                             :var.definition/path [:params 0]}
                            {:node-type :clj/var
                             :symbol 'b
                             :var.definition/path [:params 1]
                             :reactivity/type :value}
                            {:node-type :clj/var
                             :symbol 'c
                             :var.definition/path [:params 2]
                             :reactivity/type :signal}
                            {:node-type :clj/var
                             :symbol 'd
                             :var.definition/path [:params 3]
                             :reactivity/type :memo}]}}
           (-> (expand-dsl (defn foo [a ^value b ^signal c ^memo d]
                             a b c d))
               parser/dsl->ast
               sut/make-context
               sut/link-vars-to-their-definition-pass
               sut/add-reactivity-type-pass
               :root-ast)))))
