(ns vrac.dsl.parser-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl :as dsl]
            [vrac.dsl.parser :as sut]))

#?(:clj
   (deftest resolve-and-macro-expand-dsl-test
     (testing "expansion of the -> macro and symbol resolution in `let`"
       (is (= `(let [~'a 1
                     ~'b 2]
                 (prn (+ ~'a ~'b 3)))
              (sut/resolve-and-macro-expand-dsl '(let [a 1
                                                       b 2]
                                                   (-> a
                                                       (+ b 3)
                                                       prn))))))

     (testing "destructuring in `for`"
       (is (= `(for [~'a [1 2 3]
                     :let [~'b (+ ~'a 100)
                           ~'m {:x ~'a
                                :y ~'b}
                           ~'x (:x ~'m)
                           ~'y (:y ~'m)]
                     :when (< ~'a ~'b ~'x ~'y)]
                 [~'a ~'b ~'x ~'y])
              (sut/resolve-and-macro-expand-dsl '(for [a [1 2 3]
                                                       :let [b (+ a 100)
                                                             {:keys [x y] :as m} {:x a
                                                                                  :y b}]
                                                       :when (< a b x y)]
                                                      [a b x y])))))

     (testing "quoted expressions"
       (is (= `(let [~'a 1
                     ~'b ~''x
                     ~'c ~'''y]
                 ~''d)
              (sut/resolve-and-macro-expand-dsl '(let [a 1
                                                       b 'x
                                                       c ''y]
                                                   'd)))))))


(deftest dsl->ast-test
  (are [expected-ast dsl]
    (= expected-ast (sut/dsl->ast dsl))

    {:node-type :clj/var
     :symbol 'a}
    'a

    {:node-type :clj/value
     :value ''a}
    ''a

    {:node-type :clj/let
     :bindings  [{:node-type :clj/let-binding
                  :symbol    'a
                  :value     {:node-type :clj/value
                              :value     1}}]
     :bodies    [{:node-type :clj/var
                  :symbol    'a}]}
    `(let [~'a 1]
       ~'a)

    {:node-type :clj/let
     :bindings [{:node-type :clj/let-binding
                 :symbol 'a
                 :value {:node-type :clj/value
                         :value 1}}
                {:node-type :clj/let-binding
                 :symbol 'b
                 :value {:node-type :clj/value
                         :value ''x}}
                {:node-type :clj/let-binding
                 :symbol 'c
                 :value {:node-type :clj/value
                         :value '''y}}]
     :bodies   [{:node-type :clj/value
                 :value ''d}]}
    `(let [~'a 1
           ~'b ~''x
           ~'c ~'''y]
       ~''d)

    {:node-type :dsl/signal
     :body      {:node-type :clj/value
                 :value     1}}
    `(dsl/signal 1)

    {:node-type :dsl/state
     :body      {:node-type :clj/value
                 :value     1}}
    `(dsl/state 1)

    {:node-type :clj/invocation
     :function  {:node-type :clj/var
                 :symbol    `+}
     :args      [{:node-type :clj/var
                  :symbol    'a}
                 {:node-type :clj/var
                  :symbol    'b}]}
    `(+ ~'a ~'b)

    {:node-type :dsl/memo
     :body      {:node-type :clj/invocation
                 :function  {:node-type :clj/var
                             :symbol    `+}
                 :args      [{:node-type :clj/var
                              :symbol    'a}
                             {:node-type :clj/var
                              :symbol    'b}]}}
    `(dsl/memo (+ ~'a ~'b))

    {:node-type :dsl/snap
     :body      {:node-type :clj/invocation
                 :function  {:node-type :clj/var
                             :symbol    `+}
                 :args      [{:node-type :clj/var
                              :symbol    'a}
                             {:node-type :clj/var
                              :symbol    'b}]}}
    `(dsl/snap (+ ~'a ~'b))

    {:node-type :clj/do
     :bodies    [{:node-type :clj/invocation
                  :function  {:node-type :clj/var
                              :symbol    `prn}
                  :args      [{:node-type :clj/var
                               :symbol    'a}]}
                 {:node-type :clj/var
                  :symbol    'a}]}
    `(do (prn ~'a)
         ~'a)

    {:node-type :clj/if
     :cond      {:node-type :clj/value
                 :value     true}
     :then      {:node-type :clj/value
                 :value     10}
     :else      {:node-type :clj/value
                 :value     20}}
    `(if true 10 20)

    {:node-type :clj/when
     :cond      {:node-type :clj/value
                 :value     true}
     :bodies    [{:node-type :clj/value
                  :value     30}]}
    `(when true 30)

    {:node-type :clj/for
     :bindings  [{:node-type :clj/for-iteration
                  :symbol    'i
                  :value     {:node-type :clj/vector
                              :items     [{:node-type :clj/value
                                           :value     1}
                                          {:node-type :clj/value
                                           :value     2}
                                          {:node-type :clj/value
                                           :value     3}]}}
                 {:node-type :clj/for-let
                  :bindings  [{:node-type :clj/let-binding
                               :symbol    'j
                               :value     {:node-type :clj/value
                                           :value     4}}]}
                 {:node-type :clj/for-when
                  :value     {:node-type :clj/invocation
                              :function  {:node-type :clj/var
                                          :symbol    `>}
                              :args      [{:node-type :clj/var
                                           :symbol    'a}
                                          {:node-type :clj/value
                                           :value     1}]}}
                 {:node-type :clj/for-while
                  :value     {:node-type :clj/invocation
                              :function  {:node-type :clj/var
                                          :symbol    `>}
                              :args      [{:node-type :clj/var
                                           :symbol    'b}
                                          {:node-type :clj/value
                                           :value     2}]}}]
     :body      {:node-type :clj/value
                 :value     10}}
    `(for [~'i [1 2 3]
           :let [~'j 4]
           :when (> ~'a 1)
           :while (> ~'b 2)]
       10)

    {:node-type :dsl/effect
     :bodies    [{:node-type :clj/invocation
                  :function  {:node-type :clj/var
                              :symbol    `prn}
                  :args      [{:node-type :clj/invocation
                               :function  {:node-type :clj/var
                                           :symbol    `+}
                               :args      [{:node-type :clj/var
                                            :symbol    'a}
                                           {:node-type :clj/var
                                            :symbol    'b}
                                           {:node-type :clj/var
                                            :symbol    'c}
                                           {:node-type :clj/var
                                            :symbol    'd}
                                           {:node-type :clj/var
                                            :symbol    'e}]}]}]}
    `(dsl/effect
       (prn (+ ~'a ~'b ~'c ~'d ~'e)))))
