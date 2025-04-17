(ns vrac.dsl.parser-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl :as dsl]
            [vrac.dsl.macro :as macro]
            [vrac.dsl.parser :as sut]))

;; If we want to test it on CLJS, we need to go through a macro
;; in order to get the env to be used in the `resolve` function.
#?(:clj
   (deftest resolve-and-macro-expand-dsl-test
     (testing "expansion of the -> macro and symbol resolution in `let`"
       (is (= '(let [a 1]
                 (clojure.core/inc a))
              (sut/resolve-and-macro-expand-dsl '(let [a 1]
                                                   (-> a
                                                       inc))
                                                (sut/symbol-resolver *ns* nil nil)
                                                macro/default-macros))))))

(deftest expand-dsl-test
  (testing "expansion of the -> macro and symbol resolution in `let`"
    (is (= '(let [a 1]
              (clojure.core/inc a))
           (sut/expand-dsl
             (let [a 1]
               (-> a
                   inc))))))

  (testing "destructuring in `for`"
    (is (= '(for [a [1 2 3]
                  :let [b (clojure.core/+ a 100)
                        m {:x a
                           :y b}
                        x (:x m)
                        y (:y m)]
                  :when (clojure.core/< a b x y)]
              [a b x y])
           (sut/expand-dsl
             (for [a [1 2 3]
                   :let [b (+ a 100)
                         {:keys [x y] :as m} {:x a
                                              :y b}]
                   :when (< a b x y)]
               [a b x y])))))

  (testing "quoted expressions"
    (is (= '(let [a 1
                  b 'x
                  c ''y]
              'd)
           (sut/expand-dsl
             (let [a 1
                   b 'x
                   c ''y]
               'd)))))

  (testing "a signal"
    (is (= '(vrac.dsl/signal 1)
           (sut/expand-dsl
             (dsl/signal 1)))))

  (testing "a signal inside a signal"
    (is (= '(vrac.dsl/signal (vrac.dsl/signal 1))
           (sut/expand-dsl
             (dsl/signal (dsl/signal 1))))))

  (testing "a signal inside a +"
    (is (= '(clojure.core/+ (vrac.dsl/signal 1))
           (sut/expand-dsl
             (+ (dsl/signal 1))))))

  (testing "1 signal inside a let body"
    (is (= '(let []
              (vrac.dsl/signal 1))
           (sut/expand-dsl
             (let []
               (dsl/signal 1))))))

  (testing "1 signal inside a let binding"
    (is (= '(let [a (vrac.dsl/signal 1)]
              a)
           (sut/expand-dsl
             (let [a (dsl/signal 1)]
               a)))))

  (testing "effect"
    (is (= '(let [a 1
                  b 2]
              (vrac.dsl/effect
                (clojure.core/prn (clojure.core/+ a b))))
           (sut/expand-dsl
             (let [a 1
                   b 2]
               (dsl/effect
                 (prn (+ a b))))))))

  (testing "effect-on"
    (is (= '(let [a 1
                  b 2]
              (vrac.dsl/effect-on [a (clojure.core/even? b)]
                (clojure.core/prn (clojure.core/+ a b))))
           (sut/expand-dsl
             (let [a 1
                   b 2]
               (dsl/effect-on [a (even? b)]
                 (prn (+ a b))))))))

  (testing "defn"
    (let [expanded-dsl (sut/expand-dsl
                         (defn foo [a ^bar b]
                           a))]
      (is (= '(defn foo [a b]
                a)
             expanded-dsl))
      (testing "the metadata is preserved"
        (is (= {:tag 'bar}
               (-> expanded-dsl
                   (nth 2)
                   (nth 1)
                   meta)))))))


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
     :body      {:node-type :clj/var
                 :symbol    'a}}
    '(let [a 1]
       a)

    {:node-type :clj/let
     :bindings  [{:node-type :clj/let-binding
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
     :body      {:node-type :clj/value
                 :value ''d}}
    '(let [a 1
           b 'x
           c ''y]
       'd)

    {:node-type :dsl/signal
     :body      {:node-type :clj/value
                 :value     1}}
    '(vrac.dsl/signal 1)

    {:node-type :dsl/state
     :body      {:node-type :clj/value
                 :value     1}}
    '(vrac.dsl/state 1)

    {:node-type :clj/invocation
     :function  {:node-type :clj/var
                 :symbol    'clojure.core/+}
     :args      [{:node-type :clj/var
                  :symbol    'a}
                 {:node-type :clj/var
                  :symbol    'b}]}
    '(clojure.core/+ a b)

    {:node-type :dsl/once
     :body      {:node-type :clj/invocation
                 :function  {:node-type :clj/var
                             :symbol    'clojure.core/+}
                 :args      [{:node-type :clj/var
                              :symbol    'a}
                             {:node-type :clj/var
                              :symbol    'b}]}}
    '(vrac.dsl/once (clojure.core/+ a b))

    {:node-type :dsl/memo
     :body      {:node-type :clj/invocation
                 :function  {:node-type :clj/var
                             :symbol    'clojure.core/+}
                 :args      [{:node-type :clj/var
                              :symbol    'a}
                             {:node-type :clj/var
                              :symbol    'b}]}}
    '(vrac.dsl/memo (clojure.core/+ a b))

    {:node-type :clj/do
     :bodies    [{:node-type :clj/invocation
                  :function  {:node-type :clj/var
                              :symbol    'clojure.core/prn}
                  :args      [{:node-type :clj/var
                               :symbol    'a}]}
                 {:node-type :clj/var
                  :symbol    'a}]}
    '(do (clojure.core/prn a)
         a)

    {:node-type :clj/if
     :cond      {:node-type :clj/value
                 :value     true}
     :then      {:node-type :clj/value
                 :value     10}
     :else      {:node-type :clj/value
                 :value     20}}
    '(if true 10 20)

    {:node-type :clj/when
     :cond      {:node-type :clj/value
                 :value     true}
     :body      {:node-type :clj/value
                 :value     30}}
    '(when true 30)

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
                  :cond      {:node-type :clj/invocation
                              :function  {:node-type :clj/var
                                          :symbol    'clojure.core/>}
                              :args      [{:node-type :clj/var
                                           :symbol    'a}
                                          {:node-type :clj/value
                                           :value     1}]}}
                 {:node-type :clj/for-while
                  :cond      {:node-type :clj/invocation
                              :function  {:node-type :clj/var
                                          :symbol    'clojure.core/>}
                              :args      [{:node-type :clj/var
                                           :symbol    'b}
                                          {:node-type :clj/value
                                           :value     2}]}}]
     :body      {:node-type :clj/value
                 :value     10}}
    '(for [i [1 2 3]
           :let [j 4]
           :when (clojure.core/> a 1)
           :while (clojure.core/> b 2)]
       10)

    {:node-type :dsl/effect
     :body      {:node-type :clj/invocation
                 :function  {:node-type :clj/var
                             :symbol    'clojure.core/prn}
                 :args      [{:node-type :clj/invocation
                              :function  {:node-type :clj/var
                                          :symbol    'clojure.core/+}
                              :args      [{:node-type :clj/var
                                           :symbol    'a}
                                          {:node-type :clj/var
                                           :symbol    'b}]}]}}
    '(vrac.dsl/effect
       (clojure.core/prn (clojure.core/+ a b)))

    {:node-type :dsl/effect-on
     :triggers [{:node-type :clj/var
                 :symbol 'a}
                {:node-type :clj/invocation
                 :function {:node-type :clj/var
                            :symbol    'clojure.core/even?}
                 :args [{:node-type :clj/var
                         :symbol 'b}]}]
     :body     {:node-type :clj/invocation
                :function  {:node-type :clj/var
                            :symbol    'clojure.core/prn}
                :args      [{:node-type :clj/invocation
                             :function  {:node-type :clj/var
                                         :symbol    'clojure.core/+}
                             :args      [{:node-type :clj/var
                                          :symbol    'a}
                                         {:node-type :clj/var
                                          :symbol    'b}]}]}}
    '(vrac.dsl/effect-on [a (clojure.core/even? b)]
       (clojure.core/prn (clojure.core/+ a b)))

    {:node-type :clj/defn
     :fn-name 'foo
     :params [{:node-type :clj/fn-param
               :symbol 'a}
              {:node-type :clj/fn-param
               :metadata {:tag 'bar}
               :symbol 'b}]
     :body   {:node-type :clj/var
              :symbol 'a}}

    '(defn foo [a ^bar b]
       a)))
