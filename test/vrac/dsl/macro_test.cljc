(ns vrac.dsl.macro-test
  (:require [clojure.test :refer [deftest testing is are]]
            [vrac.dsl.macro :as sut]
            [vrac.test.util :refer [make-gensym]]))

(deftest thread-first-test
  (testing "the -> macro"
    (is (= `(prn (+ ~'a 1))
           (sut/thread-first `(-> ~'a (+ 1) prn))))))

(deftest thread-last-test
  (testing "the ->> macro"
    (is (= `(prn (+ 1 ~'a))
           (sut/thread-last `(->> ~'a (+ 1) prn))))))

(deftest thread-as-test
  (testing "the as-> macro"
    (is (= `(let [~'y (+ 1 2)
                  ~'y (+ ~'y 3)]
              (+ ~'y 4))
           (sut/thread-as `(as-> (+ 1 2) ~'y
                                 (+ ~'y 3)
                                 (+ ~'y 4)))))))

(deftest destruct-test
  (testing "sequential destruct"
    (is (= `[~'d ~'x
             ~'a (nth ~'d 0)
             ~'b (nth ~'d 1)
             ~'c (nth ~'d 2)
             ~'rest (drop 3 ~'d)]
           (#'sut/destructure '[[a b c & rest :as d] x]))))

  (testing "associative destruct"
    (is (= '[x y
             a (:aa x)
             bb (:bb x)
             c (:cc bb)
             d (:dd bb)]
           (#'sut/destructure '[{a        :aa
                                 {c   :cc
                                  d   :dd
                                  :as bb} :bb
                                 :as      x} y]))))

  (testing "associative destruct inside sequential destruct"
    (with-redefs [gensym (make-gensym)]
      (is (= `[~'x ~'y
               ~'map__1 (nth ~'x 0)
               ~'a (:a ~'map__1)
               ~'map__2 (nth ~'x 1)
               ~'b (:b ~'map__2)]
             (#'sut/destructure '[[{a :a} {b :b} :as x] y])))))

  (testing "sequential destruct inside associative destruct"
    (with-redefs [gensym (make-gensym)]
      (is (= `[~'x ~'y
               ~'vec__1 (:aa ~'x)
               ~'a1 (nth ~'vec__1 0)
               ~'a2 (nth ~'vec__1 1)
               ~'bb (:bb ~'x)
               ~'vec__2 (:cc ~'bb)
               ~'c1 (nth ~'vec__2 0)
               ~'c2 (nth ~'vec__2 1)]
             (#'sut/destructure '[{[a1 a2]      :aa
                                   {[c1 c2] :cc
                                    :as     bb} :bb
                                   :as          x} y])))))

  (testing "default values inside associative destruct"
    (is (= '[x y
             a (:aa x)
             b (:bb x :bb-default)
             i (:i x)
             j (:j x :jj-default)
             k (:foo/k x)
             l (:foo/l x :ll-default)
             p (:bar/p x)
             q (:bar/q x :qq-default)
             r (:foo/r x)
             s (:foo/s x :ss-default)]
           (#'sut/destructure '[{a         :aa
                                 b         :bb
                                 :keys     [i j foo/k foo/l]
                                 :bar/keys [p q foo/r foo/s]
                                 :or       {b :bb-default
                                            j :jj-default
                                            l :ll-default
                                            q :qq-default
                                            s :ss-default}
                                 :as       x} y]))))


  (testing "the :& syntax in associative destruct"
    (is (= `[~'x ~'y
             ~'a (:a ~'x)
             ~'b (:b ~'x)
             ~'c (:c ~'x)
             ~'bar (:foo/bar ~'x)
             ~'baz (:foo/baz ~'x)
             ~'rest (dissoc ~'x :a :b :c :foo/bar :foo/baz)]
           (#'sut/destructure '[{a         :a
                                 :keys     [b c]
                                 :foo/keys [bar baz]
                                 :&        rest
                                 :as       x} y])))))

(deftest expand-let-bindings-test
  (testing "let bindings expansion"
    (with-redefs [gensym (make-gensym)]
      (is (= `(~'let [~'map__1 {:a 1}
                      ~'a (:a ~'map__1)]
                ~'a)
             (sut/expand-let-bindings `(~'let [~'{a :a} {:a 1}] ~'a)))))))

(deftest expand-for-bindings-test
  (testing "for bindings expansion"
    (with-redefs [gensym (make-gensym)]
      (is (= `(~'for [~'item__1 [[1 2] [3 4]]
                      :let [~'vec__2 ~'item__1
                            ~'a (nth ~'vec__2 0)
                            ~'b (nth ~'vec__2 1)]
                      :let [~'vec__3 [10 20]
                            ~'c (nth ~'vec__3 0)
                            ~'d (nth ~'vec__3 1)]]
                [~'a ~'b ~'c ~'d])
              (sut/expand-for-bindings `(~'for [~'[a b] ~[[1 2] [3 4]]
                                                :let [~'[c d] ~'[10 20]]]
                                          ~'[a b c d])))))))
