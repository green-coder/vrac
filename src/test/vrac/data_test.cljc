(ns vrac.data-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.data :as vd]))

(comment
  (deftest find-matching-primary-keyword-test
    (let [find-primary-keyword (vd/matching-primary-keyword-finder #{:user/id
                                                                     :shop/id})]
      (are [kw result]
        (= (find-primary-keyword kw) result)

        :user/name :user/id
        :shop/articles :shop/id))))

(deftest ident-test
  (is (= (meta (vd/ident :cow/id 1))
         {:ident true})))

(deftest normalizer-entity-test
  (let [normalize (vd/normalizer #{:user/id
                                   :item/id
                                   :tag/id})]
    (are [value result]
      (= (normalize value) result)

      ; Values that should stay unchanged.
      "hello"
      [[nil nil "hello"]]

      [5 "hello"]
      [[nil nil [5 "hello"]]]

      {5 "hello"}
      [[nil nil {5 "hello"}]]

      ; One to one entity relation
      {:user/id 1
       :user/name "Johanna"
       :user/known-for #:tag{:id 1
                             :name "Clojure Skills"}}
      [[:tag/id 1 #:tag{:id 1
                        :name "Clojure Skills"}]
       [:user/id 1 #:user{:id 1
                          :name "Johanna"
                          :known-for [:tag/id 1]}]]

      ; One to many entity relation via a vector
      {:user/id 1
       :user/name "Johanna"
       :user/belongings [#:item{:id 1
                                :name "MacBook Air"}
                         #:item{:id 2
                                :name "Umbrella"}]}
      [[:item/id 1 #:item{:id 1
                          :name "MacBook Air"}]
       [:item/id 2 #:item{:id 2
                          :name "Umbrella"}]
       [:user/id 1 #:user{:belongings [[:item/id 1]
                                       [:item/id 2]]
                          :id 1
                          :name "Johanna"}]]

      ; One to many entity relation via a set
      {:user/id 1
       :user/name "Johanna"
       :user/belongings #{#:item{:id 1
                                 :name "MacBook Air"}}}
      [[:item/id 1 #:item{:id 1
                          :name "MacBook Air"}]
       [:user/id 1 #:user{:belongings #{[:item/id 1]}
                          :id 1
                          :name "Johanna"}]]

      ; Vector of mixed stuffs
      [{:user/id 1
        :user/name "Johanna"}
       3]
      [[:user/id 1 #:user{:id 1
                          :name "Johanna"}]
       [nil nil [[:user/id 1] 3]]]

      ; map of mixed stuffs
      {:a {:user/id 1
           :user/name "Johanna"}
       :b 7}
      [[:user/id 1 #:user{:id 1
                          :name "Johanna"}]
       [nil nil {:a [:user/id 1], :b 7}]])))

(comment
  (deftest db-assoc-test
    (are [db-before elements db-after]
      (= (reduce (fn [db [table id entity]]
                   (vd/db-assoc db [table id] entity))
                 db-before
                 elements)
         db-after)

      {}
      [[:cow/id 1 #:cow{:id 1, :name "la noire", :age 2}]
       [:cow/id 2 #:cow{:id 2, :name "bella", :age 3}]]
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}

      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}
      [[:cow/id 1 #:cow{:id 1, :name "la noiraude", :age 2}]]
      {:cow/id {1 #:cow{:id 1, :name "la noiraude", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}))

  (deftest db-update-test
    (are [db-before idents db-after]
      (= (reduce (fn [db ident]
                   (vd/db-update db ident update :cow/age + 5))
                 db-before
                 idents)
         db-after)

      ;; Update things in the DB, add 5 years to the specified cows.
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}
                3 #:cow{:id 3, :name "tulipe", :age 4}}}
      [[:cow/id 1]
       [:cow/id 2]]
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 7}
                2 #:cow{:id 2, :name "bella", :age 8}
                3 #:cow{:id 3, :name "tulipe", :age 4}}})))

(deftest ident?-test
  (are [val]
    (vd/ident? val)

    ^:ident [:cow/id 1]
    (vd/ident :cow/id 1))

  (is (not (vd/ident? [:cow/id 1]))))

(deftest denormalize-entity-test
  (are [db ident result]
    (= (vd/denormalize-entity db (get-in db ident))
       result)

    {:user/id {1 #:user{:id 1
                        :name "Johanna"
                        :known-for (vd/ident :tag/id 1)
                        :belongings [(vd/ident :item/id 1)
                                     (vd/ident :item/id 2)]}}
     :item/id {1 #:item{:id 1
                        :name "MacBook Air"}
               2 #:item{:id 2
                        :name "Umbrella"}}
     :tag/id {1 #:tag{:id 1
                      :name "Clojure Skills"}}}
    [:user/id 1]
    {:user/id 1
     :user/name "Johanna"
     :user/known-for #:tag{:id 1
                           :name "Clojure Skills"}
     :user/belongings [#:item{:id 1
                              :name "MacBook Air"}
                       #:item{:id 2
                              :name "Umbrella"}]}))

(deftest resolve-query-test
  (let [db {:user/id {1 #:user{:id 1
                               :name "Lania"
                               :bff (vd/ident :user/id 2)
                               :friends [(vd/ident :user/id 2)
                                         (vd/ident :user/id 3)]}
                      2 #:user{:id 2
                               :name "Nelson"
                               :bff (vd/ident :user/id 1)
                               :friends [nil
                                         (vd/ident :user/id 3)]}
                      3 #:user{:id 3
                               :name "Klonso"
                               :bff nil
                               ; Note: Friends are in a set, this time.
                               :friends #{(vd/ident :user/id 1)
                                          (vd/ident :user/id 2)
                                          (vd/ident :user/id 3)}}}}]
    (are [val query result]
      (= (vd/resolve-query db val query) result)

      (vd/ident :user/id 1)
      [:user/name
       :user/bff]
      #:user{:name "Lania"
             :bff (vd/ident :user/id 2)}

      (vd/ident :user/id 1)
      [:user/name
       {:user/bff [:user/name]}]
      #:user{:name "Lania"
             :bff {:user/name "Nelson"}}

      (vd/ident :user/id 1)
      [:user/name
       {:user/bff [:user/name
                   {:user/friends [:user/name]}]}]
      #:user{:name "Lania"
             :bff #:user{:name "Nelson"
                         :friends [nil
                                   #:user{:name "Klonso"}]}}

      (vd/ident :user/id 3)
      [:user/name
       :user/bff
       {:user/friends [:user/name]}]
      #:user{:name "Klonso"
             :bff nil
             :friends #{#:user{:name "Lania"}
                        #:user{:name "Nelson"}
                        #:user{:name "Klonso"}}})))

(deftest apply-diff-test
  (are [data diff result]
    (= (vd/apply-diff data diff) result)

    "Hello"
    "Bonjour"
    "Bonjour"

    {:a 1}
    {}
    {:a 1}

    [:a :b]
    {}
    [:a :b]

    {:id 7
     :name "Alan"
     :enemy "Grokk"}
    {:assoc {:age 35
             :friend "Binyl"}
     :update {:name "Alan Laan"}
     :dissoc [:enemy]}
    {:id 7
     :name "Alan Laan"
     :age 35
     :friend "Binyl"}

    {:stuffs [{:name "Social media"}
              {:name "Candies"}
              {:name "Switch"
               :price 200
               :quantity 0}
              {:name "BoTW"
               :multiplayer? false
               :awesome? true}
              {:name "Paperboy"
               :price 30
               :quantity 5}]}
    {:update {:stuffs {:assoc [[2 {:name "Nintendo Switch"
                                   :price 199
                                   :quantity 1}]]
                       :update [[3 {:assoc {:name "Breath of the Wild"
                                            :price 60
                                            :quantity 1}
                                    :dissoc [:multiplayer?]}]]
                       :remsert [[false 0 2]
                                 [true 0 [{:name "Sleep"
                                           :price nil}
                                          {:name "Family"
                                           :price nil}]]
                                 [false 4 1]
                                 [true 4 [{:name "Mario Kart Deluxe"
                                           :price 50
                                           :quantity 1}]]]}}}
    {:stuffs [{:name "Sleep", :price nil}
              {:name "Family", :price nil}
              {:name "Nintendo Switch", :price 199, :quantity 1}
              {:name "Breath of the Wild", :awesome? true, :price 60, :quantity 1}
              {:name "Mario Kart Deluxe", :price 50, :quantity 1}]}))
