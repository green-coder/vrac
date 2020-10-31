(ns vrac.model.vql-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.model.vql :as vql :refer [vql-model]]
            [minimallist.core :as m]
            [minimallist.helper :as h]))

(m/describe vql-model
            '[(:session/title session)
              (:session/description session)
              (for [speakers (:session/speakers session)]
                [(:speaker/name speakers)
                 (:speaker/bio speakers)])])

(m/describe vql-model
            '(let [{:session/keys [title description speakers]} session]
               [title
                session
                (for [{:speaker/keys [name bio]} speakers]
                  {:name name
                   :bio bio})]))

(deftest vql-model-test
  (are [vql-query]
    (is (m/valid? vql-model vql-query))

    ; constants
    1
    :a
    :a/b
    "abc"
    true
    nil

    ; nodes
    'a
    '(:a b)
    '(:a b 1)
    '(+ a b)
    '[3 a :a (inc 1)]
    '{:a a, :b 1}
    '(let [a 1
           b 2]
       (+ a b))
    '(for [i [1 2 3]
           j [4 5 6]]
       (* i j))
    '(if cond 1 2)
    '(when cond 1)

    ; Destruct bindings
    '(let [[a b & [c {d :d}]] d]
       a)
    '(let [{a :a, b :b
            :keys [c d]
            :foo/keys [d e]
            :or {a 1, b 2}
            :as f} g]
       a)))

(comment
  ;;;; Brainstorming

  ; EQL
  '{session [:session/title
             :session/description
             {:session/speakers [:speaker/name
                                 :speaker/bio]}]}

  ; VQL
  '[(:session/title session)
    (:session/description session)
    (for [speakers (:session/speakers session)]
      [(:speaker/name speakers)
       (:speaker/bio speakers)])]

  ;; VQL - same semantic as above
  '(let [{:session/keys [title description speakers]} session]
     [title
      session
      (for [{:speaker/keys [name bio]} speakers]
        [name
         bio])])

  ; Returns values similarly to Clojure
  ["session title"
   "session description"
   [["name1"
     "bio1"]
    ["name2"
     "bio2"]]]

  ; VQL - computed data
  '[(- (:admin.money/spent global)
       (:admin.money/earned global))
    (count (:admin/users global))]

  ; VQL - let with computed data
  '(let [{:admin/keys [users]
          :admin.money/keys [earning spending]} global
         benefit (- earning spending)
         nb-users (count users)]
     [benefit
      nb-users
      (/ benefit nb-users)])

  ; VQL - recursive query
  ; but .. it seems complicated to :
  ; - parse (minimallist)
  ; - validate (unbound variables?)
  ; - interpret (sci)
  (let [cluster (fn cluster [person]
                  {:person {:name (:name person)}
                   :friends (mapv cluster (:friends person))})]
    (cluster user)))

; We pass a user, but to get to the friends, we need the user-id.
; The approach of Pathom can help for that:
; user -> user-id -> coll-of friend's user-id -> name

'{:user [:user/id
         :user/name]}

; the resolvers of pathom are a good match to finding the path between a person and its friends,
; it will find that it can get its user/id first, then from there it can get to the list of friends.

; Pathom will be helpful to interpret the VQL's transitions and allow to use `user` instead of `user-id`.
; Now, can Pathom help to find that it's the user-id which should be sent over the network and assume that the
; server knows the other part of the story?

{:name 'friends
 :relation {:user/id [{:user/friends [:user/id]}]}}
