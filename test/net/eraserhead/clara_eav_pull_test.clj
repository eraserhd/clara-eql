(ns net.eraserhead.clara-eav-pull-test
  (:require
   [clara.rules :as r]
   [clara-eav.eav :as eav]
   [midje.sweet :refer :all]
   [net.eraserhead.clara-eav-pull :as pull]))

(facts "about pull"
  (let [session (-> (r/mk-session 'net.eraserhead.clara-eav-pull)
                    (r/insert
                     (eav/->EAV 10 :foo/uuid "aaa")
                     (eav/->EAV 10 :foo/name "a-name")
                     (eav/->EAV 10 :foo/id "a-id")
                     (eav/->EAV 10 :foo/bar 20)
                     (eav/->EAV 20 :bar/uuid "bbb")
                     (eav/->EAV 30 :foo/many "many")
                     (eav/->EAV 30 :foo/many "many1")
                     (eav/->EAV 30 :foo/many "many2")
                     (eav/->EAV :foo/many :db/cardinality :db.cardinality/many)
                     (eav/->EAV 40 :foo/lazy (constantly 42)))
                    (r/fire-rules))]
    (fact "Can pull forward single cardinality attribute"
      (pull/pull session [:foo/uuid] 10) => {:foo/uuid "aaa"})
    (fact "does not add attributes for which there are no values"
      (pull/pull session [:foo/missing] 10) => nil?
      (pull/pull session [:foo/uuid :foo/missing] 10) => {:foo/uuid "aaa"})
    (fact "can pull forward, single-cardinality ref attributes"
      (pull/pull session [:foo/bar] 10) => {:foo/bar 20})
    (fact "can pull forward, single-cardinality attributes recursively"
      (pull/pull session [{:foo/bar [:bar/uuid]}] 10) => {:foo/bar {:bar/uuid "bbb"}})
    (fact "can pull reverse, multi-cardinality attributes recursively"
      (pull/pull session [{:foo/_bar [:foo/name]}] 20) => {:foo/_bar [{:foo/name "a-name"}]})
    (fact "can pull forward, multi-cardinality attributes recursively"
      (:foo/many (pull/pull session [:foo/many] 30)) => (just ["many" "many1" "many2"] :in-any-order))
    (fact "lazy values are expanded"
      (pull/pull session [:foo/lazy] 40) => {:foo/lazy 42})))
