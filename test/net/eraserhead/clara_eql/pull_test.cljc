(ns net.eraserhead.clara-eql.pull-test
  (:require
   [clara.rules :as r]
   [clara-eav.eav :as eav]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.pull :as pull]))

(deftest t-pull
  (let [session (-> (r/mk-session 'net.eraserhead.clara-eql.pull)
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
    (is (= {:foo/uuid "aaa"} (pull/pull session [:foo/uuid] 10))
        "Can pull single cardinality attribute")
    (testing "does not add attributes for which there are no values"
      (is (nil? (pull/pull session [:foo/missing] 10)))
      (is (= {:foo/uuid "aaa"} (pull/pull session [:foo/uuid :foo/missing] 10))))
    (is (= {:foo/bar 20} (pull/pull session [:foo/bar] 10))
        "can pull single-cardinality ref attributes")
    (is (= {:foo/bar {:bar/uuid "bbb"}} (pull/pull session [{:foo/bar [:bar/uuid]}] 10))
        "can pull single-cardinality attributes recursively")
    (is (= #{"many" "many1" "many2"} (->> (pull/pull session [:foo/many] 30) :foo/many (into #{})))
        "can pull multi-cardinality attributes recursively")
    (is (= {:foo/lazy 42} (pull/pull session [:foo/lazy] 40))
        "lazy values are expanded")))
