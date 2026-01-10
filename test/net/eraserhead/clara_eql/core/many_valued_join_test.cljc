(ns net.eraserhead.clara-eql.core.many-valued-join-test
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.core :refer :all]
   [net.eraserhead.clara-eql.test-helpers :as t])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core QueryResult)))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defrule many-valued-join
  :query [{:foo/many-valued [:bar/name]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid) (= v "aaa")])

(deftest t-defrule-many-valued-join
  (testing "about joins"
    (is (= {:foo/many-valued [{:bar/name "b11"}
                              {:bar/name "b12"}]}
           (t/rule-result
            query-results
            `many-valued-join
            [[:foo/many-valued :db/cardinality :db.cardinality/many]
             [:r :foo/uuid "aaa"]
             [:r :foo/many-valued 11]
             [:r :foo/many-valued 12]
             [11 :bar/name "b11"]
             [12 :bar/name "b12"]]))
        "returns collections for many-valued nested join values")))
