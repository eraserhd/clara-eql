(ns net.eraserhead.clara-eql.core.basic-join-rule-test
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.core :refer [defrule]]
   [net.eraserhead.clara-eql.test-helpers :as t])
  #?(:clj (:import
           (clara_eav.eav EAV)
           (net.eraserhead.clara_eql.core QueryResult))))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defrule basic-join-rule
  :query [{:foo/bar [:bar/uuid]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/bar)])

(deftest t-defrule-basic-join-rule
  (testing "about joins"
    (is (= {:foo/bar {:bar/uuid "ccc"}}
           (t/rule-result
            query-results
            `basic-join-rule
            [[:r :foo/bar 10]
             [10 :bar/uuid "ccc"]]))
        "returns joined values")))
