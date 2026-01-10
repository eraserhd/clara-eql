(ns net.eraserhead.clara-eql.core.missing-property-value-test
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

(defrule missing-property-rule
 "Missing property rule"
 :query [:foo/uuid :foo/missing]
 :from ?eid
 :where
 [EAV (= e ?eid) (= a :foo/uuid)])

(deftest t-defule-missing-property-value
  (testing "about single-cardinality keys"
    (is (= {:foo/uuid "aaa"}
           (t/rule-result query-results `missing-property-rule [[:r :foo/uuid "aaa"]]))
        "returns a result when root is missing a key")))
