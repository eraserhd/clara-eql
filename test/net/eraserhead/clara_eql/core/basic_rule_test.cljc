(ns net.eraserhead.clara-eql.core.basic-rule-test
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.core :refer :all]
   [net.eraserhead.clara-eql.test-helpers :as t])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core Candidate QueryResult)))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defrule basic-rule
  "Some basic rule"
  {:salience 100}
  :query [:foo/uuid]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(deftest t-defrule-basic-rule
  (testing "about single-cardinality keys"
    (is (= {:foo/uuid "aaa"} (t/rule-result query-results `basic-rule [[:r :foo/uuid "aaa"]]))
        "returns a result when all values are present")))
