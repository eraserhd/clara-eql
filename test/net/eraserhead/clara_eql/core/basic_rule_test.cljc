(ns net.eraserhead.clara-eql.core.basic-rule-test
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.test-helpers :as t])
  (#?(:clj :require :cljs :require-macros) [net.eraserhead.clara-eql.core :refer [defrule]])
  #?(:clj  (:import  (clara_eav.eav EAV))
     :cljs (:require [clara-eav.eav :refer [EAV]]))
  #?(:clj  (:import  (net.eraserhead.clara_eql.core QueryResult))
     :cljs (:require [net.eraserhead.clara-eql.core :refer [QueryResult]])))

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

(r/defsession empty-session 'net.eraserhead.clara-eql.core.basic-rule-test)

(deftest t-defrule-basic-rule
  (testing "about single-cardinality keys"
    (is (= {:foo/uuid "aaa"} (t/rule-result empty-session query-results `basic-rule [[:r :foo/uuid "aaa"]]))
        "returns a result when all values are present")))
