(ns net.eraserhead.clara-eql.core.nested-join-rule-test
  #?(:cljs (:require-macros [net.eraserhead.clara-eql.core :refer [defrule]]))
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   #?(:clj [net.eraserhead.clara-eql.core :refer [defrule]])
   [net.eraserhead.clara-eql.test-helpers :as t])
  #?(:clj  (:import  (clara_eav.eav EAV))
     :cljs (:require [clara-eav.eav :refer [EAV]]))
  #?(:clj  (:import  (net.eraserhead.clara_eql.core QueryResult))
     :cljs (:require [net.eraserhead.clara-eql.core :refer [QueryResult]])))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defrule nested-join-rule
  :query [{:a/b [{:b/c [:c/d]}]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :a/b)])

(r/defsession empty-session 'net.eraserhead.clara-eql.core.nested-join-rule-test)

(deftest t-defrule-nested-join-rule
  (testing "about joins"
    (is (= {:a/b {:b/c {:c/d "world"}}}
           (t/rule-result
            empty-session
            query-results
            `nested-join-rule
            [[:r :a/b 60]
             [60 :b/c 70]
             [70 :c/d "world"]]))
        "returns nested join values")))
