(ns net.eraserhead.clara-eql.core.many-valued-join2-test
  #?(:cljs (:require-macros [net.eraserhead.clara-eql.core :refer [defrule]]))
  (:require
   [clara.rules :as r]
   [clojure.test :refer [deftest testing is]]
   #?(:clj [net.eraserhead.clara-eql.core :refer [defrule]])
   [net.eraserhead.clara-eql.test-helpers :as t])
  #?(:clj (:import
           (clara_eav.eav EAV)
           (net.eraserhead.clara_eql.core QueryResult))))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defrule many-valued-join2
  :query [{:foo/many-valued [:bar/name]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(deftest t-defrule-many-valued-join2
  (testing "about joins"
    ;; This was producing twice as many `{:bar/name "bXX"}` maps because
    ;; the join rules were generating a result for each root times each
    ;; entity, instead of just for each entity.
    (is (= {:foo/many-valued [{:bar/name "b11"}
                              {:bar/name "b12"}]}
           (t/rule-result
            query-results
            `many-valued-join2
            [[:foo/many-valued :db/cardinality :db.cardinality/many]
             [:r :foo/uuid "aaa"]
             [:r :foo/many-valued 11]
             [:r :foo/many-valued 12]
             [99 :foo/uuid "bbb"]
             [99 :foo/many-valued 11]
             [99 :foo/many-valued 12]
             [11 :bar/name "b11"]
             [12 :bar/name "b12"]]))
        "regression: shared subtrees aren't multiplied")))
