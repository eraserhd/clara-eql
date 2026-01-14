(ns net.eraserhead.clara-eql.core.many-valued-key-test
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

(defrule many-valued-key
  :query [:foo/uuid :foo/many-valued]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(r/defsession empty-session 'net.eraserhead.clara-eql.core.many-valued-key-test)

(deftest t-defrule-many-valued-key
  (testing "about top-level keys"
    (testing "about cardinality-many keys"
      (is (= {:foo/uuid        "aaa"
              :foo/many-valued [11 12]}
             (t/rule-result
              empty-session
              query-results
              `many-valued-key
              [[:foo/many-valued :db/cardinality :db.cardinality/many]
               [:r :foo/uuid "aaa"]
               [:r :foo/many-valued 11]
               [:r :foo/many-valued 12]]))
          "returns all values for a cardinality-many key")
      (is (= {:foo/uuid        "aaa"
              :foo/many-valued []}
             (t/rule-result
              empty-session
              query-results
              `many-valued-key
               [[:foo/many-valued :db/cardinality :db.cardinality/many]
                [:r :foo/uuid "aaa"]]))
          "returns an empty set for a cardinality-many key if no values are present"))))
