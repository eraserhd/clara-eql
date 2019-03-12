(ns net.eraserhead.clara-eql.core-test
  (:require
   [midje.sweet :refer :all]
   [clara.rules :as r]
   [clara-eav.eav :as eav]
   [clojure.spec.test.alpha]
   [net.eraserhead.clara-eql.core :refer :all])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core QueryResult)))

(clojure.spec.test.alpha/instrument)

(r/defquery query-results
  []
  [QueryResult (= e ?root) (= query ?query) (= result ?result)])

(defrule basic-rule
  "Some basic rule"
  {:salience 100}
  :query [:foo/uuid]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(defrule missing-property-rule
  "Missing property rule"
  :query [:foo/uuid :foo/missing]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(defrule many-valued-key
  :query [:foo/uuid :foo/many-valued]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(defrule basic-join-rule
  :query [{:foo/bar [:bar/uuid]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/bar)])

(defrule nested-join-rule
  :query [{:a/b [{:b/c [:c/d]}]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :a/b)])

(defrule many-valued-join
  :query [{:foo/many-valued [:bar/name]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid) (= v "aaa")])

(defn- sort-multi-values [result]
  (clojure.walk/postwalk
   (fn [x]
     (if (and (vector? x) (not (map-entry? x)))
       (->> x (map pr-str) sort (map read-string) vec)
       x))
   result))

(facts "about defrule"
  (let [session (-> (r/mk-session 'net.eraserhead.clara-eql.core
                                  'net.eraserhead.clara-eql.core-test)
                    (r/insert (eav/->EAV :foo/many-valued :db/cardinality :db.cardinality/many)
                              (eav/->EAV 10 :foo/uuid "aaa")
                              (eav/->EAV 10 :foo/many-valued 11)
                              (eav/->EAV 11 :bar/name "b11")
                              (eav/->EAV 12 :bar/name "b12")
                              (eav/->EAV 10 :foo/many-valued 12)
                              (eav/->EAV 20 :foo/uuid "bbb")
                              (eav/->EAV 30 :foo/bar 40)
                              (eav/->EAV 40 :bar/uuid "ccc")
                              (eav/->EAV 50 :a/b 60)
                              (eav/->EAV 60 :b/c 70)
                              (eav/->EAV 70 :c/d "world"))
                    (r/fire-rules))
        results (->> (r/query session query-results)
                     (map #(update % :?result sort-multi-values)))]
    (facts "about top-level keys"
      (facts "about single-cardinality keys"
        (fact "returns a result when all values are present"
          results => (contains {:?query  `basic-rule
                                :?root   10
                                :?result {:foo/uuid "aaa"}}))
        (fact "returns a result when root is missing a key"
          results => (contains {:?query  `missing-property-rule
                                :?root   10
                                :?result {:foo/uuid "aaa"}})))
      (facts "about cardinality-many keys"
        (fact "returns all values for a cardinality-many key"
          results => (contains {:?query  `many-valued-key
                                :?root   10
                                :?result {:foo/uuid        "aaa"
                                          :foo/many-valued [11 12]}}))
        (fact "returns an empty set for a cardinality-many key if no values are present"
          results => (contains {:?query  `many-valued-key
                                :?root   20
                                :?result {:foo/uuid        "bbb"
                                          :foo/many-valued []}}))))
    (facts "about joins"
      (fact "returns joined values"
        results => (contains {:?query  `basic-join-rule
                              :?root   30
                              :?result {:foo/bar {:bar/uuid "ccc"}}}))
      (fact "returns nested join values"
        results => (contains {:?query  `nested-join-rule
                              :?root   50
                              :?result {:a/b {:b/c {:c/d "world"}}}}))
      (fact "returns collections for many-valued nested join values"
        results => (contains {:?query  `many-valued-join
                              :?root   10
                              :?result {:foo/many-valued [{:bar/name "b11"}
                                                          {:bar/name "b12"}]}})))
    (facts "about unions"
      (future-fact "returns values from all branches of the union"))
    (facts "about idents"
      (future-fact "returns values from the specified object"))))
