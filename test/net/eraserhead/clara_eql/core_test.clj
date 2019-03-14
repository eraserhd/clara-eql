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

(defrule union
  :query [{:foo/many-valued
           {:baz/id [:bar/name :baz/x]
            :quux/id [:bar/name :quux/x]}}]
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
                              (eav/->EAV 11 :baz/id 42)
                              (eav/->EAV 11 :baz/x -42)
                              (eav/->EAV 12 :bar/name "b12")
                              (eav/->EAV 12 :quux/id 76)
                              (eav/->EAV 12 :quux/x -76)
                              (eav/->EAV 10 :foo/many-valued 12)
                              (eav/->EAV 20 :foo/uuid "bbb")
                              (eav/->EAV 30 :foo/bar 40)
                              (eav/->EAV 40 :bar/uuid "ccc")
                              (eav/->EAV 50 :a/b 60)
                              (eav/->EAV 60 :b/c 70)
                              (eav/->EAV 70 :c/d "world"))
                    (r/fire-rules))
        results (->> (r/query session query-results)
                     (map #(update % :?result sort-multi-values)))
        result (fn [query root]
                 (let [relevant (->> results
                                     (filter #(= root (:?root %)))
                                     (filter #(= query (:?query %))))]
                   (assert (= 1 (count relevant))
                           (str "found " (count relevant) " results: " (pr-str relevant)))
                   (:?result (first relevant))))]
    (facts "about top-level keys"
      (facts "about single-cardinality keys"
        (fact "returns a result when all values are present"
          (result `basic-rule 10) => {:foo/uuid "aaa"})
        (fact "returns a result when root is missing a key"
          (result `missing-property-rule 10) => {:foo/uuid "aaa"}))
      (facts "about cardinality-many keys"
        (fact "returns all values for a cardinality-many key"
          (result `many-valued-key 10) => {:foo/uuid        "aaa"
                                           :foo/many-valued [11 12]})
        (fact "returns an empty set for a cardinality-many key if no values are present"
          (result `many-valued-key 20) => {:foo/uuid        "bbb"
                                           :foo/many-valued []})))
    (facts "about joins"
      (fact "returns joined values"
        (result `basic-join-rule 30) => {:foo/bar {:bar/uuid "ccc"}})
      (fact "returns nested join values"
        (result `nested-join-rule 50) => {:a/b {:b/c {:c/d "world"}}})
      (fact "returns collections for many-valued nested join values"
        (result `many-valued-join 10) => {:foo/many-valued [{:bar/name "b11"}
                                                            {:bar/name "b12"}]}))
    (facts "about unions"
      (future-fact "returns values from all branches of the union"
        (let [result (->> results
                          (filter #(= `union (:?query %)))
                          (filter #(= 10 (:?root %)))
                          (map :?result))]
          result => {:foo/many-valued [{:bar/name "b11"
                                        :baz/x -42}
                                       {:bar/name "b12"
                                        :quux/x -76}]})))
    (facts "about idents"
      (future-fact "returns values from the specified object"))))
