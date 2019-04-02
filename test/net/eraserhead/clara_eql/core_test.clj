(ns net.eraserhead.clara-eql.core-test
  (:require
   [midje.sweet :refer :all]
   [clara.rules :as r]
   [clara.tools.inspect :as inspect]
   [clara-eav.eav :as eav]
   [clojure.pprint]
   [clojure.spec.test.alpha]
   [net.eraserhead.clara-eql.core :refer :all])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core QueryResult)))

(clojure.spec.test.alpha/instrument)

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(defn- sort-multi-values [result]
  (clojure.walk/postwalk
   (fn [x]
     (if (and (vector? x) (not (map-entry? x)))
       (->> x (map pr-str) sort (map read-string) vec)
       x))
   result))

(def ^:dynamic *dump-session* false)

(defn- dump-facts [session]
  (when *dump-session*
    (println "\n\n================= Fact Dump ====================")
    (doseq [[kind facts] (->> (inspect/inspect session)
                              :insertions
                              (mapcat val)
                              (map :fact)
                              (group-by class))]
      (print (str "\n" (.getSimpleName kind) "::"))
      (->> facts
        (map #(into {} %))
        (map (fn [fact]
               (if (contains? fact :query)
                 (update fact :query name)
                 fact)))
        clojure.pprint/print-table)))
  session)

(defn- check [rule facts]
  ;; Unmap other rules first to make dump-facts nice
  (doseq [[sym var] (ns-publics *ns*)
          :when (:rule (meta var))]
    (ns-unmap *ns* sym))
  (eval rule)
  (let [rule-name (symbol (str (ns-name *ns*)) (str (second rule)))
        session (-> (r/mk-session 'net.eraserhead.clara-eql.core-test)
                    (r/insert-all (map (partial apply eav/->EAV) facts))
                    (r/fire-rules)
                    dump-facts)
        results (map #(update % :?result sort-multi-values)
                     (r/query session query-results :?query rule-name))]
    (assert (= 1 (count results))
            (str "found " (count results) " results: " (pr-str results)))
    (:?result (first results))))

(facts "about defrule"
  (facts "about top-level keys"
    (facts "about single-cardinality keys"
      (fact "returns a result when all values are present"
        (check
          '(defrule basic-rule
             "Some basic rule"
             {:salience 100}
             :query [:foo/uuid]
             :from ?eid
             :where
             [EAV (= e ?eid) (= a :foo/uuid)])
          [[:r :foo/uuid "aaa"]]) => {:foo/uuid "aaa"})
      (fact "returns a result when root is missing a key"
        (check
          '(defrule missing-property-rule
             "Missing property rule"
             :query [:foo/uuid :foo/missing]
             :from ?eid
             :where
             [EAV (= e ?eid) (= a :foo/uuid)])
         [[:r :foo/uuid "aaa"]]) => {:foo/uuid "aaa"}))
    (facts "about cardinality-many keys"
      (fact "returns all values for a cardinality-many key"
        (check
          '(defrule many-valued-key
             :query [:foo/uuid :foo/many-valued]
             :from ?eid
             :where
             [EAV (= e ?eid) (= a :foo/uuid)])
         [[:foo/many-valued :db/cardinality :db.cardinality/many]
          [:r :foo/uuid "aaa"]
          [:r :foo/many-valued 11]
          [:r :foo/many-valued 12]]) => {:foo/uuid        "aaa"
                                         :foo/many-valued [11 12]})
      (fact "returns an empty set for a cardinality-many key if no values are present"
        (check
          '(defrule many-valued-key
             :query [:foo/uuid :foo/many-valued]
             :from ?eid
             :where
             [EAV (= e ?eid) (= a :foo/uuid)])
          [[:foo/many-valued :db/cardinality :db.cardinality/many]
           [:r :foo/uuid "aaa"]]) => {:foo/uuid        "aaa"
                                      :foo/many-valued []})))
  (facts "about joins"
    (fact "returns joined values"
      (check
        '(defrule basic-join-rule
           :query [{:foo/bar [:bar/uuid]}]
           :from ?eid
           :where
           [EAV (= e ?eid) (= a :foo/bar)])
        [[:r :foo/bar 10]
         [10 :bar/uuid "ccc"]]) => {:foo/bar {:bar/uuid "ccc"}})
    (fact "returns nested join values"
      (check
        '(defrule nested-join-rule
           :query [{:a/b [{:b/c [:c/d]}]}]
           :from ?eid
           :where
           [EAV (= e ?eid) (= a :a/b)])
        [[:r :a/b 60]
         [60 :b/c 70]
         [70 :c/d "world"]]) => {:a/b {:b/c {:c/d "world"}}})
    (fact "returns collections for many-valued nested join values"
      (check
        '(defrule many-valued-join
           :query [{:foo/many-valued [:bar/name]}]
           :from ?eid
           :where
           [EAV (= e ?eid) (= a :foo/uuid) (= v "aaa")])
        [[:foo/many-valued :db/cardinality :db.cardinality/many]
         [:r :foo/uuid "aaa"]
         [:r :foo/many-valued 11]
         [:r :foo/many-valued 12]
         [11 :bar/name "b11"]
         [12 :bar/name "b12"]]) => {:foo/many-valued [{:bar/name "b11"}
                                                      {:bar/name "b12"}]})
    (facts "about unions"
      (future-fact "returns values from all branches of the union"))
    (facts "about idents"
      (future-fact "returns values from the specified object"))))
