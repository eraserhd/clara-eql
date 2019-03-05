(ns net.eraserhead.clara-eql.eav-map-test
  (:require
   [clara-eav.eav :as eav]
   [medley.core :as medley]
   [midje.sweet :refer :all]
   [net.eraserhead.clara-eql.eav-map :refer [eav-map]])
  (:import
   (clara_eav.eav EAV)))

(def initial-value (:initial-value eav-map))
(def reduce-fn (:reduce-fn eav-map))
(def retract-fn (:retract-fn eav-map))
(def combine-fn (:combine-fn eav-map))

(defn- sort-eavs
  [res]
  (let [paths (for [e (keys res) a (keys (get res e))] [e a])]
    (reduce #(update-in %1 %2 sort) res paths)))

(defn- eavs
  [operations]
  (reduce
   (fn [value [op e a v]]
     (case op
       :+ (reduce-fn value (eav/->EAV e a v))
       :- (retract-fn value (eav/->EAV e a v))))
   initial-value
   operations))

(facts "about eav-map"
  (facts "about initial value"
    (fact "initial value is an empty map"
      initial-value => {}))

  (facts "about reduce-fn"
    (fact "reduce-fn accumulates datoms"
      (eavs [[:+ 42 :foo/bar "hello"]])
      =>
      {42      {:foo/bar ["hello"]}
       "hello" {:foo/_bar [42]}})
    (fact "accumulates multiple values for an (eid, attribute) pair"
      (sort-eavs
       (eavs [[:+ 42 :foo/bar "hello"] [:+ 42 :foo/bar "goodbye"]]))
      =>
      {42        {:foo/bar ["goodbye" "hello"]}
       "goodbye" {:foo/_bar [42]}
       "hello"   {:foo/_bar [42]}})
    (fact "ignores non-keyword eavs"
      (eavs [[:+ 42 68 "hello"]]) => {}))

  (facts "about retract-fn"
    (fact "can retract one of a many-valued attribute"
      (sort-eavs
       (eavs [[:+ 42 :foo/bar "hello"]
              [:+ 42 :foo/bar "goodbye"]
              [:- 42 :foo/bar "hello"]]))
      =>
      {42        {:foo/bar ["goodbye"]}
       "goodbye" {:foo/_bar [42]}})
    (fact "can retract one of a many-valued attribute when there are equal values"
      (sort-eavs
       (eavs [[:+ 42 :foo/bar "hello"]
              [:+ 42 :foo/bar "hello"]
              [:- 42 :foo/bar "hello"]]))
      =>
      {42      {:foo/bar ["hello"]}
       "hello" {:foo/_bar [42]}})
    (fact "retracting all values removes attribute from map"
      (sort-eavs
       (eavs [[:+ 42 :foo/bar "hello"]
              [:+ 42 :bar/quux "hello"]
              [:- 42 :foo/bar "hello"]]))
      =>
      {42      {:bar/quux ["hello"]}
       "hello" {:bar/_quux [42]}})
    (fact "retracting all attributes removes entity from map"
      (eavs [[:+ 42 :foo/bar "hello"] [:- 42 :foo/bar "hello"]]) => {})
    (fact "ignores non-keyword eavs"
      (eavs [[:- 42 68 "hello"]]) => {}))

  (facts "about combine-fn"
    (fact "combines insertions from two reduced values"
      (sort-eavs
       (combine-fn
        (eavs [[:+ 42 :foo/bar "hello"] [:+ 42 :foo/bar "goodbye"]])
        (eavs [[:+ 42 :foo/bar "baz"] [:+ 26 :bar/baz "foo"]])))
      =>
      {42        {:foo/bar ["baz" "goodbye" "hello"]}
       26        {:bar/baz ["foo"]}
       "baz"     {:foo/_bar [42]}
       "foo"     {:bar/_baz [26]}
       "goodbye" {:foo/_bar [42]}
       "hello"   {:foo/_bar [42]}})))
