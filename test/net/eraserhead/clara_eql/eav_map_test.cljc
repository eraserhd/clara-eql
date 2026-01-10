(ns net.eraserhead.clara-eql.eav-map-test
  (:require
   [clara-eav.eav :as eav]
   [clojure.test :refer [deftest testing is]]
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

(deftest t-eav-map
  (testing "about initial value"
    (is (= {} initial-value)
        "initial value is an empty map"))
  (testing "about reduce-fn"
    (is (= {42 {:foo/bar ["hello"]}}
           (eavs [[:+ 42 :foo/bar "hello"]]))
        "reduce-fn accumulates datoms")
    (is (= {42 {:foo/bar ["goodbye" "hello"]}}
           (sort-eavs
            (eavs [[:+ 42 :foo/bar "hello"] [:+ 42 :foo/bar "goodbye"]])))
        "accumulates multiple values for an (eid, attribute) pair")
    (is (= {} (eavs [[:+ 42 68 "hello"]]))
        "ignores non-keyword eavs"))
  (testing "about retract-fn"
    (is (= {42 {:foo/bar ["goodbye"]}}
           (sort-eavs
            (eavs [[:+ 42 :foo/bar "hello"]
                   [:+ 42 :foo/bar "goodbye"]
                   [:- 42 :foo/bar "hello"]])))
        "can retract one of a many-valued attribute")
    (is (= {42 {:foo/bar ["hello"]}}
           (sort-eavs
            (eavs [[:+ 42 :foo/bar "hello"]
                   [:+ 42 :foo/bar "hello"]
                   [:- 42 :foo/bar "hello"]])))
        "can retract one of a many-valued attribute when there are equal values")
    (is (= {42 {:bar/quux ["hello"]}}
           (sort-eavs
            (eavs [[:+ 42 :foo/bar "hello"]
                   [:+ 42 :bar/quux "hello"]
                   [:- 42 :foo/bar "hello"]])))
        "retracting all values removes attribute from map")
    (is (= {} (eavs [[:+ 42 :foo/bar "hello"] [:- 42 :foo/bar "hello"]]))
        "retracting all attributes removes entity from map")
    (is (= {} (eavs [[:- 42 68 "hello"]]))
        "ignores non-keyword eavs"))
  (testing "about combine-fn"
    (is (= {42 {:foo/bar ["baz" "goodbye" "hello"]}
            26 {:bar/baz ["foo"]}}
           (sort-eavs
            (combine-fn
             (eavs [[:+ 42 :foo/bar "hello"] [:+ 42 :foo/bar "goodbye"]])
             (eavs [[:+ 42 :foo/bar "baz"] [:+ 26 :bar/baz "foo"]]))))
        "combines insertions from two reduced values")))
