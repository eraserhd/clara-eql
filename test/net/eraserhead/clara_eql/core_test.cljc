(ns net.eraserhead.clara-eql.core-test
  (:require
   [clara.rules :as r]
   [clara.rules.accumulators :as acc]
   [clara-eav.eav :as eav]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.core :refer :all]
   [net.eraserhead.clara-eql.test-helpers :as t])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core Candidate QueryResult)))

(deftest t-defrule
  (testing "about unions")
    ;(future-fact "returns values from all branches of the union"))
  (testing "about idents"))
    ;(future-fact "returns values from the specified object")))
