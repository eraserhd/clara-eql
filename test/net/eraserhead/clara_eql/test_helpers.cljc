(ns net.eraserhead.clara-eql.test-helpers
  (:require
   [clara.rules :as r]
   [clara-eav.eav :as eav]
   [clara.tools.inspect :as inspect]
   [clojure.pprint]
   [clojure.spec.test.alpha]
   [clojure.walk]))

(clojure.spec.test.alpha/instrument)

(defn sort-multi-values [result]
  (clojure.walk/postwalk
   (fn [x]
     (if (and (vector? x) (not (map-entry? x)))
       (->> x (map pr-str) sort (map read-string) vec)
       x))
   result))

(def ^:dynamic *dump-session* false)

(defn dump-facts [session]
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

(defn rule-result [query-results rule-name facts]
  (let [session (-> (r/mk-session (symbol (namespace rule-name)))
                    (r/insert-all (map (partial apply eav/->EAV) facts))
                    (r/fire-rules)
                    dump-facts)
        results (map #(update % :?result sort-multi-values)
                     (r/query session query-results :?query rule-name))]
    (assert (= 1 (count results))
            (str "found " (count results) " results: " (pr-str results)))
    (:?result (first results))))
