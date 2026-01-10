(ns net.eraserhead.clara-eql.pull
  (:require
   [clara.rules :refer :all]
   [clara-eav.eav :as eav]
   [edn-query-language.core :as eql]
   [net.eraserhead.clara-eql.eav-map :as eav-map])
  (:import
   (clara_eav.eav EAV)))

(defquery eav-map
  []
  [?eav-map <- eav-map/eav-map :from [EAV]])

(defquery entity-ref->eid
  [:?attribute :?value]
  [EAV (= e ?attribute) (= a :db/unique) (= v :db.unique/identity)]
  [EAV (= e ?eid) (= a ?attribute) (= v ?value)])

(defn entid [session entity-ref]
  (if (coll? entity-ref)
    (-> session
        (clara.rules/query entity-ref->eid :?attribute (first entity-ref) :?value (second entity-ref))
        first
        :?eid)
    entity-ref))

(defn- pull*
  [session eav-map expanded-pattern eid]
  (-> (reduce
       (fn [result {:keys [key children] :as subpattern}]
         (if-some [values (seq (get-in eav-map [eid key]))]
           (let [many? (= :db.cardinality/many (first (get-in eav-map [key :db/cardinality])))
                 values (cond->> values
                          true (mapv (fn [value]
                                       (if (fn? value)
                                         (value session)
                                         value)))
                          children (map #(pull* session eav-map subpattern %)))
                 value (if many?
                         (vec values)
                         (first values))]
             (assoc result key value))
           result))
       {}
       (:children expanded-pattern))
      not-empty))

(defn pull
  [session pattern entity-ref]
  (let [eid       (entid session entity-ref)
        pull-expr (eql/query->ast pattern)
        eav-map   (:?eav-map (first (clara.rules/query session eav-map)))]
    (pull* session eav-map pull-expr eid)))
