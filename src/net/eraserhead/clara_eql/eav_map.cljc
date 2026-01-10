(ns net.eraserhead.clara-eql.eav-map
  (:require
   [clara.rules.accumulators :as acc]))

;; Taken from medley core
(defn- dissoc-in
  [m ks]
  (if-let [[k & ks] (seq ks)]
    (if (seq ks)
      (let [v (dissoc-in (get m k) ks)]
        (if (empty? v)
          (dissoc m k)
          (assoc m k v)))
      (dissoc m k))
    m))

(defn- reduce-fn
  [acc {:keys [e a v] :as datom}]
  (if (keyword? a)
    (update-in acc [e a] #(cons v %))
    acc))

(defn- remove-1
  [list value]
  (loop [list list
         keep '()]
    (if-let [[first & rest] (seq list)]
      (if (= value first)
        (concat rest keep)
        (recur rest (cons first keep)))
      keep)))

(defn- remove-eav
  [acc e a v]
  (if-let [res (seq (remove-1 (get-in acc [e a]) v))]
    (assoc-in acc [e a] res)
    (dissoc-in acc [e a])))

(defn- retract-fn
  [acc {:keys [e a v] :as datom}]
  (if (keyword? a)
    (remove-eav acc e a v)
    acc))

(def ^:private combine-fn
  (partial merge-with (partial merge-with concat)))

(def eav-map
  (acc/accum
   {:initial-value {}
    :reduce-fn     reduce-fn
    :retract-fn    retract-fn
    :combine-fn    combine-fn}))
