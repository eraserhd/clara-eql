(ns net.eraserhead.clara-eav-map
  (:require
   [clara.rules.accumulators :as acc]
   [medley.core :as medley]))

(defn- reversed-attribute?
  "Returns true if k is a reversed attribute."
  [k]
  (and (qualified-keyword? k)
       (= \_ (get (name k) 0))))

(defn- reverse-attribute
  "Reverse an attribute - turns normal into reversed, reversed into normal."
  [k]
  (if (reversed-attribute? k)
    (keyword (namespace k) (subs (name k) 1))
    (keyword (namespace k) (str "_" (name k)))))

(defn- reduce-fn
  [acc {:keys [e a v] :as datom}]
  (if (keyword? a)
    (let [reverse-a (reverse-attribute a)]
      (-> acc
          (update-in [e a] #(cons v %))
          (update-in [v reverse-a] #(cons e %))))
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
    (medley/dissoc-in acc [e a])))

(defn- retract-fn
  [acc {:keys [e a v] :as datom}]
  (if (keyword? a)
    (-> acc
        (remove-eav e a v)
        (remove-eav v (reverse-attribute a) e))
    acc))

(def ^:private combine-fn
  (partial merge-with (partial merge-with concat)))

(def eav-map
  (acc/accum
   {:initial-value {}
    :reduce-fn     reduce-fn
    :retract-fn    retract-fn
    :combine-fn    combine-fn}))
