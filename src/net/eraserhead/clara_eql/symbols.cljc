(ns net.eraserhead.clara-eql.symbols
  (:require
   [clojure.string :as str]))

(defn- letter? [ch]
  #?(:clj  (Character/isLetter ch)
     :cljs (some? (re-matches #"(?u)\p{L}" (str ch)))))

(defn- digit? [ch]
  (some? (re-matches #"\d" (str ch))))

(defn- char-code [ch]
     :clj  (long ch)
  #?(:cljs (.charCodeAt ch 0)))

(defn- encode-ascii-value [ch]
  #?(:clj  (format "_%02X_" (char-code ch))
     :cljs (str "_"
                (-> (char-code ch)
                    (.toString 16)
                    (.toUpperCase)
                    (.padStart 2 "0"))
                "_")))

(defn encode-symbol-char [ch]
  (case ch
   (\.)                         "_DOT_"
   (\_)                         "___"
   (\* \+ \! \- \' \? \< \> \=) (str ch)
   (if (or (letter? ch)
           (digit? ch))
     (str ch)
     (encode-ascii-value ch))))

(defn encode-symbol-part [s]
  (->> s
       (map encode-symbol-char)
       str/join))

(defn key->variable [kw]
  (if-let [n (namespace kw)]
    (symbol (str \? (encode-symbol-part n) "_SLASH_" (encode-symbol-part (name kw))))
    (symbol (str \? (encode-symbol-part (name kw))))))

