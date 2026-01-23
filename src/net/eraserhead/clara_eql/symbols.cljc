(ns net.eraserhead.clara-eql.symbols
  (:require
   [clojure.string :as str]))

(defn encode-symbol-char [ch]
  (case ch
   (\.)                         "_DOT_"
   (\_)                         "___"
   (\* \+ \! \- \' \? \< \> \=) (str ch)
   (if (or (Character/isDigit ch)
           (Character/isLetter ch))
     (str ch)
     (format "_%02X_" (long ch)))))

(defn encode-symbol-part [s]
  (->> s
       (map encode-symbol-char)
       str/join))

(defn key->variable [kw]
  (if-let [n (namespace kw)]
    (symbol (str \? (encode-symbol-part n) "_SLASH_" (encode-symbol-part (name kw))))
    (symbol (str \? (encode-symbol-part (name kw))))))

