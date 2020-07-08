(ns extension.hickory
  (:require [clojure.pprint :as pp]
            [clojure.zip :as cz]
            [hickory.core :as hc]
            [hickory.select :as hs]
            [hickory.zip :as hz]
            [taoensso.timbre :as log]))

(defn select-one
  [selector-fn tree]

  (let [xs
        (hs/select selector-fn
                   tree)]

    (if (= 1 (count xs))

      (first xs)

      (log/warnf "singular selection not found for fn: %s, tree: %s"
                 (str selector-fn)
                 (with-out-str (pp/pprint tree))))))

(defn text-selector
  [loc]
  (string? (cz/node loc)))

(defn text
  [node]
  (select-one text-selector
              node))
