(ns extension.hickory
  (:require [clojure.pprint :as pp]
            [clojure.zip :as cz]
            [hickory.core :as hc]
            [hickory.select :as hs]
            [hickory.zip :as hz]
            [taoensso.timbre :as log]))

(defn maybe-select-one
  [selector-fn tree]

  (let [xs
        (hs/select selector-fn
                   tree)]

    (when (= 1 (count xs))

      (first xs))))

(defn select-one
  ([selector-fn tree log?]
   (or (maybe-select-one selector-fn
                         tree)

       (when log?
         (log/warnf "singular selection not found for fn: %s, tree: %s"
                    (str selector-fn)
                    (with-out-str (pp/pprint tree))))))

  ([selector-fn tree]
   (select-one selector-fn
               tree
               true)))

(defn text-selector
  [loc]
  (string? (cz/node loc)))

(defn text
  [node]
  (select-one text-selector
              node))

(defn maybe-text
  [node]
  (maybe-select-one text-selector
                    node))
