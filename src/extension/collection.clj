(ns extension.collection
  "Functions specific to collections"
  (:require (clojure (set :refer (union)))))

(defn difference [xs n]
  "nth order differencing on a collection of numbers"
  (map #(- %2 %) xs (drop n xs)))
