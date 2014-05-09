(ns extension.collection
  "Functions specific to collections")

(defn difference
  "nth order differencing on a collection of numbers"
  [xs n]
  (map #(- %2 %) xs (drop n xs)))
