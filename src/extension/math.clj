(ns extension.math)

(defn mean
  [xs]
  (let [n (count xs)]
    (if (pos? n)
      (float (/ (reduce + xs)
                n))
      0.0)))

(defn sd
  [xs]
  (let [n (count xs)]
    (if (pos? n)
      (let [mean (mean xs)]
        (Math/sqrt (/ (reduce + (map (fn [x] (Math/pow (- x mean) 2)) xs)) n)))
      0.0)))
