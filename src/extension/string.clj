(ns extension.string)

(defn ->f
  [s]
  (when (string? s)
    (when-let [s (re-find #"\d*\.\d+|\d+" s)]
      (Float/parseFloat s))))

(defn ->i
  [s]
  (when (string? s)
    (when-let [s (re-find #"\d+" s)]
      (Integer/parseInt s))))
