(ns extension.map)

(defn keep-nonempty [m]
  (into {} (filter (fn [[_ v]] (not (nil? v))) m)))
