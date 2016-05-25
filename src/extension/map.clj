(ns extension.map)

(defn keep-nonempty
  [m]
  (into {}
        (filter (fn [[_ v]]
                  (not (nil? v)))
                m)))

(defn ?assoc
  [m & kvs]
  (->> kvs
       (partition 2)
       (remove (comp nil? second))
       (map vec)
       (into m)))
