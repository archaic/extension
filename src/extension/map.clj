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

(defn remove-nil-values
  [m]
  (into {}
        (remove (fn [[k v]]
                  (nil? v))
                m)))

(defn filter-keyword-keys
  [m]
  (into {}
        (filter (fn [[k v]]
                  (keyword? k))
                m)))

(defn namespace-keys
  [m ns]
  (reduce-kv (fn [m k v]

               (assoc m
                 (keyword ns
                          (name k))
                 v))
             {}
             m))

(defn deep-merge
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %)
                            (not (record? %)))
                      xs)

              (apply merge-with
                     m
                     xs)

              (last xs)))]

    (reduce m
            maps)))
