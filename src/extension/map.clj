(ns extension.map
  (:require (clojure (set :refer (index)))
            (midje (sweet :refer (facts)))))

(defn remove-keys [pred m]
  (into {} (for [k (keys m) :when (not (pred k))] [k (m k)])))

(defn filter-keys [pred m]
  (into {} (for [k (keys m) :when (pred k)] [k (m k)])))

(defn remove-vals [pred m]
  (into {} (remove (fn [[k v]] (pred v)) m)))

(defn filter-vals [pred m]
  (into {} (filter (fn [[k v]] (pred v)) m)))

(defn keys-at [value m]
  (map first (filter (fn [[k v]] (= v value)) m)))

(defn update-keys [m f & args]
  (into {} (map (fn [[k v]] [(apply f k args) v]) m)))

(defn bucket [m names-to-keys]
  (into {} (map (fn [[name ks]]
                  [name (vals (select-keys m ks))])
                names-to-keys)))

(facts
 "about bucket"
 (bucket {:a 3 :b 2 :c 6} {"A" #{:a} "B-C" #{:b :c}}) => {"A" [3] "B-C" [2 6]})
