(ns extension.map
  (:require [clojure.set :as cs]))

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

(defn namespace-keys
  [m ns]

  (reduce-kv (fn [m k v]

               (assoc m
                 (keyword ns
                          (name k))
                 v))
             {}
             m))
