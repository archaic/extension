(ns extension.datomic
  (:require [datomic.api :as d :refer [db q]]))

(defn bulk-transact! [{:keys [conn tx-data verbose?]}]
  (let [tx-partition-data (partition-all 1000 tx-data)
        n (count tx-data)
        ref-count (atom 0)]
    (when verbose?
      (println (format "Starting to cache %d tx-data." n)))
    (doseq [tx-data tx-partition-data]
      @(d/transact conn tx-data)
      (Thread/sleep 100)
      (swap! ref-count (fn [x] (+ x (count tx-data))))
      (when verbose?
        (println (format "%d/%d completed." @ref-count n))))))
