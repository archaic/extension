(ns extension.datomic
  (:require
   [clojure.repl :as cr]
   [datomic.api :as d]
   [taoensso.timbre :as log]))

(defn transact!
  [conn tx-data]

  (when (seq tx-data)
    (let [tx-partition-data
          (partition-all 1000
                         tx-data)]

      (doseq [tx-data tx-partition-data]
        (try @(d/transact conn
                          tx-data)

             (catch Exception Ex

               (let [error
                     (-> (ex-data Ex)
                         :db/error)

                     unavailable?
                     (= error
                        :db.error/transactor-unavailable)
                     
                     timeout?
                     (= error
                        :db.error/transaction-timeout)]

                 (if (or timeout?
                         unavailable?)

                   (let [error-string
                         (cond timeout? "timed out"
                               unavailable? "was unavailable")
                         
                         minutes 3]

                     (log/warnf "Transaction %s, retrying in %d minutes"
                                error-string
                                minutes)

                     (Thread/sleep (* minutes 60 1000))

                     @(d/transact conn
                                  tx-data))

                   (do (log/error (.getMessage Ex))
                       (cr/pst Ex)
                       (throw Ex))))))))))

(defn transact-all!
  [conn tx-data]

  (when (seq tx-data)

    (try @(d/transact conn
                      tx-data)

         (catch Exception Ex

           (let [error
                 (-> (ex-data Ex)
                     :db/error)

                 unavailable?
                 (= error
                    :db.error/transactor-unavailable)
                 
                 timeout?
                 (= error
                    :db.error/transaction-timeout)]

             (if (or timeout?
                     unavailable?)

               (let [error-string
                     (cond timeout? "timed out"
                           unavailable? "was unavailable")
                     
                     minutes 3]

                 (log/warnf "Transaction %s, retrying in %d minutes"
                            error-string
                            minutes)

                 (Thread/sleep (* minutes 60 1000))

                 @(d/transact conn
                              tx-data))

               (do (log/error (.getMessage Ex))
                   (cr/pst Ex)
                   (throw Ex))))))))
