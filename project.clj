(defproject extension "0.2.2"
  :dependencies [[com.datomic/datomic-free "0.9.4532"]
                 [org.clojure/clojure "1.6.0-alpha3"]]
  :description "Extensions to Clojure"
  :profiles {:dev {:dependencies [[midje "1.6.0"]]}})
