(defproject com.eoneq/extension "0.3.13"

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [instaparse "1.4.8"]]
  
  :description "Extensions to Clojure"

  :global-vars {*warn-on-reflection* true}

  :injections [(require 'clojure.pprint)])
