(ns extension.pathom
  (:require
   [clojure.string :as s]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]))

(defn flatten-query
  [xs query]
  
  (cond (vector? query)
        (mapcat (fn [sub-query]
                  (flatten-query xs
                                 sub-query)) 
                query)
        
        (map? query)
        (mapcat (fn [[kw sub-query]]
                  (conj (flatten-query xs
                                       sub-query)
                        kw)) 
                query)
        
        (keyword? query)
        (conj xs
              query)))

(defn get-ns-alias-resolvers
  [{:keys [re-xs query]}]

  (let [kws
        (flatten-query []
                       query)]

    (keep (fn [kw]

            (when-let [ns-string
                       (namespace kw)]

              (some (fn [re]

                      (when (re-find re
                                     ns-string)

                            (let [kw-name
                                  (name kw)

                                  ns-string-1
                                  (s/replace ns-string
                                             re
                                             "")

                                  kw-1
                                  (if (s/blank? ns-string-1)
                                    (keyword kw-name)
                                    (keyword ns-string-1
                                             kw-name))]

                              (pbir/equivalence-resolver kw kw-1))))
                    re-xs)))
          kws)))
