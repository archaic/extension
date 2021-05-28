(ns extension.instaparse-v2
  (:require [clojure.string :as s]
            [instaparse.core :as ip]
            [instaparse.failure :as if]
            [taoensso.timbre :as log]))

(defn syntax->sting
  [syntax prefix]

  (reduce-kv (fn [s k v]

               (str s
                    (str prefix
                         (name k))
                    " = "

                    (cond (string? v)
                          (str "'" v "';\n")
                          
                          (instance? java.util.regex.Pattern v)
                          (str "#'" v "';\n")
                          
                          (vector? v)
                          (str "( '" (s/join "' | '" v) "' );\n")

                          (map? v)
                          (let [v1
                                (into {}
                                      (filter (fn [[k v]]
                                                (keyword? k))
                                              v))

                                nesting-string
                                (case (count v1)
                                  0 ""

                                  (str "( "
                                       (->> (keys v)
                                            (map (fn [kw]
                                                   (str prefix
                                                        (name k)
                                                        "_"
                                                        (name kw))))
                                            (s/join " | "))
                                       " );\n"
                                       (syntax->sting v1
                                                      (str prefix
                                                           (name k)
                                                           "_"))))

                                v2
                                (keep (fn [[k v]]
                                        (when (instance? java.util.regex.Pattern k)
                                          k))
                                      v)

                                regex-string
                                (case (count v2)
                                  0 ""

                                  1 (str "#'" (first v2) "';\n")

                                  (str "( "
                                       (s/join " | "
                                               (map (fn [r]
                                                      (str "#'" r "'"))
                                                    v2))
                                       ");\n"))]

                            (str nesting-string
                                 regex-string)))))

             ""
             syntax))

(defn unroll-parse
  [context parse-output]

  (reduce (fn [context output]

            (if (string? output)
              context
              
              (let [[k v] output]

                (if (string? v)

                  (let [ks
                        (s/split (name k)
                                 #"_")]

                    (assoc-in context
                              (mapv keyword
                                    ks)
                              v))
                  
                  (unroll-parse context
                                (vector v))))))

          context
          parse-output))

(defn create-parser
  [{:keys [string syntax]}]

  (let [string
        (str string
             (syntax->sting syntax
                            ""))

        parser
        (ip/parser string)]
    
    (fn [{:keys [text]}]

      (let [text-1
            (-> (s/lower-case text)
                (s/replace #"\s+" " ")
                s/trim)

            parsed-text
            (parser text-1)]

        (if (ip/failure? parsed-text)

          (do (log/warnf "unable to process text: %s"
                         text)
              
              (if/pprint-failure parsed-text))

          (unroll-parse {}
                        parsed-text))))))
