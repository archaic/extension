(ns extension.instaparse
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

                          (if (== 1 (count v))

                            ;; regex with conversion
                            (str "#'" (ffirst v) "';\n")

                            (str "( "
                                 (->> (keys v)
                                      (map (fn [kw]
                                             (str prefix
                                                  (name k)
                                                  "_"
                                                  (name kw))))
                                      (s/join " | "))
                                 " );\n"
                                 (syntax->sting v
                                                (str prefix
                                                     (name k)
                                                     "_")))))))

             ""
             syntax))

(defn unroll-parse
  [context parse-output]

  (reduce (fn [context [k v]]

            (if (string? v)

              (let [ks
                    (s/split (name k)
                             #"_")]

                (assoc-in context
                          (mapv keyword
                                ks)
                          v))
              
              (unroll-parse context
                            (vector v))))

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
