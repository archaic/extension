(ns extension.string
  (:require [clojure.string :as s]
            [taoensso.timbre :as log])
  (:import [java.text Normalizer Normalizer$Form NumberFormat]
           [java.util Locale]))

(defn negative?
  [s]
  (= (first s) \-))

(defn ->f
  [s]
  (when (string? s)
    (when-let [s (re-find #"\d*\.\d+|\d+" s)]
      (Float/parseFloat s))))

(defn ->d
  [s]
  (when (string? s)
    (when-let [s (re-find #"\d*\.\d+|\d+" s)]
      (Double/parseDouble s))))

(defn ->i
  [s]
  (when (string? s)
    (when-let [s (re-find #"\d+" s)]
      (Integer/parseInt s))))

(defn parse-vulgar-fraction
  [s]
  (let [[numerator denominator]
        (-> s
            (Normalizer/normalize Normalizer$Form/NFKD)
            (s/split #"\u2044"))]
    (/ (Integer/parseInt numerator)
       (Integer/parseInt denominator))))

(def re-vulgar-fraction
  #"[\u2150-\u215F]|[\u00BC-\u00BE]")

(defn parse-currency
  [text]

  (let [text-0
        (s/trim text)]
    
    (cond (s/blank? text-0)
          (log/warnf "currency text: %s"
                     text)

          (= \$ (first text-0))
          (.parse (NumberFormat/getCurrencyInstance (Locale/US))
                  text-0)

          :else
          (.parse (NumberFormat/getCurrencyInstance (Locale/US))
                  (str "$"
                       text-0)))))
