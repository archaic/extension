(ns extension.time
  (:require [clojure.string :as s]
            [extension.string :as es]
            [instaparse.core :as ic]))

(def syntax
  (str ;; "parser = (date0 | date1);"
       "parser = (date0);"

       "<date0> = day <ws?> <separator> <ws?> month <ws?> <separator> <ws?> year;"
       ;; "<date1> = month <ws?> <separator> <ws?> day <ws?> <separator> <ws?> year;"
       
       "day = "
       "'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'10'|'11'|'12'|'13'|'14'|'15'|'16'"
       "|'17'|'18'|'19'|'20'|'21'|'22'|'23'|'24'|'25'|'26'|'27'|'28'|'29'|'30'|'31'"
       "|'01'|'02'|'03'|'04'|'05'|'06'|'07'|'08'|'09';"

       "month = '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'10'|'11'|'12'"
       "|'01'|'02'|'03'|'04'|'05'|'06'|'07'|'08'|'09'"
       "|'january'|'february'|'march'|'april'|'may'|'june'|'july'"
       "|'august'|'september'|'october'|'november'|'december'"
       "|'jan'|'feb'|'mar'|'apr'|'may'|'jun'|'jul'|'aug'|'sep'|'oct'|'nov'|'dec';"

       "year = ('20')?('01'|'02'|'03'|'04'|'05'|'06'|'07'|'08'|'09'|'10'|'11'|'12'"
       "|'13'|'14'|'15'|'16'|'17'|'18'|'19'|'20'|'21'|'22'|'23'|'24'|'25'|'26'|'27');"

       "separator = '/' | '-' | ',' | ws;"

       "ws = #'\\s+';"))

(defn month->value
  [month]
  (or (es/->i month)
      (case (-> month
                (subs 0 3)
                s/lower-case)
        "jan" 1
        "feb" 2
        "mar" 3 
        "apr" 4
        "may" 5
        "jun" 6
        "jul" 7
        "aug" 8
        "sep" 9
        "oct" 10
        "nov" 11
        "dec" 12)))

(def parser
  (ic/parser syntax
             :string-ci true))

(defn parse-date-string
  "within [2001 to 2027]"
  [s]

  (when (string? s)
    
    (let [data
          (parser s)]

      (when-not (ic/failure? data)

        (let [month-node
              (some (fn [[k v :as node]]
                      (when (= :month k)
                        node))
                    (rest data))

              day-node
              (some (fn [[k v :as node]]
                      (when (= :day k)
                        node))
                    (rest data))

              year-node
              (some (fn [[k v :as node]]
                      (when (= :year k)
                        node))
                    (rest data))
              
              day
              (-> day-node
                  last
                  es/->i)

              month
              (-> month-node
                  last
                  month->value)

              sub-year
              (-> year-node
                  last
                  es/->i)

              year
              (+ 2000
                 (int sub-year))]

          {:day day
           :month month
           :year year})))))

(defn parse-date-string+
  [s]
  (when (string? s)
    (or (parse-date-string s)
        (some parse-date-string
              (s/split s #"\s+")))))
