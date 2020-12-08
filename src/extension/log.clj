(ns extension.log
  (:require [clojure.string :as s]
            [extension.ansi :as ea]
            [taoensso.timbre :as log])
  (:import [java.util TimeZone]))

(defn default-output-fn
  ([data]
   (default-output-fn nil
                      data))

  ([opts data]
   (let [{:keys [no-stacktrace? stacktrace-fonts]} opts
         {:keys [level
                 ?err
                 msg_
                 ?ns-str
                 ?file
                 hostname_
                 timestamp_
                 ?line]} data

         level-1
         (s/upper-case (name level))

         color-level
         (case level-1
           "WARN" (ea/sgr "WARN"
                          :yellow
                          :bold)
           "ERROR" (ea/sgr "ERROR"
                           :red
                           :bold)
           level-1)]

     (str (force timestamp_)
          " "
          (force hostname_)
          " "
          color-level
          " "
          "[" (or ?ns-str ?file "?") ":" (or ?line "?") "] - "
          (force msg_)
          (when-not no-stacktrace?
            (when-let [err ?err]
              (str "\n"
                   (log/stacktrace err
                                   opts))))))))

(defn set-log-println-color!
  []
  (log/set-config! {:level :debug

                    :ns-whitelist []

                    :ns-blacklist []

                    :middleware []

                    :timestamp-opts {:pattern "yy-MM-dd HH:mm:ss"
                                     :locale :jvm-default
                                     :timezone (TimeZone/getTimeZone "Australia/Sydney")}

                    :output-fn default-output-fn

                    :appenders {:println (log/println-appender {:stream :auto})}}))
