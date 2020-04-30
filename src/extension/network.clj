(ns extension.network
  (:require [clj-http.client :as cc]
            [net.cgrand.enlive-html :as nce]
            [taoensso.timbre :as log])
  (:import [java.io ByteArrayInputStream]))

(defn get-byte-array
  "Return body of url as a byte array, or nil if unable"
  [url]

  (let [{:as response
         :keys [body
                status]}
        (try (cc/get url
                     {:as :byte-array
                      :connection-timeout 60000
                      :socket-timeout 60000
                      :throw-exceptions false})

             (catch Exception Ex
               (log/errorf "unable to GET %s, message: %s"
                           url
                           (.getMessage Ex))))]

    (if (and status
             (<= 200 status 299))

      (ByteArrayInputStream. body)

      (log/errorf "unable to pull %s, status: %s"
                  url
                  status))))

(defn html
  [url]

  (when-let [bais
             (get-byte-array url)]

    (let [nodes
          (try (nce/html-resource bais)
               (catch Exception Ex
                 (log/errorf "unable to process html, message: %s, url: %s"
                             (.getMessage Ex)
                             url)))]
      
      (.close bais)

      nodes)))

(defn xml
  [url]

  (when-let [bais
             (get-byte-array url)]

    (let [nodes
          (try (nce/xml-resource bais)
               (catch Exception Ex
                 (log/errorf "unable to process xml, message: %s, url: %s"
                             (.getMessage Ex)
                             url)))]
      
      (.close bais)

      nodes)))
