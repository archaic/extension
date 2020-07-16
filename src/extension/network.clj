(ns extension.network
  (:require [clj-http.client :as cc]
            [clj-http.conn-mgr :as ccm]
            [clj-time.coerce :as co]
            [clj-time.core :as t]
            [extension.fs :as fs]
            [hickory.core :as hc]
            [net.cgrand.enlive-html :as nce]
            [taoensso.nippy :as tn]
            [taoensso.timbre :as log])
  (:import [java.io ByteArrayInputStream]))

(defn get-byte-array
  "Return body of url as a byte array, or nil if unable"
  [url]

  (let [{:as response
         :keys [body
                status
                trace-redirects]}
        (try (cc/get url
                     {:as :byte-array
                      :connection-timeout 60000
                      :socket-timeout 60000
                      :throw-exceptions false})

             (catch Exception Ex
               (log/errorf "unable to GET %s, message: %s"
                           url
                           (.getMessage Ex))))]

    (when (seq trace-redirects)
      (log/warnf "redirect sequence from %s"
                 url)
      (log/warn trace-redirects))
    
    (if (and status
             (<= 200 status 299))

      (ByteArrayInputStream. body)

      (log/errorf "unable to pull %s, status: %s"
                  url
                  status))))

(defn get-body
  ([url]
   (get-body url
             0))

  ([url n-attempts]

   (let [{:as response
          :keys [body
                 status
                 trace-redirects]}
         (try (cc/get url
                      {:connection-timeout 60000
                       :connection-manager (ccm/make-socks-proxied-conn-manager "localhost"
                                                                                28435)
                       :headers {"User-Agent"
                                 "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"}
                       :socket-timeout 60000
                       :throw-exceptions false})

              (catch Exception Ex
                (log/errorf "unable to GET %s, message: %s"
                            url
                            (.getMessage Ex))))]

     (when (seq trace-redirects)
       (log/warnf "redirect sequence from %s"
                  url)
       (log/warn trace-redirects))
     
     (if status

       (cond (<= 200 status 299)
             body

             (= 429 status)
             (case n-attempts
               0 (do (Thread/sleep (* 30 1000))
                     (get-body url
                               (inc n-attempts)))

               1 (do (Thread/sleep (* 120 1000))
                     (get-body url
                               (inc n-attempts)))

               2
               (log/errorf "unable to pull %s status: %d"
                           url
                           status))

             :else
             (log/errorf "unable to pull %s status: %d"
                         url
                         status))

       (log/errorf "unable to pull %s status: nil"
                   url)))))

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

(defn html-tree
  [url]

  (some-> url
          get-body
          hc/parse
          hc/as-hickory))

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

(defn local-tree
  [{:as input
    :keys [directory
           relative-file-name]}]

  (let [absolute-file-name
        (str directory
             relative-file-name)]

    (when (or (fs/file? absolute-file-name)
              (fs/symlink? absolute-file-name))

      (tn/thaw-from-file absolute-file-name))))

(defn cached-tree
  [{:as input
    :keys [directory
           relative-file-name
           url]}]

  (let [absolute-file-name
        (str directory
             relative-file-name)]
    
    (if (or (fs/file? absolute-file-name)
            (fs/symlink? absolute-file-name))

      (tn/thaw-from-file absolute-file-name)

      (when-let [tree
                 (html-tree url)]

        (let [timestamp
              (co/to-long (t/now))

              relative-file-name-0
              (str timestamp
                   "-"
                   relative-file-name)

              absolute-file-name-0
              (str directory
                   relative-file-name-0)]

          (log/infof "pulling %s"
                     absolute-file-name)

          (when-not (fs/directory? directory)
            (fs/mkdir directory))

          (tn/freeze-to-file absolute-file-name-0
                             tree)

          (fs/symlink absolute-file-name
                      absolute-file-name-0)

          tree)))))
