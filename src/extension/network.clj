(ns extension.network
  (:refer-clojure :exclude (get))
  (:require [clj-http.client :as cc]
            [clj-http.conn-mgr :as ccm]
            [clojure.pprint :as pp]
            [clj-time.coerce :as co]
            [clj-time.core :as t]
            [clojure.string :as s]
            [extension.fs :as fs]
            [hickory.core :as hc]
            [net.cgrand.enlive-html :as nce]
            [taoensso.nippy :as tn]
            [taoensso.timbre :as log])
  (:import [java.io ByteArrayInputStream File]
           [org.apache.commons.io FileUtils]))

(defn get
  [{:keys [url]}]

  (let [{:as response
         :keys [trace-redirects]}
        (try (cc/get url
                     {:throw-exceptions false})

             (catch Exception Ex
               (log/errorf "unable to GET %s, %s"
                           url
                           (.getMessage Ex))))]

    (when (seq trace-redirects)
      (log/warnf "redirect sequence from %s, by %s"
                 url
                 (s/join ", "
                         trace-redirects)))

    response))

(defn post
  [uri options]

  (try (cc/post uri
                options)

       (catch Exception Ex

         (let [message
               (ex-message Ex)]
           
           (log/errorf "Unable to post uri: %s, message:\n%s"
                       uri
                       message)

           (when-let [n-retries-0
                      (:n-retries options)]

             (let [n-retries
                   (int n-retries-0)]
               
               (when (pos? n-retries)

                 (log/infof "retry %d for %s"
                            n-retries
                            uri)
                 
                 (case n-retries
                   5 (Thread/sleep 1000)
                   4 (Thread/sleep 5000)
                   3 (Thread/sleep 10000)
                   2 (Thread/sleep 30000)
                   1 (Thread/sleep 90000)
                   (Thread/sleep 1000))

                 (post uri
                       (assoc options
                         :n-retries (dec n-retries))))))))))

(defn get-byte-array
  "Return body of url as a byte array, or nil if unable"
  ([url]
   (get-byte-array url
                   0))

  ([url ^long n-attempts]

   (let [request
         {:as :byte-array
          :connection-manager
          (ccm/make-socks-proxied-conn-manager "localhost"
                                               28435)

          :connection-timeout 90000

          :headers
          {"user-agent" (str "Mozilla/5.0 "
                             "(X11; Linux x86_64) "
                             "AppleWebKit/537.36 "
                             "(KHTML, like Gecko) "
                             "Chrome/83.0.4103.97 "
                             "Safari/537.36'")}

          :so-timeout 120000
          :throw-exceptions false}
         
         {:as response
          :keys [body
                 status
                 trace-redirects]}
         (try (cc/get url
                      request)

              (catch Exception Ex

                (let [message
                      (ex-message Ex)]

                  (if (re-find #"refused"
                               message)

                    (try (cc/get url
                                 (dissoc request
                                   :connection-manager))

                         (catch Exception Ex
                           (log/errorf "unable to GET %s, message: %s"
                                       url
                                       (ex-message Ex))))

                    (log/errorf "unable to GET %s, message: %s"
                                url
                                message)))))]

     (when (seq trace-redirects)
       (log/warnf "redirect sequence from %s"
                  url)
       (log/warn trace-redirects))

     (if status

       (cond (<= 200 status 299)
             (ByteArrayInputStream. body)

             (= 429 status)
             (case n-attempts
               0 (do (Thread/sleep (* 30 1000))
                     (get-byte-array url
                                     (inc n-attempts)))

               1 (do (Thread/sleep (* 120 1000))
                     (get-byte-array url
                                     (inc n-attempts)))

               2
               (log/errorf "unable to pull %s status: %d"
                           url
                           status))

             :else
             (log/errorf "unable to pull %s status: %d"
                         url
                         status))
       
       (case n-attempts
         0 (do (Thread/sleep (* 10 1000))
               (get-byte-array url
                               (inc n-attempts)))
         
         (log/errorf "unable to pull %s status: nil"
                     url))))))

(defn get-body
  ([url]
   (get-body url
             0))

  ([url ^long n-attempts]

   (let [request
         {:connection-timeout 60000

          #_:connection-manager
          #_(ccm/make-socks-proxied-conn-manager "localhost"
                                                 28435)

          :headers
          {"user-agent" (str "Mozilla/5.0 "
                             "(X11; Linux x86_64) "
                             "AppleWebKit/537.36 "
                             "(KHTML, like Gecko) "
                             "Chrome/83.0.4103.97 "
                             "Safari/537.36'")}

          :so-timeout 120000

          :throw-exceptions false}

         {:as response
          :keys [body
                 status
                 trace-redirects]}
         (try (cc/get url
                      request)

              (catch Exception Ex

                (let [message
                      (ex-message Ex)]
                  
                  (if (re-find #"refused"
                               message)

                    (try (cc/get url
                                 (dissoc request
                                   :connection-manager))

                         (catch Exception Ex
                           (log/errorf "unable to GET %s, message: %s"
                                       url
                                       (ex-message Ex))))
                    
                    (log/errorf "unable to GET %s, message: %s"
                                url
                                message)))))]

     (when (seq trace-redirects)
       (log/warnf "redirect sequence from %s"
                  url)
       (log/warn trace-redirects))
     
     (when status

       (cond (<= 200 status 299)
             body

             (= 429 status)
             (if (< n-attempts 10)

               (do (Thread/sleep (* n-attempts 30 1000))

                   (log/warnf "unable to pull %s, attempt: %d"
                              url
                              n-attempts)   

                   (get-body url
                             (inc n-attempts)))

               (log/errorf "unable to pull %s status: %d"
                           url
                           status))

             :else
             (log/errorf "unable to pull %s status: %d"
                         url
                         status))))))

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
      
      (.close ^ByteArrayInputStream bais)

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
      
      (.close ^ByteArrayInputStream bais)

      nodes)))

(defn local-tree
  [{:as input
    :keys [absolute-file-name
           directory
           file-name
           relative-file-name]}]

  (let [relative-file-name
        (or relative-file-name
            file-name)
        
        absolute-file-name
        (or absolute-file-name
            (str directory
                 relative-file-name))]

    (when (or (fs/file? absolute-file-name)
              (fs/symlink? absolute-file-name))

      (tn/thaw-from-file absolute-file-name))))

(defn cached-tree
  [{:as input
    :keys [directory
           relative-file-name
           file-name
           fresh?
           url
           valid?]
    :or {valid? (fn [tree]
                  true)}}]

  (let [relative-file-name
        (or relative-file-name
            file-name)

        absolute-file-name
        (str directory
             relative-file-name)]

    (when-not (fs/directory? directory)
      (fs/mkdirs directory))

    (let [file-exists?
          (or (fs/file? absolute-file-name)
              (fs/symlink? absolute-file-name))]

      (if (and file-exists?
               (not fresh?))
        (tn/thaw-from-file absolute-file-name)

        (when-let [tree
                   (html-tree url)]

          (when (valid? tree)
            
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

              (tn/freeze-to-file absolute-file-name-0
                                 tree)

              (fs/symlink absolute-file-name
                          absolute-file-name-0)

              tree)))))))

(defn http-get
  [{:as input
    :keys [directory
           file-name
           options
           url]}]

  (let [absolute-file-name
        (str directory
             file-name)]

    (when-not (fs/directory? directory)
      (fs/mkdirs directory))

    (if (or (fs/file? absolute-file-name)
            (fs/symlink? absolute-file-name))

      (tn/thaw-from-file absolute-file-name)

      (let [timestamp
            (co/to-long (t/now))

            file-name-0
            (str timestamp
                 "-"
                 file-name)

            absolute-file-name-0
            (str directory
                 file-name-0)

            {:as response
             :keys [body
                    status]}
            (cc/get url
                    (merge {:throw-exceptions false}
                           options))]

        (if (and status
                 (<= 200 status 299)
                 (not (s/blank? body)))
          
          (do (log/infof "pulling %s"
                         absolute-file-name)

              (tn/freeze-to-file absolute-file-name-0
                                 body)

              (fs/symlink absolute-file-name
                          absolute-file-name-0)

              body)

          (log/warnf "unable to get %s, status: %s, options: %s, response: %s"
                     url
                     status
                     options
                     response))))))

(defn get-user-agent
  []
  (str "Mozilla/5.0 "
       "(X11; Linux x86_64) "
       "AppleWebKit/537.36 "
       "(KHTML, like Gecko) "
       "Chrome/83.0.4103.97 "
       "Safari/537.36'"))

(defn get-byte-array-v2
  "Return body of url as a byte array, or nil if unable"

  [{:as input
    :keys [n-attempts
           url
           warn-on-redirect?]
    :or {n-attempts 0
         warn-on-redirect? true}}]

  (let [n-attempts
        (int n-attempts)

        request
        {:as :byte-array
         :connection-timeout 90000
         :headers {"user-agent" (get-user-agent)}
         :so-timeout 120000
         :throw-exceptions false}

        {:as response
         :keys [body
                status
                trace-redirects]}
        (try (cc/get url
                     request)

             (catch Exception Ex

               (let [message
                     (ex-message Ex)]

                 (log/errorf "unable to GET %s, message: %s"
                             url
                             message))))]

    (when (and warn-on-redirect?
               (seq trace-redirects))
      (log/warnf "redirect sequence from %s"
                 url)
      (log/warn trace-redirects))

    (if status

      (cond (<= 200 status 299)
            (ByteArrayInputStream. body)

            (= 429 status)
            (case n-attempts
              0 (do (Thread/sleep (* 30 1000))
                    (get-byte-array (assoc input
                                      :n-attempts (inc n-attempts))))

              1 (do (Thread/sleep (* 120 1000))
                    (get-byte-array (assoc input
                                      :n-attempts (inc n-attempts))))

              2
              (log/errorf "unable to pull %s status: %d"
                          url
                          status))

            :else
            (log/errorf "unable to pull %s status: %d"
                        url
                        status))
      
      (case n-attempts
        0 (do (Thread/sleep (* 10 1000))
              (get-byte-array (assoc input
                                :n-attempts (inc n-attempts))))
        
        (log/errorf "unable to pull %s status: nil"
                    url)))))

(defn get-cached-byte-array
  "Return body of url as a byte array, or nil if unable"

  [{:as input
    :keys [directory
           file-name
           n-attempts
           url
           warn-on-redirect?]
    :or {n-attempts 0
         warn-on-redirect? true}}]

  (let [n-attempts
        (int n-attempts)

        absolute-file-name
        (str directory
             file-name)]

    (when-not (fs/directory? directory)
      (fs/mkdirs directory))

    (if (or (fs/file? absolute-file-name)
            (fs/symlink? absolute-file-name))

      (FileUtils/readFileToByteArray (File. absolute-file-name))

      (let [request
            {:as :byte-array
             :connection-timeout 90000
             :headers {"user-agent" (get-user-agent)}
             :so-timeout 120000
             :throw-exceptions false}

            {:as response
             :keys [body
                    status
                    trace-redirects]}
            (try (cc/get url
                         request)

                 (catch Exception Ex

                   (let [message
                         (ex-message Ex)]

                     (log/errorf "unable to GET %s, message: %s"
                                 url
                                 message))))]

        (when (and warn-on-redirect?
                   (seq trace-redirects))
          (log/warnf "redirect sequence from %s"
                     url)
          (log/warn trace-redirects))

        (if status

          (cond (<= 200 status 299)

                (let [bais
                      (ByteArrayInputStream. body)]

                  (let [timestamp
                        (co/to-long (t/now))

                        relative-file-name-0
                        (str timestamp
                             "-"
                             file-name)

                        absolute-file-name-0
                        (str directory
                             relative-file-name-0)]

                    (log/infof "pulling %s"
                               absolute-file-name)

                    (FileUtils/copyInputStreamToFile bais
                                                     (File. absolute-file-name-0))

                    (fs/symlink absolute-file-name
                                absolute-file-name-0)

                    bais))

                (= 429 status)
                (case n-attempts
                  0 (do (Thread/sleep (* 30 1000))
                        (get-byte-array (assoc input
                                          :n-attempts (inc n-attempts))))

                  1 (do (Thread/sleep (* 120 1000))
                        (get-byte-array (assoc input
                                          :n-attempts (inc n-attempts))))

                  2
                  (log/errorf "unable to pull %s status: %d"
                              url
                              status))

                :else
                (log/errorf "unable to pull %s status: %d"
                            url
                            status))
          
          (case n-attempts
            0 (do (Thread/sleep (* 10 1000))
                  (get-byte-array (assoc input
                                    :n-attempts (inc n-attempts))))
            
            (log/errorf "unable to pull %s status: nil"
                        url)))))))
