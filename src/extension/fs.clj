(ns extension.fs
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.string :as s])
  (:import [java.io FileInputStream]
           [java.net URL URI]
           [java.nio.file Files LinkOption Path Paths CopyOption]
           [java.nio.file.attribute FileAttribute]))

(extend Path
  io/IOFactory
  (assoc io/default-streams-impl
    :make-input-stream
    (fn [^Path x opts]
      (io/make-input-stream (FileInputStream. (.toString x))
                            opts))))

(defn ->path
  [x]
  (cond (instance? Path x) x
        (and (string? x)
             (re-find #"(?i)^file:"
                      x))
        (->path (URL. x))
        (string? x) (Paths/get x (into-array String []))
        (instance? URI x) (Paths/get x)
        (instance? URL x) (Paths/get (.toURI ^URL x))))

(defn file?
  [path]
  (Files/isRegularFile (->path path)
                       (LinkOption/values)))

(defn directory?
  [path]
  (Files/isDirectory (->path path)
                     (LinkOption/values)))

(defn children
  [path]
  (iterator-seq (.iterator (Files/newDirectoryStream (->path path)))))

(defn path-zip
  [root]
  (zip/zipper directory?
              children
              nil
              root))

(defn directories
  [path]
  (filter directory?
          (children path)))

(defn mkdir
  [^Path path]
  (Files/createDirectory (->path path)
                         (into-array FileAttribute [])))

(defn mkdirs
  [^Path path]
  (Files/createDirectories (->path path)
                           (into-array FileAttribute [])))

(defn cp
  [^Path from ^Path to]
  (Files/copy from
              to
              #^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [])))

(defn files
  "A sequence of paths within path which are regular files, path must
  be a path to a directory"
  [path]
  (let [children
        (children path)]
    (filter file?
            children)))

(defn rm
  [path]
  (when (file? path)
    (Files/delete path)))

(defn mv
  [from to]
  (Files/move (->path from)
              (->path to)
              #^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [])))

(defn create-tmp
  [prefix]
  (Files/createTempDirectory prefix
                             (into-array FileAttribute
                                         [])))

(defn symlink?
  [path]
  (Files/isSymbolicLink (->path path)))

(defn symlink
  [link target]

  (let [link-path
        (->path link)

        target-path
        (->path target)]

    (when (symlink? link-path)
      (Files/deleteIfExists link-path))

    (Files/createSymbolicLink link-path
                              target-path
                              (into-array FileAttribute
                                          []))))

(defn size-in-bytes
  [path]
  (Files/size path))

(defn to-bytes
  [path]
  (Files/readAllBytes (->path path)))

(defn alter-extension!
  [{:file.extension/keys [src dst]
    :file.absolute/keys [path]}]

  (mv path
      (s/replace path
                 (re-pattern (str src "$"))
                 dst)))

(defn file-iteration
  "iterate over all files in a directory, calling f on path"
  [{:directory.absolute/keys [path]
    :keys [f]}]

  (loop [loc 
         (path-zip path)]
    
    (when-not (zip/end? loc)

      (let [path
            (zip/node loc)]
        
        (when (file? path)
          (f path)))
      
      (recur (zip/next loc)))))

(defn alter-extensions!
  [{:as context
    :file.extension/keys [src dst]
    :directory.absolute/keys [path]}]

  (file-iteration (assoc context
                    :f (fn [path]
                         (alter-extension! {:file.extension/src src
                                            :file.extension/dst dst
                                            :file.absolute/path path})))))

(defn get-parent
  [path]
  (.getParent (->path path)))

(defn get-file-name
  [path]
  (.getFileName (->path path)))

#_(alter-extensions! {:file.extension/src "nippy"
                      :file.extension/dst "ny"
                      :directory.absolute/path "/mnt/hdd/betfair_greyhound_GB_data"})

#_(alter-extensions! {:file.extension/src "_0.ny"
                      :file.extension/dst ".ny"
                      :directory.absolute/path "/mnt/hdd/betfair_greyhound_GB_data/2020"})
