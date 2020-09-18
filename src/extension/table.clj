(ns extension.table
  (:require [clojure.string :as s]
            [extension.ansi :as ansi]))

(defn print-table
  ([ks rows]
   (when (seq rows)
     
     (let [widths
           (map (fn [k]
                  (apply max
                         (+ 2 (count (str k)))
                         (map #(+ 3 (count (str (get % k))))
                              rows)))
                ks)
           
           fmts
           (map #(str "%" % "s")
                widths)

           fmt-header
           (fn [row]
             (s/join " "
                     (map (fn [k fmt]

                            (let [data
                                  (get row k)

                                  data
                                  (if (keyword? data)
                                    (name data)
                                    data)]

                              (ansi/sgr (format fmt
                                                (str " "
                                                     (s/capitalize data)
                                                     " "))
                                        :bg-blue
                                        :white
                                        :bold)))
                          ks
                          fmts)))
           
           fmt-row
           (fn [row]
             (s/join " "
                     (map (fn [k fmt]
                            (ansi/sgr (format fmt
                                              (get row k))
                                      :blue))
                          ks
                          fmts)))]
       
       (println (fmt-header (zipmap ks ks)))
       
       (doseq [row rows]
         (println (fmt-row row))))))
  
  ([rows]
   (print-table (keys (first rows))
                rows)))

(defn print-basic-table
  ([ks rows]
   (when (seq rows)
     
     (let [widths
           (map (fn [k]
                  (apply max
                         (+ 2 (count (str k)))
                         (map #(+ 3 (count (str (get % k))))
                              rows)))
                ks)
           
           fmts
           (map #(str "%" % "s")
                widths)

           fmt-header
           (fn [row]
             (s/join " "
                     (map (fn [k fmt]

                            (let [data
                                  (get row k)

                                  data
                                  (if (keyword? data)
                                    (name data)
                                    data)]

                              (format fmt
                                      (str " "
                                           (s/capitalize data)
                                           " "))))
                          ks
                          fmts)))
           
           fmt-row
           (fn [row]
             (s/join " "
                     (map (fn [k fmt]
                            (format fmt
                                    (get row k)))
                          ks
                          fmts)))]
       
       (println (fmt-header (zipmap ks ks)))
       
       (doseq [row rows]
         (println (fmt-row row))))))
  
  ([rows]
   (print-table (keys (first rows))
                rows)))
