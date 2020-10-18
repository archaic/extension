(ns extension.ad)

(defmacro defparameters
  [& body]
  `(do ~@(->> body
              (partition-all 4)
              (map (fn [[name value]]
                     `(def ~name
                        ~value))))))
