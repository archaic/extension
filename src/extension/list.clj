(ns extension.list)

(defn some-list
  [& elements]
  (keep identity
        (apply list
               elements)))
