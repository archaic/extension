(ns extension.scalar
  (:refer-clojure :exclude (+ * -)))

(defn dispatch [x xs]
  (class xs))

(defmulti * dispatch)

(defmethod * clojure.lang.PersistentVector [x xs]
  (map #(clojure.core/* % x) xs))

(defmethod * clojure.lang.LazySeq [x xs]
  (map #(clojure.core/* % x) xs))

(defmethod * clojure.lang.Cons [x xs]
  (map #(clojure.core/* % x) xs))

(defmethod * clojure.lang.IPersistentMap [x xs]
  (into {} (map (fn [[k v]] [k (clojure.core/* v x)]) xs)))

(defmulti + dispatch)

(defmethod + clojure.lang.PersistentVector [x xs]
  (map #(clojure.core/+ % x) xs))

(defmethod + clojure.lang.PersistentList [x xs]
  (map #(clojure.core/+ % x) xs))

(defmethod + clojure.lang.LazySeq [x xs]
  (map #(clojure.core/+ % x) xs))

(defmethod + clojure.lang.Cons [x xs]
  (map #(clojure.core/+ % x) xs))

(defmethod + clojure.lang.IPersistentMap [x xs]
  (into {} (map (fn [[k v]] [k (clojure.core/+ v x)]) xs)))

(defmethod + clojure.lang.IPersistentSet [x xs]
  (set (map #(clojure.core/+ % x) xs)))

(defmulti - dispatch)

(defmethod - clojure.lang.PersistentVector [x xs]
  (map #(clojure.core/- % x) xs))

(defmethod - clojure.lang.LazySeq [x xs]
  (map #(clojure.core/- % x) xs))

(defmethod - clojure.lang.Cons [x xs]
  (map #(clojure.core/- % x) xs))

(defmethod - clojure.lang.IPersistentMap [x xs]
  (into {} (map (fn [[k v]] [k (clojure.core/- v x)]) xs)))

(defmethod - nil [x xs]
  nil)

(defmulti div dispatch)

(defmethod div clojure.lang.PersistentVector [x xs]
  (map #(/ % x) xs))

(defmethod div clojure.lang.PersistentList [x xs]
  (map #(/ % x) xs))

(defmethod div clojure.lang.LazySeq [x xs]
  (map #(/ % x) xs))

(defmethod div clojure.lang.Cons [x xs]
  (map #(/ % x) xs))

(defmethod div clojure.lang.IPersistentMap [x xs]
  (into {} (map (fn [[k v]] [k (/ v x)]) xs)))

(defmethod div clojure.lang.IPersistentSet [x xs]
  (set (map #(/ % x) xs)))
