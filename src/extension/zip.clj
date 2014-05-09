(ns extension.zip
  (:refer-clojure :exclude (last next find filter keep map))
  (:require (clojure (zip :refer (up down left right next prev node end? edit
                                     leftmost rights lefts root)))))

(defn root?
  [loc]
  (nil? (up loc)))

(defn children? [loc]
  (seq (down loc)))

(defn root-loc [loc]
  (if (root? loc)
    loc
    (recur (up loc))))

(defn last
  "Returns the loc of the node immediately preceding the end node"
  [loc]
  (loop [l loc]
    (let [n (next l)]
      (if (end? n) l (recur n)))))

(defn prewalk
  [loc f & args]
  (loop [l loc]
    (if (end? l)
      (root-loc l)
      (recur (next (apply f l args))))))

(defn node-prewalk
  "Prewalk traversal over loc, applying f to each node (with arguments)"
  [loc f & args]
  (prewalk loc (fn [loc] (apply edit loc f args))))

(defn prewalk!
  "Preorder traversal of a loc structure for side effects"
  [loc f & args]
  (loop [l loc]
    (when-not (end? l)
      (apply f l args)
      (recur (next l)))))

(defn postwalk!
  "Postorder traversal of a loc structure for side effects"
  [loc f & args]
  (let [last (last loc)]
    (loop [l last]
      (do (apply f l args)
          (if (not (root? l))
            (recur (prev l)))))))

(defn postwalk
  "f is a function that must take one+ argument, a loc then return it
   (potentially modifying the node)"
  [loc f & args]
  (let [last (last loc)]
    (loop [l last]
      (if (root? l)
        (apply f l args)
        (recur (prev (apply f l args)))))))

(defn fmap
  [f loc]
  (loop [result [] loc loc]
    (if (end? loc)
      result
      (recur (conj result (f loc)) (next loc)))))

(defn map-children
  [f loc]
  (if-let [first-child (down loc)]
    (loop [result [] loc first-child]
      (if loc
        (recur (conj result (f loc)) (right loc))
        result))))

(defn map-children-args
  [f loc coll]
  (if-let [first-child (down loc)]
    (loop [result []
           loc first-child
           coll coll]
      (if loc
        (recur (conj result (f loc (first coll)))
               (right loc)
               (rest coll))
        result))))

(defn find
  "Depth first search for loc that satisfies (pred node), or nil."
  [pred loc]
  (loop [loc loc]
    (when-not (end? loc)
      (if (pred (node loc))
        loc
        (recur (next loc))))))

(defn find-parent
  "Ancestor traversal for loc that satisfies (pred node), or nil"
  [pred loc]
  (loop [loc loc]
    (when-let [parent (up loc)]
      (if (pred (node parent))
        parent
        (recur (up loc))))))

(defn filter
  "A sequence of all locs that satisfy pred"
  [pred loc]
  (loop [l loc results []]
    (if (end? l)
      results
      (recur (next l) (if (pred l) (conj results l) results)))))

(defn keep
  "A sequence of all non-nil results of (f loc)"
  [f loc]
  (loop [l loc results []]
    (if (end? l)
      results
      (recur (next l) (if-let [r (f l)] (conj results r) results)))))

(defn map
  [f loc]
  (loop [l loc]
    (if (end? l)
      (root l)
      (recur (next (edit l f))))))

(defn smap
  "Maps over the siblings (inclusive) of loc"
  [f loc]
  (loop [l (leftmost loc)
         results []]
    (if l
      (recur (right l) (conj results (f (node l))))
      results)))

(defn singular?
  "true if the loc has no siblings"
  [loc]
  (== (+ (count (rights loc)) (count (lefts loc))) 0))
