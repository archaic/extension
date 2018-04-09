(ns extension.seq)

(defn alternates
  "Split coll into 'threads' subsequences (defaults to 2), feeding
  each alternately from the input sequence. Effectively the inverse of
  interleave:
  (alternates 3 (range 9))
  ;=> ((0 3 6) (1 4 7) (2 5 8))"
  ([coll] (alternates 2 coll))
  ([threads coll]
   (lazy-seq
    (when (seq coll)
      (apply map list (partition threads coll))))))

(defmacro lazy-loop
  "Provide a simplified version of lazy-seq to eliminate
  boilerplate. Arguments are as to the built-in (loop...recur),
  and (lazy-recur) will be defined for you. However, instead of doing
  actual tail recursion, lazy-recur trampolines through lazy-seq. In
  addition to enabling laziness, this means you can call lazy-recur
  when not in the tail position.
  Regular recurs are also supported, if they are in tail position and don't
  need any laziness."
  [bindings & body]
  (let [f 'lazy-recur
        [names values] (alternates bindings)
        blob-names (repeatedly (count names) gensym)]
    `(letfn [(~f [~@blob-names]
              (lazy-seq
               (iter# ~@blob-names)))
             (iter# [~@names]
               ~@body)]
       (~f ~@values))))

(defn glue
  "Walk over an input sequence, \"gluing\" together elements to create batches.
   Batches may be of any type you like, and are computed as follows:
   - Each batch is initialized by combining init (default false) with next-item.
   - For each additional item in coll, functions glue? and unglue? are consulted to
     decide whether the next item should be included into the current batch.
     - If (glue? current-batch next-item) returns truthy, then a prospective
       updated-batch is computed, as (combine current-batch next-item). If
       (unglue? updated-batch) returns falsey, then updated-batch is accepted and
       may be used as the target for further gluing.
     - If glue? returned falsey, or unglue? returned truthy, then the current batch
       is inserted into the output sequence, and a new batch is started as
       (combine init next-item)."
  ([combine glue? coll]
   (glue combine nil glue? coll))
  ([combine init glue? coll]
   (glue combine init glue? (constantly false) coll))
  ([combine init glue? unglue? coll]
   (lazy-seq
    (when-let [coll (seq coll)]
      (lazy-loop [glob (combine init (first coll)), coll (rest coll)]
                 (if-let [coll (seq coll)]
                   (let [x (first coll)
                         more (rest coll)
                         glued (delay (combine glob x))]
                     (if (and (glue? glob x)
                              (not (unglue? @glued)))
                       (recur @glued more)
                       (cons glob (lazy-recur (combine init x) more))))
                   (list glob)))))))

(defn partition-between
  "Partition an input seq into multiple sequences, as with partition-by.
   Walks the collection two at a time, calling (split? [a b]) for each pair.
   Any time split? returns truthy, the partition containing a ends, and a new
   one containing b begins. Note that the split? predicate should not take two
   arguments, but instead a single argument, a pair.
   Like partition-by, a lazy sequence of partitions is returned, but the
   partitions themselves are eager.
   For example, to cause each nil to be folded into the next partition:
   (partition-between (fn [[a b]] (not (nil? a))) '[1 nil nil 2 nil 3])
   => ([1] [nil nil 2] [nil 3])"
  [split? coll]
  (glue conj []
        (fn [v x]
          (not (split? [(peek v) x])))
        (constantly false)
        coll))

(defn some+
  [pred coll coll-1]
  (when (and (seq coll)
             (seq coll-1))

    (or (pred (first coll)
              (first coll-1))
        (recur pred
               (next coll)
               (next coll-1)))))
