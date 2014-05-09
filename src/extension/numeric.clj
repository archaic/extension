(ns extension.numeric)

(defn chain?
  "A chain is a sequence of consecutive integers, returns true if
   xs has a chain of length n (or longer)"
  [xs n]
  (loop [last-integer nil
         chain-length 0
         xs xs]
    (let [i (first xs)
          xs (rest xs)]
      (if (< chain-length n)
        (when i
          (recur i
                 (if (= (dec i) last-integer)
                   (inc chain-length)
                   1)
                 xs))
        true))))