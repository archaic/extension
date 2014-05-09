(ns extension.misc)

(defn ping!
  "Idempotent transformation on a reference, intended for watchers"
  [ref]
  (let [class (class ref)]
    (cond (= class clojure.lang.Atom)
          (swap! ref identity)
          (= class clojure.lang.Ref)
          (dosync (alter ref identity)))))

(defn pprn-msec
  "Aesthetically pleasing msec printout in minutes, seconds, millis."
  [msec]
  (cond (< msec 1000)
        (format "%6.6f msecs" (float msec))
        (< msec (* 60 1000))
        (format "%6.6f seconds" (float (/ msec 1000)))
        (< msec (* 60 60 1000))
        (format "%6.6f minutes" (float (/ (/ msec 1000) 60)))))

(defmacro timer
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [expr]
  `(let [start# (System/nanoTime)
         ret# ~expr
         elapsed-time# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (prn (str "Elapsed time: " (pprn-msec elapsed-time#)))
     ret#))

(defn keep-nonempty [m]
  (into {} (filter (fn [[_ v]] (not (nil? v))) m)))
