(ns extension.probability)

(defn om?
  [om]
  (and (seq om)
       (every? (fn [[selection ^double odd]]
                 (and (number? odd)
                      (<= 1 odd)))
               om)))

(defn pm?
  [pm]

  (let [probabilities
        (vals pm)]

    (and (<= 2 (count probabilities))

         (every? (fn [probability]
                   (and (number? probability)
                        (< 0 probability 1)))
                 probabilities)

         (< (- 1 1e-6)
            (reduce +
                    probabilities)
            (+ 1 1e-6)))))

(defn bm?
  [om]
  (and (seq om)
       (every? (fn [[selection ^double bet-amount]]
                 (and (number? bet-amount)
                      (<= 0 bet-amount)))
               om)))

(defn pm->om
  [pm ^double take]
  (into {}
        (map (fn [[k ^double prob]]
               [k (float (/ (* prob take)))])
             pm)))

(defn get-take
  [om]
  (reduce +
          (map (comp float
                     /)
               (vals om))))

(defn om->pm
  ([om]
   (om->pm om
           (get-take om)))

  ([om ^double take]
   (into {}
         (map (fn [[k ^double odd]]
                [k (float (/ (* odd take)))])
              om))))
