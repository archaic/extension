(ns extension.math)

(defn mean
  [xs]
  (let [n (count xs)]
    (if (pos? n)
      (float (/ (reduce + xs)
                n))
      0.0)))

(defn sd
  ([xs]
   (sd xs
       (mean xs)))
  
  ([xs mean]
   (let [n (count xs)]

     (if (pos? n)
       
       (Math/sqrt (/ (reduce +
                             (map (fn [x]
                                    (Math/pow (- x mean)
                                              2))
                                  xs))
                     n))
       0.0))))

(defn statistics
  [xs ]

  (let [n
        (count xs)]

    (when (< 0 n)

      {:mean
       :variance
       :n n})))

(defn ceil-50
  "Rounds amount down to the nearest 50 cents"
  [x]
  (let [characteristic
        (int x)
        
        mantissa
        (- x characteristic)
        
        eps 1e-6]

    (cond (< 0 mantissa eps)
          characteristic
          
          (< 0 mantissa
             (+ 0.5 eps))
          (+ characteristic 0.5)
          
          :else (Math/ceil x))))

(defn ceil
  [number]
  (Math/ceil number))

(defn floor-10
  "Rounds amount down to the nearest 10 cents"
  [x]
  (/ (Math/floor (* 10
                    x))
     10))

(defn roughly?
  [x y tol]
  (let [^Double difference
        (- x y)]
    (< (Math/abs difference)
       tol)))

(defn median
  [coll]
  (when (seq coll)
    (let [sorted (sort coll)
          cnt (count sorted)
          halfway (quot cnt 2)]
      (if (odd? cnt)
        (nth sorted halfway)
        (let [bottom (dec halfway)
              bottom-val (nth sorted bottom)
              top-val (nth sorted halfway)]
          (mean [bottom-val top-val])))))) 

(defn modes
  "return set of elements with highest mode"
  [xs]

  (let [data
        (frequencies xs)

        mode-value
        (apply max
               (vals data))]

    (set (keep (fn [[k v]]
                 (when (= v mode-value)
                   k))
               data))))


(defn interquartile-range
  [coll]
  (when (seq coll)
    (let [sorted (sort coll)
          cnt (count sorted)
          halfway (quot cnt 2)
          q1 (median (take halfway sorted))
          q3 (median (take-last halfway sorted))]

      {:q1 q1
       :q3 q3})))

(defn skewness
  [xs]

  (let [m
        (mean xs)

        s
        (sd xs)

        n
        (count xs)

        numerator
        (reduce + (map (fn [x]
                         (Math/pow (- x m)
                                   3))
                       xs))

        denominator
        (* (- n 1)
           (Math/pow s 3))]

    (/ numerator
       denominator)))

(defn kurtosis
  [xs]

  (let [m
        (mean xs)

        s
        (sd xs)

        n
        (count xs)
        
        m4
        (/ (reduce +
                   (map (fn [x]
                          (Math/pow (- x m) 4))
                        xs))
           n)]
    (when (< 0 s)
      (/ m4
         (Math/pow s 4)))))


(defn jarque-bera
  [xs]

  (let [n
        (count xs)

        s
        (skewness xs)

        k
        (kurtosis xs)]

    (* (/ n 6)
       (+ (Math/pow s 2)
          (* 0.25
             (Math/pow (- k 3)
                       2))))))

(defn skew
  [xs]
  (let [m (mean xs)
        med (median xs)
        s (sd xs)]
    (when (and m med s
               (< 0 s))
      (/ (- m med)
         s))))

