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
