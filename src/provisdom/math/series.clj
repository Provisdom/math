(ns provisdom.math.series
  (:require [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]
             [combinatorics :as cm]
             [calculus :as ca]
             [matrix :as mx]]
            [taoensso.truss :as truss :refer (have have! have?)]))

(set! *warn-on-reflection* true)

;;;DECLARATIONS
(declare polynomial-fn)

;;;CONSTANTS
(def ^:const ^:private chebyshev-poly-first-kind-fns 
  [(fn [x] 1.0), #(* 1 %), #(dec (* 2.0 (m/sq %))), 
   #(+ (* 4 (m/cube %)) (* -3 %)),  #(+ (* 8 (m/pow % 4)) (* -8 (m/sq %)) 1), 
   #(+ (* 16 (m/pow % 5)) (* -20 (m/cube %)) (* 5 %)), 
   #(+ (* 32 (m/pow % 6)) (* -48 (m/pow % 4)) (* 18 (m/sq %)) -1), 
   #(+ (* 64 (m/pow % 7)) (* -112 (m/pow % 5)) (* 56 (m/cube %)) (* -7 %)), 
   #(+ (* 128 (m/pow % 8)) (* -256 (m/pow % 6)) (* 160 (m/pow % 4)) 
       (* -32 (m/sq %)) 1), 
   #(+ (* 256 (m/pow % 9)) (* -576 (m/pow % 7)) (* 432 (m/pow % 5)) 
       (* -120 (m/cube %)) (* 9 %)),
   #(+ (* 512 (m/pow % 10)) (* -1280 (m/pow % 8)) (* 1120 (m/pow % 6)) 
       (* -400 (m/pow % 4)) (* 50 (m/sq %)) -1),
   #(+ (* 1024 (m/pow % 11)) (* -2816 (m/pow % 9)) (* 2816 (m/pow % 7)) 
       (* -1232 (m/pow % 5)) (* 220 (m/cube %)) (* -11 %))])

(def ^:const ^:private chebyshev-poly-second-kind-fns 
  [(fn [x] 1.0), #(* 2 %), #(dec (* 4.0 (m/sq %))), 
   #(+ (* 8 (m/cube %)) (* -4 %)),  #(+ (* 16 (m/pow % 4)) (* -12 (m/sq %)) 1),
   #(+ (* 32 (m/pow % 5)) (* -32 (m/cube %)) (* 6 %)), 
   #(+ (* 64 (m/pow % 6)) (* -80 (m/pow % 4)) (* 24 (m/sq %)) -1), 
   #(+ (* 128 (m/pow % 7)) (* -192 (m/pow % 5)) (* 80 (m/cube %)) (* -8 %)), 
   #(+ (* 256 (m/pow % 8)) (* -448 (m/pow % 6)) (* 240 (m/pow % 4)) 
       (* -40 (m/sq %)) 1),
   #(+ (* 512 (m/pow % 9)) (* -1024 (m/pow % 7)) (* 672 (m/pow % 5)) 
       (* -160 (m/cube %)) (* 10 %))])

;;;CHEBYSHEV POLYNOMIALS
(defn chebyshev-polynomial-fn
  "Returns a chebyshev polynomial function.  
Can optionally use first kind (default) or second kind."
  [n & second-kind?]
  {:pre [(have? m/long-able-non-? n)]}
  (let [n (long n), g #(let [[x y] %, z (- (* 2 y) x)] [y z]), 
        c (if second-kind? chebyshev-poly-second-kind-fns 
            chebyshev-poly-first-kind-fns), 
        m (if second-kind? 9 11)] 
    (if (<= n m) (c n)
      #(second (last (take (inc (- n m)) 
                           (iterate g [((c (dec m)) %) ((c m) %)])))))))

;;http://en.wikipedia.org/wiki/Chebyshev_polynomials 
;;-- also solved for the deriv of second-kind at x = +-1
(defn chebyshev-derivative-fn 
  "Returns a chebyshev-derivative function.  
Can optionally use first kind (default) or second kind.
Will use numerical derivative when necessary."
  [n deriv & second-kind?]
  {:pre [(have? m/long-able-non-? n) (have? m/long-able? deriv) (pos? deriv)]}
  (let [n (long n)] 
    (cond (zero? n) (fn [x] 0.0)
          (m/one? deriv) (if second-kind? 
                           (let [y (inc n)] 
                             #(if (m/one? (m/abs %)) 
                                (* (/ 3) (m/pow (m/sgn %) y) 
                                   (- (m/cube y) y))
                                (/ (- (* y ((chebyshev-polynomial-fn y) %)) 
                                      (* % ((chebyshev-polynomial-fn 
                                              n true) %))) 
                                   (dec (m/sq %)))))
                           #(* n ((chebyshev-polynomial-fn (dec n) true) %)))
          (and (= 2 deriv) 
               (not second-kind?)) #(when (m/one? (m/abs %)) 
                                      (* (/ 3) (m/pow (m/sgn %) n) 
                                         (- (m/pow n 4) (m/sq n))))
          :else (ca/derivative-fn (chebyshev-polynomial-fn n second-kind?) 
                                  :derivative deriv))))

(defn chebyshev-polynomial-factors-to-regular-polynomial-factors
  "Returns polynomial factors a (i.e., a0 + a1 * x + a2 * x^2 +...) 
   from chebyshev factors (i.e., b0 + b1 * x + b2 * (2x^2 - 1) + ...)
Can optionally use first kind (default) or second kind."
  [chebyshev-factors & second-kind?]
  (let [n (count chebyshev-factors)]
    (map (fn [i] 
           ((ca/derivative-fn 
              #(mx/inner-product 
                 chebyshev-factors 
                 ((polynomial-fn 
                    (dec n) :chebyshev-kind (if second-kind? 2 1)) %)) 
              :derivative i) 0.0)) 
         (range n))))

;;;SERIES
(defn- polynomial-functions
  "Cheybshev-kind can be 0 (default), 1, or 2, 
   where 0 means a regular polynomial."
  [chebyshev-kind] 
  (condp = chebyshev-kind
    0 (fn [x] #(m/pow x %))
    1 (fn [x] #((chebyshev-polynomial-fn %) x))
    2 (fn [x] #((chebyshev-polynomial-fn % true) x))
    (throw (ex-info (format "Chebyshev-kind %i doesn't exist" chebyshev-kind) ; TODO - truss or assert
                    {:fn (var polynomial-functions)}))))

(defn- polynomial-2D-degrees ^double [count] 
  (- (m/sqrt (+ 0.25 (* 2 count))) 1.5)) 

(defn polynomial-fn
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular 
   polynomial.
   Returns a function that takes a number and returns a vector"
  [end-degree & {:keys [start-degree chebyshev-kind] 
                 :or {start-degree 0, chebyshev-kind 0}}]
    (fn [x] (map (fn [i] (((polynomial-functions chebyshev-kind) x) i)) 
                 (range start-degree (inc end-degree)))))

(defn polynomial-fns
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular 
   polynomial.
Returns a collection of functions that each take a number and return a 
   number"
  [end-degree & {:keys [start-degree chebyshev-kind] 
                 :or {start-degree 0, chebyshev-kind 0}}]
    (map (fn [i] (fn [x] (((polynomial-functions chebyshev-kind) x) i))) 
         (range start-degree (inc end-degree))))

(defn polynomial-2D-count
  (^long [^long end-degree] (polynomial-2D-count 0 end-degree))
  (^long [^long start-degree ^long end-degree]
   {:pre [(have? m/non-? start-degree) (have? m/non-? end-degree)]}
    (let [d (inc end-degree), f #(* 0.5 (+ % (m/sq %)))]
      (- (f d) (f start-degree)))))

(defn polynomial-2D-fn
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular 
   polynomial.
Order retains x to the highest powers first, e.g., 
   [1 x y x^2 xy y^2 x^3 (x^2 * y) (y^2 * x) y^3].
If by-count? (default is false), then returns basis with basis of 
   'end-degree' count.
Returns a function that takes a tuple [x y] and returns a vector."
  [end-degree & {:keys [by-count? start-degree chebyshev-kind] 
                 :or {by-count? false, start-degree 0, chebyshev-kind 0}}]
  (if by-count? (let [d (m/ceil (polynomial-2D-degrees end-degree))] 
                  (fn [[x y]] 
                    (vec (take 
                           end-degree 
                           ((polynomial-2D-fn d :start-degree start-degree 
                                              :chebyshev-kind chebyshev-kind) 
                             [x y])))))
    (let [p (polynomial-functions chebyshev-kind)]
      (fn [[x y]] (let [fx (p x), fy (p y)]
                    (loop [arr [], i start-degree]
                      (if (> i end-degree) arr
                        (recur (reduce (fn [tot e] 
                                         (conj tot (* (fx e) (fy (- i e))))) 
                                       arr (range (inc i))) (inc i)))))))))

(defn polynomial-ND-fn
  "Terms are sorted by order and then by dimension, e.g., 
   [1 x y z x^2 xy xz y^2 yz z^2 x^3 (x^2 * y) (x^2 * z) (x * y^2) 
   (x * y * z) (x * z^2) y^3 (y^2 * z) (y * z^2) z^3]
Returns a function that takes a vector of length ndim [x y z ...] and 
   returns a (usually much longer) vector."
  [^long ndim ^long end-degree]
  (fn [v] (map (fn [p] (reduce (fn [tot e] 
                                 (if (zero? e) tot 
                                   (* tot (nth v (dec e))))) 
                               1.0 p)) 
               (distinct 
                 (cm/combinations 
                   (flatten (map #(repeat end-degree %) (range (inc ndim)))) 
                   end-degree)))))

(defn polynomial-ND-fn-without-cross 
  "Terms are sorted by dimension, e.g., [1 x x^2 y y^2 z z^2]
Returns a function that takes a vector of length ndim [x y z ...] and 
   returns a vector."
  [ndim end-degree & {:keys [chebyshev-kind] :or {chebyshev-kind 0}}]
  #(cons 1.0 (mapcat (polynomial-fn 2 :start-degree 1 
                                    :chebyshev-kind chebyshev-kind) %)))

(defn power-series-fn 
  "Returns the power series of a value x using a term series: (a_n * x^n)"
  [term-series]
  (fn [x] (map-indexed #(* %2 (m/pow x %)) term-series)))

(defn power-series-derivative-fn
  "Returns the derivative of a power series"
  [term-series]
  (fn [x] (map-indexed #(* %2 % (m/pow x (dec %))) term-series)))

(defn power-series-integral-fn
  "Returns the integral of a power series"
  [term-series]
  (fn [x] (map-indexed #(* %2 (/ (inc %)) (m/pow x (inc %))) term-series)))

(defn continued-fraction 
  "Returns the continued-fraction series for a term series: 
   a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 + ..."
  [term-series]
  (let [[h & t] term-series]
    (cons h
          (letfn [(f [[ch & ct :as c] kn2 kn1 m]
                     (if-let [an ch]
                       (let [kn (+ kn2 (* an kn1)), v (/ m (* kn1 kn))]
                         (lazy-seq (cons v (f ct kn1 kn (- m)))))
                       c))]
            (f t 1 h -1)))))

(defn generalized-continued-fraction 
  "Returns the generalized-continued-fraction series: 
   a0 + (b0 / (a1 + b1 / (a2 + b2 / (a3 + ..."
  [a-term-series b-term-series]
  (let [[h & t] a-term-series]
    (cons h
          (letfn [(f [[cbh & cbt] [cah & cat] b2 a2 b1 a1]
                     (if (or (nil? cbh) (nil? cah))
                       '()
                       (let [b0 (+ (* cah b1) (* cbh b2)), 
                             a0 (+ (* cah a1) (* cbh a2))]
                         (lazy-seq (cons (- (/ b0 a0) (/ b1 a1)) 
                                         (f cbt cat b1 a1 b0 a0))))))]
            (f b-term-series t 1 0 h 1)))))

;;;SUMMATION 
(defn sum-convergent-series
  "Returns the sum of a convergent series.  
Predicates take the sum value, an index, and the series value.
Options:
   kahan? (default false) -- set to true for greater floating-point 
      summation accuracy, as a fast alternative to bigDecimal.
   converged-pred -- predicate indicating that series has converged 
      (default is that index > m/*min-iter* AND the abs value 
      is <= m/*quad-close* or the abs value is m/*quad-close* times 
      smaller than the abs sum)
   error-pred -- predicate indicating an error 
      (default is that index is > m/*max-iter*)
   error-return-function -- function that throws exception upon error"
  [coll &
   {:keys [kahan? converged-pred err-pred err-ret-fn]
    :or   {kahan?         false,
           converged-pred (fn [sum i val]
                            (and (>= i m/*min-iter*)
                                 (or (<= (m/abs val) m/*quad-close*)
                                     (<= (m/abs (/ val sum)) m/*quad-close*))))
           err-pred       (fn [sum i val] (> i m/*max-iter*)),
           err-ret-fn     (fn [sum i val]
                            (throw (ex-info
                                     (str "Series evaluation failed to converge " sum)
                                     {:fn (var sum-convergent-series)})))}}]
  (if kahan?
    (first (co/reduce-kv-with-stop 
             (fn [sum i val] 
               (let [[tot carry] sum, y (- val carry), new-tot (+ y tot), 
                     new-carry (- new-tot tot y)] [new-tot new-carry])) 
             [0.0 0.0] 
             coll 
             (fn [sum i val] (converged-pred (first sum) i val)) 
             (fn [sum i val] (err-pred (first sum) i val)) 
             (fn [sum i val] (err-ret-fn (first sum) i val))))
    (co/reduce-kv-with-stop 
      (fn [sum i val] (+ sum val)) 0 coll converged-pred err-pred err-ret-fn)))

 