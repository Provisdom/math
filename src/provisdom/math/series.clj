(ns provisdom.math.series
  "Infinite series summation with convergence testing.
  
  Provides robust algorithms for summing infinite series with adaptive 
  convergence detection. Supports both regular and Kahan summation
  for improved numerical stability.
  
  Key features:
  - Adaptive convergence testing with customizable criteria
  - Kahan summation for reduced floating-point error accumulation  
  - Configurable iteration limits and relative accuracy targets
  - Anomaly handling for non-convergent series"
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;;DECLARATIONS
(s/def ::term-series (s/every ::m/number))
(s/def ::kahan? boolean?)

(s/def ::rel-accu ::m/finite+)
(s/def ::max-iter ::m/int+)

(s/def ::number->term-series
  (s/fspec :args (s/cat :number ::m/number)
    :ret ::term-series))

(s/def ::converged-pred
  (s/fspec :args (s/cat :sum ::m/number :i ::m/long-non- :val ::m/number)
    :ret boolean?))

(s/def ::error-pred
  (s/fspec :args (s/cat :sum ::m/number :i ::m/long-non- :val ::m/number)
    :ret boolean?))

(defn power-series-fn
  "Returns a function that evaluates a power series at a given point.
  
  Takes a sequence of coefficients `term-series` [a₀ a₁ a₂ ...] and returns a function that
  computes the power series Σ(aₙ × xⁿ) for a given x.
  
  Example:
    ((power-series-fn [1 2 3]) 2) ; => [1.0 4.0 12.0] (coeffs: 1×2⁰, 2×2¹, 3×2²)"
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (m/pow x n)))
            term-series)))

(s/fdef power-series-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-derivative-fn
  "Returns a function that evaluates the derivative of a power series.
  
  Takes coefficients `term-series` [a₀ a₁ a₂ ...] and returns a function computing the
  derivative: Σ(n × aₙ × xⁿ⁻¹).
  
  Example:
    ((power-series-derivative-fn [1 2 3]) 2) ; => [0.0 4.0 24.0] (derivatives)"
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (double n) (m/pow x (dec n))))
            term-series)))

(s/fdef power-series-derivative-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-integral-fn
  "Returns a function that evaluates the indefinite integral of a power series.
  
  Takes coefficients `term-series` [a₀ a₁ a₂ ...] and returns a function computing the
  integral: Σ(aₙ × xⁿ⁺¹ / (n+1)).
  
  Example:
    ((power-series-integral-fn [1 2 3]) 2) ; => [2.0 4.0 8.0] (integrals)"
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an
                           (/ (inc n))
                           (m/pow x (inc n))))
            term-series)))

(s/fdef power-series-integral-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn continued-fraction
  "Converts sequence `term-series` to its continued fraction representation.
  
  Given coefficients [a₀ a₁ a₂ ...], computes the continued fraction:
  a₀ + 1/(a₁ + 1/(a₂ + 1/(a₃ + ...)))
  
  Returns a lazy sequence of partial convergents.
  
  Example:
    (continued-fraction [1.0 3.0 6.0 8.0]) ; => [1.0 -0.25 0.01 ...]"
  [term-series]
  (if (empty? term-series)
    '()
    (let [[h & t] term-series]
      (cons h
        (letfn [(f [[ch & ct] kn2 kn1 m]
                  (if ch
                    (let [kn (+ kn2 (* (double ch) kn1))
                          v (m/div m (* kn1 kn))]
                      (lazy-seq (cons v (f ct kn1 kn (- m)))))
                    '()))]
          (f t 1.0 h -1.0))))))

(s/fdef continued-fraction
  :args (s/cat :term-series ::term-series)
  :ret ::term-series)

(defn generalized-continued-fraction
  "Computes a generalized continued fraction from two coefficient sequences.
  
  Given sequences `a-term-series` [a₀ a₁ a₂ ...] and `b-term-series` [b₀ b₁ b₂ ...], computes:
  a₀ + b₀/(a₁ + b₁/(a₂ + b₂/(a₃ + ...)))
  
  Returns a lazy sequence of partial convergents.
  
  Example:
    (generalized-continued-fraction [1 3 6 8] [2 3 2 6]) ; => convergent series"
  [a-term-series b-term-series]
  (if (empty? a-term-series)
    '()
    (let [[h & t] a-term-series]
      (cons h
        (letfn [(f [[cbh & cbt] [cah & cat] b2' a2' b1' a1']
                  (if (or (nil? cbh) (nil? cah))
                    '()
                    (let [cah (double cah)
                          cbh (double cbh)
                          b0' (+ (* cah b1') (* cbh b2'))
                          a0' (+ (* cah a1') (* cbh a2'))
                          v (- (m/div b0' a0') (m/div b1' a1'))]
                      (lazy-seq (cons v (f cbt cat b1' a1' b0' a0'))))))]
          (f b-term-series t 1.0 0.0 h 1.0))))))

(s/fdef generalized-continued-fraction
  :args (s/cat :a-term-series ::term-series :b-term-series ::term-series)
  :ret ::term-series)

(defn multiplicative-continued-fraction
  "Computes continued fraction for `term-series` using multiplicative convergence algorithm.
  
  More numerically stable than the standard algorithm in some cases.
  Uses the relative accuracy threshold to avoid division by zero.
  
  Options:
    ::rel-accu - Relative accuracy threshold (default 1e-50)
  
  Example:
    (multiplicative-continued-fraction [1.0 3.0 6.0 8.0]) ; => stable convergents"
  ([term-series] (multiplicative-continued-fraction term-series {}))
  ([term-series {::keys [rel-accu] :or {rel-accu 1e-50}}]
   (if (empty? term-series)
     '()
     (let [[a0 & ta] term-series
           h-prev (if (m/roughly? a0 0.0 rel-accu) rel-accu a0)]
       (cons h-prev
         (letfn [(f [[ch & ct] d-prev c-prev]
                   (if ch
                     (let [ch (double ch)
                           d-n (+ ch d-prev)
                           d-n (if (m/roughly? d-n 0.0 rel-accu) rel-accu d-n)
                           d-n (/ d-n)
                           c-n (+ ch (/ c-prev))
                           c-n (if (m/roughly? c-n 0.0 rel-accu) rel-accu c-n)
                           delta-n (* c-n d-n)]
                       (lazy-seq (cons delta-n (f ct d-n c-n))))
                     '()))]
           (f ta 0.0 h-prev)))))))

(s/fdef multiplicative-continued-fraction
  :args (s/cat :term-series ::term-series
          :opts (s/? (s/keys :opt [::rel-accu])))
  :ret ::term-series)

(defn multiplicative-generalized-continued-fraction
  "Computes generalized continued fraction for `a-term-series` and `b-term-series` using multiplicative algorithm.
  
  More numerically stable than standard algorithm, especially useful for
  special functions. Based on Didonato and Morris (1992) Algorithm 708.
  
  Options:
    ::rel-accu - Relative accuracy threshold (default 1e-50)
  
  Example:
    (multiplicative-generalized-continued-fraction [1 3 6 8] [2 3 2 6])
    ; => numerically stable convergents"
  ([a-term-series b-term-series]
   (multiplicative-generalized-continued-fraction
     a-term-series b-term-series {}))
  ([a-term-series b-term-series {::keys [rel-accu] :or {rel-accu 1e-50}}]
   (if (empty? a-term-series)
     '()
     (let [[a0 & ta] a-term-series
           h-prev (if (m/roughly? a0 0.0 rel-accu) rel-accu a0)]
       (cons h-prev
         (letfn [(f [[cbh & cbt] [cah & cat] d-prev c-prev]
                   (if (or (nil? cbh) (nil? cah))
                     '()
                     (let [cah (double cah)
                           cbh (double cbh)
                           d-n (+ cah (* cbh d-prev))
                           d-n (if (m/roughly? d-n 0.0 rel-accu) rel-accu d-n)
                           d-n (/ d-n)
                           c-n (+ cah (/ cbh c-prev))
                           c-n (if (m/roughly? c-n 0.0 rel-accu) rel-accu c-n)
                           delta-n (* c-n d-n)]
                       (lazy-seq (cons delta-n (f cbt cat d-n c-n))))))]
           (f b-term-series ta 0.0 h-prev)))))))

(s/fdef multiplicative-generalized-continued-fraction
  :args (s/cat :a-term-series ::term-series
          :b-term-series ::term-series
          :opts (s/? (s/keys :opt [::rel-accu])))
  :ret ::term-series)

;;;SUMMATION
(defn sum-convergent-series
  "Sums infinite series `term-series` with adaptive convergence detection.
  
  Iteratively sums terms until convergence criteria are met or an error
  condition is detected. Supports Kahan summation for improved numerical accuracy.
  
  Parameters:
    `term-series` - Lazy sequence of series terms
  
  Options:
    ::kahan? - Use Kahan summation for reduced floating-point error (default true)
    ::converged-pred - Function (sum index term) -> boolean indicating convergence
                      (default: after 5 terms, |term| ≤ quad-close or |term/sum| ≤ quad-close)
    ::error-pred - Function (sum index term) -> boolean indicating error condition
                  (default: index > 10000 or sum not finite)
  
  Returns the series sum or an anomaly map if convergence fails.
  
  Example:
    (sum-convergent-series (map #(/ (math/pow -1 %) (inc %)) (range)))
    ; => ln(2) ≈ 0.693..."
  ([term-series] (sum-convergent-series term-series {}))
  ([term-series {::keys [kahan? converged-pred error-pred]
                 :or    {kahan?         true
                         converged-pred (fn [sum i val]
                                          (and (>= i 5)
                                            (or (<= (m/abs val) m/quad-close)
                                              (<= (m/abs (m/div val sum))
                                                m/quad-close))))
                         error-pred     (fn [sum i val]
                                          (or (> i 10000)
                                            (not (m/finite? sum))))}}]
   (loop [i 0
          [val & t] term-series
          sum 0.0
          carry 0.0]
     (cond (not val) sum

           (error-pred sum i val)
           {::anomalies/message  (str "Error predicate true. "
                                   " Sum: " sum
                                   " Iteration: " i
                                   " Next value: " val)
            ::anomalies/fn       (var sum-convergent-series)
            ::anomalies/category ::anomalies/no-solve}

           (converged-pred sum i val) (+ sum val)

           :else
           (if kahan?
             (let [y (- val carry)
                   new-sum (+ y sum)
                   new-carry (- new-sum sum y)]
               (recur (inc i) t new-sum new-carry))
             (recur (inc i) t (+ sum val) 0.0))))))

(s/fdef sum-convergent-series
  :args (s/cat :term-series ::term-series
          :opts (s/? (s/keys :opt [::kahan? ::converged-pred ::error-pred])))
  :ret (s/or :number ::m/number
         :anomaly ::anomalies/anomaly))

(defn multiplicative-sum-convergent-series
  "Computes the product of infinite series `term-series` with convergence detection.
  
  Multiplies terms until convergence criteria are met. Useful for series
  where terms approach 1.0 rather than 0.0.
  
  Parameters:
    `term-series` - Lazy sequence of series terms
  
  Options:
    ::converged-pred - Function (product index term) -> boolean indicating convergence
                      (default: term within 1e-14 of 1.0)
    ::error-pred - Function (product index term) -> boolean indicating error
                  (default: index ≥ 100000 or product not finite)
  
  Returns the series product or an anomaly map if convergence fails.
  
  Example:
    (multiplicative-sum-convergent-series
      (map #(+ 1.0 (/ (math/pow -1 %) %)) (range 1 1000)))
    ; => converging infinite product"
  ([term-series] (multiplicative-sum-convergent-series term-series {}))
  ([term-series {::keys [converged-pred error-pred]
                 :or    {converged-pred (fn [sum i val]
                                          (and (not (zero? i))
                                            (m/roughly? val 1.0 1e-14)))
                         error-pred     (fn [sum i val]
                                          (or (> i 100000)
                                            (not (m/finite? sum))))}}]
   (loop [i 0
          [val & t] term-series
          sum 1.0]
     (cond (not val) (if (zero? i) 0.0 sum)

           (error-pred sum i val)
           {::anomalies/message  (str "Error predicate true. "
                                   " Sum: " sum
                                   " Iteration: " i
                                   " Next value: " val)
            ::anomalies/fn       (var multiplicative-sum-convergent-series)
            ::anomalies/category ::anomalies/no-solve}

           (converged-pred sum i val) (* sum val)
           :else (recur (inc i) t (* sum val))))))

(s/fdef multiplicative-sum-convergent-series
  :args (s/cat :term-series ::term-series
          :opts (s/? (s/keys :opt [::converged-pred ::error-pred])))
  :ret (s/or :number ::m/number
         :anomaly ::anomalies/anomaly))
