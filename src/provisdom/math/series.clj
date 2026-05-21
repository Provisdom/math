(ns provisdom.math.series
  "Infinite series summation with convergence testing and acceleration.

  ## Usage Guide

  ### Choosing a Summation Method

  | Function                   | Use when...                                              |
  |----------------------------|----------------------------------------------------------|
  | [[sum-convergent-series]]  | Default choice for infinite series with unknown terms    |
  | [[sum-with-diagnostics]]   | Need convergence info (iterations, error estimate)       |
  | [[neumaier-sum]]           | Finite sequence with widely varying magnitudes           |
  | [[pairwise-sum]]           | Finite sequence, want parallelization or O(log n) error  |

  For infinite series, use `sum-convergent-series` or `sum-with-diagnostics`. For finite
  sequences where you just need a sum, use `neumaier-sum` (most accurate) or `pairwise-sum`
  (parallel-friendly).

  ### Choosing an Acceleration Method

  | Function                  | Best for...                                               |
  |---------------------------|-----------------------------------------------------------|
  | [[wynn-epsilon]]          | General purpose; try this first                           |
  | [[euler-transform]]       | Alternating series (signs flip each term)                 |
  | [[aitken-accelerate]]     | Linearly convergent sequences; simple and fast            |
  | [[richardson-extrapolate]]| Numerical integration with halved step sizes              |

  Acceleration methods transform partial sums into faster-converging sequences:
  ```clojure
  (->> terms
       (reductions +)           ; partial sums
       wynn-epsilon             ; accelerate
       (take 20)
       last)                    ; best estimate
  ```

  ### Continued Fractions

  | Function                                          | Use when...                            |
  |---------------------------------------------------|----------------------------------------|
  | [[continued-fraction]]                            | Standard continued fraction conversion |
  | [[multiplicative-continued-fraction]]             | Better numerical stability needed      |
  | [[generalized-continued-fraction]]                | Have separate `a` and `b` sequences    |
  | [[multiplicative-generalized-continued-fraction]] | Generalized + stability (special fns)  |

  The multiplicative variants avoid division-by-zero issues and are preferred for evaluating
  special functions.

  ### Power Series Operations

  - [[power-series-fn]], [[power-series-derivative-fn]], [[power-series-integral-fn]] - Create
    functions from coefficient sequences
  - [[cauchy-product]] - Multiply two power series
  - [[power-series-compose]] - Compute `f(g(x))` from coefficients
  - [[power-series-inverse]] - Find compositional inverse
  - [[radius-of-convergence]] - Estimate where series converges

  ### Rational Approximation

  Use [[pade-approximant]] + [[evaluate-pade]] when Taylor series converge slowly or diverge.
  Padé approximants often work outside the radius of convergence.

  ---

  Provides robust algorithms for summing infinite series with adaptive convergence detection.
  Supports multiple summation strategies and series acceleration methods for improved numerical
  stability."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;;DECLARATIONS
(s/def ::term-series
  (s/with-gen
    (s/every ::m/number)
    #(gen/vector (s/gen ::m/finite) 0 10)))

(s/def ::coefficients
  (s/with-gen
    (s/every ::m/number)
    #(gen/vector (s/gen ::m/finite) 0 10)))

(s/def ::kahan? boolean?)
(s/def ::neumaier? boolean?)

(s/def ::denominator ::coefficients)

(s/def ::inverse-coefficients
  ;;coefficients valid for power-series-inverse: [0 non-zero ...]
  (s/with-gen
    (s/and ::coefficients
      #(and (>= (count %) 2)
         (m/roughly? (first %) 0.0 m/quad-close)
         (not (m/roughly? (second %) 0.0 m/quad-close))))
    #(gen/fmap (fn [[a1 rest-coeffs]]
                 (vec (concat [0.0 a1] rest-coeffs)))
       (gen/tuple
         (gen/such-that (fn [x] (not (m/roughly? x 0.0 m/quad-close))) (s/gen ::m/finite))
         (gen/vector (s/gen ::m/finite) 0 8)))))

(s/def ::max-iter ::m/int+)
(s/def ::num-terms ::m/int+)
(s/def ::numerator ::coefficients)
(s/def ::order ::m/int+)
(s/def ::rel-accu ::m/finite+)
(s/def ::threshold ::m/int+)

(s/def ::number->term-series
  (s/fspec :args (s/cat :number ::m/number)
    :ret ::term-series))

(s/def ::converged-pred
  (s/fspec :args (s/cat :sum ::m/number :i ::m/long-non- :val ::m/number)
    :ret boolean?))

(s/def ::error-pred
  (s/fspec :args (s/cat :sum ::m/number :i ::m/long-non- :val ::m/number)
    :ret boolean?))

;;; Convergence result specs
(s/def ::sum ::m/number)
(s/def ::iterations ::m/long-non-)
(s/def ::final-term ::m/number)
(s/def ::estimated-error ::m/finite-non-)
(s/def ::converged? boolean?)

(s/def ::convergence-result
  (s/keys :req [::converged? ::iterations ::sum]
    :opt [::estimated-error ::final-term]))

(defn power-series-fn
  "Returns a function that evaluates a power series at a given point.

  Takes a sequence of coefficients `term-series` `[a0 a1 a2 ...]` and returns a function that
  computes the power series `sum(an * x^n)` for a given `x`.

  Example:
    `((power-series-fn [1 2 3]) 2)` => `[1.0 4.0 12.0]` (coeffs: `1*2^0`, `2*2^1`, `3*2^2`)"
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (m/pow x n)))
            term-series)))

(s/fdef power-series-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-derivative-fn
  "Returns a function that evaluates the derivative of a power series.

  Takes coefficients `term-series` `[a0 a1 a2 ...]` and returns a function computing the
  derivative: `sum(n * an * x^(n-1))`.

  Example:
    `((power-series-derivative-fn [1 2 3]) 2)` => `[0.0 4.0 24.0]` (derivatives)"
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (double n) (m/pow x (dec n))))
            term-series)))

(s/fdef power-series-derivative-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-integral-fn
  "Returns a function that evaluates the indefinite integral of a power series.

  Takes coefficients `term-series` `[a0 a1 a2 ...]` and returns a function computing the integral:
  `sum(an * x^(n+1) / (n+1))`.

  Example:
    `((power-series-integral-fn [1 2 3]) 2)` => `[2.0 4.0 8.0]` (integrals)"
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

  Given coefficients `[a0 a1 a2 ...]`, computes the continued fraction:
  `a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))`

  Returns a lazy sequence of partial convergents.

  Example:
    `(continued-fraction [1.0 3.0 6.0 8.0])` => `[1.0 -0.25 0.01 ...]`"
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

  Given sequences `a-term-series` `[a0 a1 a2 ...]` and `b-term-series` `[b0 b1 b2 ...]`, computes:
  `a0 + b0/(a1 + b1/(a2 + b2/(a3 + ...)))`

  Returns a lazy sequence of partial convergents.

  Example:
    `(generalized-continued-fraction [1 3 6 8] [2 3 2 6])` => convergent series"
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

  More numerically stable than the standard algorithm in some cases. Uses the relative accuracy
  threshold to avoid division by zero.

  Options:
    `::rel-accu` - relative accuracy threshold (default `1e-50`)

  Example:
    `(multiplicative-continued-fraction [1.0 3.0 6.0 8.0])` => stable convergents"
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
  "Computes generalized continued fraction for `a-term-series` and `b-term-series` using
  multiplicative algorithm.

  More numerically stable than standard algorithm, especially useful for special functions. Based on
  Didonato and Morris (1992) Algorithm 708.

  Options:
    `::rel-accu` - relative accuracy threshold (default `1e-50`)

  Example:
    `(multiplicative-generalized-continued-fraction [1 3 6 8] [2 3 2 6])`
    => numerically stable convergents"
  ([a-term-series b-term-series]
   (multiplicative-generalized-continued-fraction a-term-series b-term-series {}))
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

;;;SERIES ACCELERATION
(defn aitken-accelerate
  "Applies Aitken's delta-squared process to accelerate convergence.

  Given a sequence of `partial-sums`, returns a new sequence with faster convergence. Particularly
  effective for linearly convergent sequences.

  The formula is: `s'n = sn - (s(n+1) - sn)^2 / (s(n+2) - 2*s(n+1) + sn)`

  Returns a lazy sequence of accelerated values (2 fewer than input).

  Example:
    `(aitken-accelerate [1.0 0.5 0.75 0.625 0.6875])` => faster convergence to `ln(2)`"
  [partial-sums]
  (letfn [(f [[s0 s1 s2 & rest]]
            (when s2
              (let [s0 (double s0)
                    s1 (double s1)
                    s2 (double s2)
                    d1 (- s1 s0)
                    d2 (- s2 s1)
                    denom (- d2 d1)]
                (lazy-seq
                  (cons (if (m/roughly? denom 0.0 m/quad-close)
                          s2
                          (- s0 (m/div (m/sq d1) denom)))
                    (f (cons s1 (cons s2 rest))))))))]
    (or (f partial-sums) '())))

(s/fdef aitken-accelerate
  :args (s/cat :partial-sums ::term-series)
  :ret ::term-series)

(defn wynn-epsilon
  "Applies Wynn's epsilon algorithm for series acceleration.

  One of the most powerful general-purpose acceleration methods. Transforms a sequence of partial
  sums into a faster-converging sequence. Particularly effective for many types of slowly
  converging series.

  Takes `partial-sums` and returns a lazy sequence of accelerated values. Each output value uses
  progressively more input terms.

  Options:
    `::max-iter` - maximum iterations (default `50`)

  Example:
    `(wynn-epsilon (reductions + (map #(/ (m/pow -1 %) (inc %)) (range))))`
    => rapidly converging sequence approaching `ln(2)`"
  ([partial-sums] (wynn-epsilon partial-sums {}))
  ([partial-sums {::keys [max-iter] :or {max-iter 50}}]
   (letfn [(epsilon-step [eps-table sn n]
             (let [eps-table (assoc eps-table [n -1] 0.0 [n 0] sn)]
               (loop [k 1
                      tbl eps-table]
                 (if (or (> k n) (> k max-iter))
                   tbl
                   (let [prev-k-1 (get tbl [(dec n) (dec k)] 0.0)
                         prev-k (get tbl [(dec n) k] 0.0)
                         curr-k-1 (get tbl [n (dec k)] 0.0)
                         diff (- (double curr-k-1) prev-k-1)]
                     (if (m/roughly? diff 0.0 m/quad-close)
                       tbl
                       (recur (inc k)
                         (assoc tbl [n k] (+ prev-k (/ diff))))))))))]
     (let [sums (vec (take (+ max-iter 3) partial-sums))]
       (if (empty? sums)
         []
         (loop [n 0
                tbl {}
                results []]
           (if (>= n (count sums))
             results
             (let [new-tbl (epsilon-step tbl (nth sums n) n)
                   best-k (min n max-iter)
                   best-k (if (even? best-k) best-k (dec best-k))
                   result (if (pos? best-k)
                            (get new-tbl [n best-k] (nth sums n))
                            (nth sums n))]
               (recur (inc n) new-tbl (conj results result))))))))))

(s/fdef wynn-epsilon
  :args (s/cat :partial-sums ::term-series
          :opts (s/? (s/keys :opt [::max-iter])))
  :ret ::term-series)

(defn euler-transform
  "Applies Euler transformation to accelerate alternating series.

  Transforms an alternating series into one with better convergence. Input is the original series
  `terms` (not partial sums).

  For alternating series `sum((-1)^n * an)`, this computes the Euler transform which can
  dramatically improve convergence for slowly converging alternating series.

  Returns a lazy sequence of transformed terms that can be summed.

  Example:
    `(reduce + (take 20 (euler-transform (map #(/ 1.0 (inc %)) (range)))))`
    => approximates `ln(2)` faster than naive summation"
  [terms]
  (letfn [(binomial-row [row]
            (vec (concat [1.0] (map + row (rest row)) [1.0])))
          (transform [ts binomial-coeffs n]
            (when (seq ts)
              (let [ts-vec (vec (take (inc n) ts))
                    term (when-not (< (count ts-vec) (inc n))
                           (/ (reduce + (map-indexed
                                          (fn [k ak]
                                            (* (double (nth binomial-coeffs k 0))
                                              (if (even? k) (double ak) (- (double ak)))))
                                          ts-vec))
                             (m/pow 2.0 (inc n))))]
                (when term
                  (lazy-seq
                    (cons term
                      (transform (rest ts) (binomial-row binomial-coeffs) (inc n))))))))]
    (or (transform terms [1.0 1.0] 1) '())))

(s/fdef euler-transform
  :args (s/cat :terms ::term-series)
  :ret ::term-series)

(defn richardson-extrapolate
  "Applies Richardson extrapolation to improve convergence.

  Given a sequence of approximations `approxs` computed at different step sizes (assumed to be
  halved each time), extrapolates to the limit value.

  The `::order` option specifies the order of the error term being eliminated (default `2`, suitable
  for trapezoidal rule errors).

  Returns a sequence of progressively refined estimates.

  Example:
    `(richardson-extrapolate [2.0 1.5 1.25 1.125] {::order 2})`
    => refined estimates approaching the true integral"
  ([approxs] (richardson-extrapolate approxs {}))
  ([approxs {::keys [order] :or {order 2}}]
   (letfn [(extrapolate [[a0 a1 & rest] p]
             (when a1
               (let [factor (m/pow 2 p)
                     refined (/ (- (* factor a1) a0) (dec factor))]
                 (lazy-seq (cons refined (extrapolate (cons a1 rest) p))))))]
     (if (< (count (take 2 approxs)) 2)
       approxs
       (let [first-refined (extrapolate approxs order)]
         (cons (first first-refined)
           (lazy-seq (richardson-extrapolate (rest first-refined)
                       {::order (+ order 2)}))))))))

(s/fdef richardson-extrapolate
  :args (s/cat :approxs ::term-series
          :opts (s/? (s/keys :opt [::order])))
  :ret ::term-series)

;;;POWER SERIES OPERATIONS
(defn cauchy-product
  "Computes the Cauchy product (convolution) of two power series.

  Given coefficients `a-coeffs` `[a0 a1 a2 ...]` and `b-coeffs` `[b0 b1 b2 ...]`, returns
  coefficients `[c0 c1 c2 ...]` where `cn = sum(i=0 to n, ai * b(n-i))`.

  This corresponds to multiplication of the power series: `(sum an*x^n)(sum bn*x^n) = sum cn*x^n`

  Options:
    `::max-iter` - maximum number of output terms (default `100`)

  Example:
    `(cauchy-product [1 1 1] [1 1])` ; `(1+x+x^2)(1+x) = 1+2x+2x^2+x^3`
    => `[1 2 2 1]`"
  ([a-coeffs b-coeffs] (cauchy-product a-coeffs b-coeffs {}))
  ([a-coeffs b-coeffs {::keys [max-iter] :or {max-iter 100}}]
   (let [a (vec (take max-iter a-coeffs))
         b (vec (take max-iter b-coeffs))
         na (count a)
         nb (count b)
         nc (min max-iter (dec (+ na nb)))]
     (mapv (fn [n]
             (reduce + 0.0
               (for [i (range (inc n))
                     :when (and (< i na) (< (- n i) nb))]
                 (* (double (nth a i 0))
                   (double (nth b (- n i) 0))))))
       (range nc)))))

(s/fdef cauchy-product
  :args (s/cat :a-coeffs ::coefficients
          :b-coeffs ::coefficients
          :opts (s/? (s/keys :opt [::max-iter])))
  :ret ::coefficients)

(defn power-series-compose
  "Computes the composition of two power series.

  Given `outer-coeffs` for `f(x)` and `inner-coeffs` for `g(x)`, returns coefficients for `f(g(x))`.

  IMPORTANT: `g(0)` must be `0` (i.e., first coefficient of inner must be `0`) for the composition
  to be well-defined as a power series.

  Options:
    `::max-iter` - maximum number of output terms (default `20`)

  Example:
    `(power-series-compose [1 1 1] [0 2])` ; `f(x) = 1 + x + x^2`, `g(x) = 2x`,
                                             `f(2x) = 1 + 2x + 4x^2`
    => `[1.0 2.0 4.0]`"
  ([outer-coeffs inner-coeffs]
   (power-series-compose outer-coeffs inner-coeffs {}))
  ([outer-coeffs inner-coeffs {::keys [max-iter] :or {max-iter 20}}]
   (let [outer (vec (take max-iter outer-coeffs))
         inner (vec (take max-iter inner-coeffs))]
     (if (or (empty? outer) (empty? inner))
       []
       (let [n (min max-iter (count outer))]
         (loop [k 0
                g-power (vec (concat [1.0] (repeat (dec max-iter) 0.0)))
                result (vec (repeat max-iter 0.0))]
           (if (>= k n)
             (->> result
               (map-indexed vector)
               (take-while (fn [[i v]] (or (< i n) (not (zero? v)))))
               (mapv second))
             (let [ak (double (nth outer k 0))
                   new-result (mapv (fn [i]
                                      (+ (nth result i) (* ak (nth g-power i 0.0))))
                                (range max-iter))
                   new-g-power (cauchy-product g-power inner {::max-iter max-iter})]
               (recur (inc k)
                 (vec (take max-iter (concat new-g-power (repeat 0.0))))
                 new-result)))))))))

(s/fdef power-series-compose
  :args (s/with-gen
          (s/cat :outer-coeffs ::coefficients
            :inner-coeffs ::coefficients
            :opts (s/? (s/keys :opt [::max-iter])))
          #(gen/tuple (s/gen ::coefficients)
             (s/gen ::coefficients)
             (gen/fmap (fn [n] {::max-iter n}) (gen/choose 2 20))))
  :ret ::coefficients)

(defn power-series-inverse
  "Computes coefficients for the compositional inverse of a power series.

  Given `coeffs` for `f(x)` where `f(0) = 0` and `f'(0) != 0`, returns coefficients for `g(x)` such
  that `f(g(x)) = x`.

  Requires: first coefficient is `0`, second coefficient is non-zero.

  Options:
    `::max-iter` - maximum number of output terms (default `20`)

  Example:
    `(power-series-inverse [0 1 1])` ; `f(x) = x + x^2`, find `g` such that `f(g(x)) = x`
    => `[0.0 1.0 -1.0 2.0 -5.0 ...]` (Catalan numbers with signs)"
  ([coeffs] (power-series-inverse coeffs {}))
  ([coeffs {::keys [max-iter] :or {max-iter 20}}]
   (let [cs (vec (take max-iter coeffs))
         a0 (nth cs 0 0)
         a1 (nth cs 1 0)]
     (cond (not (m/roughly? a0 0.0 m/quad-close))
       {::anomalies/category ::anomalies/incorrect
        ::anomalies/fn       (var power-series-inverse)
        ::anomalies/message  "First coefficient must be 0 for power series inverse"}

       (m/roughly? a1 0.0 m/quad-close)
       {::anomalies/category ::anomalies/incorrect
        ::anomalies/fn       (var power-series-inverse)
        ::anomalies/message  "Second coefficient must be non-zero for power series inverse"}

       :else
       (loop [n 1
              result [0.0 (/ a1)]]
         (if (>= n max-iter)
           (vec (take max-iter result))
           (let [result-padded (vec (concat result (repeat (- max-iter (count result)) 0.0)))
                 composed (power-series-compose cs result-padded {::max-iter max-iter})
                 cn+1 (nth composed (inc n) 0.0)
                 b1 (nth result 1)
                 b1-pow (m/pow b1 (inc n))]
             (if (zero? b1-pow)
               {::anomalies/category ::anomalies/incorrect
                ::anomalies/fn       (var power-series-inverse)
                ::anomalies/message  "Numerical underflow: coefficient too large for inverse computation"}
               (recur (inc n)
                 (conj result (- (/ cn+1 b1-pow))))))))))))

(s/fdef power-series-inverse
  :args (s/with-gen
          (s/cat :coeffs ::coefficients
            :opts (s/? (s/keys :opt [::max-iter])))
          #(gen/tuple (s/gen ::inverse-coefficients)
             (gen/fmap (fn [n] {::max-iter n}) (gen/choose 2 10))))
  :ret (s/or :coefficients ::coefficients
         :anomaly ::anomalies/anomaly))

(defn radius-of-convergence
  "Estimates the radius of convergence of a power series.

  Uses the ratio test `(lim |a(n+1)/an|)` and root test `(lim |an|^(1/n))` on the given `coeffs` to
  estimate the radius of convergence.

  Returns a map with:
    `::ratio-estimate` - estimate from ratio test
    `::root-estimate` - estimate from root test
    `::combined-estimate` - best estimate (minimum of finite estimates)

  Options:
    `::num-terms` - number of terms to use for estimation (default `20`)

  Example:
    `(radius-of-convergence [1 1 1 1 1 1 1 1 1 1])` ; geometric series has radius `1`
    => `{::ratio-estimate 1.0 ::root-estimate 1.0 ::combined-estimate 1.0}`"
  ([coeffs] (radius-of-convergence coeffs {}))
  ([coeffs {::keys [num-terms] :or {num-terms 20}}]
   (let [cs (vec (take num-terms coeffs))
         n (count cs)]
     (if (< n 3)
       {::combined-estimate m/inf+
        ::ratio-estimate    m/inf+
        ::root-estimate     m/inf+}
       (let [;; Ratio test: R = lim |aₙ/aₙ₊₁|
             ratios (for [i (range (dec n))
                          :let [ai (m/abs (nth cs i))
                                ai+1 (m/abs (nth cs (inc i)))]
                          :when (and (pos? ai) (pos? ai+1))]
                      (/ ai ai+1))
             ratio-est (if (seq ratios)
                         (last ratios)
                         m/inf+)
             ;; Root test: R = 1 / lim |aₙ|^(1/n)
             roots (for [i (range 1 n)
                         :let [ai (m/abs (nth cs i))]
                         :when (pos? ai)]
                     (m/pow ai (/ i)))
             root-est (if (seq roots)
                        (let [lim (last roots)]
                          (if (pos? lim) (/ lim) m/inf+))
                        m/inf+)
             combined (cond
                        (and (m/finite? ratio-est) (m/finite? root-est))
                        (min ratio-est root-est)
                        (m/finite? ratio-est) ratio-est
                        (m/finite? root-est) root-est
                        :else m/inf+)]
         {::combined-estimate combined
          ::ratio-estimate    ratio-est
          ::root-estimate     root-est})))))

(s/fdef radius-of-convergence
  :args (s/cat :coeffs ::coefficients
          :opts (s/? (s/keys :opt [::num-terms])))
  :ret (s/keys :req [::combined-estimate ::ratio-estimate ::root-estimate]))

;;;PADE APPROXIMANTS
(defn pade-approximant
  "Computes a Pade approximant [`L`/`M`] from power series coefficients.

  Given `coeffs` `[c0 c1 c2 ...]` representing `f(x) = sum(cn*x^n)`, computes the [`L`/`M`] Pade
  approximant `P(x)/Q(x)` where `P` has degree `L` and `Q` has degree `M`.

  Returns a map with:
    `::numerator` - coefficients of `P(x)`
    `::denominator` - coefficients of `Q(x)` (normalized so `Q(0) = 1`)

  The Pade approximant often converges where Taylor series diverge and can provide better rational
  approximations.

  Example:
    `(pade-approximant [1 1 0.5 0.167] 1 1)` ; `[1/1]` Pade for `e^x = 1 + x + x^2/2 + ...`
    => `{::numerator [1.0 0.5] ::denominator [1.0 -0.5]}`"
  [coeffs L M]
  (let [cs (vec (concat (take (+ L M 1) coeffs)
                  (repeat (max 0 (- (+ L M 1) (count (take (+ L M 1) coeffs)))) 0.0)))
        c (fn [i] (double (nth cs i 0.0)))]
    (if (zero? M)
      ;; No denominator, just truncated series
      {::denominator [1.0]
       ::numerator   (mapv double (take (inc L) cs))}
      ;; Solve linear system for denominator coefficients
      ;; Q(x) = 1 + q₁x + q₂x² + ... + qₘxᴹ
      ;; System: Σⱼ qⱼ·c_{L+i-j} = -c_{L+1+i} for i = 0..M-1, j = 0..M-1
      (let [;; Build matrix and RHS for the linear system
            matrix (vec (for [i (range M)]
                          (vec (for [j (range M)]
                                 (c (- (+ L i) j))))))
            rhs (vec (for [i (range M)]
                       (- (c (+ L 1 i)))))
            ;; Simple Gaussian elimination for small systems
            solve-system (fn [A b]
                           (let [n (count b)
                                 Ab (vec (map-indexed
                                           (fn [i row]
                                             (conj (vec row) (nth b i)))
                                           A))]
                             (loop [k 0
                                    Ab Ab]
                               (if (>= k n)
                                 ;; Back substitution
                                 (loop [i (dec n)
                                        x (vec (repeat n 0.0))]
                                   (if (neg? i)
                                     x
                                     (let [row (nth Ab i)
                                           sum (reduce + (map * (subvec (vec row) (inc i) n)
                                                           (subvec x (inc i) n)))
                                           pivot (nth row i)]
                                       (if (m/roughly? pivot 0.0 m/quad-close)
                                         x
                                         (recur (dec i)
                                           (assoc x i (/ (- (nth row n) sum) pivot)))))))
                                 ;; Forward elimination
                                 (let [;; Find pivot
                                       max-row (apply max-key
                                                 (fn [i] (m/abs (nth (nth Ab i) k)))
                                                 (range k n))
                                       Ab (if (= k max-row)
                                            Ab
                                            (let [tmp (nth Ab k)]
                                              (-> Ab
                                                (assoc k (nth Ab max-row))
                                                (assoc max-row tmp))))
                                       pivot (nth (nth Ab k) k)]
                                   (if (m/roughly? pivot 0.0 m/quad-close)
                                     (recur (inc k) Ab)
                                     (let [Ab (reduce
                                                (fn [Ab i]
                                                  (let [factor (/ (nth (nth Ab i) k) pivot)
                                                        new-row (mapv - (nth Ab i)
                                                                  (mapv #(* factor %) (nth Ab k)))]
                                                    (assoc Ab i new-row)))
                                                Ab
                                                (range (inc k) n))]
                                       (recur (inc k) Ab))))))))
            q-coeffs (if (and (seq matrix) (seq rhs))
                       (solve-system matrix rhs)
                       (vec (repeat M 0.0)))
            ;; Denominator: 1 + q₁x + q₂x² + ...
            denom (vec (concat [1.0] q-coeffs))
            ;; Numerator: multiply c(x) by Q(x) and take first L+1 terms
            ;; p_k = Σⱼ qⱼ·c_{k-j} for j=0..min(k,M), where q₀=1
            numer (vec (for [k (range (inc L))]
                         (reduce + (for [j (range (inc (min k M)))]
                                     (* (nth denom j 0.0) (c (- k j)))))))]
        {::denominator denom
         ::numerator   numer}))))

(s/fdef pade-approximant
  :args (s/with-gen
          (s/cat :coeffs ::coefficients :L ::m/int-non- :M ::m/int-non-)
          #(gen/tuple (s/gen ::coefficients)
             (gen/choose 0 5)
             (gen/choose 0 5)))
  :ret (s/keys :req [::denominator ::numerator]))

(defn evaluate-pade
  "Evaluates a Pade approximant at a given point.

  Takes the result of [[pade-approximant]] and evaluates `P(x)/Q(x)`.

  Example:
    `(let [pade (pade-approximant [1 1 0.5 0.167] 2 2)] (evaluate-pade pade 0.5))`
    => approximation of `e^0.5`"
  [{::keys [numerator denominator]} x]
  (let [eval-poly (fn [coeffs x]
                    (reduce (fn [acc [i c]]
                              (+ acc (* c (m/pow x i))))
                      0.0
                      (map-indexed vector coeffs)))
        p (eval-poly numerator x)
        q (eval-poly denominator x)]
    (m/div p q)))

(s/fdef evaluate-pade
  :args (s/cat :pade (s/keys :req [::denominator ::numerator])
          :x ::m/number)
  :ret ::m/number)

;;;SUMMATION
(defn sum-convergent-series
  "Sums infinite series `term-series` with adaptive convergence detection.

  Iteratively sums terms until convergence criteria are met or an error condition is detected.
  Supports Kahan summation for improved numerical accuracy.

  Parameters:
    `term-series` - lazy sequence of series terms

  Options:
    `::kahan?` - use Kahan summation for reduced floating-point error (default `true`)
    `::converged-pred` - function `(sum index term) -> boolean` indicating convergence (default:
                         after 5 terms, `|term| <= quad-close` or `|term/sum| <= quad-close`)
    `::error-pred` - function `(sum index term) -> boolean` indicating error condition (default:
                     `index > 10000` or sum not finite)

  Returns the series sum or an anomaly map if convergence fails.

  Example:
    `(sum-convergent-series (map #(/ (m/pow -1 %) (inc %)) (range)))` => `ln(2)` ~ `0.693...`"
  ([term-series] (sum-convergent-series term-series {}))
  ([term-series {::keys [kahan? converged-pred error-pred]
                 :or    {kahan?         true
                         converged-pred (fn [sum i val]
                                          (and (>= i 5)
                                            (or (<= (m/abs val) m/quad-close)
                                              (<= (m/abs (m/div val sum))
                                                m/quad-close))))
                         error-pred     (fn [sum i _val]
                                          (or (> i 10000)
                                            (not (m/finite? sum))))}}]
   (loop [i 0
          [val & t] term-series
          sum 0.0
          carry 0.0]
     (cond (not val) sum

       (error-pred sum i val)
       {::anomalies/category ::anomalies/no-solve
        ::anomalies/fn       (var sum-convergent-series)
        ::anomalies/message  (str "Error predicate true. "
                               " Sum: " sum
                               " Iteration: " i
                               " Next value: " val)}

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
          :opts (s/? (s/keys :opt [::converged-pred ::error-pred ::kahan?])))
  :ret (s/or :number ::m/number
         :anomaly ::anomalies/anomaly))

(defn multiplicative-sum-convergent-series
  "Computes the product of infinite series `term-series` with convergence detection.

  Multiplies terms until convergence criteria are met. Useful for series where terms approach `1.0`
  rather than `0.0`.

  Parameters:
    `term-series` - lazy sequence of series terms

  Options:
    `::converged-pred` - function `(product index term) -> boolean` indicating convergence
                         (default: term within `1e-14` of `1.0`)
    `::error-pred` - function `(product index term) -> boolean` indicating error (default:
                     `index >= 100000` or product not finite)

  Returns the series product or an anomaly map if convergence fails.

  Example:
    `(multiplicative-sum-convergent-series (map #(+ 1.0 (/ (m/pow -1 %) %)) (range 1 1000)))`
    => converging infinite product"
  ([term-series] (multiplicative-sum-convergent-series term-series {}))
  ([term-series {::keys [converged-pred error-pred]
                 :or    {converged-pred (fn [_sum i val]
                                          (and (not (zero? i))
                                            (m/roughly? val 1.0 1e-14)))
                         error-pred     (fn [sum i _val]
                                          (or (> i 100000)
                                            (not (m/finite? sum))))}}]
   (loop [i 0
          [val & t] term-series
          sum 1.0]
     (cond (not val) (if (zero? i) 0.0 sum)

       (error-pred sum i val)
       {::anomalies/category ::anomalies/no-solve
        ::anomalies/fn       (var multiplicative-sum-convergent-series)
        ::anomalies/message  (str "Error predicate true. Sum: " sum " Iteration: " i
                               " Next value: " val)}

       (converged-pred sum i val) (* sum val)
       :else (recur (inc i) t (* sum val))))))

(s/fdef multiplicative-sum-convergent-series
  :args (s/cat :term-series ::term-series
          :opts (s/? (s/keys :opt [::converged-pred ::error-pred])))
  :ret (s/or :number ::m/number
         :anomaly ::anomalies/anomaly))

;;;IMPROVED SUMMATION METHODS
(defn neumaier-sum
  "Sums a sequence using Neumaier's improved Kahan summation algorithm.

  An improvement over Kahan summation that handles the case where the next term to be added is
  larger in magnitude than the running sum. This provides better numerical accuracy for sequences
  where term magnitudes vary widely.

  Example:
    `(neumaier-sum [1.0 1e100 1.0 -1e100])` => `2.0` (exactly, despite intermediate large values)"
  [terms]
  (loop [[val & t] terms
         sum 0.0
         c 0.0]
    (if (nil? val)
      (+ sum c)
      (let [val (double val)
            temp (+ sum val)
            ;; Compute correction term carefully to preserve round-off
            correction (if (>= (m/abs sum) (m/abs val))
                         (+ (- sum temp) val)
                         (+ (- val temp) sum))]
        (recur t temp (+ c correction))))))

(s/fdef neumaier-sum
  :args (s/cat :terms ::term-series)
  :ret ::m/number)

(defn pairwise-sum
  "Sums a sequence using pairwise summation algorithm.

  Recursively divides the sequence in half and sums each half, then adds the results. This achieves
  `O(log n)` error growth compared to `O(n)` for naive summation, while being more
  parallelization-friendly than Kahan summation.

  Options:
    `::threshold` - below this size, use direct summation (default `16`)

  Example:
    `(pairwise-sum (range 1 1001))` => `500500.0`"
  ([terms] (pairwise-sum terms {}))
  ([terms {::keys [threshold] :or {threshold 16}}]
   (let [v (vec terms)
         n (count v)]
     (if (<= n threshold)
       (reduce + 0.0 v)
       (let [mid (quot n 2)]
         (+ (pairwise-sum (subvec v 0 mid) {::threshold threshold})
           (pairwise-sum (subvec v mid) {::threshold threshold})))))))

(s/fdef pairwise-sum
  :args (s/cat :terms ::term-series
          :opts (s/? (s/keys :opt [::threshold])))
  :ret ::m/number)

(defn sum-with-diagnostics
  "Sums infinite series with detailed convergence diagnostics.

  Like [[sum-convergent-series]] but returns a map with convergence information instead of just the
  sum.

  Parameters:
    `term-series` - lazy sequence of series terms

  Options:
    `::kahan?` - use Kahan summation (default `true`)
    `::neumaier?` - use Neumaier summation instead of Kahan (default `false`)
    `::rel-accu` - relative accuracy target (default `quad-close`)
    `::max-iter` - maximum iterations (default `10000`)

  Returns a map with:
    `::sum` - the computed sum
    `::iterations` - number of terms summed
    `::final-term` - the last term added
    `::estimated-error` - estimated remaining error (`|final-term|`)
    `::converged?` - whether convergence was achieved

  Example:
    `(sum-with-diagnostics (map #(/ (m/pow -1 %) (inc %)) (range)))`
    => `{::sum 0.693... ::iterations 42 ::converged? true ...}`"
  ([term-series] (sum-with-diagnostics term-series {}))
  ([term-series {::keys [kahan? neumaier? rel-accu max-iter]
                 :or    {kahan?    true
                         neumaier? false
                         rel-accu  m/quad-close
                         max-iter  10000}}]
   (loop [i 0
          [val & t] term-series
          sum 0.0
          carry 0.0]
     (cond
       ;; End of sequence
       (nil? val)
       {::converged?       true
        ::estimated-error 0.0
        ::final-term      0.0
        ::iterations      i
        ::sum             sum}

       ;; Max iterations reached
       (>= i max-iter)
       {::converged?       false
        ::estimated-error (m/abs (double val))
        ::final-term      (double val)
        ::iterations      (inc i)
        ::sum             (+ sum (double val))}

       ;; Check convergence
       (and (>= i 5)
         (or (<= (m/abs (double val)) rel-accu)
           (<= (m/abs (m/div (double val) sum)) rel-accu)))
       {::converged?       true
        ::estimated-error (m/abs (double val))
        ::final-term      (double val)
        ::iterations      (inc i)
        ::sum             (+ sum (double val))}

       ;; Continue summing
       :else
       (let [val (double val)]
         (if neumaier?
           ;; Neumaier summation
           (let [temp (+ sum val)]
             (if (>= (m/abs sum) (m/abs val))
               (recur (inc i) t temp (+ carry (- sum temp) val))
               (recur (inc i) t temp (+ carry (- val temp) sum))))
           (if kahan?
             ;; Kahan summation
             (let [y (- val carry)
                   new-sum (+ y sum)
                   new-carry (- new-sum sum y)]
               (recur (inc i) t new-sum new-carry))
             ;; Naive summation
             (recur (inc i) t (+ sum val) 0.0))))))))

(s/fdef sum-with-diagnostics
  :args (s/cat :term-series ::term-series
          :opts (s/? (s/keys :opt [::kahan? ::max-iter ::neumaier? ::rel-accu])))
  :ret ::convergence-result)
