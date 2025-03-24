(ns provisdom.math.series
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
  "Returns a function that takes a number and returns the power series of a
  value 'x' using a `term-series`: (a_n × x^n)."
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (m/pow x n)))
            term-series)))

(s/fdef power-series-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-derivative-fn
  "Returns a function that takes a number and returns the derivative of the
  power series of a value 'x' using a `term-series`: (a_n × x^n)."
  [term-series]
  (fn [x] (map-indexed (fn [n an]
                         (* an (double n) (m/pow x (dec n))))
            term-series)))

(s/fdef power-series-derivative-fn
  :args (s/cat :term-series ::term-series)
  :ret ::number->term-series)

(defn power-series-integral-fn
  "Returns a function that takes a number and returns the integral of the power
  series of a value x using a `term-series`: (a_n × x^n)."
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
  "Returns the continued fraction series for a `term-series`:
   a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 + ..."
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
  "Returns the generalized continued fraction series:
   a0 + (b0 / (a1 + b1 / (a2 + b2 / (a3 + ..."
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
  "Returns the continued fraction series for a `term-series`:
   a0 + (1 / (a1 + 1 / (a2 + 1 / (a3 + ..."
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
  "Returns the generalized continued fraction series:
   a0 + (b0 / (a1 + b1 / (a2 + b2 / (a3 + ...

   The multiplicative algorithm works better in some cases. Adopted from Apache,
   which took it from Didonato and Morris (1992). 'Algorithm 708: Significant
   Digit Computation of the Incomplete Beta Function Ratios, TOMS 18(3),
   360-373.'"
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
  "Returns the sum of a convergent series. The functions `converged-pred` and
  `error-pred` take the sum, an index, and the next series value.
  Options:
    `::kahan?` (default false) -- set to true for greater floating-point
      summation accuracy.
    `::converged-pred` -- predicate indicating that series has converged
      (default is that index >= 5 AND the abs next value is less than or equal
      to m/quad-close or the abs next value is m/quad-close times smaller than
      the abs sum).
    `::error-pred` -- predicate indicating an error (default is that index is >
      10000 or the sum is not finite)."
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
  "Returns the multiplicative sum of a convergent series. The functions
  `converged-pred` and `error-pred` take the sum, an index, and the next series
  value.
  Options:
    `::converged-pred` -- predicate indicating that series has converged
      (default is that next value is within 1e-14 of 1.0).
    `::error-pred` -- predicate indicating an error (default is that index is >=
      100000 or the sum is not finite.)."
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
