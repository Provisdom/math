(ns provisdom.math.series
  (:require
    [clojure.spec.alpha :as s]
    [provisdom.utility-belt.anomalies :as anomalies]
    [provisdom.math.core :as m]))

;;;DECLARATIONS
(s/def ::term-series (s/every ::m/number))
(s/def ::kahan? boolean?)

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
            (letfn [(f [[ch & ct :as c] kn2 kn1 m]
                      (if-let [an ch]
                        (let [kn (+ kn2 (* (double an) kn1))
                              v (m/div m (* kn1 kn))]
                          (lazy-seq (cons v (f ct kn1 kn (- m)))))
                        c))]
              (f t 1 h -1))))))

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
            (letfn [(f [[cbh & cbt] [cah & cat] b2 a2 b1 a1]
                      (if (or (nil? cbh) (nil? cah))
                        '()
                        (let [cah (double cah)
                              cbh (double cbh)
                              b0 (+ (* cah b1) (* cbh b2)),
                              a0 (+ (* cah a1) (* cbh a2))]
                          (lazy-seq (cons (- (m/div b0 a0) (m/div b1 a1))
                                          (f cbt cat b1 a1 b0 a0))))))]
              (f b-term-series t 1 0 h 1))))))

(s/fdef generalized-continued-fraction
  :args (s/cat :a-term-series ::term-series :b-term-series ::term-series)
  :ret ::term-series)

;;;SUMMATION
(defn sum-convergent-series
  "Returns the sum of a convergent series. The functions `converged-pred` and
  `error-pred` take the sum, an index, and the next series value.
  Options:
    `::kahan?` (default false) -- set to true for greater floating-point
      summation accuracy.
    `::converged-pred` -- predicate indicating that series has converged
      (default is that index >= 5 AND the abs value is <= m/quad-close or the
      abs value is m/quad-close times smaller than the abs sum).
    `::error-pred` -- predicate indicating an error (default is that index is >
      10000)."
  ([term-series] (sum-convergent-series term-series {}))
  ([term-series {::keys [kahan? converged-pred error-pred]
                 :or    {kahan?         true
                         converged-pred (fn [sum i val]
                                          (and (>= i 5)
                                               (or (m/inf? sum)
                                                   (m/nan? sum)
                                                   (<= (m/abs val) m/quad-close)
                                                   (<= (m/abs (m/div val sum))
                                                       m/quad-close))))
                         error-pred     (fn [sum i val] (> i 10000))}}]
   (loop [i 0
          [val & t] term-series
          sum 0.0
          carry 0.0]
     (cond
       (not val) sum
       (converged-pred sum i val) (+ sum val)

       (error-pred sum i val)
       {::anomalies/message  (str "Error predicate true. "
                                  " Sum: " sum
                                  " Iteration: " i
                                  " Next value: " val)
        ::anomalies/fn       (var sum-convergent-series)
        ::anomalies/category ::anomalies/no-solve}

       :else (if kahan?
               (let [y (- val carry)
                     new-sum (+ y sum)
                     new-carry (- new-sum sum y)]
                 (recur (inc i) t new-sum new-carry))
               (recur (inc i) t (+ sum val) 0.0))))))

(s/fdef sum-convergent-series
  :args (s/cat :term-series ::term-series
               :opts (s/? (s/keys :opt [::kahan? ::converged-pred ::error-pred])))
  :ret (s/or :anomaly ::anomalies/anomaly
             :number ::m/number))
