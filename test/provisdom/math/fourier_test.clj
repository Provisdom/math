(ns provisdom.math.fourier-test
  (:require
    [provisdom.math.complex :as complex]
    [provisdom.math.core :as m]
    [provisdom.math.fourier :as fourier]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;2 seconds

(set! *warn-on-reflection* true)

;;;HELPERS (test-only, no s/fdef)
(defn- real->complex-vec
  "Lifts a real vector to a complex vector (zero imaginary parts)."
  [xs]
  (mapv #(complex/->complex (double %) 0.0) xs))

(def ^:private overflow-input
  "Two complex numbers whose butterfly sum overflows a double."
  [(complex/->complex 1.7e308 0.0)
   (complex/->complex 1.7e308 0.0)])

(defn- overflow-anomaly
  "The expected anomaly when the transform attributed to `fn-var` overflows."
  [fn-var]
  {::anomalies/category ::anomalies/no-solve
   ::anomalies/fn       fn-var
   ::anomalies/message  "Non-finite value in FFT result (overflow)."})

;;;POWER-OF-2 UTILITIES
(t/deftest power-of-2?-test
  (t/with-instrument `fourier/power-of-2?
    (t/is-spec-check fourier/power-of-2?))
  (t/with-instrument :all
    (t/is (fourier/power-of-2? 1))
    (t/is (fourier/power-of-2? 2))
    (t/is (fourier/power-of-2? 4))
    (t/is (fourier/power-of-2? 1024))
    (t/is-not (fourier/power-of-2? 3))
    (t/is-not (fourier/power-of-2? 5))
    (t/is-not (fourier/power-of-2? 6))
    (t/is-not (fourier/power-of-2? 1000))))

(t/deftest next-pow2-test
  (t/with-instrument `fourier/next-pow2
    (t/is-spec-check fourier/next-pow2))
  (t/with-instrument :all
    (t/is= 1 (fourier/next-pow2 1))
    (t/is= 2 (fourier/next-pow2 2))
    (t/is= 4 (fourier/next-pow2 3))
    (t/is= 4 (fourier/next-pow2 4))
    (t/is= 8 (fourier/next-pow2 5))
    (t/is= 16 (fourier/next-pow2 9))
    (t/is= 1024 (fourier/next-pow2 513))
    (t/is= 1024 (fourier/next-pow2 1024))
    ;; 2^62 is the largest power of two representable in a long
    (t/is= 4611686018427387904 (fourier/next-pow2 4611686018427387904))
    (t/is= {::anomalies/category ::anomalies/no-solve
            ::anomalies/fn       (var fourier/next-pow2)
            ::anomalies/message  "No power of two >= 4611686018427387905 fits in a long."}
      (fourier/next-pow2 4611686018427387905))))

;;;NAIVE DFT (reference)
(t/deftest dft-naive-test
  (t/with-instrument `fourier/dft-naive
    (t/is-spec-check fourier/dft-naive))
  (t/with-instrument :all
    ;; Empty input
    (t/is= [] (fourier/dft-naive []))
    ;; Length 1: X[0] = x[0]
    (t/is= [(complex/->complex 5.0 0.0)] (fourier/dft-naive [(complex/->complex 5.0 0.0)]))
    ;; Length 4 unit impulse: DFT is all ones
    (t/is-data-approx= (real->complex-vec [1.0 1.0 1.0 1.0])
      (fourier/dft-naive (real->complex-vec [1.0 0.0 0.0 0.0])) :tolerance 1e-12)
    ;; DFT of [1 2 3 4] is exactly [10, -2+2i, -2, -2-2i] ;;Math
    (t/is-data-approx= [(complex/->complex 10.0 0.0)
                        (complex/->complex -2.0 2.0)
                        (complex/->complex -2.0 0.0)
                        (complex/->complex -2.0 -2.0)]
      (fourier/dft-naive (real->complex-vec [1.0 2.0 3.0 4.0])) :tolerance 1e-12)
    ;; Overflowing intermediate sum returns an anomaly (from complex/add; dft-naive is the
    ;; boxed reference implementation)
    (t/is= {::anomalies/category ::anomalies/no-solve
            ::anomalies/fn       (var complex/add)
            ::anomalies/message  "Non-finite complex result: re=Infinity im=0.0"}
      (fourier/dft-naive overflow-input))))

;;;FFT
(t/deftest fft-test
  (t/with-instrument `fourier/fft
    (t/is-spec-check fourier/fft))
  (t/with-instrument :all
    ;; Empty input
    (t/is= [] (fourier/fft []))
    ;; Length 1
    (t/is= [(complex/->complex 3.5 0.0)] (fourier/fft [(complex/->complex 3.5 0.0)]))
    ;; FFT of [1,0,0,0] = [1,1,1,1] (all ones)
    (t/is-data-approx= (real->complex-vec [1.0 1.0 1.0 1.0])
      (fourier/fft (real->complex-vec [1.0 0.0 0.0 0.0])) :tolerance 1e-12)
    ;; FFT of [1,0,0,0,0,0,0,0] (length 8 impulse) = [1,1,1,1,1,1,1,1]
    (t/is-data-approx= (real->complex-vec [1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0])
      (fourier/fft (real->complex-vec [1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])) :tolerance 1e-12)
    ;; FFT matches the naive DFT on a power-of-2 input
    (let [xs (real->complex-vec [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])]
      (t/is-data-approx= (fourier/dft-naive xs) (fourier/fft xs) :tolerance 1e-12))
    ;; FFT of cos(2*pi*k0*n/N) with k0=2, N=8: spikes of N/2 = 4 + 0i at k=2 and k=6,
    ;; zero elsewhere ;;Math
    (let [xs (mapv (fn [n]
                     (complex/->complex (m/cos (/ (* 2.0 m/PI 2 n) 8)) 0.0))
               (range 8))]
      (t/is-data-approx= (real->complex-vec [0.0 0.0 4.0 0.0 0.0 0.0 4.0 0.0])
        (fourier/fft xs) :tolerance 1e-12))
    ;; Non-power-of-2 input zero-pads to [1 2 3 0]: spectrum [6, -2-2i, 2, -2+2i] ;;Math
    (t/is-data-approx= [(complex/->complex 6.0 0.0)
                        (complex/->complex -2.0 -2.0)
                        (complex/->complex 2.0 0.0)
                        (complex/->complex -2.0 2.0)]
      (fourier/fft (real->complex-vec [1.0 2.0 3.0])) :tolerance 1e-12)
    ;; Overflowing butterfly returns an anomaly
    (t/is= (overflow-anomaly (var fourier/fft)) (fourier/fft overflow-input))))

(t/deftest ifft-test
  (t/with-instrument `fourier/ifft
    (t/is-spec-check fourier/ifft))
  (t/with-instrument :all
    ;; Empty input
    (t/is= [] (fourier/ifft []))
    ;; Length 1 passthrough
    (t/is= [(complex/->complex 2.0 0.0)] (fourier/ifft [(complex/->complex 2.0 0.0)]))
    ;; FFT/IFFT roundtrip recovers input (length 8, real)
    (let [xs (real->complex-vec [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])]
      (t/is-data-approx= xs (fourier/ifft (fourier/fft xs)) :tolerance 1e-12))
    ;; FFT/IFFT roundtrip recovers a complex input
    (let [xs [(complex/->complex 1.0 2.0)
              (complex/->complex 3.0 -1.0)
              (complex/->complex -2.0 0.5)
              (complex/->complex 0.0 4.0)]]
      (t/is-data-approx= xs (fourier/ifft (fourier/fft xs)) :tolerance 1e-12))
    ;; IFFT of [1,1,1,1] = [1,0,0,0] (inverse of impulse spectrum)
    (t/is-data-approx= (real->complex-vec [1.0 0.0 0.0 0.0])
      (fourier/ifft (real->complex-vec [1.0 1.0 1.0 1.0])) :tolerance 1e-12)
    ;; Non-power-of-2 spectrum zero-pads to [1 2 3 0]:
    ;; inverse is [1.5, -0.5+0.5i, 0.5, -0.5-0.5i] ;;Math
    (t/is-data-approx= [(complex/->complex 1.5 0.0)
                        (complex/->complex -0.5 0.5)
                        (complex/->complex 0.5 0.0)
                        (complex/->complex -0.5 -0.5)]
      (fourier/ifft (real->complex-vec [1.0 2.0 3.0])) :tolerance 1e-12)
    ;; Overflowing butterfly returns an anomaly
    (t/is= (overflow-anomaly (var fourier/ifft)) (fourier/ifft overflow-input))))

(t/deftest fft-real-test
  (t/with-instrument `fourier/fft-real
    (t/is-spec-check fourier/fft-real))
  (t/with-instrument :all
    ;; Empty input
    (t/is= [] (fourier/fft-real []))
    ;; Matches fft on lifted input (length 8)
    (let [xs [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0]]
      (t/is-data-approx= (fourier/fft (real->complex-vec xs))
        (fourier/fft-real xs) :tolerance 1e-12))
    ;; Hermitian symmetry: X[N-k] = conj(X[k]) for real input
    (let [out (fourier/fft-real [1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0])
          N (count out)]
      (doseq [k (range 1 (quot N 2))]
        (let [xk (nth out k)
              xnk (nth out (- N k))]
          (t/is-approx= (::complex/re xk) (::complex/re xnk) :tolerance 1e-12)
          (t/is-approx= (::complex/im xk) (- (::complex/im xnk)) :tolerance 1e-12))))
    ;; FFT of [1 2 3 4] is exactly [10, -2+2i, -2, -2-2i] ;;Math
    (t/is-data-approx= [(complex/->complex 10.0 0.0)
                        (complex/->complex -2.0 2.0)
                        (complex/->complex -2.0 0.0)
                        (complex/->complex -2.0 -2.0)]
      (fourier/fft-real [1.0 2.0 3.0 4.0]) :tolerance 1e-12)
    ;; Non-power-of-2 input zero-pads, matching fft on the lifted input
    (t/is-data-approx= (fourier/fft (real->complex-vec [1.0 2.0 3.0]))
      (fourier/fft-real [1.0 2.0 3.0]) :tolerance 1e-12)
    ;; Overflowing transform returns an anomaly
    (t/is= (overflow-anomaly (var fourier/fft-real)) (fourier/fft-real [1.7e308 1.7e308]))))
