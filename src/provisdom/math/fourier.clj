(ns provisdom.math.fourier
  "Discrete Fourier Transform via the radix-2 Cooley-Tukey FFT.

  Inputs and outputs are vectors of complex-number maps (spec `::complex/complex` from
  `provisdom.math.complex`). [[fft-real]] accepts a real-valued vector for convenience.

  Implementation choice:
  - Hipparchus's `FastFourierTransformer` is NOT on the math library classpath (it lives in the
    solvers deps), so we implement Cooley-Tukey directly in Clojure on primitive double arrays. The
    radix-2 path runs in `O(N log N)` for power-of-2 lengths.
  - Non-power-of-2 inputs are zero-padded to the next power of 2 (`next-pow2`). This is the simplest
    correct approach: spectra of a zero-padded sequence are an interpolation of the original DFT
    spectrum. (Bluestein's chirp-z algorithm would produce same-length output without padding, but
    is not implemented.)
  - [[dft-naive]] provides an `O(N^2)` reference used by tests as ground truth.
  - Arithmetic overflow inside a transform returns an anomaly (`provisdom.utility-belt.anomalies`)
    rather than throwing.

  Sign convention follows the standard forward transform:
  `X[k] = sum_{n=0..N-1} x[n] * exp(-2*pi*i*k*n / N)` and
  inverse `x[n] = (1/N) sum_{k} X[k] * exp(+2*pi*i*k*n / N)`."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.complex :as complex]
    [provisdom.math.core :as m]
    [provisdom.math.vector :as vector]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;;SPECS
(s/def ::complex-vector
  (s/with-gen (s/coll-of ::complex/complex :kind vector?)
    #(gen/vector (s/gen ::complex/complex) 0 8)))

(s/def ::complex-vector-non-empty
  (s/with-gen (s/coll-of ::complex/complex :kind vector? :min-count 1)
    #(gen/vector (s/gen ::complex/complex) 1 8)))

;;;POWER-OF-2 UTILITIES
(defn power-of-2?
  "Returns `true` when positive long `n` is a power of two."
  [n]
  (let [n (long n)]
    (and (pos? n) (zero? (bit-and n (dec n))))))

(s/fdef power-of-2?
  :args (s/cat :n ::m/long+)
  :ret boolean?)

(defn next-pow2
  "Returns the smallest power of two `>= n` for positive long `n`.

  Returns an anomaly when `n` exceeds `2^62` (the largest power of two representable in a long),
  since no power of two `>= n` fits."
  [n]
  (let [n (long n)
        cap (bit-shift-left 1 62)]
    (cond (<= n 1) 1
      (<= n cap) (bit-shift-left 1 (- 64 (Long/numberOfLeadingZeros (dec n))))
      :else {::anomalies/category ::anomalies/no-solve
             ::anomalies/fn       (var next-pow2)
             ::anomalies/message  (str "No power of two >= " n " fits in a long.")})))

(s/fdef next-pow2
  :args (s/cat :n ::m/long+)
  :ret (s/or :anomaly ::anomalies/anomaly
         :pow2 ::m/long+))

;;;NAIVE DFT (reference)
(defn dft-naive
  "Computes the forward DFT of complex vector `xs` by direct evaluation in `O(N^2)`.

  Used for small inputs and as the ground-truth reference in tests. Returns an empty vector for
  empty input, and an anomaly when an intermediate product or sum overflows.

  Sign convention: `X[k] = sum_{n=0..N-1} x[n] * exp(-2*pi*i*k*n / N)`."
  [xs]
  (let [xs (vec xs)
        N (count xs)]
    (if (zero? N)
      []
      (let [theta-base (/ (* -2.0 m/PI) (double N))]
        (reduce (fn [out k]
                  (let [Xk (reduce (fn [acc n]
                                     (let [angle (* theta-base (double k) n)
                                           w (complex/->complex (m/cos angle) (m/sin angle))
                                           prod (complex/mul (get xs n) w)
                                           sum (if (anomalies/anomaly? prod)
                                                 prod
                                                 (complex/add acc prod))]
                                       (if (anomalies/anomaly? sum)
                                         (reduced sum)
                                         sum)))
                             complex/zero
                             (range N))]
                    (if (anomalies/anomaly? Xk)
                      (reduced Xk)
                      (conj out Xk))))
          []
          (range N))))))

(s/fdef dft-naive
  :args (s/cat :xs ::complex-vector)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex-vector ::complex-vector))

;;;COOLEY-TUKEY HELPERS
(defn- bit-reverse
  "Returns the `bits`-wide bit-reversal of long `x`."
  [x bits]
  (let [bits (long bits)]
    (loop [i 0
           x (long x)
           r 0]
      (if (= i bits)
        r
        (recur (inc i)
          (bit-shift-right x 1)
          (bit-or (bit-shift-left r 1) (bit-and x 1)))))))

(defn- log2-of-pow2
  "Returns the bit position of power-of-2 long `n` (so `(log2-of-pow2 8)` is `3`)."
  [n]
  (loop [n (long n)
         k 0]
    (if (<= n 1) k (recur (bit-shift-right n 1) (inc k)))))

(defn- fill-twiddles!
  "Fills `cos-table`/`sin-table` so entry `k` is `exp(sign * 2*pi*i * k / n)`."
  [^doubles cos-table ^doubles sin-table n sign]
  (let [n (double n)
        sign (double sign)]
    (dotimes [k (alength cos-table)]
      (let [theta (/ (* sign m/two-pi k) n)]
        ;; explicit casts: m/cos and m/sin return untyped values, and without the cast each
        ;; aset falls back to reflection, which dominates the whole transform
        (aset cos-table k (double (m/cos theta)))
        (aset sin-table k (double (m/sin theta)))))))

(defn- butterflies!
  "Runs the in-place radix-2 butterfly passes over `re`/`im`, which must hold the input in
  bit-reversed order. `sign` is -1.0 for the forward transform, 1.0 for the inverse."
  [^doubles re ^doubles im sign]
  (let [N (alength re)
        half-N (quot N 2)
        cos-table (double-array half-N)
        sin-table (double-array half-N)]
    (fill-twiddles! cos-table sin-table N sign)
    (loop [size 2]
      (when (<= size N)
        (let [half (quot size 2)
              stride (quot N size)]
          (loop [start 0]
            (when (< start N)
              (dotimes [j half]
                (let [w-idx (* j stride)
                      wr (aget cos-table w-idx)
                      wi (aget sin-table w-idx)
                      i1 (+ start j)
                      i2 (+ i1 half)
                      br (aget re i2)
                      bi (aget im i2)
                      tr (- (* wr br) (* wi bi))
                      ti (+ (* wr bi) (* wi br))
                      ar (aget re i1)
                      ai (aget im i1)]
                  (aset re i1 (+ ar tr))
                  (aset im i1 (+ ai ti))
                  (aset re i2 (- ar tr))
                  (aset im i2 (- ai ti))))
              (recur (+ start size)))))
        (recur (bit-shift-left size 1))))))

(defn- finalize-arrays
  "Converts `re`/`im` arrays to a complex-map vector, multiplying each component by `scale`.

  A single finite scan here replaces per-butterfly anomaly checks: Inf/NaN propagates harmlessly
  through the primitive arrays and is caught at the end. Returns an anomaly attributed to `fn-var`
  when any component is non-finite."
  [fn-var ^doubles re ^doubles im scale]
  (let [N (alength re)
        scale (double scale)]
    (loop [i 0]
      (cond (= i N)
        (mapv (fn [k]
                {::complex/im (* scale (aget im k))
                 ::complex/re (* scale (aget re k))})
          (range N))
        (and (m/finite? (aget re i)) (m/finite? (aget im i))) (recur (inc i))
        :else {::anomalies/category ::anomalies/no-solve
               ::anomalies/fn       fn-var
               ::anomalies/message  "Non-finite value in FFT result (overflow)."}))))

(defn- cooley-tukey
  "Radix-2 Cooley-Tukey FFT of a power-of-2 length complex vector `xs` on primitive double arrays.

  Loads `xs` in bit-reversed order, runs [[butterflies!]], and converts back to complex maps with
  each component multiplied by `scale` (`1.0` for the forward transform, `1/N` for the inverse —
  see [[ifft]]). When `inverse?` is `true` uses twiddle factor sign +; otherwise -. Returns an
  anomaly attributed to `fn-var` when the transform produces a non-finite value (overflow)."
  [fn-var xs inverse? scale]
  (let [N (count xs)
        bits (log2-of-pow2 N)
        re (double-array N)
        im (double-array N)]
    (dotimes [i N]
      (let [{ci ::complex/im cr ::complex/re} (get xs (bit-reverse i bits))]
        (aset re i (double cr))
        (aset im i (double ci))))
    (butterflies! re im (if inverse? 1.0 -1.0))
    (finalize-arrays fn-var re im scale)))

(defn- pad-to-pow2
  "Zero-pads complex vector `xs` (right-side) to the next power-of-two length."
  [xs]
  (let [xs (vec xs)
        N (count xs)
        N2 (next-pow2 (max 1 N))]
    (if (= N N2)
      xs
      (into xs (repeat (- N2 N) complex/zero)))))

;;;FFT
(defn fft
  "Returns the forward FFT of complex vector `xs`.

  When `(count xs)` is a power of two, runs radix-2 Cooley-Tukey in `O(N log N)`. Otherwise, the
  input is zero-padded to the next power of two (the returned length matches the padded length, not
  the original).

  Empty input returns `[]`. Length-1 input returns the input unchanged. Returns an anomaly when the
  transform overflows to a non-finite value."
  [xs]
  (let [xs (vec xs)
        N (count xs)]
    (cond (zero? N) []
      (= N 1) xs
      (power-of-2? N) (cooley-tukey (var fft) xs false 1.0)
      :else (cooley-tukey (var fft) (pad-to-pow2 xs) false 1.0))))

(s/fdef fft
  :args (s/cat :xs ::complex-vector)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex-vector ::complex-vector))

(defn ifft
  "Returns the inverse FFT of complex vector `xs`, normalized by `1/N`.

  Satisfies `(ifft (fft xs)) = xs` (to floating-point tolerance) when `(count xs)` is a power of
  two. For non-power-of-2 inputs, the same zero-padding rule as [[fft]] applies. Returns an anomaly
  when the transform overflows to a non-finite value."
  [xs]
  (let [xs (vec xs)
        N (count xs)]
    (cond (zero? N) []
      (= N 1) xs
      :else (let [padded (if (power-of-2? N) xs (pad-to-pow2 xs))]
              (cooley-tukey (var ifft) padded true (/ (double (count padded))))))))

(s/fdef ifft
  :args (s/cat :xs ::complex-vector)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex-vector ::complex-vector))

(defn fft-real
  "Returns the forward FFT of a real-valued vector `xs`.

  Uses the real-input optimization: packs even/odd samples into a half-length complex FFT and
  unpacks via Hermitian symmetry, roughly halving the work versus lifting to [[fft]]. The returned
  spectrum is complex and satisfies `X[N-k] = conj(X[k])`. Non-power-of-2 inputs are zero-padded
  like [[fft]]. Returns an anomaly when the transform overflows to a non-finite value."
  [xs]
  (let [xs (vec xs)
        n0 (count xs)]
    (cond (zero? n0) []
      (= n0 1) [(complex/->complex (double (first xs)) 0.0)]
      :else (let [N (long (if (power-of-2? n0) n0 (next-pow2 n0)))
                  half (quot N 2)
                  bits (log2-of-pow2 half)
                  zre (double-array half)
                  zim (double-array half)
                  x (fn [idx] (if (< idx n0) (double (get xs idx)) 0.0))]
              ;; pack z[n] = x[2n] + i*x[2n+1], loaded in bit-reversed order
              (dotimes [i half]
                (let [src (bit-reverse i bits)]
                  (aset zre i (double (x (* 2 src))))
                  (aset zim i (double (x (inc (* 2 src)))))))
              (butterflies! zre zim -1.0)
              ;; unpack the half-length spectrum Z into the full length-N spectrum:
              ;; even spectrum E[k] = (Z[k] + conj(Z[half-k]))/2,
              ;; odd O[k] = -i*(Z[k] - conj(Z[half-k]))/2,
              ;; X[k] = E[k] + W^k * O[k] with W = exp(-2*pi*i/N), and X[N-k] = conj(X[k])
              (let [re (double-array N)
                    im (double-array N)
                    cos-table (double-array half)
                    sin-table (double-array half)
                    z0r (aget zre 0)
                    z0i (aget zim 0)]
                (fill-twiddles! cos-table sin-table N -1.0)
                (aset re 0 (+ z0r z0i))
                (aset im 0 0.0)
                (aset re half (- z0r z0i))
                (aset im half 0.0)
                (loop [k 1]
                  (when (< k half)
                    (let [zkr (aget zre k)
                          zki (aget zim k)
                          j (- half k)
                          zjr (aget zre j)
                          zji (- (aget zim j))
                          er (* 0.5 (+ zkr zjr))
                          ei (* 0.5 (+ zki zji))
                          dr (* 0.5 (- zkr zjr))
                          di (* 0.5 (- zki zji))
                          odd-r di
                          odd-i (- dr)
                          wr (aget cos-table k)
                          wi (aget sin-table k)
                          tr (- (* wr odd-r) (* wi odd-i))
                          ti (+ (* wr odd-i) (* wi odd-r))
                          xkr (+ er tr)
                          xki (+ ei ti)]
                      (aset re k xkr)
                      (aset im k xki)
                      (aset re (- N k) xkr)
                      (aset im (- N k) (- xki)))
                    (recur (inc k))))
                (finalize-arrays (var fft-real) re im 1.0))))))

(s/fdef fft-real
  :args (s/cat :xs ::vector/vector-finite)
  :ret (s/or :anomaly ::anomalies/anomaly
         :complex-vector ::complex-vector))
