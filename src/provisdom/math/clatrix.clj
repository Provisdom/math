(ns provisdom.math.clatrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.vector :as vector]
            [provisdom.math.matrix :as mx]
            [clatrix.core :as clatrix]
            [provisdom.math.random2 :as random]
            [provisdom.math.tensor :as tensor]))

(set! *warn-on-reflection* true)

(declare clatrix rows columns diagonal some-kv transpose eigen-decomposition mx*
         correlation-clatrix-finite-by-squaring positive-semidefinite-clatrix-finite-by-squaring
         positive-definite-clatrix-finite-by-squaring symmetric-clatrix-by-averaging add
         covariance-clatrix->correlation-clatrix assoc-diagonal ===)

(s/def ::exception (partial instance? Exception))
(s/def ::accu ::mx/accu)
(s/def ::by-row? ::mx/by-row?)
(s/def ::row ::mx/row)
(s/def ::column ::mx/column)
(s/def ::rows ::mx/rows)
(s/def ::columns ::mx/columns)
(s/def ::number ::m/number)
(s/def ::matrix ::mx/matrix)
(s/def ::rank ::m/int-non-)
(s/def ::vector ::vector/vector)
(s/def ::vector-finite ::vector/vector-finite)
(s/def ::vector-finite+ ::vector/vector-finite+)
(s/def ::size ::mx/size)
(s/def ::numbers ::mx/numbers)

;;;TYPES
(defn clatrix?
  "Returns true if a Clatrix."
  [x] (clatrix/matrix? x))

(s/fdef clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::clatrix (s/with-gen clatrix? #(gen/fmap clatrix (s/gen ::matrix))))

(defn empty-clatrix?
  "Returns true is an empty Clatrix."
  [x] (and (clatrix? x) (zero? (rows x))))

(s/fdef empty-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::empty-clatrix (s/with-gen empty-clatrix? #(= (clatrix [[]]) %)))

(defn clatrix-finite?
  "Returns true if an Clatrix without infinite numbers."
  [x] (and (clatrix? x) (nil? (some-kv (fn [_ _ number] (m/inf? number)) x))))

(s/fdef clatrix-finite?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::clatrix-finite (s/with-gen clatrix-finite? #(gen/fmap clatrix (s/gen ::mx/matrix-finite))))

(defn row-clatrix?
  "Returns true is a row Clatrix."
  [x] (clatrix/row? x))

(s/fdef row-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::row-clatrix (s/with-gen row-clatrix? #(gen/fmap clatrix (s/gen ::mx/row-matrix))))

(defn column-clatrix?
  "Returns true is a column Clatrix."
  [x] (clatrix/column? x))

(s/fdef column-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::column-clatrix (s/with-gen column-clatrix? #(gen/fmap clatrix (s/gen ::mx/column-matrix))))

(defn square-clatrix?
  "Returns true if a square Clatrix."
  [x] (and (clatrix? x) (clatrix/square? x)))

(s/fdef square-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::square-clatrix (s/with-gen square-clatrix? #(gen/fmap clatrix (s/gen ::mx/square-matrix))))
(s/def ::square-clatrix-finite
  (s/with-gen (s/and square-clatrix? clatrix-finite?)
              #(gen/fmap clatrix (s/gen ::mx/square-matrix-finite))))

(defn diagonal-clatrix?
  "Returns true if a diagonal matrix (the entries outside the main diagonal are all zero)."
  [x] (and (clatrix? x) (nil? (some-kv (fn [r c e] (not (or (= r c) (zero? e)))) x))))

(s/fdef diagonal-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::diagonal-clatrix (s/with-gen diagonal-clatrix? #(gen/fmap clatrix (s/gen ::mx/diagonal-matrix))))

(defn upper-triangular-clatrix?
  "Returns true if an upper triangular matrix (square with the entries below the main diagonal all zero)."
  [x] (and (square-clatrix? x) (nil? (some-kv (fn [r c e] (not (or (<= r c) (zero? e)))) x))))

(s/fdef upper-triangular-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::upper-triangular-clatrix
  (s/with-gen upper-triangular-clatrix? #(gen/fmap clatrix (s/gen ::mx/upper-triangular-matrix))))

(defn lower-triangular-clatrix?
  "Returns true if a lower triangular matrix (square with the entries above the main diagonal all zero)."
  [x] (and (square-clatrix? x) (nil? (some-kv (fn [r c e] (not (or (>= r c) (zero? e)))) x))))

(s/fdef lower-triangular-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::lower-triangular-clatrix
  (s/with-gen lower-triangular-clatrix? #(gen/fmap clatrix (s/gen ::mx/lower-triangular-matrix))))

(defn symmetric-clatrix?
  "Returns true is a symmetric Clatrix."
  [x] (and (clatrix? x) (=== (transpose x) x)))

(s/fdef symmetric-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::symmetric-clatrix
  (s/with-gen symmetric-clatrix? #(gen/fmap clatrix (s/gen ::mx/symmetric-matrix))))

(defn positive-semidefinite-clatrix-finite?
  "Returns true if a finite positive semi-definite Clatrix."
  [x accu]
  (and (symmetric-clatrix? x)
       (clatrix-finite? x)
       (or (zero? (rows x))
           (let [eig (eigen-decomposition x)]
             (if eig
               (every? #(m/roughly-non-? % accu) (::eigenvalues eig))
               false)))))

(s/fdef positive-semidefinite-clatrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::positive-semidefinite-clatrix-finite
  (s/with-gen #(positive-semidefinite-clatrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (positive-semidefinite-clatrix-finite-by-squaring (clatrix m)))
                         (s/gen ::mx/square-matrix-finite))))

(defn positive-definite-clatrix-finite?
  "Returns true if a finite positive definite Clatrix."
  [x accu]
  (and (symmetric-clatrix? x)
       (clatrix-finite? x)
       (or (zero? (rows x))
           (let [eig (eigen-decomposition x)]
             (if eig
               (every? #(> % accu) (::eigenvalues eig))
               false)))))

(s/fdef positive-definite-clatrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::positive-definite-clatrix-finite
  (s/with-gen #(positive-definite-clatrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (positive-definite-clatrix-finite-by-squaring (clatrix m)))
                         (s/gen ::mx/square-matrix-finite))))

(defn correlation-clatrix-finite?
  "Returns true if a finite positive definite Clatrix with a unit diagonal (all ones on the diagonal)."
  [x accu] (and (clatrix? x) (every? m/one? (diagonal x)) (positive-definite-clatrix-finite? x accu)))

(s/fdef correlation-clatrix-finite?
        :args (s/cat :x any? :accu ::accu)
        :ret boolean?)

(s/def ::correlation-clatrix-finite
  (s/with-gen #(correlation-clatrix-finite? % m/*sgl-close*)
              #(gen/fmap (fn [m] (correlation-clatrix-finite-by-squaring (clatrix m)))
                         (s/gen ::mx/square-matrix-finite))))

;;;CONSTRUCTORS
(defn clatrix
  "Returns a Clatrix (Clatrix matrix)."
  [m] (clatrix/clatrix m))

(s/fdef clatrix
        :args (s/cat :m ::matrix)
        :ret ::clatrix)

(defn clatrix->matrix
  "Converts a Clatrix into a matrix."
  [clatrix-m]
  (cond (row-clatrix? clatrix-m) (vector (vec clatrix-m))
        (column-clatrix? clatrix-m) (mapv vector (vec clatrix-m))
        (zero? (rows clatrix-m)) [[]]
        :else (mapv vec clatrix-m)))

(s/fdef clatrix->matrix
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::matrix)

(defn positive-semidefinite-clatrix-finite-by-squaring
  "Returns a finite positive semidefinite Clatrix by first squaring 'square-clatrix-m-finite'."
  [square-clatrix-m-finite]
  (let [size (rows square-clatrix-m-finite)]
    (if (zero? size)
      square-clatrix-m-finite
      (loop [i 0]
        (when (< i 10)
          (let [lower-m (mx* square-clatrix-m-finite
                             (clatrix (mx/diagonal-matrix (repeat size (m/pow 10 (m/one- (m/pow 2 i)))))))
                lower-m (mx* lower-m (transpose lower-m))
                lower-m (symmetric-clatrix-by-averaging lower-m)]
            (if (positive-semidefinite-clatrix-finite? lower-m m/*sgl-close*)
              lower-m
              (recur (inc i)))))))))

(s/fdef positive-semidefinite-clatrix-finite-by-squaring
        :args (s/cat :square-clatrix-m-finite ::square-clatrix-finite)
        :ret (s/nilable ::positive-semidefinite-clatrix-finite))

(defn positive-definite-clatrix-finite-by-squaring
  "Returns a finite positive definite Clatrix squaring it.
  Will tweak original matrix if necessary to ensure positive definite."
  [square-clatrix-m-finite]
  (let [size (rows square-clatrix-m-finite)]
    (if (zero? size)
      square-clatrix-m-finite
      (loop [i 0]
        (when (< i 10)
          (let [lower-m (mx* square-clatrix-m-finite
                             (clatrix (mx/diagonal-matrix (repeat size (m/pow 10 (m/one- (m/pow 2 i)))))))
                lower-m (if (zero? i)
                          lower-m
                          (add lower-m
                               (clatrix
                                 (mx/diagonal-matrix
                                   size (fn [_] (* (if (odd? i) (- 1.0) 1.0) (m/pow 10 (- i m/*sgl-digits*))))))))
                lower-m (mx* lower-m (transpose lower-m))
                lower-m (symmetric-clatrix-by-averaging lower-m)]
            (if (positive-definite-clatrix-finite? lower-m m/*sgl-close*)
              lower-m
              (recur (inc i)))))))))

(s/fdef positive-definite-clatrix-finite-by-squaring
        :args (s/cat :square-clatrix-m-finite ::square-clatrix-finite)
        :ret (s/nilable ::positive-definite-clatrix-finite))

(defn correlation-clatrix-finite-by-squaring
  "Returns a finite Correlation Clatrix by first squaring 'square-clatrix-m'."
  [square-clatrix-m-finite]
  (if (zero? (rows square-clatrix-m-finite))
    square-clatrix-m-finite
    (let [new-m (covariance-clatrix->correlation-clatrix
                  (positive-definite-clatrix-finite-by-squaring square-clatrix-m-finite))
          size (when new-m (rows new-m))]
      (when new-m
        (loop [i 0]
          (when (< i 100)
            (let [lower-m (mx* new-m (clatrix (mx/diagonal-matrix (repeat size (m/one- (* 0.01 i))))))
                  lower-m (symmetric-clatrix-by-averaging lower-m)
                  lower-m (assoc-diagonal lower-m (repeat size 1.0))]
              (if (correlation-clatrix-finite? lower-m m/*sgl-close*)
                lower-m
                (recur (inc i))))))))))

(s/fdef correlation-clatrix-finite-by-squaring
        :args (s/cat :square-clatrix-m-finite ::square-clatrix-finite)
        :ret (s/nilable ::correlation-clatrix-finite))

(defn rnd-positive-definite-clatrix-finite!
  "Returns a finite positive definite Clatrix with a random spectrum.
  The orthogonal matrices are generated by using 2 × `size` composed Householder reflections.
  Alternative #1: Sample from the Inverse-Wishart Distribution.
  Alternative #2: Use [[positive-definite-clatrix-by-squaring]] with a random square matrix."
  [size]
  (if (zero? size)
    (clatrix [[]])
    (loop [i 0]
      (if (< i 100)
        (let [m (clatrix (mx/rnd-spectral-matrix! (vec (take size (random/rand-double-lazy!)))))]
          (if (positive-definite-clatrix-finite? m m/*sgl-close*)
            m
            (recur (inc i))))))))

(s/fdef rnd-positive-definite-clatrix-finite!
        :args (s/cat :size ::size)
        :ret ::positive-definite-clatrix-finite)

(defn rnd-correlation-clatrix-finite!
  "Returns a finite correlation Clatrix from a covariance matrix with a random spectrum.
  The orthogonal matrices are generated by using 2 * `size` composed Householder reflections.
  Alternative #1: Sample Covariance from the Inverse-Wishart Distribution.
  Alternative #2: Use [[correlation-clatrix-by-squaring]] with a random square matrix."
  [size]
  (if (zero? size)
    (clatrix [[]])
    (covariance-clatrix->correlation-clatrix (rnd-positive-definite-clatrix-finite! size))))

(s/fdef rnd-correlation-clatrix-finite!
        :args (s/cat :size ::size)
        :ret ::correlation-clatrix-finite)

;;;MATRIX INFO
(defn rows
  "Returns the number of rows of a Clatrix."
  [clatrix-m] (clatrix/nrows clatrix-m))

(s/fdef rows
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::rows)

(defn columns
  "Returns the number of columns of a Clatrix."
  [clatrix-m] (clatrix/ncols clatrix-m))

(s/fdef columns
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::columns)

(defn get-entry
  "Returns the specified Clatrix element."
  [clatrix-m row column] (try (clatrix/get clatrix-m row column) (catch Exception _ m/nan)))

(s/fdef get-entry
        :args (s/and (s/cat :clatrix-m ::clatrix :row ::row :column ::column)
                     #(and (< (:row %) (rows (:clatrix-m %))) (< (:column %) (columns (:clatrix-m %)))))
        :ret ::number)

(defn get-row
  "Gets a `row` of an Clatrix, as a vector."
  [clatrix-m row] (try (vec (clatrix/slice-row clatrix-m row)) (catch Exception _ nil)))

(s/fdef get-row
        :args (s/and (s/cat :clatrix-m ::clatrix :row ::row)

                     #(< (:row %) (rows (:clatrix-m %))))
        :ret (s/nilable ::vector))

(defn get-column
  "Gets a `column` of an Clatrix, as a vector."
  [clatrix-m column] (try (vec (clatrix/slice-column clatrix-m column)) (catch Exception _ nil)))

(s/fdef get-column
        :args (s/and (s/cat :clatrix-m ::clatrix :column ::column)
                     #(< (:column %) (columns (:clatrix-m %))))
        :ret (s/nilable ::vector))

(defn diagonal
  "Returns diagonal of a Clatrix."
  [clatrix-m] (vec (clatrix/diag clatrix-m)))

(s/fdef diagonal
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::vector)

(defn some-kv
  "Returns the first logical true value of (pred row column number) for any number in Clatrix, else nil.
  Options: `::by-row?` (default: true)."
  ([pred clatrix-m] (some-kv pred clatrix-m {::by-row? true}))
  ([pred clatrix-m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row? clatrix-m (transpose clatrix-m))
         rows (rows mt)]
     (loop [c 0, s (clatrix->matrix mt)]
       (when (< c rows)
         (or (vector/some-kv #(pred c % %2) (first s)) (recur (inc c) (next s))))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :row ::row :column ::column :number ::m/number)
                                    :ret boolean?)
                     :clatrix-m ::clatrix
                     :opts (s/? (s/keys :opt [::by-row?])))
        :ret (s/nilable ::m/number))

;;;MATRIX MANIPULATION
(defn transpose
  "Returns transpose of a Clatrix."
  [clatrix-m] (clatrix/t clatrix-m))

(s/fdef transpose
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::clatrix)

(defn assoc-diagonal
  "Sets a diagonal in a Clatrix using the specified numbers."
  [clatrix-m numbers]
  (cond (empty-clatrix? clatrix-m) (clatrix (mx/diagonal-matrix numbers))

        (= (count numbers) (count (diagonal clatrix-m)))
        (let [v (vec numbers)]
          (clatrix (vec (for [row (range (count numbers))]
                          (assoc (get-row clatrix-m row) row (get v row 0.0))))))

        :else nil))

(s/fdef assoc-diagonal
        :args (s/cat :clatrix-m ::clatrix :numbers ::numbers)
        :ret (s/nilable ::clatrix))

(defn symmetric-clatrix-by-averaging
  "Returns a symmetric Clatrix where each element above or below the diagonal is equal to the
  average of the corresponding numbers.
  This is useful to help with rounding errors."
  [square-clatrix-m]
  (let [size (rows square-clatrix-m)]
    (clatrix (mx/compute-matrix size size (fn [row column]
                                            (if (== column row)
                                              (or (get-entry square-clatrix-m row column) 0.0)
                                              (* 0.5 (+ (or (get-entry square-clatrix-m row column) 0.0)
                                                        (or (get-entry square-clatrix-m column row) 0.0)))))))))

(s/fdef symmetric-clatrix-by-averaging
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret ::symmetric-clatrix)

(defn correlation-clatrix->covariance-clatrix
  "Returns Covariance Clatrix from a Correlation Clatrix."
  [correlation-clatrix-m-finite variances]
  (when (= (count variances) (rows correlation-clatrix-m-finite))
    (if (zero? (count variances))
      correlation-clatrix-m-finite
      (let [sqrt-m (clatrix (mx/diagonal-matrix (mapv m/sqrt variances)))
            cov (mx* sqrt-m correlation-clatrix-m-finite sqrt-m)
            cov (symmetric-clatrix-by-averaging cov)]
        (when (positive-definite-clatrix-finite? cov m/*sgl-close*) cov)))))

(s/fdef correlation-clatrix->covariance-clatrix
        :args (s/cat :correlation-clatrix-m-finite ::correlation-clatrix-finite :variances ::vector-finite+)
        :ret (s/nilable ::positive-definite-clatrix-finite))

(defn covariance-clatrix->correlation-clatrix
  "Returns Correlation Clatrix from a Covariance Clatrix."
  [covariance-clatrix-m-finite]
  (if (zero? (rows covariance-clatrix-m-finite))
    covariance-clatrix-m-finite
    (let [inv-sqrt (clatrix (mx/diagonal-matrix (map #(m/pow % -0.5) (diagonal covariance-clatrix-m-finite))))
          corr (mx* inv-sqrt covariance-clatrix-m-finite inv-sqrt)
          corr (symmetric-clatrix-by-averaging corr)
          corr (assoc-diagonal corr (repeat (rows inv-sqrt) 1.0))]
      (when (correlation-clatrix-finite? corr m/*sgl-close*) corr))))

(s/fdef covariance-clatrix->correlation-clatrix
        :args (s/cat :covariance-clatrix-m-finite ::positive-definite-clatrix-finite)
        :ret (s/nilable ::correlation-clatrix-finite))

;;;MATRIX MATH
(defn ===
  "Clatrix equality that works with NaN."
  ([clatrix-m] true)
  ([clatrix-m1 clatrix-m2] (tensor/=== (clatrix->matrix clatrix-m1) (clatrix->matrix clatrix-m2)))
  ([clatrix-m1 clatrix-m2 & clatrix-ms]
   (apply tensor/=== (clatrix->matrix clatrix-m1) (clatrix->matrix clatrix-m2) (map clatrix->matrix clatrix-ms))))

(s/fdef ===
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two+ (s/cat :clatrix-m1 ::clatrix
                                 :clatrix-m2 ::clatrix
                                 :clatrix-ms (s/* ::clatrix)))
        :ret boolean?)

(defn mx*
  "Multiplies Clatrices."
  ([clatrix-m] clatrix-m)
  ([clatrix-m1 clatrix-m2]
   (when (= (columns clatrix-m1) (rows clatrix-m2))
     (if (zero? (rows clatrix-m1))
       clatrix-m1
       (clatrix/* clatrix-m1 clatrix-m2))))
  ([clatrix-m1 clatrix-m2 & clatrix-ms]
   (when-let [clatrix-m3 (mx* clatrix-m1 clatrix-m2)] (apply mx* clatrix-m3 clatrix-ms))))

(s/fdef mx*
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two+ (s/cat :clatrix-m1 ::clatrix
                                 :clatrix-m2 ::clatrix
                                 :clatrix-ms (s/* ::clatrix)))
        :ret (s/nilable ::clatrix))

(defn add
  "Clatrix addition."
  ([clatrix-m] clatrix-m)
  ([clatrix-m1 clatrix-m2]
   (if (and (zero? (rows clatrix-m1)) (zero? (rows clatrix-m2)))
     clatrix-m1
     (try (clatrix/+ clatrix-m1 clatrix-m2)
          (catch Exception _ nil))))
  ([clatrix-m1 clatrix-m2 & clatrix-ms]
   (when-let [clatrix-m3 (add clatrix-m1 clatrix-m2)] (apply add clatrix-m3 clatrix-ms))))

(s/fdef add
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two+ (s/cat :clatrix-m1 ::clatrix
                                 :clatrix-m2 ::clatrix
                                 :clatrix-ms (s/* ::clatrix)))
        :ret (s/nilable ::clatrix))

(defn subtract
  "Clatrix subtraction."
  ([clatrix-m] clatrix-m)
  ([clatrix-m1 clatrix-m2]
   (if (and (zero? (rows clatrix-m1)) (zero? (rows clatrix-m2)))
     clatrix-m1
     (try (clatrix/- clatrix-m1 clatrix-m2)
          (catch Exception _ nil))))
  ([clatrix-m1 clatrix-m2 & clatrix-ms]
   (when-let [clatrix-m3 (subtract clatrix-m1 clatrix-m2)] (apply subtract clatrix-m3 clatrix-ms))))

(s/fdef subtract
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two+ (s/cat :clatrix-m1 ::clatrix
                                 :clatrix-m2 ::clatrix
                                 :clatrix-ms (s/* ::clatrix)))
        :ret (s/nilable ::clatrix))

;;;MATRIX DECOMPOSITION
(s/def ::inverse (s/nilable ::square-clatrix))
(defn inverse
  "Computes the inverse of a number, vector, or matrix.
   This is done via Gaussian elimination.
   It can be numerically very unstable if the matrix is nearly singular.
   Positivity and symmetry hints are used to cause the solve to use optimized LAPACK routines."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    (clatrix [[]])
    (try (clatrix/i (clatrix/maybe-positive (clatrix/maybe-symmetric square-clatrix-m)))
         (catch Exception _ nil))))

(s/fdef inverse
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret (s/nilable ::inverse))

(defn determinant
  "Calculates the determinant of a square Clatrix."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    m/nan
    (clatrix/det square-clatrix-m)))

(s/fdef determinant
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret ::number)

(s/def ::L ::lower-triangular-clatrix)
(s/def ::U ::upper-triangular-clatrix)
(s/def ::LU-permutation (s/nilable ::square-clatrix))
(defn lu-decomposition
  "Returns a map containing:
      ::L -- the lower triangular factor
      ::U -- the upper triangular factor
      ::LU-permutation -- the permutation matrix."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    {::L (clatrix [[]]), ::U (clatrix [[]]), ::LU-permutation (clatrix [[]])}
    (let [r (clatrix/lu square-clatrix-m)]
      {::L (:l r), ::U (:u r), ::LU-permutation (:p r)})))

(s/fdef lu-decomposition
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret (s/keys :req [::L ::U ::LU-permutation]))

(s/def ::eigenvalues ::vector)
(s/def ::eigenvectors ::clatrix)
(defn eigen-decomposition
  "Returns map with a vector of the real parts of eigenvalues and a Clatrix of eigenvectors.
  A matrix can be decomposed as A = Q × L × Q^-1, where L is the diagonal matrix of `eigenvalues`,
  Q is the `eigenvalues` matrix, and Q^-1 is the inverse of Q."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    {::eigenvalues  []
     ::eigenvectors (clatrix [[]])}
    (try (let [r (clatrix/eigen (clatrix/maybe-symmetric square-clatrix-m))]
           (when-not (or (:ivalues r) (:ivectors r))
             {::eigenvalues  (vec (:values r))
              ::eigenvectors (:vectors r)}))
         (catch Exception _ nil))))

(s/fdef eigen-decomposition
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret (s/nilable (s/keys :req [::eigenvalues ::eigenvectors])))

(defn upper-cholesky-decomposition
  "Computes the Cholesky decomposition of a Clatrix.
   This is the Cholesky square root of a Clatrix, U such that `positive-definite-clatrix-m` = UT × U.
   Note that `positive-definite-clatrix-m` must be positive (semi) definite for this to exist,
      but [[upper-cholesky-decomposition]] requires strict positivity."
  [positive-definite-clatrix-m-finite]
  (if (empty-clatrix? positive-definite-clatrix-m-finite)
    (clatrix [[]])
    (try (clatrix/cholesky positive-definite-clatrix-m-finite)
         (catch Exception e (ex-info (.getMessage e) {:fn (var upper-cholesky-decomposition)})))))

(s/fdef upper-cholesky-decomposition
        :args (s/cat :positive-definite-clatrix-m-finite ::positive-definite-clatrix-finite)
        :ret (s/or :exception ::exception
                   :res ::upper-triangular-clatrix))

(s/def ::svd-left ::clatrix-finite)
(s/def ::svd-right ::clatrix-finite)
(s/def ::singular-values ::diagonal-clatrix)
(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a Clatrix.
  The Singular Value Decomposition of `clatrix-m-finite` is a set of three
  matrices: `svd-left`, `singular-values`, and `svd-right` such that:
  `clatrix-m-finite` = `svd-left` × `singular-values` × `svd-right`.
  Let `clatrix-m-finite` be a m × n matrix, then `svd-left` is a m × p orthogonal matrix of the left singular vectors,
  `singular-values` is a p × p diagonal matrix of singular values with positive or nil elements,
  and are ordered from largest to smallest.
  `svd-right` is a p × n orthogonal matrix of the right singular vectors where p = min(m,n).
  Note that Identity Matrix = (transpose `svd-left`) × `svd-left` = `svd-right` × (transpose `svd-right`).
  Returns a map containing:
  ::svd-left -- finite Clatrix of left singular vectors
  ::singular-values -- diagonal Clatrix
  ::svd-right -- transpose of finite Clatrix of right singular vectors
  ::rank -- rank."
  [clatrix-m-finite]
  (if (empty-clatrix? clatrix-m-finite)
    {::svd-left        (clatrix [[]])
     ::singular-values (clatrix [[]])
     ::svd-right       (clatrix [[]])
     ::rank            0}
    (try (let [r (clatrix/svd clatrix-m-finite)]
           {::svd-left        (:left r)
            ::singular-values (clatrix (mx/diagonal-matrix (vec (:values r))))
            ::svd-right       (:right r)
            ::rank            (:rank r)})
         (catch Exception e (ex-info (.getMessage e) {:fn (var sv-decomposition)})))))

(s/fdef sv-decomposition
        :args (s/cat :clatrix-m-finite ::clatrix-finite)
        :ret (s/or :exception ::exception
                   :res (s/keys :req [::svd-left ::singular-values ::svd-right ::rank])))

(defn condition
  "The `singular-values-clatrix` is the diagonal matrix of ::singular-values from [[sv-decomposition]].
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-clatrix` divided by the minimum element value."
  [singular-values-clatrix]
  (if (zero? (rows singular-values-clatrix))
    m/nan
    (let [vs (diagonal singular-values-clatrix)]
      (m/div (apply max vs) (apply min vs) m/nan))))

(s/fdef condition
        :args (s/cat :singular-values-clatrix ::singular-values)
        :ret ::number)

(s/def ::Q ::clatrix)
(s/def ::R ::clatrix)
(defn qr-decomposition
  "Computes the QR decomposition of a Clatrix.
  Returns a map containing Clatrices Q and R.
   Q -- orthogonal factors
   R -- the upper triangular factors (not necessarily an upper triangular Clatrix)."
  [clatrix-m]
  (if (empty-clatrix? clatrix-m)
    {::Q (clatrix [[]]), ::R (clatrix [[]])}
    (let [r (clatrix/qr clatrix-m)]
      {::Q (:q r), ::R (:r r)})))

(s/fdef qr-decomposition
        :args (s/cat :clatrix-m ::clatrix)
        :ret (s/keys :req [::Q ::R]))