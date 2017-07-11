(ns provisdom.math.clatrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.vector :as vector]
            [provisdom.math.matrix :as mx]
            [clojure.core.matrix :as mxc]
            [clatrix.core :as clx]))

(set! *warn-on-reflection* true)

(declare clatrix? clatrix square-clatrix? maybe-convert-clatrix-row-or-column eigenvalues
         rows columns transpose diagonal diagonal-clatrix? upper-triangular-clatrix?
         lower-triangular-clatrix? symmetric-clatrix? some-kv)

(s/def ::rows ::mx/rows)
(s/def ::columns ::mx/columns)
(s/def ::number ::m/number)
(s/def ::vector ::vector/vector)
(s/def ::matrix ::mx/matrix)
(s/def ::clatrix (s/with-gen clatrix? #(gen/fmap clatrix (s/gen ::matrix))))
(s/def ::square-clatrix (s/with-gen square-clatrix? #(gen/fmap clatrix (s/gen ::mx/square-matrix))))
(s/def ::diagonal-clatrix (s/with-gen diagonal-clatrix? #(gen/fmap clatrix (s/gen ::mx/diagonal-matrix))))
(s/def ::upper-triangular-clatrix
  (s/with-gen upper-triangular-clatrix? #(gen/fmap clatrix (s/gen ::mx/upper-triangular-matrix))))
(s/def ::lower-triangular-clatrix
  (s/with-gen lower-triangular-clatrix? #(gen/fmap clatrix (s/gen ::mx/lower-triangular-matrix))))
(s/def ::symmetric-clatrix
  (s/with-gen symmetric-clatrix? #(gen/fmap clatrix (s/gen ::mx/symmetric-matrix))))
(s/def ::S ::diagonal-clatrix)
(s/def ::U ::upper-triangular-clatrix)
(s/def ::VT ::symmetric-clatrix)
(s/def ::L ::lower-triangular-matrix)
(s/def ::P ::clatrix)
(s/def ::Q ::symmetric-clatrix)
(s/def ::R ::upper-triangular-clatrix)
(s/def ::rank ::m/int-non-)

;;;TYPES
(defn clatrix?
  "Returns true if a Clatrix."
  [x] (clx/matrix? x))

(s/fdef clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn empty-clatrix?
  "Returns true is an empty Clatrix."
  [x] (and (clatrix? x) (zero? (rows x))))

(s/fdef empty-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn row-clatrix?
  "Returns true is a row Clatrix."
  [x] (clx/row? x))

(s/fdef row-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn column-clatrix?
  "Returns true is a column Clatrix."
  [x] (clx/column? x))

(s/fdef column-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn square-clatrix?
  "Returns true if a square Clatrix."
  [x] (and (clx/matrix? x) (clx/square? x)))

(s/fdef square-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn diagonal-clatrix?
  "Returns true if a diagonal matrix (the entries outside the main diagonal are all zero)."
  [x] (and (clatrix? x) (nil? (some-kv (fn [i j e] (not (or (= i j) (zero? e)))) x))))

(s/fdef diagonal-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn upper-triangular-clatrix?
  "Returns true if an upper triangular matrix (the entries below the main diagonal are all zero)."
  [x] (and (clatrix? x) (nil? (some-kv (fn [i j e] (not (or (<= i j) (zero? e)))) x))))

(s/fdef upper-triangular-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn lower-triangular-clatrix?
  "Returns true if a lower triangular matrix (the entries above the main diagonal are all zero)."
  [x] (and (clatrix? x) (nil? (some-kv (fn [i j e] (not (or (>= i j) (zero? e)))) x))))

(s/fdef lower-triangular-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn symmetric-clatrix?
  "Returns true is a symmetric Clatrix."
  [x] (and (clx/matrix? x) (= (transpose x) x)))

(s/fdef symmetric-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn positive-clatrix?
  "Returns true if a positive definite Clatrix."
  ([x] (positive-clatrix? x m/*dbl-close*))
  ([x accu] (and (symmetric-clatrix? x) (every? #(> % accu) (eigenvalues x)))))

(s/fdef positive-clatrix?
        :args (s/cat :x any? :accu (s/? ::accu))
        :ret boolean?)

(defn clatrix-with-unit-diagonal?
  "Returns true is a Clatrix with a unit diagonal (all ones on the diagonal)."
  [x] (and (clatrix? x) (every? m/one? (diagonal x))))

(s/fdef clatrix-with-unit-diagonal?
        :args (s/cat :x any?)
        :ret boolean?)

(defn positive-clatrix-with-unit-diagonal?
  "Returns true if a positive definite Clatrix with a unit diagonal (all ones on the diagonal)."
  ([x] (positive-clatrix-with-unit-diagonal? x))
  ([x accu] (and (clatrix-with-unit-diagonal? x) (positive-clatrix? x accu))))

(s/fdef positive-clatrix-with-unit-diagonal?
        :args (s/cat :x any? :accu (s/? ::accu))
        :ret boolean?)

(defn non-negative-clatrix?
  "Returns true if a non-negative Clatrix."
  ([x] (non-negative-clatrix? x m/*dbl-close*))
  ([x accu] (and (symmetric-clatrix? x) (every? #(m/roughly-non-? % accu) (eigenvalues x)))))

(s/fdef non-negative-clatrix?
        :args (s/cat :x any? :accu (s/? ::accu))
        :ret boolean?)

;;;CONSTRUCTORS
(defn clatrix
  "Returns a Clatrix (Clatrix matrix)."
  [m] (mxc/matrix :clatrix m))

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

;;;MATRIX INFO
(defn rows
  "Returns the number of rows of a Clatrix."
  [clatrix-m] (clx/nrows clatrix-m))

(s/fdef rows
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::rows)

(defn columns
  "Returns the number of columns of a Clatrix."
  [clatrix-m] (clx/ncols clatrix-m))

(s/fdef columns
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::columns)

(defn diagonal
  "Returns diagonal of a Clatrix."
  [clatrix-m] (vec (clx/diag clatrix-m)))

(s/fdef diagonal
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::vector)

(defn some-kv
  "Returns the first logical true value of (pred row column e) for any e in Clatrix, else nil.
  Options: `::by-row?` (default: true)."
  ([pred clatrix-m] (some-kv pred clatrix-m {::by-row? true}))
  ([pred clatrix-m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row? clatrix-m (transpose clatrix-m))
         rows (rows mt)]
     (loop [c 0, s mt]
       (when (< c rows)
         (or (vector/some-kv #(pred c % %2) (first s)) (recur (inc c) (next s))))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :r ::row :c ::column :e ::m/number)
                                    :ret boolean?)
                     :clatrix-m ::clatrix
                     :opts (s/? (s/keys :opt [::by-row?])))
        :ret (s/nilable ::m/number))

;;;MATRIX MANIPULATION
(defn transpose
  "Returns transpose of a Clatrix."
  [clatrix-m] (clx/t clatrix-m))

(s/fdef transpose
        :args (s/cat :clatrix-m ::clatrix)
        :ret ::clatrix)

;;;MATRIX MATH
(defn mx*
  "Multiplies Clatrices."
  ([clatrix-m] clatrix-m)
  ([clatrix-m1 clatrix-m2]
   (if (or (empty-clatrix? clatrix-m1) (empty-clatrix? clatrix-m2))
     (clatrix [[]])
     (clx/* clatrix-m1 clatrix-m2)))
  ([clatrix-m1 clatrix-m2 & clatrix-ms] (apply clx/* clatrix-m1 clatrix-m2 clatrix-ms)))

(s/fdef mx*
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two+ (s/cat :clatrix-m1 ::clatrix
                                 :clatrix-m2 ::clatrix
                                 :clatrix-ms (s/? (s/keys* :opt [::clatrix])))))

;;;MATRIX DECOMPOSITION
(defn inverse
  "Computes the inverse of a number, vector, or matrix.
   This is done via Gaussian elimination.
   It can be numerically very unstable if the matrix is nearly singular.
   Positivity and symmetry hints are used to cause the solve to use optimized LAPACK routines."
  [clatrix-square-m] (clx/i (clx/maybe-positive (clx/maybe-symmetric clatrix-square-m))))

(s/fdef inverse
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::clatrix-square-matrix)

(defn upper-cholesky-decomposition
  "Computes the Cholesky decomposition of a Clatrix.
   This is the Cholesky square root of a Clatrix, U such that (matrix-multiply UT U) = m
   Note that `clatrix-square-m` must be positive (semi) definite for this to exist,
      but [[upper-cholesky-decomposition]] requires strict positivity."
  [clatrix-square-m] (clx/cholesky clatrix-square-m))

(s/fdef upper-cholesky-decomposition
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::clatrix)

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a Clatrix.
  The Singular Value Decomposition of Clatrix A is a set of three matrices: U, S and V such that A = U × S × VT.
  Let A be a m × n matrix, then U is a m × p orthogonal matrix,
  S is a p × p diagonal matrix with positive or null elements,
  V is a p × n orthogonal matrix (hence VT is also orthogonal) where p=min(m,n).
  Returns a map containing:
      :S -- diagonal Clatrix S
      :U -- Clatrix U
      :VT -- transpose of Clatrix V
      :rank -- rank."
  [clatrix-m]
  (let [r (clx/svd clatrix-m)]
    {::S    (clatrix (mx/diagonal-matrix (vec (:values r))))
     ::U    (:left r)
     ::VT   (:right r)
     ::rank (:rank r)}))

(s/fdef sv-decomposition
        :args (s/cat :clatrix-m ::clatrix)
        :ret (s/keys :req [::S ::U ::VT ::rank]))

(defn condition
  "The `singular-values-clatrix` is the diagonal Clatrix of singular values, the `S`, from an SVD decomposition.
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-clatrix` divided by the minimum element value."
  [singular-values-clatrix]
  (let [vs (flatten singular-values-clatrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :clatrix-singular-values-matrix ::clatrix)
        :ret (s/nilable ::number))

(defn lu-decomposition
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix."
  [clatrix-square-m]
  (let [r (clx/lu clatrix-square-m)]
    {::L (:l r), ::U (:u r), ::P (:p r)}))

(s/fdef lu-decomposition
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret (s/keys :req [::L ::U ::P]))

(defn determinant
  "Calculates the determinant of a square Clatrix."
  [clatrix-square-m] (clx/det clatrix-square-m))

(s/fdef determinant
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::number)

(defn qr-decomposition
  "Computes the QR decomposition of a Clatrix.
  Returns a map containing Clatrices Q and R.
   Q -- orthogonal factors
   R -- the upper triangular factors."
  [clatrix-m]
  (let [r (clx/qr clatrix-m)]
    {::Q (:q r), ::R (:r r)}))

(s/fdef qr-decomposition
        :args (s/cat :clatrix-m ::clatrix)
        :ret (s/keys :req [::Q ::R]))

(defn eigenvalues
  "Returns vector of the real parts of eigenvalues."
  [clatrix-square-m]
  (let [r (clx/eigen (clx/maybe-symmetric (clatrix clatrix-square-m)))]
    (when-not (or (:ivalues r) (:ivectors r)) (vec (:values r)))))

(s/fdef eigenvalues
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::vector)