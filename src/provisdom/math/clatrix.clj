(ns provisdom.math.clatrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.vector :as vector]
            [provisdom.math.matrix :as mx]
            [clatrix.core :as clatrix]))

(set! *warn-on-reflection* true)

(declare clatrix rows columns diagonal some-kv transpose eigen-decomposition)

(s/def ::accu ::mx/accu)
(s/def ::by-row? ::mx/by-row?)
(s/def ::row ::mx/row)
(s/def ::column ::mx/column)
(s/def ::rows ::mx/rows)
(s/def ::columns ::mx/columns)
(s/def ::number ::m/number)
(s/def ::vector ::vector/vector)
(s/def ::matrix ::mx/matrix)
(s/def ::S ::diagonal-clatrix)
(s/def ::D ::clatrix)
(s/def ::VT ::clatrix)
(s/def ::L ::lower-triangular-clatrix)
(s/def ::U ::upper-triangular-clatrix)
(s/def ::P ::clatrix)
(s/def ::Q ::clatrix)
(s/def ::R ::clatrix)
(s/def ::rank ::m/int-non-)
(s/def ::eigenvalues ::vector)
(s/def ::eigenvectors ::clatrix)

;;TODO: finish spec'ing all the types below (not the functions)

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

(defn row-clatrix?
  "Returns true is a row Clatrix."
  [x] (clatrix/row? x))

(s/fdef row-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn column-clatrix?
  "Returns true is a column Clatrix."
  [x] (clatrix/column? x))

(s/fdef column-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn square-clatrix?
  "Returns true if a square Clatrix."
  [x] (and (clatrix? x) (clatrix/square? x)))

(s/fdef square-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::square-clatrix (s/with-gen square-clatrix? #(gen/fmap clatrix (s/gen ::mx/square-matrix))))

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
  [x] (and (clatrix? x) (= (transpose x) x)))

(s/fdef symmetric-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::symmetric-clatrix
  (s/with-gen symmetric-clatrix? #(gen/fmap clatrix (s/gen ::mx/symmetric-matrix))))

(defn positive-clatrix?
  "Returns true if a positive definite Clatrix."
  ([x] (positive-clatrix? x {::accu m/*dbl-close*}))
  ([x {::keys [accu] :or {accu true}}]
   (and (symmetric-clatrix? x)
        (or (zero? (rows x))
            (let [eig (eigen-decomposition x)]
              (if eig
                (every? #(> % accu) (::eigenvalues eig))
                false))))))

(s/fdef positive-clatrix?
        :args (s/cat :x any? :accu (s/? (s/keys :opt [::accu])))
        :ret boolean?)

(s/def ::positive-clatrix
  (s/with-gen positive-clatrix?
              #(gen/fmap (fn [m] (clatrix (mx/positive-matrix m (mx/rows m)))) (s/gen ::mx/square-matrix))))

(defn non-negative-clatrix?
  "Returns true if a non-negative Clatrix."
  ([x] (non-negative-clatrix? x {::accu m/*dbl-close*}))
  ([x {::keys [accu] :or {accu true}}]
   (and (symmetric-clatrix? x)
        (or (zero? (rows x))
            (let [eig (eigen-decomposition x)]
              (if eig
                (every? #(m/roughly-non-? % accu) (::eigenvalues eig))
                false))))))

(s/fdef non-negative-clatrix?
        :args (s/cat :x any? :accu (s/? (s/keys :opt [::accu])))
        :ret boolean?)

(defn correlation-clatrix?
  "Returns true if a positive definite Clatrix with a unit diagonal (all ones on the diagonal)."
  ([x] (correlation-clatrix? x {::accu m/*dbl-close*}))
  ([x {::keys [accu] :or {accu true}}]
   (and (clatrix? x) (every? m/one? (diagonal x)) (positive-clatrix? x {::accu accu}))))

(s/fdef correlation-clatrix?
        :args (s/cat :x any? :accu (s/? (s/keys :opt [::accu])))
        :ret boolean?)

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

;;;MATRIX MATH
(defn mx*
  "Multiplies Clatrices."
  ([clatrix-m] clatrix-m)
  ([clatrix-m1 clatrix-m2]
   (if (or (empty-clatrix? clatrix-m1) (empty-clatrix? clatrix-m2))
     (clatrix [[]])
     (clatrix/* clatrix-m1 clatrix-m2)))
  ([clatrix-m1 clatrix-m2 & clatrix-ms] (apply mx* (mx* clatrix-m1 clatrix-m2) clatrix-ms)))

(s/fdef mx*
        :args (s/or :one (s/cat :clatrix-m ::clatrix)
                    :two (s/and (s/cat :clatrix-m1 ::clatrix
                                       :clatrix-m2 ::clatrix)
                                #(= (columns (:clatrix-m1 %)) (rows (:clatrix-m2 %))))
                    :three+ (s/and (s/cat :clatrix-m1 ::clatrix
                                          :clatrix-m2 ::clatrix
                                          :clatrix-ms (s/with-gen (s/* ::clatrix) #(gen/vector (s/gen ::clatrix) 0 2)))
                                   #(not (true? (reduce (fn [lc e] (if (= (rows e) lc) (columns e) (reduced true)))
                                                        (columns (:clatrix-m1 %))
                                                        (cons (:clatrix-m2 %) (:clatrix-ms %)))))))
        :ret ::clatrix)

;;;MATRIX DECOMPOSITION
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
        :ret (s/nilable ::square-clatrix))

(defn eigen-decomposition
  "Returns map with a vector of the real parts of eigenvalues and a Clatrix of eigenvectors.
  A matrix can be decomposed as A=Q × L × Q^-1, where L is the diagonal matrix of `eigenvalues`,
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
   This is the Cholesky square root of a Clatrix, U such that `positive-clatrix-m` = UT × U.
   Note that `positive-clatrix-m` must be positive (semi) definite for this to exist,
      but [[upper-cholesky-decomposition]] requires strict positivity."
  [positive-clatrix-m]
  (if (empty-clatrix? positive-clatrix-m)
    (clatrix [[]])
    (clatrix/cholesky positive-clatrix-m)))

(s/fdef upper-cholesky-decomposition
        :args (s/cat :positive-clatrix-m ::positive-clatrix)
        :ret ::upper-triangular-clatrix)

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a Clatrix.
  The Singular Value Decomposition of Clatrix A is a set of three matrices: D, S and V such that A = D × S × VT.
  Let A be a m × n matrix, then D is a m × p orthogonal matrix,
  S is a p × p diagonal matrix with positive or null elements,
  V is a p × n orthogonal matrix (hence VT is also orthogonal) where p=min(m,n).
  Returns a map containing:
      :S -- diagonal Clatrix S
      :D -- Clatrix D
      :VT -- transpose of Clatrix V
      :rank -- rank."
  [clatrix-m]
  (if (empty-clatrix? clatrix-m)
    {::S    (clatrix [[]])
     ::D    (clatrix [[]])
     ::VT   (clatrix [[]])
     ::rank 0}
    (let [r (clatrix/svd clatrix-m)]
      {::S    (clatrix (mx/diagonal-matrix (vec (:values r))))
       ::D    (:left r)
       ::VT   (:right r)
       ::rank (:rank r)})))

(s/fdef sv-decomposition
        :args (s/cat :clatrix-m ::clatrix)
        :ret (s/keys :req [::S ::D ::VT ::rank]))

(defn condition
  "The `singular-values-clatrix` is the diagonal Clatrix of singular values, the `S`, from an SVD decomposition.
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-clatrix` divided by the minimum element value."
  [singular-values-clatrix]
  (let [vs (diagonal singular-values-clatrix)]
    (if (empty? vs)
      m/nan
      (m/div (apply max vs) (apply min vs) m/nan))))

(s/fdef condition
        :args (s/cat :singular-values-clatrix ::diagonal-clatrix)
        :ret ::number)

(defn lu-decomposition
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    {::L (clatrix [[]]), ::U (clatrix [[]]), ::P (clatrix [[]])}
    (let [r (clatrix/lu square-clatrix-m)]
      {::L (:l r), ::U (:u r), ::P (:p r)})))

(s/fdef lu-decomposition
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret (s/keys :req [::L ::U ::P]))

(defn determinant
  "Calculates the determinant of a square Clatrix."
  [square-clatrix-m]
  (if (empty-clatrix? square-clatrix-m)
    m/nan
    (clatrix/det square-clatrix-m)))

(s/fdef determinant
        :args (s/cat :square-clatrix-m ::square-clatrix)
        :ret ::number)

(defn qr-decomposition
  "Computes the QR decomposition of a Clatrix.
  Returns a map containing Clatrices Q and R.
   Q -- orthogonal factors
   R -- the upper triangular factors (not necessarily an upper triangular Clatrix)"
  [clatrix-m]
  (if (empty-clatrix? clatrix-m)
    {::Q (clatrix [[]]), ::R (clatrix [[]])}
    (let [r (clatrix/qr clatrix-m)]
      {::Q (:q r), ::R (:r r)})))

(s/fdef qr-decomposition
        :args (s/cat :clatrix-m ::clatrix)
        :ret (s/keys :req [::Q ::R]))