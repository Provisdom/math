(ns provisdom.math.clatrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.matrix :as mx]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix :as mxc]
            [clatrix.core :as clx]
            [provisdom.math.vector :as vector]
            [provisdom.math.tensor :as tensor]))

(set! *warn-on-reflection* true)

(declare clatrix? clatrix square-clatrix? maybe-convert-clatrix-row-or-column eigenvalues rrqr-decomposition)

(s/def ::number ::m/number)
(s/def ::matrix ::mx/matrix)
(s/def ::square-matrix ::mx/square-matrix)
(s/def ::clatrix (s/with-gen clatrix? #(gen/fmap clatrix (s/gen ::matrix))))
(s/def ::clatrix-square-matrix (s/with-gen square-clatrix? #(gen/fmap clatrix (s/gen ::square-matrix))))
(s/def ::clatrix-vector ::vector/vector)

;;;CLATRIX
(defn clatrix?
  "Returns true if a Clatrix."
  [x] (clx/matrix? x))

(s/fdef clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn square-clatrix?
  "Returns true if a square Clatrix."
  [x] (and (clx/matrix? x) (= (clx/nrows x) (clx/ncols x))))

(s/fdef square-clatrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn- maybe-convert-clatrix-row-or-column
  [m]
  (cond (not (clatrix? m)) m
        (mx/row-matrix? m) [(vector/to-vector m)]
        (mx/column-matrix? m) (mxc/to-nested-vectors
                                (mx/column-matrix :persistent-vector (vector/to-vector m)))
        (zero? (mx/ecount m)) []
        :else m))

(defn clatrix
  "Returns a matrix using the Clatrix matrix implementation."
  [m] (mxc/matrix :clatrix m))

(s/fdef clatrix
        :args (s/cat :m ::matrix)
        :ret ::clatrix)

;;;MATRIX TYPES
(defn positive-matrix?
  "Returns true if `m` is a positive definite matrix."
  ([square-m] (positive-matrix? square-m m/*dbl-close*))
  ([square-m accu] (and (mx/symmetric-matrix? square-m) (every? #(> % accu) (eigenvalues square-m)))))

(s/fdef positive-matrix?
        :args (s/cat :square-m ::square-matrix :accu (s/? ::accu))
        :ret boolean?)

(defn positive-matrix-with-unit-diagonal?
  "Returns true if `m` has a unit diagonal and is a positive definite matrix."
  ([m] (and (mx/matrix-with-unit-diagonal? m) (positive-matrix? m)))
  ([m accu] (and (mx/matrix-with-unit-diagonal? m) (positive-matrix? m accu))))

(s/fdef positive-matrix-with-unit-diagonal?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

(defn non-negative-matrix?
  "Returns true if `m` is a non-negative matrix."
  ([m] (non-negative-matrix? m m/*dbl-close*))
  ([m accu] (and (mx/symmetric-matrix? m) (every? #(m/roughly-non-? % accu) (eigenvalues m)))))

(s/fdef non-negative-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

;;;MATRIX DECOMPOSITION
(defn inverse
  "Computes the inverse of a number, vector, or matrix.
   This is done via Gaussian elmination.
   It can be numerically very unstable if the matrix is nearly singular.
   Positivity and symmetry hints are used to cause `solve` to use optimized LAPACK routines."
  [clatrix-square-m] (clx/i (clx/maybe-positive (clx/maybe-symmetric clatrix-square-m))))

(s/fdef inverse
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::clatrix-square-matrix)

(defn upper-cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix.
   This is the Cholesky square root of a matrix, U such that (matrix-multiply UT U) = m
   Note that m must be positive (semi) definite for this to exist,
      but [[upper-cholesky-decomposition]] requires strict positivity."
  [clatrix-square-m] (clx/cholesky clatrix-square-m))

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a matrix.
The Singular Value Decomposition of matrix A is a set of three
   matrices: U, S and V such that A = U × S × VT.
Let A be a m × n matrix, then U is a m × p orthogonal matrix,
   S is a p × p diagonal matrix with positive or null elements,
V is a p × n orthogonal matrix (hence VT is also orthogonal) where p=min(m,n).
Returns a map containing:
      :S -- diagonal matrix S
      :U -- matrix U
      :VT -- transpose of matrix V
      :rank -- rank"
  [clatrix-m]
  (let [r (clx/svd clatrix-m)]
    {::S    (mx/diagonal-matrix clatrix-m (:values r))
     ::U    (:left r)
     ::VT   (:right r)
     ::rank (:rank r)}))

(defn condition
  "The `clatrix-singular-values-matrix` is the diagonal matrix of singular values, the `S`, from an SVD decomposition.
  Returns the norm2 condition number,
  which is the maximum element value from the singular-values-matrix divided by the minimum element value."
  [clatrix-singular-values-matrix]
  (let [vs (flatten clatrix-singular-values-matrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :clatrix-singular-values-matrix ::clatrix)
        :ret (s/nilable ::number))

(defn lu-decomposition
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix"
  [clatrix-square-m]
  (let [r (clx/lu clatrix-square-m)]
    {::L (:l r), ::U (:u r), ::P (:p r)}))

;;could instead be calculated from [[lu-decomposition-with-permutation-matrix]]
;;probably better to use apache (or another library)
(defn determinant
  "Calculates the determinant of a square matrix."          ;;test these
  [clatrix-square-m] (or (clx/det clatrix-square-m) (mxc/det clatrix-square-m)))

(s/fdef determinant
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::number)

(defn qr-decomposition
  "Computes the QR decomposition of a matrix.
Returns a vector containing two matrices [Q R]
Intended usage: (let [[Q R] (qr-decomposition M)] ....)
   Q -- orthogonal factors
   R -- the upper triangular factors"
  [clatrix-m]
  (let [r (clx/qr clatrix-m)]
    {::Q (:q r) ::R (:r r)}))

(defn eigenvalues
  "Returns vector of real parts of eigenvalues."
  [clatrix-square-m]
  (let [r (clx/eigen (clx/maybe-symmetric (clatrix clatrix-square-m)))]
    (when-not (or (:ivalues r) (:ivectors r)) (:values r))))

(s/fdef eigenvalues
        :args (s/cat :clatrix-square-m ::clatrix-square-matrix)
        :ret ::clatrix-vector)