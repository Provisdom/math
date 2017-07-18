(ns provisdom.math.apache-matrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.matrix :as mx]
            [provisdom.math.vector :as vector]
            [provisdom.math.tensor :as tensor]
            [provisdom.math.arrays :as ar])
  (:import [org.apache.commons.math3.linear RealMatrix Array2DRowRealMatrix RealVector
                                            QRDecomposition LUDecomposition CholeskyDecomposition
                                            RectangularCholeskyDecomposition Array2DRowRealMatrix
                                            EigenDecomposition SingularValueDecomposition RRQRDecomposition
                                            DecompositionSolver ConjugateGradient SymmLQ
                                            PreconditionedIterativeLinearSolver RealLinearOperator]))

(set! *warn-on-reflection* true)

(declare apache-matrix? apache-square-matrix? apache-matrix eigenvalues rrqr-decomposition)

(s/def ::accu ::tensor/accu)
(s/def ::number ::m/number)
(s/def ::vector ::vector/vector)
(s/def ::matrix ::mx/matrix)
(s/def ::apache-matrix (s/with-gen apache-matrix? #(gen/fmap apache-matrix (s/gen ::matrix))))
(s/def ::square-apache-matrix (s/with-gen square-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/square-matrix))))
(s/def ::diagonal-apache-matrix
  (s/with-gen diagonal-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/diagonal-matrix))))
(s/def ::upper-triangular-apache-matrix
  (s/with-gen upper-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/upper-triangular-matrix))))
(s/def ::lower-triangular-apache-matrix
  (s/with-gen lower-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/lower-triangular-matrix))))
(s/def ::symmetric-apache-matrix
  (s/with-gen symmetric-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/symmetric-matrix))))
(s/def ::positive-apache-matrix
  (s/with-gen positive-apache-matrix?
              #(gen/fmap (fn [m] (apache-matrix (mx/positive-matrix m (mx/rows m)))) (s/gen ::mx/square-matrix))))
(s/def ::L ::lower-triangular-apache-matrix)
(s/def ::U ::upper-triangular-apache-matrix)
(s/def ::UT ::lower-triangular-apache-matrix)
(s/def ::V ::apache-matrix)
(s/def ::VT ::apache-matrix)
(s/def ::B ::apache-matrix)
(s/def ::S ::diagonal-apache-matrix)
(s/def ::P ::apache-matrix)
(s/def ::Q ::apache-matrix)
(s/def ::QT ::apache-matrix)
(s/def ::R ::apache-matrix)
(s/def ::D ::apache-matrix)
(s/def ::inverse ::apache-square-matrix)
(s/def ::determinant ::number)
(s/def ::rank ::m/int-non-)

;;;APACHE MATRIX
(defn apache-matrix?
  "Returns true if an Apache Commons matrix."
  [x] (instance? Array2DRowRealMatrix x))

(s/fdef apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn apache-square-matrix?
  "Returns true if an Apache Commons square matrix."
  [x]
  (and (instance? Array2DRowRealMatrix x)
       (= (.getColumnDimension ^Array2DRowRealMatrix x)
          (.getRowDimension ^Array2DRowRealMatrix x))))

(s/fdef apache-square-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(defn apache-matrix
  "Returns a matrix using the Apache Commons matrix implementation."
  [m] (Array2DRowRealMatrix. ^"[[D" (ar/jagged-2D-array :d m)))

(s/fdef apache-matrix
        :args (s/cat :m ::matrix)
        :ret ::apache-matrix)

;;;MATRIX TYPES
(defn positive-matrix?
  "Returns true if `m` is a positive definite matrix."
  ([apache-square-m] (positive-matrix? apache-square-m m/*dbl-close*))
  ([apache-square-m accu]
   (and (mx/symmetric-matrix? apache-square-m) (every? #(> % accu) (eigenvalues apache-square-m)))))

(s/fdef positive-matrix?
        :args (s/cat :apache-square-m ::apache-square-matrix :accu (s/? ::accu))
        :ret boolean?)

(defn non-negative-matrix?
  "Returns true if `m` is a non-negative matrix."
  ([m] (non-negative-matrix? m m/*dbl-close*))
  ([m accu] (and (mx/symmetric-matrix? m) (every? #(m/roughly-non-? % accu) (eigenvalues m)))))

(s/fdef non-negative-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

(defn apache-correlation-matrix?
  "Returns true if `m` has a unit diagonal and is a positive definite matrix.
  Test for a correlation matrix."
  ([m] (and (mx/symmetric-matrix-with-unit-diagonal? m) (positive-matrix? m)))
  ([m accu] (and (mx/symmetric-matrix-with-unit-diagonal? m) (positive-matrix? m accu))))

(s/fdef apache-correlation-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

;;;MATRIX DECOMPOSITION
(defn lu-decomposition-with-inverse-and-determinant
  "Computes the LU Decomposition, the inverse, and the determinant."
  [apache-square-m]
  (let [lud (LUDecomposition. apache-square-m)
        s (.getSolver lud)
        inverse (.getInverse s)
        det (.getDeterminant lud)]
    {::inverse inverse, ::determinant det, ::L (.getL lud), ::U (.getU lud), ::P (.getP lud)}))

(s/fdef lu-decomposition-with-inverse-and-determinant
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret (s/keys :req [::inverse ::determinant ::L ::U ::P]))

(defn inverse                                               ;;probable that Clatrix is faster
  "Computes the inverse of an Apache matrix through LU Decomposition."
  [apache-square-m]
  (let [lud (LUDecomposition. apache-square-m)
        s (.getSolver lud)
        sol (.getInverse s)]
    sol))

(s/fdef inverse
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret ::inverse)

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix.
   Returns a map of two Apache matrices ::L and ::U.
   Intended usage: (let [[L U] (cholesky-decomposition M)] ....).
   This is the Cholesky square root of a matrix, `L` and `U` such that `positive-apache-m` = L × U.
   Note that `apache-square-m` must be positive (semi) definite for this to exist,
      but [[cholesky-decomposition]] requires strict positivity."
  [positive-apache-m]
  (let [r (CholeskyDecomposition. positive-apache-m)]
    {::L (.getL r) ::U (.getLT r)}))

(s/fdef cholesky-decomposition
        :args (s/cat :positive-apache-m ::positive-apache-matrix)
        :ret (s/keys :req [::L ::U]))

(defn cholesky-rectangular
  "Calculates the rectangular Cholesky decomposition of a matrix.
  The rectangular Cholesky decomposition of a real symmetric positive semidefinite matrix m consists of a
  rectangular matrix B with the same number of rows such that:
      m is almost equal to BB*, depending on a user-defined tolerance.
  In a sense, this is the square root of m.
  The difference with respect to the regular CholeskyDecomposition is that rows/columns may be permuted
  (hence the rectangular shape instead of the traditional triangular shape) and there is a threshold to ignore small
   diagonal elements.
   This is used for example to generate correlated random n-dimensions vectors in a p-dimension subspace (p < n).
   In other words, it allows generating random vectors from a covariance matrix that is only positive semidefinite,
   and not positive definite.
   accu - Diagonal elements threshold under which columns are considered to be dependent on
      previous ones and are discarded.
      Returns a map containing:
         :B -- rectangular root matrix
         :rank -- rank is the number of independent rows of original matrix,
            and the number of columns of root matrix B"
  [apache-square-m accu]
  (let [r (RectangularCholeskyDecomposition. apache-square-m (double accu))]
    {::B    (.getRootMatrix r)
     ::rank (.getRank r)}))

(s/fdef cholesky-rectangular
        :args (s/cat :apache-square-m ::apache-square-matrix :accu ::accu)
        :ret (s/keys :req [::B ::rank]))

(defn cholesky-decomposition-semi-definite
  "Returns a vector containing two matrices [L L*],
      where 'm' may have zero (or close to zero) rows"
  [square-m ^double accu]
  (if (positive-matrix? square-m)
    (cholesky-decomposition square-m)
    (let [c (mx/rows square-m)
          {b :B, r :rank} (cholesky-rectangular square-m accu)
          s (- c r)
          nm (if (zero? s) b (mx/conj-columns b (mx/constant-matrix c s)))]
      [nm (mx/transpose nm)])))

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of a matrix.
  The Singular Value Decomposition of matrix A is a set of three
   matrices: U, S and V such that A = U × S × VT.
   Let A be a m × n matrix, then U is a m × p orthogonal matrix of the left singular vectors,
   S is a p × p diagonal matrix of singular values with positive or null elements,
   V is a p × n orthogonal matrix of the right singular vectors (hence VT is also orthogonal) where p=min(m,n).
   Returns a map containing:
      :S -- diagonal matrix S
      :V -- matrix V
      :U -- matrix U
      :VT -- transpose of matrix V
      :UT -- transpose of matrix U
      :rank -- rank"
  [apache-m]
  (let [d (SingularValueDecomposition. apache-m)]
    {::S (.getS d), ::V (.getV d), ::U (.getU d), ::VT (.getVT d), ::UT (.getUT d), ::rank (.getRank d)}))

(s/fdef sv-decomposition
        :args (s/cat :apache-m ::apache-matrix)
        :ret (s/keys :req [::S ::V ::U ::VT ::UT ::rank]))

(defn condition
  "The `singular-values-matrix` is the diagonal matrix of singular values, the `S`, from an [[SV-decomposition]].
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-matrix` divided by the minimum element value."
  [apache-singular-values-matrix]
  (let [vs (flatten apache-singular-values-matrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :apache-singular-values-matrix ::apache-matrix)
        :ret (s/nilable ::number))

(defn lu-decomposition
  "Returns a map containing:
      ::L -- the lower triangular factor
      ::U -- the upper triangular factor
      ::P -- the permutation matrix"
  [apache-square-m]
  (let [lud (LUDecomposition. apache-square-m)]
    {::L (.getL lud), ::U (.getU lud), ::P (.getP lud)}))

(s/fdef lu-decomposition
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret (s/keys :req [::L ::U ::P]))

(defn determinant
  "Calculates the determinant of a square matrix through LU Decomposition."
  [apache-square-m]
  (let [lud (LUDecomposition. apache-square-m) ;probably don't want to always lose this decomp plus inverse
        det (.getDeterminant lud)]
    det))

(s/fdef determinant
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret ::number)

(defn qr-decomposition
  "Computes the QR decomposition of a matrix.
Returns a vector containing two matrices [Q R]
Intended usage: (let [[Q R] (qr-decomposition M)] ....)
   Q -- orthogonal factors
   R -- the upper triangular factors."
  [apache-m]
  (let [d (QRDecomposition. apache-m)]
    {::Q (.getQ d), ::R (.getR d)}))

(s/fdef qr-decomposition
        :args (s/cat :apache-m ::apache-matrix)
        :ret (s/keys :req [::Q ::R]))

(defn rank-revealing-qr-decomposition
  "Calculates the rank-revealing QR-decomposition of a matrix, with column pivoting.
The rank-revealing QR-decomposition of a matrix A consists of three matrices Q, R, and P such that AP = QR.
Q is orthogonal (QTQ = I), and R is upper triangular.
If A is m×n, Q is m×m and R is m×n and P is n×n.
QR decomposition with column pivoting produces a rank-revealing QR decomposition.
This class computes the decomposition using Householder reflectors.
Returns a map containing:
      :Q -- orthogonal factors
      :QT -- transform of orthogonal factors
      :R -- the upper triangular factors
      :P -- P Matrix
      :rank -- the rank."
  [apache-m accu]
  (let [d (RRQRDecomposition. apache-m accu)]
    {:Q    (.getQ d), :R (.getR d),
     :QT   (.getQT d), :P (.getP d),
     :rank (.getRank d accu)}))

(s/fdef rank-revealing-qr-decomposition
        :args (s/cat :apache-m ::apache-matrix :accu ::accu)
        :ret (s/keys :req [::Q ::R ::P ::QT ::rank]))

(defn eigenvalues
  "Returns Apache vector of real parts of eigenvalues."
  [apache-square-m]
  (let [r (EigenDecomposition. apache-square-m)]
    (when-not (.hasComplexEigenvalues r)
      (.getRealEigenvalues r))))

(s/fdef eigenvalues
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret (s/nilable ::apache-vector))

(defn eigen-decomposition                                   ;doc differs from output
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a vector containing three matrices [Q A Qinv]
   A is a diagonal matrix whose diagonal elements are the eigenvalues.
   Intended usage: (let [[Q A Qinv] (eigen-decomposition M)] ....)"
  [apache-square-m]
  (let [r (EigenDecomposition. apache-square-m)]
    {::V (.getV r) ::D (.getD r) ::VT (.getVT r)}))

(s/fdef eigen-decomposition
        :args (s/cat :apache-square-m ::apache-square-matrix)
        :ret (s/keys :req [::V ::D ::VT]))

(comment "MATRIX SOLVE")
(defn linear-least-squares
  "Returns a vector."
  [apache-m1 apache-m2]
  (let [^DecompositionSolver s (.getSolver (QRDecomposition. apache-m1))]
    (vec (.toArray ^RealVector (.solve s ^RealMatrix apache-m2)))))

(s/fdef linear-least-squares
        :args (s/cat :apache-m1 ::apache-matrix :apache-m2 ::apache-matrix)
        :ret ::vector)

(defn linear-least-squares-with-error-matrix
  "Returns a map containing:
      :S -- solution
      :E -- error matrix"
  [apache-m1 apache-m2]
  (let [d (QRDecomposition. apache-m1)
        r (.getR d)
        r (mx/square-matrix r)]
    (when-not (= (mx/columns r) (mx/columns apache-m1))
      (ex-info "Icky matrices" {:fn (var linear-least-squares-with-error-matrix)}))
    (let [ri (inverse r)
          e (mx/mx* ri (mx/transpose ri))
          ^DecompositionSolver s (.getSolver d)]
      {:S (vec (.toArray ^RealVector (.solve s ^RealMatrix apache-m2))),
       :E e})))

(defn matrix-solve-iterative
  "This seems to solve only when the matrix 'm' is psd.
  Not sure of any advantages over linear least squares.
  This could be improved by running both sovers on parallel threads.
  m * x = v.
  Returns the vector 'x'.
  'solver' types:
  ::cg Conjugate Gradient
  ::symm SymmLQ (default)
  A default stopping criterion is implemented.
  The iterations stop when || r || ≤ δ || v ||, where v is the right-hand side vector,
  r the current estimate of the residual, and δ a user-specified tolerance.
  It should be noted that r is the so-called updated residual,
  which might differ from the true residual due to rounding-off errors (see e.g. Strakos and Tichy, 2002).

  Implementation of the SYMMLQ iterative linear solver proposed by Paige and Saunders (1975).
  This implementation is largely based on the FORTRAN code by Pr. Michael A. Saunders.
  SYMMLQ is designed to solve the system of linear equations A · x = b where A
  is an n × n self-adjoint linear operator (defined as a RealLinearOperator), and b is a given vector.
  The operator A is not required to be positive definite.
  If A is known to be definite, the method of conjugate gradients might be preferred,
  since it will require about the same number of iterations as SYMMLQ but slightly less work per iteration.
  SYMMLQ is designed to solve the system (A - shift · I) · x = b, where shift is a specified scalar value.
  If shift and b are suitably chosen, the computed vector x may approximate an (unnormalized) eigenvector of A,
  as in the methods of inverse iteration and/or Rayleigh-quotient iteration.
  Again, the linear operator (A - shift · I) need not be positive definite (but must be self-adjoint).
  The work per iteration is very slightly less if shift = 0."
  [apache-m apache-v
   & {:keys [solver apache-matrix-guess max-iter delta check?]
      :or   {solver ::symm, max-iter m/*max-iter*, delta m/*dbl-close*, check? true}}]
  (let [^RealMatrix a apache-m
        ^RealVector b apache-v
        ^RealVector g apache-matrix-guess
        ^PreconditionedIterativeLinearSolver s
        (condp = solver
          ::cg (ConjugateGradient. ^long max-iter ^double delta ^boolean check?)
          ::symm (SymmLQ. ^long max-iter ^double delta ^boolean check?)
          nil)]
    (vec (.toArray (if g
                     ^RealVector (.solve s ^RealLinearOperator a b g)
                     ^RealVector (.solve s a b))))))