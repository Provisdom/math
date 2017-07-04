(ns provisdom.math.matrix-decomposition
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [clojure.core.matrix :as mxc]
            [clatrix.core :as clx]))

(set! *warn-on-reflection* true)

(declare maybe-convert-clatrix-row-or-column)


(defn coerce
  "Coerces `param` into the provided implementation or matrix type"
  [matrix-or-implementation param]
  (mxc/coerce matrix-or-implementation
              (maybe-convert-clatrix-row-or-column param)))

;;;CLATRIX
(defn clatrix
  "Returns a matrix using the Clatrix matrix implementation."
  [data]
  (cond (= data []) (clx/matrix [])
        (nil? data) nil
        :else (mxc/matrix :clatrix data)))

(defn clatrix?
  "Returns true if `m` is a Clatrix matrix."
  [m] (clx/matrix? m))

(defn clatrix-impl?
  "Returns true if `impl` is a Clatrix implementation.
  This can be either :clatrix or a matrix that is an instance of a Clatrix matrix."
  [impl] (or (= impl :clatrix) (clatrix? impl)))

(defn clatrix-vec?
  "Returns true if `m` is a Clatrix vector."
  [m] (clx/vec? m))

;; Not entirely sure why this exists. Clatrix may have problems handling row, column, and zero element matrices
(defn- maybe-convert-clatrix-row-or-column
  [m]
  (cond (not (clatrix? m)) m
        (row-matrix? m) [(to-vector m)]
        (column-matrix? m) (mxc/to-nested-vectors
                             (column-matrix :persistent-vector (to-vector m)))
        (zero? (ecount m)) []
        :else m))


;;;MATRIX DECOMPOSITION
(defn inverse
  "Computes the inverse of a number, vector, or matrix.
   For Clatrix:
      this is done via Gaussian elmination.
      It can be numerically very unstable if the matrix is nearly singular.
      Positivity and symmetry hints are used to cause `solve` to use optimized
         LAPACK routines."
  [m]
  {:pre [(have? [:or number? square-matrix? numbers?] m)]}
  (cond (number? m) (m/div m)
        (numbers? m) (compute-vector m (emap m/div m))
        (apache-commons? m) (mxc/inverse m)
        :else (coerce m (clx/i (clx/maybe-positive          ;;looks like Clatrix may have been faster than Apache
                                 (clx/maybe-symmetric (clatrix m)))))))

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a matrix.
   Returns a vector containing two matrices [L U].
   Intended usage: (let [[L U] (cholesky-decomosition M)] ....).
   This is the Cholesky square root of a matrix, L such that (mmul L U) = m
   Note that m must be positive (semi) definite for this to exist,
      but `cholesky-decomposition` requires strict positivity."
  [m]
  {:pre [(have? square-matrix? m)]}
  (cond (apache-commons? m) (let [r (CholeskyDecomposition. m)]
                              [(.getL r) (.getLT r)])
        :else (let [u (clx/cholesky (clatrix m))]
                [(coerce m (clx/t u)) (coerce m u)])))

(defn lower-cholesky [m] (first (cholesky-decomposition m)))

(defn upper-cholesky [m] (second (cholesky-decomposition m)))

(defn cholesky-rectangular
  "Calculates the rectangular Cholesky decomposition of a matrix.
The rectangular Cholesky decomposition of a real symmetric positive
   semidefinite matrix m consists of a rectangular matrix B with the same
   number of rows such that:
      m is almost equal to BB*, depending on a user-defined tolerance.
In a sense, this is the square root of m.
The difference with respect to the regular CholeskyDecomposition is that
   rows/columns may be permuted (hence the rectangular shape instead of the
   traditional triangular shape) and there is a threshold to ignore small
   diagonal elements.
This is used for example to generate correlated random n-dimensions vectors
   in a p-dimension subspace (p < n).
In other words, it allows generating random vectors from a covariance matrix
   that is only positive semidefinite, and not positive definite.
accu - Diagonal elements threshold under which columns are considered to be
       dependent on previous ones and are discarded.
Returns a map containing:
      :B -- rectangular root matrix
      :rank -- rank is the number of independent rows of original matrix,
               and the number of columns of root matrix B"
  [m ^double accu]
  {:pre [(have? square-matrix? m)]}
  (let [r (RectangularCholeskyDecomposition. (apache-commons m) accu)]
    {:B (coerce m (.getRootMatrix r)), :rank (.getRank r)}))

(defn cholesky-decomposition-semi-definite
  "Returns a vector containing two matrices [L L*],
      where 'm' may have zero (or close to zero) rows"
  [m ^double accu]
  (if (positive-matrix? m) (cholesky-decomposition m)
                           (let [c (row-count m), {b :B, r :rank} (cholesky-rectangular m accu),
                                 s (- c r), nm (if (zero? s) b (conj-columns b (constant-matrix c s)))]
                             [nm (transpose nm)])))

(defn sv-decomposition-with-rank
  "Calculates the compact Singular Value Decomposition of a matrix.
The Singular Value Decomposition of matrix A is a set of three
   matrices: U, S and V such that A = U × S × VT.
Let A be a m × n matrix, then U is a m × p orthogonal matrix,
   S is a p × p diagonal matrix with positive or null elements,
V is a p × n orthogonal matrix (hence VT is also orthogonal) where p=min(m,n).
Returns a map containing:
      :S -- diagonal matrix S
      :V -- matrix V
      :U -- matrix U
      :VT -- transpose of matrix V
      :UT -- transpose of matrix U
      :rank -- rank"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (clatrix? m) (let [r (clx/svd m)]
                       {:S    (diagonal-matrix m (:values r)),
                        :V    (transpose (:right r)), :U (:left r),
                        :VT   (:right r), :UT (transpose (:left r)),
                        :rank (:rank r)})
        :else (let [d (SingularValueDecomposition. (apache-commons m))]
                {:S  (coerce m (.getS d)), :V (coerce m (.getV d)),
                 :U  (coerce m (.getU d)), :VT (coerce m (.getVT d)),
                 :UT (coerce m (.getUT d)), :rank (.getRank d)})))

(defn sv-decomposition
  "Computes the Singular Value decomposition of a matrix.
   Intended usage: (let [[U S V*] (sv-decomosition M)] ....)
   Returns a vector containing three matrices [U S V*] such
      that U (diag S) V = m with:
      U -- the left singular vectors U
      S -- the diagonal matrix of singular values S
           (the diagonal in vector form)
      V -- the right singular vectors V
   If 'm' is n x p and the rank is k, then 'U' is n x k, 'S' is k x k,
       and 'V*' is k x p"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (apache-commons? m) (let [r (sv-decomposition-with-rank m)]
                              [(:U r), (:S r), (:VT r)])
        :else (let [r (clx/svd (clatrix m))]
                [(coerce m (:left r)), (diagonal-matrix m (:values r)),
                 (coerce m (:right r))])))

(defn rank
  (^long [m] (:rank (sv-decomposition-with-rank m)))
  (^long [m accu] (:rank (rrqr-decomposition m accu))))     ; (mxc/rank m))

(defn condition
  "The `singular-values-matrix` is the diagonal matrix of singular values from an SVD decomposition.
  Returns the norm2 condition number,
  which is the maximum element value from the singular-values-matrix divided by the minimum element value."
  [singular-values-matrix]
  (let [vs (flatten singular-values-matrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :singular-values-matrix ::matrix)
        :ret (s/nilable ::number))

(defn lu-decomposition-with-permutation-matrix
  "Returns a map containing:
      :L -- the lower triangular factor
      :U -- the upper triangular factor
      :P -- the permutation matrix"
  [m]
  {:pre [(have? square-matrix? m)]}
  (cond (apache-commons? m) (let [r (LUDecomposition. m)]
                              {:L (.getL r), :U (.getU r), :P (.getP r)})
        :else (let [r (clx/lu (clatrix m))]
                {:L (coerce m (:l r)), :U (coerce m (:u r)),
                 :P (coerce m (:p r))})))

;;could instead be calculated from [[lu-decomposition-with-permutation-matrix]]
;;probably better to use apache (or another library)
(defn determinant
  "Calculates the determinant of a square matrix."
  [square-m] (mxc/det (apache-commons square-m)))

(s/fdef determinant
        :args (s/cat :square-m ::square-matrix)
        :ret ::number)

(defn lu-decomposition
  "Computes the LU decomposition of a matrix.
Returns a vector containing two matrices [L U]
Intended usage: (let [[L U] (lu-decomosition M)] ....)"
  [m]
  {:pre [(have? matrix? m)]}
  (let [r (lu-decomposition-with-permutation-matrix m)] [(:L r), (:U r)]))

(defn qr-decomposition
  "Computes the QR decomposition of a matrix.
Returns a vector containing two matrices [Q R]
Intended usage: (let [[Q R] (qr-decomposition M)] ....)
   Q -- orthogonal factors
   R -- the upper triangular factors"
  [m]
  {:pre [(have? matrix? m)]}
  (cond (apache-commons? m) (let [d (QRDecomposition. m)]
                              [(.getQ d), (.getR d)])
        :else (let [r (clx/qr (clatrix m))]
                [(coerce m (:q r)), (coerce m (:r r))])))

(defn rrqr-decomposition
  "Calculates the rank-revealing QR-decomposition of a matrix,
   with column pivoting.
The rank-revealing QR-decomposition of a matrix A consists of three
   matrices Q, R and P such that AP=QR.
Q is orthogonal (QTQ = I), and R is upper triangular.
If A is m×n, Q is m×m and R is m×n and P is n×n.
QR decomposition with column pivoting produces a rank-revealing
   QR decomposition and the getRank(double) method may be used to return the
   rank of the input matrix A.
This class compute the decomposition using Householder reflectors.
Returns a map containing:
      :Q -- orthogonal factors
      :QT -- transform of orthogonal factors
      :R -- the upper triangular factors
      :P -- P Matrix
      :rank -- the rank"
  [m ^double accu]
  {:pre [(have? matrix? m)]}
  (let [d (RRQRDecomposition. (apache-commons m) accu)]
    {:Q    (coerce m (.getQ d)), :R (coerce m (.getR d)),
     :QT   (coerce m (.getQT d)), :P (coerce m (.getP d)),
     :rank (.getRank d accu)}))

(defn eigenvalues
  "Returns vector of real parts of eigenvalues"
  [m]
  {:pre [(have? square-matrix? m)]}
  (cond (diagonal-matrix? m) (sort (diagonal m))
        (clatrix? m) (let [r (clx/eigen (clx/maybe-symmetric (clatrix m)))]
                       (if (or (:ivalues r) (:ivectors r)) nil (:values r)))
        :else (let [r (EigenDecomposition. (apache-commons m))]
                (when-not (.hasComplexEigenvalues r)
                  (vec (.getRealEigenvalues r))))))

(defn eigen-decomposition
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a vector containing three matrices [Q A Qinv]
   A is a diagonal matrix whose diagonal elements are the eigenvalues.
   Intended usage: (let [[Q A Qinv] (eigen-decomosition M)] ....)"
  [m]
  {:pre [(have? square-matrix? m)]}
  (let [r (EigenDecomposition. (apache-commons m))]
    [(coerce m (.getV r)) (coerce m (.getD r)) (coerce m (.getVT r))]))

(comment "MATRIX SOLVE")
(defn linear-least-squares
  "Returns a vector"
  [m1 m2]
  {:pre [(have? matrix? m1)]}
  (let [^DecompositionSolver s (.getSolver (QRDecomposition. (apache-commons m1))),
        m (apache-commons m2)]
    (vec (.toArray (if (numbers? m) ^RealVector (.solve s ^RealVector m)
                                    ^RealVector (.solve s ^RealMatrix m))))))

(defn linear-least-squares-with-error-matrix
  "Returns a map containing:
      :S -- solution
      :E -- error matrix"
  [m1 m2]
  (let [d (QRDecomposition. (apache-commons m1))
        r (.getR d)
        r (square-matrix r)]
    (when-not (= (column-count r) (column-count m1))        ; TODO - use truss or assert
      (throw (ex-info "Icky matrices" {:fn (var linear-least-squares-with-error-matrix)})))
    (let [ri (inverse r), e (matrix-multiply ri (transpose ri)),
          ^DecompositionSolver s (.getSolver d), m (apache-commons m2)]
      {:S (vec (.toArray (if (numbers? m)
                           ^RealVector (.solve s ^RealVector m)
                           ^RealVector (.solve s ^RealMatrix m)))),
       :E (coerce m1 e)})))

(defn matrix-solve-iterative
  "This seems to solve only when the matrix 'm' is psd.
Not sure of any advantages over linear least squares.
This could be improved by running both sovers on parallel threads.
m * x = v.
Returns the vector 'x'
'solver' types:
:cg Conjugate Gradient
:symm SymmLQ (default)
  A default stopping criterion is implemented.
The iterations stop when || r || ≤ δ || v ||, where v is the right-hand side
   vector, r the current estimate of the residual, and δ a user-specified
   tolerance.
It should be noted that r is the so-called updated residual, which might
   differ from the true residual due to rounding-off
   errors (see e.g. Strakos and Tichy, 2002).

Implementation of the SYMMLQ iterative linear solver proposed by Paige and
   Saunders (1975).
This implementation is largely based on the FORTRAN code
   by Pr. Michael A. Saunders.
SYMMLQ is designed to solve the system of linear equations A · x = b where A
   is an n × n self-adjoint linear operator (defined as a RealLinearOperator),
   and b is a given vector.
The operator A is not required to be positive definite.
If A is known to be definite, the method of conjugate gradients might
be preferred, since it will require about the same number of iterations
as SYMMLQ but slightly less work per iteration.
SYMMLQ is designed to solve the system (A - shift · I) · x = b, where
   shift is a specified scalar value.
If shift and b are suitably chosen, the computed vector x may approximate
   an (unnormalized) eigenvector of A, as in the methods of inverse
   iteration and/or Rayleigh-quotient iteration.
Again, the linear operator (A - shift · I) need not be positive definite
   (but must be self-adjoint).
The work per iteration is very slightly less if shift = 0."
  [m v
   & {:keys [solver guess max-iter delta check?]
      :or   {solver :symm, max-iter m/*max-iter*, delta m/*dbl-close*,
             check? true}}]
  (let [^RealMatrix a (apache-commons m), ^RealVector b (apache-commons v),
        ^RealVector g (if guess (apache-commons guess)),
        ^PreconditionedIterativeLinearSolver s
        (condp = solver
          :cg (ConjugateGradient. ^long max-iter ^double delta
                                  ^boolean check?)
          :symm (SymmLQ. ^long max-iter ^double delta ^boolean check?)
          (throw (ex-info (format "Invalid solver type specified %s" solver)
                          {:fn 'matrix-solve-iterative})))]
    (vec (.toArray (if guess ^RealVector (.solve s ^RealLinearOperator a b g)
                             ^RealVector (.solve s a b))))))