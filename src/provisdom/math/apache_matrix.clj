(ns provisdom.math.apache-matrix
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.matrix :as mx]
            [provisdom.math.vector :as vector]
            [provisdom.math.tensor :as tensor]
            [provisdom.math.arrays :as ar]
            [provisdom.math.random2 :as random])
  (:import [org.apache.commons.math3.linear RealMatrix Array2DRowRealMatrix RealVector
                                            QRDecomposition LUDecomposition CholeskyDecomposition
                                            RectangularCholeskyDecomposition Array2DRowRealMatrix
                                            EigenDecomposition SingularValueDecomposition RRQRDecomposition
                                            DecompositionSolver ConjugateGradient SymmLQ
                                            PreconditionedIterativeLinearSolver RealLinearOperator
                                            MatrixUtils]))

(set! *warn-on-reflection* true)

(declare apache-matrix? apache-square-matrix? apache-matrix eigenvalues rrqr-decomposition rows columns transpose
         positive-definite-apache-matrix positive-semidefinite-apache-matrix diagonal mx* add
         covariance-apache-matrix->correlation-apache-matrix correlation-apache-matrix some-kv)

(s/def ::accu ::tensor/accu)
(s/def ::number ::m/number)
(s/def ::vector ::vector/vector)
(s/def ::matrix ::mx/matrix)
(s/def ::determinant ::number)
(s/def ::rank ::m/int-non-)
(s/def ::rows ::mx/rows)
(s/def ::columns ::mx/columns)
(s/def ::by-row? ::mx/by-row?)

;;;MATRIX TYPES
(defn apache-matrix?
  "Returns true if an Apache Commons matrix."
  [x] (instance? Array2DRowRealMatrix x))

(s/fdef apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::apache-matrix (s/with-gen apache-matrix? #(gen/fmap apache-matrix (s/gen ::matrix))))
(s/def ::VT ::apache-matrix)
(s/def ::B ::apache-matrix)
(s/def ::P ::apache-matrix)
(s/def ::Q ::apache-matrix)
(s/def ::QT ::apache-matrix)
(s/def ::R ::apache-matrix)
(s/def ::D ::apache-matrix)

(defn symmetric-apache-matrix?
  "Returns true is a symmetric Apache Commons matrix."
  [x] (and (apache-matrix? x) (MatrixUtils/isSymmetric x 0.0)))

(s/fdef symmetric-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::symmetric-apache-matrix
  (s/with-gen symmetric-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/symmetric-matrix))))

(defn square-apache-matrix?
  "Returns true if a square Apache Commons matrix (i.e., same number of rows and columns)."
  [x] (and (apache-matrix? x) (.isSquare ^Array2DRowRealMatrix x)))

(s/fdef square-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::square-apache-matrix (s/with-gen square-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/square-matrix))))
(s/def ::V ::square-apache-matrix)
(s/def ::inverse ::square-apache-matrix)

(defn diagonal-apache-matrix?
  "Returns true if a diagonal matrix (the entries outside the main diagonal are all zero)."
  [x] (and (apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (= i j) (zero? e)))) x))))

(s/fdef diagonal-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::diagonal-apache-matrix (s/with-gen diagonal-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/diagonal-matrix))))
(s/def ::S ::diagonal-apache-matrix)

(defn upper-triangular-apache-matrix?
  "Returns true if an upper triangular matrix (the entries below the main diagonal are all zero)."
  [x] (and (square-apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (<= i j) (zero? e)))) x))))

(s/fdef upper-triangular-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::upper-triangular-apache-matrix
  (s/with-gen upper-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/upper-triangular-matrix))))
(s/def ::U ::upper-triangular-apache-matrix)

(defn lower-triangular-apache-matrix?
  "Returns true if a lower triangular matrix (the entries above the main diagonal are all zero)."
  [x] (and (square-apache-matrix? x) (nil? (some-kv (fn [i j e] (not (or (>= i j) (zero? e)))) x))))

(s/fdef lower-triangular-apache-matrix?
        :args (s/cat :x any?)
        :ret boolean?)

(s/def ::lower-triangular-apache-matrix
  (s/with-gen lower-triangular-apache-matrix? #(gen/fmap apache-matrix (s/gen ::mx/lower-triangular-matrix))))
(s/def ::L ::lower-triangular-apache-matrix)
(s/def ::UT ::lower-triangular-apache-matrix)

(defn positive-semidefinite-apache-matrix?
  "Returns true if `apache-m` is a positive-semidefinite Apache matrix."
  ([apache-m] (positive-semidefinite-apache-matrix? apache-m m/*dbl-close*))
  ([apache-m accu]
   (and (symmetric-apache-matrix? apache-m) (every? #(m/roughly-non-? % accu) (eigenvalues apache-m)))))

(s/fdef positive-semidefinite-apache-matrix?
        :args (s/cat :apache-m ::apache-matrix :accu (s/? ::accu))
        :ret boolean?)

(s/def ::positive-semidefinite-apache-matrix
  (s/with-gen positive-semidefinite-apache-matrix?
              #(gen/fmap (fn [m] (positive-semidefinite-apache-matrix m)) (s/gen ::mx/square-matrix))))

(defn positive-definite-apache-matrix?
  "Returns true if `apache-square-m` is a positive definite Apache matrix."
  ([square-apache-m] (positive-definite-apache-matrix? square-apache-m m/*dbl-close*))
  ([square-apache-m accu]
   (and (symmetric-apache-matrix? square-apache-m) (every? #(> % accu) (eigenvalues square-apache-m)))))

(s/fdef positive-definite-apache-matrix?
        :args (s/cat :square-apache-m ::square-apache-matrix :accu (s/? ::accu))
        :ret boolean?)

(s/def ::positive-definite-apache-matrix
  (s/with-gen positive-definite-apache-matrix?
              #(gen/fmap (fn [m] (positive-definite-apache-matrix m)) (s/gen ::mx/square-matrix))))

(defn correlation-apache-matrix?
  "Returns true if `apache-m` has a unit diagonal and is a positive definite Apache Commons matrix."
  ([apache-m] (and (positive-definite-apache-matrix? apache-m) (every? m/one? (diagonal apache-m))))
  ([apache-m accu] (and (positive-definite-apache-matrix? apache-m accu) (every? m/one? (diagonal apache-m)))))

(s/fdef correlation-apache-matrix?
        :args (s/cat :m ::matrix :accu (s/? ::accu))
        :ret boolean?)

(s/def ::correlation-apache-matrix
  (s/with-gen correlation-apache-matrix?
              #(gen/fmap (fn [m] (correlation-apache-matrix m)) (s/gen ::mx/square-matrix))))

;;;MATRIX CONSTRUCTORS
(defn apache-matrix
  "Returns a matrix using the Apache Commons matrix implementation."
  [m] (when-not (mx/empty-matrix? m) (Array2DRowRealMatrix. ^"[[D" (ar/jagged-2D-array :d m))))

(s/fdef apache-matrix
        :args (s/cat :m ::matrix)
        :ret (s/nilable ::apache-matrix))

(defn apache-matrix->matrix
  "Converts an Apache Commons matrix into a matrix."
  [apache-m] (mapv vec (.getData ^Array2DRowRealMatrix apache-m)))

(s/fdef apache-matrix->matrix
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::matrix)

(defn positive-semidefinite-apache-matrix
  "Returns a positive semidefinite Apache Commons matrix."
  [square-apache-m] (mx* square-apache-m (transpose square-apache-m)))

(s/fdef positive-semidefinite-apache-matrix
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::positive-semidefinite-apache-matrix)

(defn positive-definite-apache-matrix
  "Returns a positive definite Apache Commons matrix."
  [square-apache-m]
  (let [size (rows square-apache-m)
        shake-m (add square-apache-m (mx/constant-matrix size size 2E-15))]
    (positive-semidefinite-apache-matrix shake-m)))

(s/fdef positive-definite-apache-matrix
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::positive-definite-apache-matrix)

(defn correlation-apache-matrix
  "Returns a Correlation Apache Commons matrix."
  [square-apache-m]
  (covariance-apache-matrix->correlation-apache-matrix (positive-definite-apache-matrix square-apache-m)))

(s/fdef correlation-apache-matrix
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::positive-definite-apache-matrix)

(defn rnd-positive-definite-apache-matrix!
  "Returns a positive definite Apache Commons matrix with a random spectrum.
  The orthogonal matrices are generated by using 2 × `size` composed Householder reflections.
  Alternative #1: Sample from the Inverse-Wishart Distribution.
  Alternative #2: Use [[positive-definite-apache-matrix]] with a random square matrix."
  [size] (apache-matrix (mx/rnd-spectral-matrix! (vec (take size (random/rand-double-lazy!))))))

(s/fdef rnd-positive-definite-apache-matrix!
        :args (s/cat :size ::size)
        :ret ::positive-apache-matrix)

(defn rnd-correlation-apache-matrix!
  "Returns a correlation Apache Commons matrix from a covariance matrix with a random spectrum.
  The orthogonal matrices are generated by using 2 * `size` composed Householder reflections.
  Alternative #1: Sample Covariance from the Inverse-Wishart Distribution.
  Alternative #2: Use [[correlation-apache-matrix]] with a random square matrix."
  [size] (covariance-apache-matrix->correlation-apache-matrix (rnd-positive-definite-apache-matrix! size)))

(s/fdef rnd-correlation-apache-matrix!
        :args (s/cat :size ::size)
        :ret ::correlation-apache-matrix)

;;;MATRIX INFO
(defn rows
  "Returns the number of rows."
  [apache-m] (.getRowDimension ^Array2DRowRealMatrix apache-m))

(s/fdef rows
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::rows)

(defn columns
  "Returns the number of columns."
  [apache-m] (.getColumnDimension ^Array2DRowRealMatrix apache-m))

(s/fdef columns
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::columns)

(defn get-entry
  "Returns the specified Apache Commons matrix element."
  [apache-m row column] (.getEntry ^Array2DRowRealMatrix apache-m row column))

(s/fdef get-entry
        :args (s/cat :apache-m ::apache-matrix :row ::row :column ::column)
        :ret ::number)

(defn get-row
  "Gets a `row` of an Apache Commons matrix, as a vector."
  [apache-m row] (vec (.getRow ^Array2DRowRealMatrix apache-m row)))

(s/fdef get-row
        :args (s/cat :apache-m ::apache-mmatrix :row ::row)
        :ret ::vector)

(defn get-column
  "Gets a `column` of an Apache Commons matrix, as a vector."
  [apache-m column] (vec (.getColumn ^Array2DRowRealMatrix apache-m column)))

(s/fdef get-column
        :args (s/cat :apache-mm ::apache-mmatrix :column ::column)
        :ret ::vector)

(defn diagonal
  "Returns the specified diagonal of an Apache Commons matrix as a vector.
   If `k`>0, returns a diagonal above the main diagonal.
   If `k`<0, returns a diagonal below the main diagonal.
   Works on both square and rectangular matrices."
  ([apache-m]
   (reduce (fn [tot e] (conj tot (get-entry apache-m e e))) [] (range (min (rows apache-m) (columns apache-m)))))
  ([apache-m k]
   (let [r (if (neg? k) (- k) 0)
           c (if (pos? k) k 0)
           nc (- (columns apache-m) c)
           nr (- (rows apache-m) r)
           start (- (min r c))
           end (min nc nr)]
       (if (pos? end)
         (vec (for [i (range start end)] (get-entry apache-m (+ i r) (+ i c))))
         []))))

(s/fdef diagonal
        :args (s/cat :apache-m ::apache-matrix :k (s/? ::m/int))
        :ret ::vector)

(defn some-kv
  "Returns the first logical true value of (pred row column number) for any number in Apache Commons matrix, else nil.
  Options: `::by-row?` (default: true)."
  ([pred apache-m] (some-kv pred apache-m {::by-row? true}))
  ([pred apache-m {::keys [by-row?] :or {by-row? true}}]
   (let [mt (if by-row? apache-m (transpose apache-m))
         rows (rows mt)]
     (loop [row 0]
       (when (< row rows)
         (or (vector/some-kv (fn [column number] (pred row column number)) (get-row mt row))
             (recur (inc row))))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :r ::row :c ::column :e ::m/number)
                                    :ret boolean?)
                     :apache-m ::apache-matrix
                     :opts (s/? (s/keys :opt [::by-row?])))
        :ret (s/nilable ::m/number))

;;;MATRIX MANIPULATION
(defn transpose
  "Transposes an Apache Commons matrix by swapping rows and columns, returning a new Apache Commons matrix."
  [apache-m] (.transpose ^Array2DRowRealMatrix apache-m))

(s/fdef transpose
        :args (s/cat :apache-m ::apache-matrix)
        :ret ::apache-matrix)

(defn correlation-apache-matrix->covariance-apache-matrix
  "Returns Covariance Apache Commons matrix from a Correlation Apache Commons matrix."
  [correlation-apache-matrix variances]
  (let [sqrt-m (apache-matrix (mx/diagonal-matrix (mapv m/sqrt (mx/diagonal-matrix variances))))]
    (mx* sqrt-m correlation-apache-matrix sqrt-m)))

(s/fdef correlation-apache-matrix->covariance-apache-matrix
        :args (s/cat :correlation-apache-matrix ::correlation-apache-matrix :variances ::vector)
        :ret ::positive-definite-apache-matrix)

(defn covariance-apache-matrix->correlation-apache-matrix
  "Returns Correlation Apache Commons matrix from a Covariance Apache Commons matrix."
  [covariance-apache-matrix]
  (let [inv-sqrt (apache-matrix (mx/diagonal-matrix (map #(m/pow % -0.5) (diagonal covariance-apache-matrix))))]
    (mx* inv-sqrt covariance-apache-matrix inv-sqrt)))

(s/fdef covariance-apache-matrix->correlation-apache-matrix
        :args (s/cat :covariance-apache-matrix ::positive-definite-apache-matrix)
        :ret ::correlation-apache-matrix)

;;;MATRIX MATH
(defn mx*
  "Apache Commons matrix multiplication.
  Number of columns of the first matrix must match the number of rows of the second matrix."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (when (= (columns apache-m1) (rows apache-m2))
     (.multiply ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2)))
  ([apache-m1 apache-m2 & apache-ms] (when-let [apache-m3 (mx* apache-m1 apache-m2)] (apply mx* apache-m3 apache-ms))))

(s/fdef mx*
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn add
  "Apache Commons matrix addition."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (try (.add ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2)
        (catch Exception _ nil)))
  ([apache-m1 apache-m2 & apache-ms] (when-let [apache-m3 (add apache-m1 apache-m2)] (apply add apache-m3 apache-ms))))

(s/fdef add
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

(defn subtract
  "Apache Commons matrix subtraction."
  ([apache-m] apache-m)
  ([apache-m1 apache-m2]
   (try (.subtract ^Array2DRowRealMatrix apache-m1 ^Array2DRowRealMatrix apache-m2)
        (catch Exception _ nil)))
  ([apache-m1 apache-m2 & apache-ms] (when-let [apache-m3 (subtract apache-m1 apache-m2)] (apply subtract apache-m3 apache-ms))))

(s/fdef subtract
        :args (s/or :one (s/cat :apache-m ::apache-matrix)
                    :two+ (s/cat :apache-m1 ::apache-matrix
                                 :apache-m2 ::apache-matrix
                                 :apache-ms (s/* ::apache-matrix)))
        :ret (s/nilable ::apache-matrix))

;;;MATRIX DECOMPOSITION
(defn lu-decomposition-with-inverse-and-determinant
  "Computes the LU Decomposition, the inverse, and the determinant."
  [apache-square-m]
  (let [lud (LUDecomposition. ^Array2DRowRealMatrix apache-square-m)
        s (.getSolver lud)
        inverse (.getInverse s)
        det (.getDeterminant lud)]
    {::inverse inverse, ::determinant det, ::L (.getL lud), ::U (.getU lud), ::P (.getP lud)}))

(s/fdef lu-decomposition-with-inverse-and-determinant
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/keys :req [::inverse ::determinant ::L ::U ::P]))

(defn inverse                                               ;;probable that Clatrix is faster
  "Computes the inverse of an Apache matrix through LU Decomposition."
  [apache-square-m]
  (let [lud (LUDecomposition. ^Array2DRowRealMatrix apache-square-m)
        s (.getSolver lud)
        sol (.getInverse s)]
    sol))

(s/fdef inverse
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::inverse)

(defn cholesky-decomposition
  "Computes the Cholesky decomposition of a positive definite Apache Commons matrix.
   Returns a map of two Apache Commons matrices ::L and ::U.
   Intended usage: (let [[L U] (cholesky-decomposition M)] ....).
   This is the Cholesky square root of a matrix, `L` and `U` such that `positive-definite-apache-m` = L × U.
   Note that `positive-definite-apache-m` must be positive semidefinite for this to exist,
      but [[cholesky-decomposition]] requires strict positivity."
  [positive-definite-apache-m]
  (let [r (CholeskyDecomposition. ^Array2DRowRealMatrix positive-definite-apache-m)]
    {::L (.getL r) ::U (.getLT r)}))

(s/fdef cholesky-decomposition
        :args (s/cat :positive-definite-apache-m ::positive-definite-apache-matrix)
        :ret (s/keys :req [::L ::U]))

(defn cholesky-rectangular
  "Calculates the rectangular Cholesky decomposition of a positive semidefinite Apache Commons matrix.
  The rectangular Cholesky decomposition of a real 'positive-semidefinite-apache-m' consists of a
  rectangular matrix B with the same number of rows such that:
      'positive-semidefinite-apache-m' is almost equal to BB*, depending on a user-defined tolerance.
  In a sense, this is the square root of 'positive-semidefinite-apache-m'.
  The difference with respect to the regular CholeskyDecomposition is that rows/columns may be permuted
  (hence the rectangular shape instead of the traditional triangular shape) and there is a threshold to ignore small
   diagonal elements.
   This is used for example to generate correlated random n-dimensions vectors in a p-dimension subspace (p < n).
   In other words, it allows generating random vectors from a covariance matrix that is only positive semidefinite,
   and not positive definite.
   'accu' - Diagonal elements threshold under which columns are considered to be dependent on
      previous ones and are discarded.
      Returns a map containing:
         :B -- rectangular root Apache Commons matrix
         :rank -- rank is the number of independent rows of original matrix,
            and the number of columns of root matrix B."
  [positive-semidefinite-apache-m accu]
  (let [r (RectangularCholeskyDecomposition. ^Array2DRowRealMatrix positive-semidefinite-apache-m (double accu))]
    {::B    (.getRootMatrix r)
     ::rank (.getRank r)}))

(s/fdef cholesky-rectangular
        :args (s/cat :positive-semidefinite-apache-m ::positive-semidefinite-apache-matrix :accu ::accu)
        :ret (s/keys :req [::B ::rank]))

(defn sv-decomposition
  "Calculates the compact Singular Value Decomposition of an Apache Commons matrix.
  The Singular Value Decomposition of matrix A is a set of three
   matrices: U, S and V such that A = U × S × VT.
   Let A be a m × n matrix, then U is a m × p orthogonal matrix of the left singular vectors,
   S is a p × p diagonal matrix of singular values with positive or null elements,
   V is a p × n orthogonal matrix of the right singular vectors (hence VT is also orthogonal) where p=min(m,n).
   Returns a map containing:
      :S -- diagonal Apache Commons matrix S
      :V -- Apache Commons matrix V
      :U -- Apache Commons matrix U
      :VT -- transpose of Apache Commons matrix V
      :UT -- transpose of Apache Commons matrix U
      :rank -- rank"
  [apache-m]
  (let [d (SingularValueDecomposition. ^Array2DRowRealMatrix apache-m)]
    {::S (.getS d), ::V (.getV d), ::U (.getU d), ::VT (.getVT d), ::UT (.getUT d), ::rank (.getRank d)}))

(s/fdef sv-decomposition
        :args (s/cat :apache-m ::apache-matrix)
        :ret (s/keys :req [::S ::V ::U ::VT ::UT ::rank]))

(defn condition
  "The `singular-values-apache-matrix` is the diagonal matrix of singular values, the `S`, from [[sv-decomposition]].
  Returns the norm2 condition number,
  which is the maximum element value from the `singular-values-apache-matrix` divided by the minimum element value."
  [singular-values-apache-matrix]
  (let [vs (flatten singular-values-apache-matrix)]
    (m/div (apply max vs) (apply min vs) nil)))

(s/fdef condition
        :args (s/cat :apache-singular-values-matrix ::apache-matrix)
        :ret (s/nilable ::number))

(defn lu-decomposition
  "Returns a map containing:
      ::L -- the lower triangular factor Apache Commons matrix
      ::U -- the upper triangular factor Apache Commons matrix
      ::P -- the permutation Apache Commons matrix"
  [square-apache-m]
  (let [lud (LUDecomposition. ^Array2DRowRealMatrix square-apache-m)]
    {::L (.getL lud), ::U (.getU lud), ::P (.getP lud)}))

(s/fdef lu-decomposition
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/keys :req [::L ::U ::P]))

(defn determinant
  "Calculates the determinant of a square Apache Commons matrix through LU Decomposition."
  [square-apache-m]
  (let [lud (LUDecomposition. ^Array2DRowRealMatrix square-apache-m)
        det (.getDeterminant lud)]
    det))

(s/fdef determinant
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret ::number)

(defn qr-decomposition
  "Computes the QR decomposition of a matrix.
Returns a vector containing two matrices [Q R]
Intended usage: (let [[Q R] (qr-decomposition M)] ....)
   Q -- orthogonal factors
   R -- the upper triangular factors."
  [apache-m]
  (let [d (QRDecomposition. ^Array2DRowRealMatrix apache-m)]
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
  (let [d (RRQRDecomposition. ^Array2DRowRealMatrix apache-m (double accu))]
    {:Q    (.getQ d), :R (.getR d),
     :QT   (.getQT d), :P (.getP d),
     :rank (.getRank d accu)}))

(s/fdef rank-revealing-qr-decomposition
        :args (s/cat :apache-m ::apache-matrix :accu ::accu)
        :ret (s/keys :req [::Q ::R ::P ::QT ::rank]))

(defn eigenvalues
  "Returns vector of real parts of eigenvalues."
  [square-apache-m]
  (let [r (EigenDecomposition. ^Array2DRowRealMatrix square-apache-m)]
    (when-not (.hasComplexEigenvalues r)
      (vec (.toArray ^RealVector (.getRealEigenvalues r))))))

(s/fdef eigenvalues
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/nilable ::vector))

(defn eigen-decomposition
  "Computes the Eigendecomposition of a diagonalisable matrix.
   Returns a map containing two matrices ::V and ::D.
   ::D is a diagonal Apache Commons matrix whose diagonal elements are the eigenvalues.
   ::V is a square Apache Commons matrix with each column containing the eigenvectors.
   'square-apache-m' = V × D × V-1."
  [square-apache-m]
  (let [r (EigenDecomposition. ^Array2DRowRealMatrix square-apache-m)]
    {::V (.getV r) ::D (.getD r)}))

(s/fdef eigen-decomposition
        :args (s/cat :square-apache-m ::square-apache-matrix)
        :ret (s/keys :req [::V ::D]))

(comment "MATRIX SOLVE")
(defn linear-least-squares
  "Returns a vector."
  [apache-m1 apache-m2]
  (let [^DecompositionSolver s (.getSolver (QRDecomposition. ^Array2DRowRealMatrix apache-m1))]
    (vec (.toArray ^RealVector (.solve s ^Array2DRowRealMatrix apache-m2)))))

(s/fdef linear-least-squares
        :args (s/cat :apache-m1 ::apache-matrix :apache-m2 ::apache-matrix)
        :ret ::vector)

(defn linear-least-squares-with-error-matrix
  "Returns a map containing:
      :S -- solution
      :E -- error matrix"
  [apache-m1 apache-m2]
  (let [d (QRDecomposition. ^Array2DRowRealMatrix apache-m1)
        r (.getR d)
        r (mx/square-matrix-by-trimming r)]
    (when-not (= (mx/columns r) (mx/columns apache-m1))
      (ex-info "Icky matrices" {:fn (var linear-least-squares-with-error-matrix)}))
    (let [ri (inverse r)
          e (mx/mx* ri (mx/transpose ri))
          ^DecompositionSolver s (.getSolver d)]
      {:S (vec (.toArray ^RealVector (.solve s ^Array2DRowRealMatrix apache-m2))),
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