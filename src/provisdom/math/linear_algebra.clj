(ns provisdom.math.linear-algebra
  "Linear algebra operations for matrices.

  This namespace provides:
  - Matrix decompositions (LU, Cholesky, QR, Eigen, SVD)
  - Linear system solvers (solve, least-squares)
  - Matrix properties (determinant, inverse, pseudoinverse, rank, condition number)
  - Induced matrix norms (1-norm, infinity-norm, spectral norm)
  - Positive definite/semi-definite matrix utilities
  - Correlation/covariance matrix conversions

  All functions operate on matrices represented as vectors of vectors.
  Matrices should be created using provisdom.math.matrix functions.

  Complexity notes:
  - Most decompositions are O(n³) for n×n matrices
  - SVD and eigendecomposition use iterative methods"
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]
    [provisdom.utility-belt.anomalies :as anom]))

(declare back-substitution
  correlation-matrix?
  eigen-decomposition
  forward-substitution
  least-squares
  pos-definite-matrix-finite?
  pos-semidefinite-matrix-finite?
  sv-decomposition)

;;;SPECS
(s/def ::determinant ::m/number)
(s/def ::inverse (s/nilable ::mx/square-matrix))
(s/def ::L ::mx/lower-triangular-matrix)
(s/def ::LU-permutation ::mx/square-matrix)
(s/def ::rank ::m/int-non-)
(s/def ::singular? boolean?)
(s/def ::U ::mx/upper-triangular-matrix)
(s/def ::cholesky-L ::mx/lower-triangular-matrix)
(s/def ::cholesky-LT ::mx/upper-triangular-matrix)
(s/def ::rectangular-root ::mx/matrix)
(s/def ::Q ::mx/matrix)
(s/def ::RRQR-permutation ::mx/square-matrix)
(s/def ::R ::mx/matrix)
(s/def ::eigenvalues ::vector/vector)
(s/def ::eigenvalues-matrix ::mx/diagonal-matrix)
(s/def ::eigenvectors ::mx/square-matrix)
(s/def ::eigenvectorsT ::mx/square-matrix)
(s/def ::singular-values ::vector/vector)
(s/def ::singular-values-matrix ::mx/matrix)
(s/def ::svd-left ::mx/matrix)
(s/def ::svd-right ::mx/matrix)
(s/def ::condition-number ::m/number)
(s/def ::norm-spectral ::m/non-)
(s/def ::max-iterations pos-int?)
(s/def ::tolerance ::m/finite+)
(s/def ::rank-tolerance ::m/finite-non-)
(s/def ::pade-order (s/int-in 1 14))
(s/def ::schur-Q ::mx/square-matrix)
(s/def ::schur-T ::mx/square-matrix)
(s/def ::least-squares-solution (s/nilable ::vector/vector))

(s/def ::correlation-matrix
  (s/with-gen
    #(correlation-matrix? % m/sgl-close)
    #(gen/fmap (fn [m]
                 (let [mt (mx/transpose m)
                       cov (mx/mx* m mt)
                       diag (mx/diagonal cov)
                       n (count diag)]
                   (if (or (zero? n) (some (fn [d] (<= d 0)) diag))
                     (mx/identity-matrix (max 1 n))
                     (mx/compute-matrix n n
                       (fn [i j]
                         (let [cov-ij (get-in cov [i j] 0.0)
                               d-i (get diag i 1.0)
                               d-j (get diag j 1.0)]
                           (m/div cov-ij (m/sqrt (* d-i d-j)))))))))
       (s/gen ::mx/square-matrix-finite))))

(s/def ::pos-definite-matrix-finite
  (s/with-gen
    #(pos-definite-matrix-finite? %)
    #(gen/fmap (fn [m]
                 (let [mt (mx/transpose m)]
                   (tensor/add (mx/mx* m mt)
                     (mx/diagonal-matrix (mx/rows m) (constantly 0.1)))))
       (s/gen ::mx/square-matrix-finite))))

(s/def ::pos-semidefinite-matrix-finite
  (s/with-gen
    #(pos-semidefinite-matrix-finite? % m/sgl-close)
    #(gen/fmap (fn [m]
                 (let [mt (mx/transpose m)]
                   (mx/mx* m mt)))
       (s/gen ::mx/square-matrix-finite))))

(s/def ::svd-result
  (s/with-gen
    (s/keys :req [::svd-left ::singular-values ::svd-right])
    #(gen/fmap (fn [m]
                 (if-let [svd (sv-decomposition m {:rank-tolerance 0.0})]
                   (select-keys svd [::svd-left ::singular-values ::svd-right])
                   ;; Fallback for matrices where SVD fails
                   {::svd-left        [[1.0]]
                    ::singular-values [1.0]
                    ::svd-right       [[1.0]]}))
       (s/gen ::mx/matrix-finite))))

;;;LU DECOMPOSITION
;; Forward declarations for functions used in compute-inverse-from-lu
(defn- swap-rows
  "Swaps rows i and j in matrix m."
  [m i j]
  (if (= i j)
    m
    (let [row-i (get m i)
          row-j (get m j)]
      (-> m
        (assoc i row-j)
        (assoc j row-i)))))

(defn- lu-decomposition-impl
  "Implementation of LU decomposition with partial pivoting.
  Returns [L U P singular?] where PA = LU."
  [m]
  (let [n (mx/rows m)]
    (if (zero? n)
      [[[]] [[]] [[]] false]
      (loop [k 0
             U (mapv #(mapv double %) m)
             L (mx/identity-matrix n)
             P (mx/identity-matrix n)
             singular? false]
        (if (>= k n)
          [L U P singular?]
          (let [pivot-row (reduce (fn [best-row row]
                                    (if (> (m/abs (get-in U [row k]))
                                          (m/abs (get-in U [best-row k])))
                                      row
                                      best-row))
                            k
                            (range k n))
                U (swap-rows U k pivot-row)
                P (swap-rows P k pivot-row)
                L (if (and (pos? k) (not= k pivot-row))
                    (let [L-row-k (subvec (get L k) 0 k)
                          L-row-pivot (subvec (get L pivot-row) 0 k)]
                      (-> L
                        (update k #(vec (concat L-row-pivot (subvec % k))))
                        (update pivot-row #(vec (concat L-row-k (subvec % k))))))
                    L)
                pivot-val (get-in U [k k])
                is-singular? (or singular? (m/roughly? pivot-val 0.0 m/dbl-close))]
            (if is-singular?
              (recur (inc k) U L P true)
              (let [[new-L new-U]
                    (reduce (fn [[L-acc U-acc] i]
                              (let [factor (m/div (get-in U-acc [i k]) pivot-val)
                                    L-acc (assoc-in L-acc [i k] factor)
                                    U-row-i (get U-acc i)
                                    U-row-k (get U-acc k)
                                    new-U-row-i (mapv (fn [col]
                                                        (- (get U-row-i col)
                                                          (* factor (get U-row-k col))))
                                                  (range n))
                                    U-acc (assoc U-acc i new-U-row-i)]
                                [L-acc U-acc]))
                      [L U]
                      (range (inc k) n))]
                (recur (inc k) new-U new-L P is-singular?)))))))))

(defn- compute-determinant-from-lu
  "Computes determinant from L, U, P matrices."
  [U LU-permutation]
  (let [diag-product (reduce * (mx/diagonal U))
        n (mx/rows LU-permutation)
        perm-vec (mapv (fn [row]
                         (first (keep-indexed (fn [col val]
                                                (when (m/one? val) col))
                                  (get LU-permutation row))))
                   (range n))
        inversions (reduce + (for [i (range n)
                                   j (range (inc i) n)
                                   :when (> (get perm-vec i) (get perm-vec j))]
                               1))
        sign (if (even? inversions) 1.0 -1.0)]
    (* sign diag-product)))

(defn- compute-inverse-from-lu
  "Computes inverse from L, U, P matrices. Returns nil if singular."
  [L U LU-permutation singular?]
  (when-not singular?
    (let [n (mx/rows U)
          inv-columns
          (mapv (fn [col]
                  (let [e-i (assoc (vec (repeat n 0.0)) col 1.0)
                        Pe-i (mapv (fn [row]
                                     (reduce + (map * (get LU-permutation row) e-i)))
                               (range n))
                        y (forward-substitution L Pe-i)
                        x (back-substitution U y)]
                    x))
            (range n))]
      (mx/transpose inv-columns))))

(defn lu-decomposition
  "Computes LU decomposition with partial pivoting.

  Decomposes a square matrix A into P * A = L * U where:
  - L is lower triangular with 1s on the diagonal
  - U is upper triangular
  - P is a permutation matrix

  Complexity: O(n^3) for n x n matrix

  Returns a map with keys:
  - `::L` - lower triangular matrix
  - `::U` - upper triangular matrix
  - `::LU-permutation` - permutation matrix P
  - `::singular?` - `true` if matrix is singular (not invertible)
  - `::determinant` - the determinant of the matrix
  - `::inverse` - the inverse matrix (`nil` if singular)

  Returns `nil` for non-square, empty, non-finite, or numerically unstable matrices (where the
  decomposition fails to produce valid triangular matrices).

  Examples:
    (lu-decomposition [[4 3] [6 3]])
    ;=> {::L [[1.0 0.0] [0.667 1.0]]
    ;    ::U [[6.0 3.0] [0.0 1.0]]
    ;    ::LU-permutation [[0.0 1.0] [1.0 0.0]]
    ;    ::singular? false
    ;    ::determinant -6.0
    ;    ::inverse [[-0.5 0.5] [1.0 -0.667]]}

  See also: [[determinant]], [[inverse]], [[solve]]"
  [square-m]
  (when (and (mx/square-matrix? square-m)
          (not (mx/empty-matrix? square-m))
          (mx/matrix-finite? square-m))
    (let [[L U P singular?] (lu-decomposition-impl square-m)]
      (when (and (mx/lower-triangular-matrix? L)
              (mx/upper-triangular-matrix? U))
        (let [det (compute-determinant-from-lu U P)
              inv (compute-inverse-from-lu L U P singular?)]
          {::L              L
           ::U              U
           ::LU-permutation P
           ::singular?      singular?
           ::determinant    det
           ::inverse        inv})))))

(s/fdef lu-decomposition
  :args (s/cat :square-m ::mx/square-matrix)
  :ret (s/nilable (s/keys :req [::L ::U ::LU-permutation ::singular? ::determinant ::inverse])))

;;;DETERMINANT
(defn determinant-from-lu
  "Extracts the determinant from an existing LU decomposition result.

  This avoids redundant computation when you need both the LU decomposition and the determinant.

  Takes the map returned by [[lu-decomposition]] which includes `::determinant`.

  Examples:
    (let [lu (lu-decomposition [[4 3] [6 3]])]
      (determinant-from-lu lu)) ;=> -6.0

  See also: [[lu-decomposition]], [[determinant]]"
  [{::keys [determinant]}]
  determinant)

(s/fdef determinant-from-lu
  :args (s/cat :lu-result (s/keys :req [::determinant]))
  :ret ::determinant)

(defn determinant
  "Computes the determinant of a square matrix.

  Uses LU decomposition. The determinant equals the product of the diagonal elements of U, with
  sign determined by the number of row swaps in P.

  Complexity: O(n^3) for n x n matrix

  Returns `NaN` for empty matrices, `nil` for non-square matrices.

  Examples:
    (determinant [[1 2] [3 4]]) ;=> -2.0
    (determinant [[1 0] [0 1]]) ;=> 1.0
    (determinant [[1 2] [2 4]]) ;=> 0.0 (singular)

  See also: [[inverse]], [[lu-decomposition]], [[determinant-from-lu]]"
  [square-m]
  (cond
    (not (mx/square-matrix? square-m)) nil
    (mx/empty-matrix? square-m) m/nan
    :else (determinant-from-lu (lu-decomposition square-m))))

(s/fdef determinant
  :args (s/cat :square-m ::mx/square-matrix)
  :ret (s/nilable ::determinant))

;;;MINORS AND COFACTORS
(defn minor
  "Computes the (`i`,`j`) minor of a matrix.

  The minor M_ij is the determinant of the matrix with row `i` and column `j` removed. Indices are
  0-based.

  Complexity: O(n^3) for n x n matrix

  Returns `nil` for empty or 1x1 matrices, or if indices are out of bounds.

  Examples:
    (minor [[1 2 3] [4 5 6] [7 8 9]] 0 0) ;=> determinant of [[5 6] [8 9]] = -3.0
    (minor [[1 2] [3 4]] 0 0) ;=> 4.0

  See also: [[cofactor]], [[determinant]]"
  [square-m i j]
  (let [n (mx/rows square-m)]
    (when (and (> n 1)
            (< i n)
            (< j n)
            (>= i 0)
            (>= j 0))
      (let [submatrix (mapv (fn [row-idx]
                              (vec (keep-indexed
                                     (fn [col-idx v]
                                       (when (not= col-idx j) v))
                                     (get square-m row-idx))))
                        (filter #(not= % i) (range n)))]
        (determinant submatrix)))))

(s/fdef minor
  :args (s/cat :square-m ::mx/square-matrix :i ::mx/row :j ::mx/column)
  :ret (s/nilable ::determinant))

(defn cofactor
  "Computes the (`i`,`j`) cofactor of a matrix.

  The cofactor C_ij = (-1)^(i+j) * M_ij, where M_ij is the [[minor]]. Indices are 0-based.

  Complexity: O(n^3) for n x n matrix

  Examples:
    (cofactor [[1 2 3] [4 5 6] [7 8 9]] 0 0) ;=> -3.0
    (cofactor [[1 2 3] [4 5 6] [7 8 9]] 0 1) ;=> 6.0 (sign flip)

  See also: [[minor]], [[cofactor-matrix]], [[adjugate]]"
  [square-m i j]
  (when-let [m (minor square-m i j)]
    (let [sign (if (even? (+ i j)) 1.0 -1.0)]
      (* sign m))))

(s/fdef cofactor
  :args (s/cat :square-m ::mx/square-matrix :i ::mx/row :j ::mx/column)
  :ret (s/nilable ::determinant))

(defn cofactor-matrix
  "Computes the matrix of all cofactors.

  Each element C[i,j] is the cofactor of the original matrix at position (i,j).

  Complexity: O(n⁵) for n×n matrix (n² cofactors, each O(n³))

  Returns nil for empty matrices or if any cofactor calculation fails
  (e.g., for ill-conditioned submatrices). For 1×1 matrices, returns [[1.0]]
  since the determinant of the empty 0×0 matrix is 1 by convention.

  Examples:
    (cofactor-matrix [[1 2] [3 4]]) ;=> [[4.0 -3.0] [-2.0 1.0]]
    (cofactor-matrix [[5]]) ;=> [[1.0]]

  See also: cofactor, adjugate, inverse"
  [square-m]
  (let [n (mx/rows square-m)]
    (cond
      (zero? n) nil
      (= n 1) [[1.0]]
      :else (let [result (mapv (fn [i]
                                 (mapv (fn [j]
                                         (cofactor square-m i j))
                                   (range n)))
                           (range n))]
              (when (every? (fn [row] (every? some? row)) result)
                result)))))

(s/fdef cofactor-matrix
  :args (s/cat :square-m ::mx/square-matrix-finite)
  :ret (s/nilable ::mx/matrix))

(defn adjugate
  "Computes the adjugate (classical adjoint) of a matrix.

  The adjugate is the transpose of the cofactor matrix.
  For an invertible matrix A: A^(-1) = adj(A) / det(A)

  Complexity: O(n⁵) for n×n matrix

  Returns nil for empty matrices.

  Examples:
    (adjugate [[1 2] [3 4]]) ;=> [[4.0 -2.0] [-3.0 1.0]]

  See also: cofactor-matrix, inverse, determinant"
  [square-m]
  (when-let [cof (cofactor-matrix square-m)]
    (mx/transpose cof)))

(s/fdef adjugate
  :args (s/cat :square-m ::mx/square-matrix-finite)
  :ret (s/nilable ::mx/matrix))

;;;FORWARD/BACK SUBSTITUTION
(defn- forward-substitution
  "Solves Lx = b where L is lower triangular with 1s on diagonal."
  [L b]
  (let [n (count b)]
    (reduce (fn [x i]
              (let [sum (reduce (fn [acc j]
                                  (+ acc (* (get-in L [i j]) (get x j))))
                          0.0
                          (range i))
                    xi (- (get b i) sum)]
                (conj x xi)))
      []
      (range n))))

(defn- back-substitution
  "Solves Ux = b where U is upper triangular."
  [U b]
  (let [n (count b)]
    (reduce (fn [x i]
              (let [sum (reduce (fn [acc j]
                                  (+ acc (* (get-in U [i j]) (get x j 0.0))))
                          0.0
                          (range (inc i) n))
                    diag (get-in U [i i])
                    xi (if (m/roughly? diag 0.0 m/dbl-close)
                         m/nan
                         (m/div (- (get b i) sum) diag))]
                (assoc x i xi)))
      (vec (repeat n 0.0))
      (reverse (range n)))))

;;;INVERSE
(defn inverse-from-lu
  "Extracts the inverse from an existing LU decomposition result.

  This avoids redundant computation when you need both the LU decomposition and the inverse.

  Takes the map returned by `lu-decomposition` which includes ::inverse and ::singular?.

  Returns:
  - The inverse matrix on success
  - Anomaly with ::anom/no-solve if the matrix is singular (not invertible)

  Examples:
    (let [lu (lu-decomposition [[4 7] [2 6]])]
      (inverse-from-lu lu)) ;=> [[0.6 -0.7] [-0.2 0.4]]

  See also: lu-decomposition, inverse"
  [{::keys [inverse singular?]}]
  (if (or singular? (nil? inverse))
    {::anom/category ::anom/no-solve
     ::anom/message  "Matrix is singular (not invertible)"}
    inverse))

(s/fdef inverse-from-lu
  :args (s/cat :lu-result (s/keys :req [::inverse ::singular?]))
  :ret (s/or :matrix ::mx/square-matrix :anomaly ::anom/anomaly))

(defn inverse
  "Computes the inverse of a square matrix.

  Uses LU decomposition to solve A * A^(-1) = I column by column.

  Complexity: O(n³) for n×n matrix

  Returns:
  - The inverse matrix on success
  - The empty matrix [[]] for empty input
  - Anomaly with ::anom/no-solve if the matrix is singular (not invertible)

  Examples:
    (inverse [[4 7] [2 6]]) ;=> [[0.6 -0.7] [-0.2 0.4]]
    (inverse [[1 2] [2 4]]) ;=> anomaly (singular matrix)

  See also: pseudoinverse, determinant, solve, inverse-from-lu"
  [square-m]
  (if (mx/empty-matrix? square-m)
    square-m
    (if-let [lu-result (lu-decomposition square-m)]
      (inverse-from-lu lu-result)
      {::anom/category ::anom/no-solve
       ::anom/message  "LU decomposition failed (matrix may be ill-conditioned)"})))

(s/fdef inverse
  :args (s/cat :square-m ::mx/square-matrix-finite)
  :ret (s/or :matrix ::mx/square-matrix :anomaly ::anom/anomaly))

;;;SOLVE LINEAR SYSTEM
(defn- solve-square
  "Internal: Solves square system Ax = b using LU decomposition.
  Returns solution vector or nil if singular or decomposition fails."
  [square-m b]
  (when-let [lu-result (lu-decomposition square-m)]
    (let [{::keys [L U LU-permutation singular?]} lu-result]
      (when-not singular?
        (let [n (mx/rows square-m)
              Pb (mapv (fn [row]
                         (reduce + (map * (get LU-permutation row) b)))
                   (range n))
              y (forward-substitution L Pb)
              x (back-substitution U y)]
          x)))))

(defn solve
  "Solves the linear system Ax = b for x.

  Automatically selects the appropriate algorithm:
  - Square systems (rows = columns): LU decomposition for exact solution
  - Overdetermined systems (rows > columns): least squares minimizing ||Ax - b||²
  - Underdetermined systems (rows < columns): returns anomaly

  Complexity: O(n³) for square systems, O(mn²) for overdetermined m×n

  Returns:
  - Solution vector x on success
  - Empty vector for empty matrices
  - Anomaly with ::anom/no-solve for singular/underdetermined systems
  - Anomaly with ::anom/incorrect for dimension mismatches

  Examples:
    (solve [[2 1] [1 3]] [4 5]) ;=> [1.4 1.2]
    (solve [[1 1] [2 1] [3 1]] [1 2 4]) ;=> least squares solution
    (solve [[1 2] [2 4]] [1 2]) ;=> anomaly (singular)

  See also: least-squares, inverse, lu-decomposition"
  [A b]
  (let [nr (mx/rows A)
        nc (mx/columns A)
        nb (count b)]
    (cond
      (not= nr nb)
      {::anom/category ::anom/incorrect
       ::anom/message  (str "Vector length " nb " does not match matrix rows " nr)}

      (mx/empty-matrix? A)
      []

      (< nr nc)
      {::anom/category ::anom/no-solve
       ::anom/message  "Underdetermined system: more columns than rows"}

      (> nr nc)
      (or (least-squares A b)
        {::anom/category ::anom/no-solve
         ::anom/message  "Least squares failed (rank deficient)"})

      :else
      (or (solve-square A b)
        {::anom/category ::anom/no-solve
         ::anom/message  "Matrix is singular"}))))

(s/fdef solve
  :args (s/cat :A ::mx/matrix
          :b ::vector/vector)
  :ret (s/or :solution ::vector/vector
         :anomaly ::anom/anomaly))

;;;CHOLESKY DECOMPOSITION
(defn cholesky-decomposition
  "Computes Cholesky decomposition of a symmetric positive-definite matrix.

  Decomposes a symmetric positive-definite matrix A into A = L * L^T where
  L is a lower triangular matrix. This is essentially the matrix square root.

  Complexity: O(n³/3) for n×n matrix - faster than LU

  Useful for:
  - Solving linear systems (faster than LU for positive-definite matrices)
  - Generating correlated random numbers
  - Testing positive-definiteness

  Returns a map with keys:
  - ::cholesky-L - lower triangular matrix L
  - ::cholesky-LT - upper triangular matrix L^T (transpose of L)

  Returns nil if the matrix is not positive-definite.

  Examples:
    (cholesky-decomposition [[4 2] [2 2]])
    ;=> {::cholesky-L [[2.0 0.0] [1.0 1.0]]
    ;    ::cholesky-LT [[2.0 1.0] [0.0 1.0]]}

  See also: pos-definite-matrix-finite?, lu-decomposition"
  [symmetric-pos-def-m]
  (when (and (mx/symmetric-matrix? symmetric-pos-def-m)
          (not (mx/empty-matrix? symmetric-pos-def-m)))
    (let [n (mx/rows symmetric-pos-def-m)
          result
          (reduce
            (fn [L i]
              (when-not (nil? L)
                (reduce
                  (fn [L j]
                    (when-not (nil? L)
                      (if (= i j)
                        (let [sum (reduce (fn [acc k]
                                            (+ acc (m/sq (get-in L [i k]))))
                                    0.0
                                    (range i))
                              val (- (get-in symmetric-pos-def-m [i i]) sum)]
                          (when-not (<= val 0.0)
                            (assoc-in L [i i] (m/sqrt val))))
                        (let [sum (reduce (fn [acc k]
                                            (+ acc (* (get-in L [i k]) (get-in L [j k]))))
                                    0.0
                                    (range j))
                              L-jj (get-in L [j j])]
                          (when-not (m/roughly? L-jj 0.0 m/dbl-close)
                            (assoc-in L [i j]
                              (m/div (- (get-in symmetric-pos-def-m [i j]) sum)
                                L-jj)))))))
                  L
                  (range (inc i)))))
            (mx/constant-matrix n n 0.0)
            (range n))]
      (when result
        {::cholesky-L  result
         ::cholesky-LT (mx/transpose result)}))))

(s/fdef cholesky-decomposition
  :args (s/cat :symmetric-pos-def-m ::mx/symmetric-matrix)
  :ret (s/nilable (s/keys :req [::cholesky-L ::cholesky-LT])))

(defn rectangular-cholesky-decomposition
  "Computes the rectangular Cholesky decomposition of a positive semi-definite matrix.

  For a symmetric positive semi-definite matrix A, finds a rectangular matrix B
  (n rows, r columns where r = rank) such that A ≈ B × B^T within tolerance.

  This is useful for:
  - Generating correlated random vectors from a covariance matrix that is only
    positive semi-definite (not positive definite)
  - Working with rank-deficient covariance matrices

  Uses eigendecomposition: A = V × D × V^T, then B = V × sqrt(D) for positive eigenvalues only.

  Parameters:
  - pos-semidefinite-m: symmetric positive semi-definite matrix
  - tolerance: eigenvalues below this threshold are considered zero

  Returns a map with keys:
  - ::rectangular-root - n×r matrix B where A ≈ B × B^T
  - ::rank - number of columns with positive eigenvalues (r)

  Returns nil for non-symmetric or empty matrices.
  Returns anomaly if matrix is not positive semi-definite.

  Examples:
    ;; Full rank (2x2 positive definite)
    (rectangular-cholesky-decomposition [[4 2] [2 2]] 1e-10)
    ;=> {::rectangular-root [[...] [...]] ::rank 2}

    ;; Rank deficient (singular covariance)
    (rectangular-cholesky-decomposition [[1 1] [1 1]] 1e-10)
    ;=> {::rectangular-root [[...] [...]] ::rank 1}

  See also: cholesky-decomposition, pos-semidefinite-matrix-finite?, eigen-decomposition"
  [pos-semidefinite-m tolerance]
  (when (and (mx/symmetric-matrix? pos-semidefinite-m)
          (not (mx/empty-matrix? pos-semidefinite-m)))
    (if-let [eigen-result (eigen-decomposition pos-semidefinite-m)]
      (let [eigenvalues (::eigenvalues eigen-result)
            V (::eigenvectors eigen-result)
            n (count eigenvalues)
            ;; Check for negative eigenvalues (not positive semi-definite)
            min-eigenvalue (apply min eigenvalues)]
        (if (< min-eigenvalue (- tolerance))
          {::anom/category ::anom/incorrect
           ::anom/message  (str "Matrix is not positive semi-definite. Eigenvalue "
                             min-eigenvalue " is negative.")}
          ;; Find positive eigenvalues and their indices
          (let [positive-indices (filterv #(> (get eigenvalues %) tolerance) (range n))
                r (count positive-indices)]
            (if (zero? r)
              {::rectangular-root [[]]
               ::rank             0}
              ;; B = V × sqrt(D) for positive eigenvalues only
              ;; B[i,j] = V[i, positive-indices[j]] * sqrt(eigenvalues[positive-indices[j]])
              (let [B (mapv (fn [i]
                              (mapv (fn [j-idx]
                                      (let [orig-col (get positive-indices j-idx)
                                            v-ij (get-in V [i orig-col])
                                            lambda (get eigenvalues orig-col)]
                                        (* v-ij (m/sqrt lambda))))
                                (range r)))
                        (range n))]
                {::rectangular-root B
                 ::rank             r})))))
      ;; Eigendecomposition failed
      nil)))

(s/fdef rectangular-cholesky-decomposition
  :args (s/cat :pos-semidefinite-m ::mx/symmetric-matrix
          :tolerance ::m/finite-non-)
  :ret (s/nilable (s/or :result (s/keys :req [::rectangular-root ::rank])
                    :anomaly ::anom/anomaly)))

;;;POSITIVE DEFINITE TESTS
(defn pos-definite-matrix-finite?
  "Returns true if the matrix is symmetric, positive definite, and finite.

  A matrix is positive definite if it is symmetric and all eigenvalues are positive.
  Tests by attempting Cholesky decomposition.

  Examples:
    (pos-definite-matrix-finite? [[2 1] [1 2]]) ;=> true
    (pos-definite-matrix-finite? [[1 2] [2 1]]) ;=> false"
  [m]
  (and (mx/matrix-finite? m)
    (some? (cholesky-decomposition m))))

(s/fdef pos-definite-matrix-finite?
  :args (s/cat :m any?)
  :ret boolean?)

(defn pos-semidefinite-matrix-finite?
  "Returns true if the matrix is symmetric, positive semi-definite, and finite.

  A matrix is positive semi-definite if symmetric and all eigenvalues >= 0.

  Examples:
    (pos-semidefinite-matrix-finite? [[1 0] [0 0]] 1e-10) ;=> true
    (pos-semidefinite-matrix-finite? [[1 2] [2 1]] 1e-10) ;=> false"
  [m accu]
  (boolean
    (and (mx/matrix-finite? m)
      (mx/symmetric-matrix? m)
      (when-let [eigen-result (eigen-decomposition m)]
        (every? #(>= % (- accu)) (::eigenvalues eigen-result))))))

(s/fdef pos-semidefinite-matrix-finite?
  :args (s/cat :m any? :accu ::m/accu)
  :ret boolean?)

(defn pos-semidefinite-matrix-finite-by-squaring
  "Creates a positive semi-definite matrix by computing M * M^T.

  Examples:
    (pos-semidefinite-matrix-finite-by-squaring [[1 2] [3 4]])
    ;=> [[5.0 11.0] [11.0 25.0]]"
  [square-m]
  (let [mt (mx/transpose square-m)]
    (mx/mx* square-m mt)))

(s/fdef pos-semidefinite-matrix-finite-by-squaring
  :args (s/cat :square-m ::mx/square-matrix-finite)
  :ret ::mx/symmetric-matrix)

(defn pos-definite-matrix-finite-by-squaring
  "Creates a positive definite matrix by computing M * M^T + epsilon*I.

  Examples:
    (pos-definite-matrix-finite-by-squaring [[1 2] [3 4]])
    ;=> approximately [[5.0000001 11.0] [11.0 25.0000001]]"
  ([square-m] (pos-definite-matrix-finite-by-squaring square-m 1e-10))
  ([square-m epsilon]
   (let [mt (mx/transpose square-m)
         n (mx/rows square-m)
         result (mx/mx* square-m mt)]
     (if (zero? n)
       result
       (mapv (fn [i row]
               (update row i + epsilon))
         (range n) result)))))

(s/fdef pos-definite-matrix-finite-by-squaring
  :args (s/cat :square-m ::mx/square-matrix-finite
          :epsilon (s/? ::m/finite+))
  :ret ::mx/symmetric-matrix)

;;;CORRELATION MATRIX
(defn correlation-matrix?
  "Returns true if the matrix is a valid correlation matrix.

  A correlation matrix must be symmetric, positive semi-definite, have 1.0 on the diagonal, and
  elements in [-1, 1].

  Examples:
    (correlation-matrix? [[1.0 0.5] [0.5 1.0]] 1e-10) ;=> true
    (correlation-matrix? [[1.0 1.5] [1.5 1.0]] 1e-10) ;=> false"
  [m accu]
  (and (pos-semidefinite-matrix-finite? m accu)
    (every? #(m/roughly? % 1.0 accu) (mx/diagonal m))
    (every? #(<= (m/abs %) 1.0) (apply concat m))))

(s/fdef correlation-matrix?
  :args (s/cat :m any? :accu ::m/accu)
  :ret boolean?)

(defn covariance-matrix->correlation-matrix
  "Converts a covariance matrix to a correlation matrix.

  corr[i,j] = cov[i,j] / (sqrt(cov[i,i]) * sqrt(cov[j,j]))

  Examples:
    (covariance-matrix->correlation-matrix [[4.0 2.0] [2.0 4.0]])
    ;=> [[1.0 0.5] [0.5 1.0]]"
  [covariance-m]
  (let [n (mx/rows covariance-m)]
    (if (zero? n)
      covariance-m
      (let [diag (mx/diagonal covariance-m)
            inv-sqrt-diag (mapv #(m/pow % -0.5) diag)]
        (when (every? m/finite? inv-sqrt-diag)
          (let [inv-sqrt-m (mx/diagonal-matrix inv-sqrt-diag)
                corr (mx/mx* inv-sqrt-m covariance-m inv-sqrt-m)
                corr-sym (mx/symmetric-matrix-by-averaging corr)
                corr-fixed (mapv (fn [i row]
                                   (assoc row i 1.0))
                             (range n) corr-sym)]
            (when (correlation-matrix? corr-fixed m/sgl-close)
              corr-fixed)))))))

(s/fdef covariance-matrix->correlation-matrix
  :args (s/cat :covariance-m ::pos-definite-matrix-finite)
  :ret (s/nilable ::correlation-matrix))

(defn correlation-matrix->covariance-matrix
  "Converts a correlation matrix to a covariance matrix given variances.

  cov[i,j] = corr[i,j] * sqrt(var[i]) * sqrt(var[j])

  Examples:
    (correlation-matrix->covariance-matrix [[1.0 0.5] [0.5 1.0]] [4.0 4.0])
    ;=> [[4.0 2.0] [2.0 4.0]]"
  [correlation-m variances]
  (let [n (mx/rows correlation-m)]
    (when (= (count variances) n)
      (if (zero? n)
        correlation-m
        (let [sqrt-vars (mapv m/sqrt variances)
              sqrt-m (mx/diagonal-matrix sqrt-vars)
              cov (mx/mx* sqrt-m correlation-m sqrt-m)
              cov-sym (mx/symmetric-matrix-by-averaging cov)]
          (when (pos-definite-matrix-finite? cov-sym)
            cov-sym))))))

(s/fdef correlation-matrix->covariance-matrix
  :args (s/cat :correlation-m ::correlation-matrix
          :variances ::vector/vector-finite+)
  :ret (s/nilable ::pos-definite-matrix-finite))

(defn correlation-matrix-by-squaring
  "Creates a correlation matrix from a square matrix by squaring and normalizing."
  [square-m]
  (let [n (mx/rows square-m)]
    (if (zero? n)
      nil
      (when-let [corr (covariance-matrix->correlation-matrix
                        (pos-definite-matrix-finite-by-squaring square-m))]
        (loop [i 0
               current corr]
          (if (>= i 100)
            nil
            (if (correlation-matrix? current m/sgl-close)
              current
              (let [scale (m/one- (* 0.01 i))
                    scaled (mx/compute-matrix n n
                             (fn [r c]
                               (if (= r c)
                                 1.0
                                 (* scale (get-in current [r c] 0.0)))))]
                (recur (inc i) (mx/symmetric-matrix-by-averaging scaled))))))))))

(s/fdef correlation-matrix-by-squaring
  :args (s/cat :square-m ::mx/square-matrix-finite)
  :ret (s/nilable ::correlation-matrix))

(defn rnd-pos-definite-matrix-finite!
  "Generates a random positive definite matrix of given size."
  [size]
  (if (zero? size)
    nil
    (loop [i 0]
      (when (< i 100)
        (let [eigenvalues (vec (take size (random/rnd-lazy!)))
              m (mx/rnd-spectral-matrix! eigenvalues)]
          (if (pos-definite-matrix-finite? m)
            m
            (recur (inc i))))))))

(s/fdef rnd-pos-definite-matrix-finite!
  :args (s/cat :size ::vector/size)
  :ret (s/nilable ::pos-definite-matrix-finite))

(defn rnd-correlation-matrix!
  "Generates a random correlation matrix of given size.

  Returns nil if generation fails (e.g., if the underlying rnd-pos-definite-matrix-finite! fails
  after 100 attempts or covariance-to-correlation conversion fails)."
  [size]
  (when-not (zero? size)
    (covariance-matrix->correlation-matrix
      (rnd-pos-definite-matrix-finite! size))))

(s/fdef rnd-correlation-matrix!
  :args (s/cat :size ::vector/size)
  :ret (s/nilable ::correlation-matrix))

;;;QR DECOMPOSITION
(defn- householder-vector
  "Computes the Householder vector for zeroing out elements below the diagonal."
  [x]
  (let [n (count x)
        norm-x (tensor/norm x)
        x0 (get x 0 0.0)]
    (if (m/roughly? norm-x 0.0 m/dbl-close)
      (assoc (vec (repeat n 0.0)) 0 1.0)
      (let [sign (if (>= x0 0.0) 1.0 -1.0)
            v0 (+ x0 (* sign norm-x))
            v (assoc x 0 v0)]
        v))))

(defn- apply-householder
  "Applies Householder transformation H = I - 2*v*v^T/(v^T*v) to matrix A."
  [A v start-row start-col]
  (let [n-cols (mx/columns A)
        v-len (count v)
        vtv (reduce + (map #(* % %) v))]
    (if (m/roughly? vtv 0.0 m/dbl-close)
      A
      (let [beta (/ 2.0 vtv)]
        (reduce
          (fn [A-acc col]
            (let [col-vec (mapv #(get-in A-acc [(+ start-row %) col] 0.0)
                            (range v-len))
                  vta (reduce + (map * v col-vec))
                  factor (* beta vta)]
              (reduce
                (fn [A-acc2 i]
                  (let [row-idx (+ start-row i)
                        old-val (get-in A-acc2 [row-idx col] 0.0)
                        new-val (- old-val (* factor (get v i)))]
                    (assoc-in A-acc2 [row-idx col] new-val)))
                A-acc
                (range v-len))))
          A
          (range start-col n-cols))))))

(defn- accumulate-Q
  "Accumulates Householder reflections to form Q matrix."
  [Q v start-row]
  (let [n (mx/rows Q)
        v-len (count v)
        vtv (reduce + (map #(* % %) v))]
    (if (m/roughly? vtv 0.0 m/dbl-close)
      Q
      (let [beta (/ 2.0 vtv)]
        (reduce
          (fn [Q-acc row]
            (let [row-vec (mapv #(get-in Q-acc [row (+ start-row %)] 0.0)
                            (range v-len))
                  rv (reduce + (map * row-vec v))
                  factor (* beta rv)]
              (reduce
                (fn [Q-acc2 j]
                  (let [col-idx (+ start-row j)
                        old-val (get-in Q-acc2 [row col-idx] 0.0)
                        new-val (- old-val (* factor (get v j)))]
                    (assoc-in Q-acc2 [row col-idx] new-val)))
                Q-acc
                (range v-len))))
          Q
          (range n))))))

(defn qr-decomposition
  "Computes QR decomposition using Householder reflections.

  Decomposes a matrix A (m×n) into A = Q * R where:
  - Q is an orthogonal matrix (m×m): Q^T * Q = I
  - R is upper triangular (m×n)

  Complexity: O(mn²) for m×n matrix

  1-arity: Returns a map with keys ::Q and ::R, or nil for empty matrices.

  2-arity: Also computes the least-squares solution for Ax = b, returning
  ::least-squares-solution in addition to ::Q and ::R. The solution is nil
  if the matrix has more columns than rows.

  Examples:
    (qr-decomposition [[1 2] [3 4] [5 6]])
    ;=> Q is 3x3 orthogonal, R is 3x2 upper triangular

    ;; With least-squares: fit line y = mx + c to points (1,1), (2,2), (3,4)
    (qr-decomposition [[1 1] [2 1] [3 1]] [1 2 4])
    ;=> includes ::least-squares-solution [1.5 -0.333]

  See also: least-squares, eigen-decomposition"
  ([m]
   (when (and (mx/matrix? m) (not (mx/empty-matrix? m)))
     (let [nr (mx/rows m)
           nc (mx/columns m)
           min-dim (min nr nc)
           initial-Q (mx/identity-matrix nr)
           initial-R (mapv #(mapv double %) m)
           [Q R]
           (reduce
             (fn [[Q-acc R-acc] k]
               (let [col-k (mapv #(get-in R-acc [% k] 0.0)
                             (range k nr))
                     v (householder-vector col-k)
                     R-new (apply-householder R-acc v k k)
                     Q-new (accumulate-Q Q-acc v k)]
                 [Q-new R-new]))
             [initial-Q initial-R]
             (range min-dim))]
       {::Q Q
        ::R R})))
  ([m b]
   (when-let [qr-result (qr-decomposition m)]
     (let [{::keys [Q R]} qr-result
           nr (mx/rows m)
           nc (mx/columns m)
           solution (when (and (>= nr nc) (= nr (count b)))
                      (let [Qt (mx/transpose Q)
                            Qtb (mapv (fn [row]
                                        (reduce + (map * (get Qt row) b)))
                                  (range nr))
                            R-square (mapv #(subvec % 0 nc) (subvec R 0 nc))
                            b-truncated (subvec (vec Qtb) 0 nc)]
                        (back-substitution R-square b-truncated)))]
       (assoc qr-result ::least-squares-solution solution)))))

(s/fdef qr-decomposition
  :args (s/cat :m ::mx/matrix
          :b (s/? ::vector/vector))
  :ret (s/nilable (s/keys :req [::Q ::R]
                    :opt [::least-squares-solution])))

(defn- column-norms-squared
  "Computes squared norms of columns from row start-row to end."
  [m start-row]
  (let [nr (mx/rows m)
        nc (mx/columns m)]
    (mapv (fn [col]
            (reduce + (map (fn [row] (m/sq (get-in m [row col] 0.0)))
                        (range start-row nr))))
      (range nc))))

(defn rank-revealing-qr-decomposition
  "Computes the rank-revealing QR decomposition of a matrix with column pivoting.

  The rank-revealing QR decomposition of matrix A consists of three matrices
  Q, R, and P (permutation) such that: A × P = Q × R

  - Q is orthogonal (Q^T × Q = I)
  - R is upper triangular with diagonal elements in decreasing order of magnitude
  - P is a permutation matrix

  Column pivoting selects the column with the largest remaining norm at each step,
  which reveals the numerical rank of the matrix.

  Parameters:
    m         - Input matrix (m × n)
    tolerance - Threshold for determining rank. Diagonal elements of R with
                absolute value below this are considered zero.

  Returns a map containing:
    ::Q                - Orthogonal matrix (m × m)
    ::R                - Upper triangular matrix (m × n)
    ::RRQR-permutation - Permutation matrix (n × n), such that A × P = Q × R
    ::rank             - Numerical rank of the matrix

  Returns nil for empty matrices.

  Complexity: O(mn²) for m×n matrix

  Example:
    (rank-revealing-qr-decomposition [[1 2 3] [4 5 6] [7 8 9]] 1e-10)
    ;=> reveals rank 2 (matrix is rank-deficient)"
  [m tolerance]
  (when (and (mx/matrix? m) (not (mx/empty-matrix? m)))
    (let [nr (mx/rows m)
          nc (mx/columns m)
          min-dim (min nr nc)]
      (if (zero? min-dim)
        {::Q                (mx/identity-matrix nr)
         ::R                m
         ::RRQR-permutation (mx/identity-matrix nc)
         ::rank             0}
        ;; Apply column pivoting during Householder QR
        (loop [k 0
               R (mapv #(mapv double %) m)
               Q (mx/identity-matrix nr)
               perm (vec (range nc))                        ; track column permutation as indices
               col-norms-sq (column-norms-squared m 0)]
          (if (>= k min-dim)
            ;; Build permutation matrix and compute rank
            (let [P (mx/compute-matrix nc nc
                      (fn [i j] (if (= (get perm j) i) 1.0 0.0)))
                  rank (count (filter (fn [i]
                                        (and (< i nr)
                                          (> (m/abs (get-in R [i i] 0.0)) tolerance)))
                                (range min-dim)))]
              {::Q                Q
               ::R                R
               ::RRQR-permutation P
               ::rank             rank})
            ;; Find column with maximum remaining norm (from k to nc-1)
            (let [remaining-norms (mapv (fn [col]
                                          (if (< col k)
                                            0.0             ; already processed
                                            (get col-norms-sq col 0.0)))
                                    (range nc))
                  max-col (reduce (fn [best col]
                                    (if (> (get remaining-norms col)
                                          (get remaining-norms best))
                                      col
                                      best))
                            k (range k nc))
                  ;; Swap columns k and max-col in R
                  [R perm] (if (= k max-col)
                             [R perm]
                             (let [R-swapped (mapv (fn [row]
                                                     (let [v-k (get row k)
                                                           v-max (get row max-col)]
                                                       (-> row
                                                         (assoc k v-max)
                                                         (assoc max-col v-k))))
                                               R)
                                   perm-swapped (-> perm
                                                  (assoc k (get perm max-col))
                                                  (assoc max-col (get perm k)))]
                               [R-swapped perm-swapped]))
                  ;; Also swap in col-norms-sq
                  col-norms-sq (if (= k max-col)
                                 col-norms-sq
                                 (-> col-norms-sq
                                   (assoc k (get col-norms-sq max-col))
                                   (assoc max-col (get col-norms-sq k))))
                  ;; Compute Householder reflection for column k using helper functions
                  col-k (mapv #(get-in R [% k] 0.0) (range k nr))
                  v (householder-vector col-k)
                  R-new (apply-householder R v k k)
                  Q-new (accumulate-Q Q v k)
                  ;; Update column norms for remaining columns (subtract contribution from row k)
                  col-norms-sq-updated (mapv (fn [col]
                                               (if (<= col k)
                                                 (get col-norms-sq col)
                                                 (max 0.0 (- (get col-norms-sq col)
                                                            (m/sq (get-in R-new [k col] 0.0))))))
                                         (range nc))]
              (recur (inc k) R-new Q-new perm col-norms-sq-updated))))))))

(s/fdef rank-revealing-qr-decomposition
  :args (s/cat :m ::mx/matrix :tolerance ::m/accu)
  :ret (s/nilable (s/keys :req [::Q ::R ::RRQR-permutation ::rank])))

;;;LEAST SQUARES
(defn least-squares
  "Solves the linear least squares problem: find x that minimizes ||Ax - b||².

  Uses QR decomposition for numerical stability. Works for overdetermined systems (more rows than
  columns).

  Complexity: O(mn²) for m×n matrix

  Returns nil if matrix has more columns than rows or is rank-deficient.

  Examples:
    ;; Fit line y = mx + c to points (1,1), (2,2), (3,4)
    (least-squares [[1 1] [2 1] [3 1]] [1 2 4])
    ;=> approximately [1.5 -0.333]

  See also: solve, qr-decomposition"
  [A b]
  (when (and (mx/matrix? A)
          (not (mx/empty-matrix? A))
          (>= (mx/rows A) (mx/columns A))
          (= (mx/rows A) (count b)))
    (let [{::keys [Q R]} (qr-decomposition A)
          nr (mx/rows A)
          nc (mx/columns A)
          Qt (mx/transpose Q)
          Qtb (mapv (fn [row]
                      (reduce + (map * (get Qt row) b)))
                (range nr))
          R-square (mapv #(subvec % 0 nc) (subvec R 0 nc))
          b-truncated (subvec (vec Qtb) 0 nc)]
      (back-substitution R-square b-truncated))))

(s/fdef least-squares
  :args (s/cat :A ::mx/matrix
          :b ::vector/vector)
  :ret (s/nilable ::vector/vector))

;;;SCHUR DECOMPOSITION
(defn- hessenberg-reduction
  "Reduces a square matrix to upper Hessenberg form using Householder reflections.
  Returns [H Q] where H = Q^T * A * Q and H is upper Hessenberg.
  An upper Hessenberg matrix has zeros below the first subdiagonal."
  [A]
  (let [n (mx/rows A)]
    (if (< n 3)
      [A (mx/identity-matrix n)]
      (loop [k 0
             H (mapv #(mapv double %) A)
             Q (mx/identity-matrix n)]
        (if (>= k (- n 2))
          [H Q]
          (let [;; Extract column below diagonal
                col-below (mapv #(get-in H [% k] 0.0) (range (inc k) n))
                norm-col (m/sqrt (reduce + (map m/sq col-below)))
                ;; Skip if column is effectively zero
                skip? (< norm-col m/dbl-close)]
            (if skip?
              (recur (inc k) H Q)
              (let [;; Householder vector
                    sign (if (>= (first col-below) 0.0) 1.0 -1.0)
                    v0 (+ (first col-below) (* sign norm-col))
                    v (into [v0] (rest col-below))
                    v-norm-sq (reduce + (map m/sq v))
                    ;; Apply Householder to H from left: H <- (I - 2vv^T/|v|^2) * H
                    m-size (- n (inc k))
                    H-after-left
                    (reduce
                      (fn [H-cur j]
                        (let [dot-vH (reduce + (map-indexed
                                                 (fn [i vi]
                                                   (* vi (get-in H-cur [(+ (inc k) i) j] 0.0)))
                                                 v))
                              factor (/ (* 2.0 dot-vH) v-norm-sq)]
                          (reduce
                            (fn [H-inner i]
                              (let [row (+ (inc k) i)]
                                (update-in H-inner [row j]
                                  #(- % (* factor (nth v i))))))
                            H-cur
                            (range m-size))))
                      H
                      (range n))
                    ;; Apply Householder to H from right: H <- H * (I - 2vv^T/|v|^2)
                    H-after-right
                    (reduce
                      (fn [H-cur i]
                        (let [dot-Hv (reduce + (map-indexed
                                                 (fn [j vj]
                                                   (* vj (get-in H-cur [i (+ (inc k) j)] 0.0)))
                                                 v))
                              factor (/ (* 2.0 dot-Hv) v-norm-sq)]
                          (reduce
                            (fn [H-inner j]
                              (let [col (+ (inc k) j)]
                                (update-in H-inner [i col]
                                  #(- % (* factor (nth v j))))))
                            H-cur
                            (range m-size))))
                      H-after-left
                      (range n))
                    ;; Apply Householder to Q from right: Q <- Q * (I - 2vv^T/|v|^2)
                    Q-new
                    (reduce
                      (fn [Q-cur i]
                        (let [dot-Qv (reduce + (map-indexed
                                                 (fn [j vj]
                                                   (* vj (get-in Q-cur [i (+ (inc k) j)] 0.0)))
                                                 v))
                              factor (/ (* 2.0 dot-Qv) v-norm-sq)]
                          (reduce
                            (fn [Q-inner j]
                              (let [col (+ (inc k) j)]
                                (update-in Q-inner [i col]
                                  #(- % (* factor (nth v j))))))
                            Q-cur
                            (range m-size))))
                      Q
                      (range n))]
                (recur (inc k) H-after-right Q-new)))))))))

(defn- schur-converged?
  "Checks if Schur decomposition has converged.
  Converged when all subdiagonal elements are negligible except possibly
  within 2x2 blocks for complex eigenvalues."
  [T tolerance]
  (let [n (mx/rows T)]
    (if (< n 2)
      true
      (loop [i 0]
        (if (>= i (dec n))
          true
          (let [sub-diag (m/abs (get-in T [(inc i) i] 0.0))
                diag-sum (+ (m/abs (get-in T [i i] 0.0))
                           (m/abs (get-in T [(inc i) (inc i)] 0.0)))]
            (if (< sub-diag (* tolerance (max diag-sum 1.0)))
              ;; Element is negligible, check next
              (recur (inc i))
              ;; Check if this is a 2x2 block with complex eigenvalues
              (let [a (get-in T [i i] 0.0)
                    b (get-in T [i (inc i)] 0.0)
                    c (get-in T [(inc i) i] 0.0)
                    d (get-in T [(inc i) (inc i)] 0.0)
                    ;; Discriminant of 2x2 block eigenvalue equation
                    trace (+ a d)
                    det (- (* a d) (* b c))
                    discriminant (- (m/sq trace) (* 4.0 det))]
                (if (neg? discriminant)
                  ;; Complex eigenvalues - this is a valid 2x2 block, skip it
                  (if (< (+ i 2) n)
                    ;; Check element below the 2x2 block
                    (let [below-block (m/abs (get-in T [(+ i 2) (inc i)] 0.0))]
                      (if (< below-block (* tolerance (max diag-sum 1.0)))
                        (recur (+ i 2))
                        false))
                    true)                                   ;; 2x2 block at bottom, converged
                  ;; Real eigenvalues but non-zero subdiagonal - not converged
                  false)))))))))

(defn- schur-qr-step
  "Performs one QR iteration step with Francis double shift on Hessenberg matrix.
  Uses implicit QR for efficiency. Returns [T-new Q-step]."
  [T]
  (let [n (mx/rows T)]
    (if (< n 2)
      [T (mx/identity-matrix n)]
      (let [;; Wilkinson shift from bottom 2x2 corner
            a (get-in T [(- n 2) (- n 2)] 0.0)
            b (get-in T [(- n 2) (- n 1)] 0.0)
            c (get-in T [(- n 1) (- n 2)] 0.0)
            d (get-in T [(- n 1) (- n 1)] 0.0)
            ;; Use the eigenvalue of the 2x2 block closest to d
            delta (/ (- a d) 2.0)
            sign (if (>= delta 0.0) 1.0 -1.0)
            denom (+ (m/abs delta) (m/sqrt (+ (m/sq delta) (* b c))))
            shift (if (m/roughly? denom 0.0 m/dbl-close)
                    d
                    (- d (/ (* sign b c) denom)))
            ;; Apply shift
            T-shifted (mx/compute-matrix n n
                        (fn [i j]
                          (if (= i j)
                            (- (get-in T [i j] 0.0) shift)
                            (get-in T [i j] 0.0))))
            ;; QR decomposition
            {::keys [Q R]} (qr-decomposition T-shifted)
            ;; Compute R*Q and add shift back
            RQ (mx/mx* R Q)
            T-new (mx/compute-matrix n n
                    (fn [i j]
                      (if (= i j)
                        (+ (get-in RQ [i j] 0.0) shift)
                        (get-in RQ [i j] 0.0))))]
        [T-new Q]))))

(defn schur-decomposition
  "Computes the real Schur decomposition of a square matrix.

  Decomposes A = Q * T * Q^T where:
  - Q is orthogonal (Q^T * Q = I)
  - T is quasi-upper-triangular (upper triangular with possible 2x2 blocks
    on the diagonal for complex conjugate eigenvalue pairs)

  Uses Hessenberg reduction followed by QR iteration with Wilkinson shifts.

  Complexity: O(n^3) with iterative refinement

  Options:
  - :max-iterations - Maximum QR iterations (default 100)
  - :tolerance - Convergence tolerance (default 1e-12)

  Returns a map with ::schur-Q and ::schur-T, or nil for invalid input.

  Examples:
    (schur-decomposition [[4 1] [2 3]])
    ;=> {::schur-Q [[...] [...]] ::schur-T [[...] [...]]}

    ;; Verify: A = Q * T * Q^T
    (let [{::keys [schur-Q schur-T]} (schur-decomposition m)]
      (mx/mx* schur-Q (mx/mx* schur-T (mx/transpose schur-Q))))

  Notes:
  - Real eigenvalues appear on the diagonal of T
  - Complex conjugate eigenvalue pairs appear as 2x2 blocks on the diagonal
  - More numerically stable than eigendecomposition for non-symmetric matrices

  See also: eigen-decomposition, qr-decomposition"
  ([square-m] (schur-decomposition square-m {}))
  ([square-m {:keys [max-iterations tolerance]
              :or   {max-iterations 100
                     tolerance      1e-12}}]
   (when (and (mx/square-matrix? square-m)
           (not (mx/empty-matrix? square-m))
           (mx/matrix-finite? square-m))
     (let [n (mx/rows square-m)]
       (if (= n 1)
         ;; 1x1 case: already in Schur form
         {::schur-Q [[1.0]]
          ::schur-T (mapv #(mapv double %) square-m)}
         ;; General case: Hessenberg reduction + QR iteration
         (let [[H Q-hess] (hessenberg-reduction square-m)
               [final-T final-Q converged?]
               (loop [T H
                      Q Q-hess
                      iter 0]
                 (if (>= iter max-iterations)
                   [T Q false]
                   (if (schur-converged? T tolerance)
                     [T Q true]
                     (let [[T-new Q-step] (schur-qr-step T)
                           Q-new (mx/mx* Q Q-step)]
                       (recur T-new Q-new (inc iter))))))]
           (when converged?
             {::schur-Q final-Q
              ::schur-T final-T})))))))

(s/fdef schur-decomposition
  :args (s/cat :square-m ::mx/square-matrix
          :opts (s/? (s/keys :opt-un [::max-iterations ::tolerance])))
  :ret (s/nilable (s/keys :req [::schur-Q ::schur-T])))

;;;EIGENDECOMPOSITION
(defn- find-unreduced-block
  "Finds the largest unreduced block in a tridiagonal/symmetric matrix.
  Returns [start end] indices of the block that still needs iteration.
  If all converged, returns nil."
  [A tolerance]
  (let [n (mx/rows A)]
    (when (> n 1)
      ;; Find the last non-negligible subdiagonal from bottom
      (loop [end (dec n)]
        (if (< end 1)
          nil                                               ; All converged
          (let [sub-diag (m/abs (get-in A [(dec end) end] 0.0))
                diag-sum (+ (m/abs (get-in A [(dec end) (dec end)] 0.0))
                           (m/abs (get-in A [end end] 0.0)))]
            (if (< sub-diag (* tolerance (max diag-sum 1.0)))
              (recur (dec end))                             ; This one converged, check earlier
              ;; Found non-converged element, now find start
              (loop [start (dec end)]
                (if (< start 1)
                  [0 end]                                   ; Block starts at 0
                  (let [sub-diag (m/abs (get-in A [(dec start) start] 0.0))
                        diag-sum (+ (m/abs (get-in A [(dec start) (dec start)] 0.0))
                                   (m/abs (get-in A [start start] 0.0)))]
                    (if (< sub-diag (* tolerance (max diag-sum 1.0)))
                      [start end]                           ; Block starts here
                      (recur (dec start)))))))))))))

(defn- qr-iteration-step-block
  "Performs one step of QR iteration with Wilkinson shift on a subblock.
  block-start and block-end define the unreduced portion [start, end] inclusive."
  [A block-start block-end]
  (let [n (mx/rows A)
        ;; Compute Wilkinson shift from bottom-right 2x2 of the block
        shift (if (= block-start block-end)
                0.0
                (let [a (get-in A [(dec block-end) (dec block-end)] 0.0)
                      b (get-in A [(dec block-end) block-end] 0.0)
                      c (get-in A [block-end block-end] 0.0)
                      delta (/ (- a c) 2.0)
                      sign (if (>= delta 0.0) 1.0 -1.0)
                      denom (+ (m/abs delta) (m/sqrt (+ (m/sq delta) (m/sq b))))
                      mu (if (m/roughly? denom 0.0 m/dbl-close)
                           c
                           (- c (/ (* sign b b) denom)))]
                  mu))
        ;; Apply shift only to the block
        A-shifted (mx/compute-matrix n n
                    (fn [i j]
                      (if (and (= i j) (<= block-start i block-end))
                        (- (get-in A [i j] 0.0) shift)
                        (get-in A [i j] 0.0))))
        {::keys [Q R]} (qr-decomposition A-shifted)
        RQ (mx/mx* R Q)
        ;; Add shift back
        A-new (mx/compute-matrix n n
                (fn [i j]
                  (if (and (= i j) (<= block-start i block-end))
                    (+ (get-in RQ [i j] 0.0) shift)
                    (get-in RQ [i j] 0.0))))]
    [A-new Q]))

(defn eigen-decomposition
  "Computes eigenvalue decomposition of a symmetric matrix.

  Uses the QR algorithm with Wilkinson shifts and implicit deflation.
  Decomposes a symmetric matrix A into A = V * D * V^T where D is diagonal
  (eigenvalues) and V is orthogonal (eigenvectors as columns).

  Complexity: O(n³) per iteration, typically O(n³) total for convergence

  Only works reliably for symmetric matrices.

  Options:
  - :max-iterations (default 1000)
  - :tolerance (default 1e-10)

  Returns a map with ::eigenvalues, ::eigenvalues-matrix, ::eigenvectors,
  ::eigenvectorsT, or nil if fails.

  Examples:
    (eigen-decomposition [[2 1] [1 2]])
    ;=> eigenvalues [3.0 1.0]

  See also: sv-decomposition, cholesky-decomposition"
  ([symmetric-m] (eigen-decomposition symmetric-m {}))
  ([symmetric-m {:keys [max-iterations tolerance]
                 :or   {max-iterations 1000
                        tolerance      1e-10}}]
   (when (and (mx/symmetric-matrix? symmetric-m)
           (not (mx/empty-matrix? symmetric-m)))
     (let [n (mx/rows symmetric-m)
           initial-A (mapv #(mapv double %) symmetric-m)
           initial-V (mx/identity-matrix n)
           [final-A final-V converged?]
           (loop [A initial-A
                  V initial-V
                  iter 0]
             (if (>= iter max-iterations)
               [A V false]
               ;; Use implicit deflation: find the unreduced block
               (if-let [[block-start block-end] (find-unreduced-block A tolerance)]
                 ;; Still have unreduced elements - iterate on the block
                 (let [[A-new Q] (qr-iteration-step-block A block-start block-end)
                       V-new (mx/mx* V Q)]
                   (recur A-new V-new (inc iter)))
                 ;; All converged
                 [A V true])))]
       (when converged?
         (let [eigenvalues (mx/diagonal final-A)
               indexed (map-indexed vector eigenvalues)
               sorted-indices (map first (sort-by #(- (m/abs (second %))) indexed))
               sorted-eigenvalues (mapv #(get eigenvalues %) sorted-indices)
               sorted-V (mx/transpose (mapv #(mx/get-column final-V %) sorted-indices))]
           {::eigenvalues        sorted-eigenvalues
            ::eigenvalues-matrix (mx/diagonal-matrix sorted-eigenvalues)
            ::eigenvectors       sorted-V
            ::eigenvectorsT      (mx/transpose sorted-V)}))))))

(s/fdef eigen-decomposition
  :args (s/cat :symmetric-m ::mx/symmetric-matrix
          :opts (s/? (s/keys :opt-un [::max-iterations ::tolerance])))
  :ret (s/nilable (s/keys :req [::eigenvalues ::eigenvalues-matrix
                                ::eigenvectors ::eigenvectorsT])))

;;;SVD (SINGULAR VALUE DECOMPOSITION)
(defn sv-decomposition
  "Computes the Singular Value Decomposition (SVD) of a matrix.

  Decomposes a matrix A (m×n) into A = U * Σ * V^T where:
  - U is m×m orthogonal (left singular vectors)
  - Σ is m×n diagonal (singular values, non-negative, descending)
  - V^T is n×n orthogonal (right singular vectors transposed)

  Complexity: O(min(mn², m²n))

  Returns a map with ::svd-left (U), ::singular-values, ::singular-values-matrix (Σ),
  ::svd-right (V^T), ::rank, ::condition-number, ::norm-spectral, or nil if fails.

  Examples:
    (sv-decomposition [[1 2] [3 4] [5 6]])
    ;=> U is 3x3, Σ is 3x2 diagonal, V^T is 2x2

  See also: eigen-decomposition, pseudoinverse, condition-number"
  ([m] (sv-decomposition m {}))
  ([m {:keys [rank-tolerance]
       :or   {rank-tolerance 1e-10}}]
   (when (and (mx/matrix? m) (not (mx/empty-matrix? m)))
     (let [nr (mx/rows m)
           nc (mx/columns m)
           At (mx/transpose m)
           AtA (mx/mx* At m)
           eigen-result (eigen-decomposition AtA)]
       (when eigen-result
         (let [eigenvalues (::eigenvalues eigen-result)
               V (::eigenvectors eigen-result)
               singular-values (mapv #(if (> % 0) (m/sqrt %) 0.0) eigenvalues)
               U-columns
               (mapv (fn [i]
                       (let [sigma (get singular-values i)
                             v-i (mx/get-column V i)]
                         (if (> sigma rank-tolerance)
                           (let [Av (mapv (fn [row]
                                            (reduce + (map * (get m row) v-i)))
                                      (range nr))]
                             (mapv #(/ % sigma) Av))
                           nil)))
                 (range nc))
               valid-U-columns (filterv some? U-columns)
               U (if (= (count valid-U-columns) nr)
                   (mx/transpose valid-U-columns)
                   (let [partial-U (if (empty? valid-U-columns)
                                     (mx/identity-matrix nr)
                                     (let [needed (- nr (count valid-U-columns))
                                           extended (concat valid-U-columns
                                                      (take needed
                                                        (map #(mx/get-column (mx/identity-matrix nr)
                                                                %)
                                                          (range nr))))
                                           extended-m (mx/transpose (vec (take nr extended)))
                                           {::keys [Q]} (qr-decomposition extended-m)
                                           ;; Fix sign flips: QR can negate columns. Correct signs
                                           ;; for the first (count valid-U-columns) columns of Q
                                           ;; to match the original U-columns.
                                           n-valid (count valid-U-columns)
                                           Q-corrected
                                           (mx/transpose
                                             (mapv (fn [col-idx]
                                                     (let [q-col (mx/get-column Q col-idx)]
                                                       (if (< col-idx n-valid)
                                                         (let [u-col (get valid-U-columns col-idx)
                                                               dot (reduce + (map * q-col u-col))]
                                                           (if (neg? dot)
                                                             (mapv - q-col)
                                                             q-col))
                                                         q-col)))
                                               (range nr)))]
                                       Q-corrected))]
                     partial-U))
               sigma-matrix (mx/compute-matrix nr nc
                              (fn [i j]
                                (if (= i j)
                                  (get singular-values i 0.0)
                                  0.0)))
               numerical-rank (count (filter #(> % rank-tolerance) singular-values))
               max-sigma (apply max singular-values)
               min-sigma (apply min singular-values)
               cond-num (if (m/roughly? min-sigma 0.0 m/dbl-close)
                          m/inf+
                          (/ max-sigma min-sigma))]
           {::svd-left               U
            ::singular-values        singular-values
            ::singular-values-matrix sigma-matrix
            ::svd-right              (mx/transpose V)
            ::rank                   numerical-rank
            ::condition-number       cond-num
            ::norm-spectral          max-sigma}))))))

(s/fdef sv-decomposition
  :args (s/cat :m ::mx/matrix
          :opts (s/? (s/keys :opt-un [::rank-tolerance])))
  :ret (s/nilable (s/keys :req [::svd-left ::singular-values ::singular-values-matrix
                                ::svd-right ::rank ::condition-number ::norm-spectral])))

;;;CONDITION NUMBER
(defn condition-number-from-svd
  "Extracts the condition number from an existing SVD result.

  This avoids redundant computation when you need both the SVD and the condition number.

  Takes the map returned by `sv-decomposition` which includes ::condition-number.

  Examples:
    (let [svd (sv-decomposition [[1 2] [3 4]])]
      (condition-number-from-svd svd)) ;=> ~14.93

  See also: sv-decomposition, condition-number"
  [{::keys [condition-number]}]
  condition-number)

(s/fdef condition-number-from-svd
  :args (s/cat :svd-result (s/keys :req [::condition-number]))
  :ret ::condition-number)

(defn condition-number
  "Computes the condition number of a matrix using SVD.

  The condition number is the ratio of largest to smallest singular value.
  Large values indicate ill-conditioning.

  Examples:
    (condition-number [[1 0] [0 1]]) ;=> 1.0
    (condition-number [[1 1] [1 1.001]]) ;=> ~2001

  See also: sv-decomposition, matrix-rank, condition-number-from-svd"
  [m]
  (if (mx/empty-matrix? m)
    m/nan
    (if-let [svd-result (sv-decomposition m)]
      (condition-number-from-svd svd-result)
      m/nan)))

(s/fdef condition-number
  :args (s/cat :m ::mx/matrix)
  :ret ::m/number)

;;;MATRIX RANK
(defn matrix-rank
  "Computes the numerical rank of a matrix using SVD.

  The rank is the number of singular values greater than tolerance.

  Examples:
    (matrix-rank [[1 2] [3 4]]) ;=> 2
    (matrix-rank [[1 2] [2 4]]) ;=> 1

  See also: sv-decomposition, condition-number"
  ([m] (matrix-rank m {}))
  ([m {:keys [tolerance]}]
   (if (mx/empty-matrix? m)
     0
     (let [svd-result (sv-decomposition m {:rank-tolerance 0.0})
           singular-values (::singular-values svd-result)]
       (if (empty? singular-values)
         0
         (let [max-sv (apply max singular-values)
               max-dim (max (mx/rows m) (mx/columns m))
               effective-tol (or tolerance (* max-dim 1e-6 max-sv))]
           (count (filter #(> % effective-tol) singular-values))))))))

(s/fdef matrix-rank
  :args (s/cat :m ::mx/matrix
          :opts (s/? (s/keys :opt-un [::tolerance])))
  :ret ::rank)

;;;INDUCED MATRIX NORMS
(defn norm-1
  "Computes the induced 1-norm (maximum absolute column sum).

  Examples:
    (norm-1 [[1 -2] [3 4]]) ;=> 6.0
    (norm-1 [[1 0] [0 1]]) ;=> 1.0

  See also: norm-inf-matrix, norm-spectral"
  [m]
  (if (mx/empty-matrix? m)
    0.0
    (apply max 0.0 (map #(reduce + (map m/abs %)) (mx/transpose m)))))

(s/fdef norm-1
  :args (s/cat :m ::mx/matrix-finite)
  :ret ::m/finite-non-)

(defn norm-inf-matrix
  "Computes the induced infinity-norm (maximum absolute row sum).

  Examples:
    (norm-inf-matrix [[1 -2] [3 4]]) ;=> 7.0
    (norm-inf-matrix [[1 0] [0 1]]) ;=> 1.0

  See also: norm-1, norm-spectral"
  [m]
  (if (mx/empty-matrix? m)
    0.0
    (apply max 0.0 (map #(reduce + (map m/abs %)) m))))

(s/fdef norm-inf-matrix
  :args (s/cat :m ::mx/matrix-finite)
  :ret ::m/finite-non-)

(defn norm-spectral-from-svd
  "Extracts the spectral norm from an existing SVD result.

  This avoids redundant computation when you need both the SVD and the spectral norm.

  Takes the map returned by `sv-decomposition` which includes ::norm-spectral.

  Examples:
    (let [svd (sv-decomposition [[3 0] [0 4]])]
      (norm-spectral-from-svd svd)) ;=> 4.0

  See also: sv-decomposition, norm-spectral"
  [{::keys [norm-spectral]}]
  norm-spectral)

(s/fdef norm-spectral-from-svd
  :args (s/cat :svd-result (s/keys :req [::norm-spectral]))
  :ret ::norm-spectral)

(defn norm-spectral
  "Computes the spectral norm (induced 2-norm, largest singular value).

  Note: For Frobenius norm, use tensor/norm.

  Examples:
    (norm-spectral [[1 0] [0 1]]) ;=> 1.0
    (norm-spectral [[3 0] [0 4]]) ;=> 4.0

  See also: norm-1, norm-inf-matrix, sv-decomposition, norm-spectral-from-svd"
  [m]
  (if (mx/empty-matrix? m)
    0.0
    (if-let [svd-result (sv-decomposition m)]
      (norm-spectral-from-svd svd-result)
      m/nan)))

(s/fdef norm-spectral
  :args (s/cat :m ::mx/matrix)
  :ret ::m/number)

;;;PSEUDOINVERSE
(defn pseudoinverse-from-svd
  "Computes the Moore-Penrose pseudoinverse from an existing SVD result.

  This avoids redundant computation when you need both the SVD and the pseudoinverse.

  For A = U * Σ * V^T, the pseudoinverse is A+ = V * Σ+ * U^T.

  Takes the map returned by `sv-decomposition` which includes
  ::svd-left, ::singular-values, and ::svd-right.

  Options:
  - :tolerance - threshold for zero singular values (default 1e-10)

  Note: For best results, call sv-decomposition with {:rank-tolerance 0.0}
  to get all singular values, then use this function's :tolerance option.

  Examples:
    (let [svd (sv-decomposition [[1 0] [0 2]] {:rank-tolerance 0.0})]
      (pseudoinverse-from-svd svd)) ;=> [[1.0 0.0] [0.0 0.5]]

  See also: sv-decomposition, pseudoinverse"
  ([svd-result] (pseudoinverse-from-svd svd-result {}))
  ([{::keys [svd-left singular-values svd-right]} {:keys [tolerance] :or {tolerance 1e-10}}]
   (let [nr (mx/rows svd-left)
         nc (mx/columns svd-right)]
     (if (empty? singular-values)
       (mx/constant-matrix nc nr 0.0)
       (let [sigma-plus-diag (mapv #(if (> % tolerance) (/ 1.0 %) 0.0) singular-values)
             sigma-plus (mx/compute-matrix nc nr
                          (fn [i j]
                            (if (= i j)
                              (get sigma-plus-diag i 0.0)
                              0.0)))
             V (mx/transpose svd-right)
             Ut (mx/transpose svd-left)]
         (mx/mx* V sigma-plus Ut))))))

(s/fdef pseudoinverse-from-svd
  :args (s/cat :svd-result ::svd-result
          :opts (s/? (s/keys :opt-un [::tolerance])))
  :ret ::mx/matrix)

(defn pseudoinverse
  "Computes the Moore-Penrose pseudoinverse using SVD.

  For A = U * Σ * V^T, the pseudoinverse is A+ = V * Σ+ * U^T.

  Properties:
  - For full-rank square matrices, equals the regular inverse
  - For overdetermined systems, A+ solves least squares
  - For underdetermined systems, gives minimum norm solution

  Options:
  - :tolerance - threshold for zero singular values (default 1e-10)

  Examples:
    (pseudoinverse [[1 0] [0 2]]) ;=> [[1.0 0.0] [0.0 0.5]]

  See also: inverse, sv-decomposition, solve, pseudoinverse-from-svd"
  ([m] (pseudoinverse m {}))
  ([m opts]
   (when-not (mx/empty-matrix? m)
     (when-let [svd-result (sv-decomposition m {:rank-tolerance 0.0})]
       (pseudoinverse-from-svd svd-result opts)))))

(s/fdef pseudoinverse
  :args (s/cat :m ::mx/matrix
          :opts (s/? (s/keys :opt-un [::tolerance])))
  :ret (s/nilable ::mx/matrix))

;;;MATRIX POWER
(defn matrix-power
  "Raises a square matrix to an integer power using binary exponentiation.

  For positive n: computes M^n = M * M * ... * M (n times)
  For n = 0: returns identity matrix
  For negative n: computes (M^(-1))^|n|

  Complexity: O(n³ log k) for n×n matrix raised to power k

  Returns:
  - The resulting matrix on success
  - Identity matrix for n = 0
  - Anomaly with ::anom/incorrect for non-square matrices
  - Anomaly with ::anom/no-solve for negative powers of singular matrices

  Examples:
    (matrix-power [[1 2] [3 4]] 2) ;=> [[7.0 10.0] [15.0 22.0]]
    (matrix-power [[1 2] [3 4]] 0) ;=> [[1.0 0.0] [0.0 1.0]]
    (matrix-power [[2 0] [0 3]] -1) ;=> [[0.5 0.0] [0.0 0.333...]]

  See also: inverse, mx*"
  [square-m n]
  (cond
    (not (mx/square-matrix? square-m))
    {::anom/category ::anom/incorrect
     ::anom/message  "matrix-power requires a square matrix"}

    (mx/empty-matrix? square-m)
    square-m

    (zero? n)
    (mx/identity-matrix (mx/rows square-m))

    (neg? n)
    (let [inv (inverse square-m)]
      (if (anom/anomaly? inv)
        inv
        (matrix-power inv (- n))))

    :else
    (loop [result (mx/identity-matrix (mx/rows square-m))
           base (mapv #(mapv double %) square-m)
           exp n]
      (if (zero? exp)
        result
        (let [new-result (if (odd? exp)
                           (mx/mx* result base)
                           result)
              new-base (mx/mx* base base)
              new-exp (quot exp 2)]
          (recur new-result new-base new-exp))))))

(s/fdef matrix-power
  :args (s/cat :square-m ::mx/square-matrix :n ::m/int)
  :ret (s/or :matrix ::mx/square-matrix :anomaly ::anom/anomaly))

;;;MATRIX EXPONENTIAL
(defn- matrix-exp-pade
  "Padé approximation for matrix exponential.
  Uses [p/q] Padé approximant where p = q = order."
  [m order]
  (let [n (mx/rows m)
        I (mx/identity-matrix n)
        ;; Compute coefficients for Padé approximation
        ;; c_k = (2p - k)! * p! / ((2p)! * k! * (p - k)!)
        factorial (fn [x] (reduce * 1N (range 1 (inc x))))
        pade-coef (fn [k p]
                    (/ (* (factorial (- (* 2 p) k)) (factorial p))
                      (* (factorial (* 2 p)) (factorial k) (factorial (- p k)))))
        ;; Compute powers of M
        powers (loop [k 1
                      current m
                      acc {0 I, 1 m}]
                 (if (>= k order)
                   acc
                   (let [next-pow (mx/mx* current m)]
                     (recur (inc k) next-pow (assoc acc (inc k) next-pow)))))
        ;; Compute numerator N = sum_{k=0}^{p} c_k * M^k
        ;; Compute denominator D = sum_{k=0}^{p} (-1)^k * c_k * M^k
        [numer denom]
        (reduce (fn [[N D] k]
                  (let [coef (double (pade-coef k order))
                        M-k (get powers k I)
                        sign (if (even? k) 1.0 -1.0)]
                    [(tensor/add N (tensor/multiply coef M-k))
                     (tensor/add D (tensor/multiply (* sign coef) M-k))]))
          [(mx/constant-matrix n n 0.0) (mx/constant-matrix n n 0.0)]
          (range (inc order)))]
    ;; Return N * D^(-1)
    (let [D-inv (inverse denom)]
      (when-not (anom/anomaly? D-inv)
        (mx/mx* numer D-inv)))))

(defn matrix-exp
  "Computes the matrix exponential e^M using scaling and squaring with Padé approximation.

  The matrix exponential is defined as:
    e^M = I + M + M²/2! + M³/3! + ...

  Uses the scaling and squaring method for numerical stability:
    e^M = (e^(M/2^s))^(2^s)

  Complexity: O(n³ log(||M||)) for n×n matrix

  Options:
  - :pade-order - order of Padé approximation (default 6)

  Returns the matrix exponential, or nil if computation fails.

  Examples:
    (matrix-exp [[0 0] [0 0]]) ;=> [[1.0 0.0] [0.0 1.0]]
    (matrix-exp [[1 0] [0 1]]) ;=> [[e 0] [0 e]] ≈ [[2.718... 0] [0 2.718...]]
    (matrix-exp [[0 1] [-1 0]]) ;=> rotation matrix

  See also: matrix-power, eigen-decomposition"
  ([m] (matrix-exp m {}))
  ([m {:keys [pade-order] :or {pade-order 6}}]
   (when (and (mx/square-matrix? m) (not (mx/empty-matrix? m)))
     (let [;; Scale matrix so ||M/2^s|| < 1
           norm (norm-1 m)
           s (max 0 (long (m/ceil (m/log2 (max 1.0 norm)))))
           scale (m/pow 2.0 (- s))
           M-scaled (tensor/multiply scale m)
           ;; Compute Padé approximation of e^(M/2^s)
           exp-scaled (matrix-exp-pade M-scaled pade-order)]
       (when exp-scaled
         ;; Square s times to get e^M
         (loop [result exp-scaled
                k 0]
           (if (>= k s)
             result
             (recur (mx/mx* result result) (inc k)))))))))

(s/fdef matrix-exp
  :args (s/cat :m ::mx/square-matrix-finite
          :opts (s/? (s/keys :opt-un [::pade-order])))
  :ret (s/nilable ::mx/square-matrix))
