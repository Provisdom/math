(ns provisdom.math.linear-algebra-test
  (:require
    [provisdom.math.core :as m]
    [provisdom.math.linear-algebra :as la]
    [provisdom.math.matrix :as mx]
    [provisdom.math.random :as random]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anom]))

;;52 seconds

(set! *warn-on-reflection* true)

;;;LU DECOMPOSITION
(t/deftest lu-decomposition-test
  (t/with-instrument `la/lu-decomposition
    (t/is-spec-check la/lu-decomposition))
  (t/with-instrument :all
    (t/is= nil (la/lu-decomposition [[]]))
    ;;Math Det[{{1, 0.5}, {2, 4}}] = 3
    ;;Math Inverse[{{1, 0.5}, {2, 4}}] = {{4/3, -1/6}, {-2/3, 1/3}}
    (let [m [[1.0 0.5] [2.0 4.0]]
          result (la/lu-decomposition m)
          {::la/keys [L U LU-permutation determinant inverse singular?]} result]
      (t/is-not singular?)
      (t/is= [[1.0 0.0] [0.5 1.0]] L)
      (t/is= [[2.0 4.0] [0.0 -1.5]] U)
      (t/is= [[0.0 1.0] [1.0 0.0]] LU-permutation)
      (t/is-approx= 3.0 determinant :tolerance 1e-10)
      (t/is-data-approx= [[1.3333333333333333 -0.16666666666666663]
                          [-0.6666666666666666 0.3333333333333333]]
        inverse :tolerance 1e-10))
    ;; Reconstruction test: P * L * U = A (since PA = LU means P^-1 * L * U = A,
    ;; and for permutation matrices P^-1 = P^T = P when it's a simple swap)
    (let [m [[4.0 3.0] [6.0 3.0]]
          result (la/lu-decomposition m)
          {::la/keys [L U LU-permutation singular?]} result
          reconstructed (mx/mx* LU-permutation (mx/mx* L U))]
      (t/is-not singular?)
      (t/is-data-approx= m reconstructed :tolerance 1e-10))))

;;;DETERMINANT
(t/deftest determinant-from-lu-test
  (t/with-instrument `la/determinant-from-lu
    (t/is-spec-check la/determinant-from-lu))
  (t/with-instrument :all
    (let [lu (la/lu-decomposition [[4.0 3.0] [6.0 3.0]])]
      (t/is-approx= -6.0 (la/determinant-from-lu lu) :tolerance 1e-10))
    (let [lu (la/lu-decomposition [[1.0 2.0] [3.0 4.0]])]
      (t/is-approx= -2.0 (la/determinant-from-lu lu) :tolerance 1e-10))))

(t/deftest determinant-test
  (t/with-instrument `la/determinant
    (t/is-spec-check la/determinant))
  (t/with-instrument :all
    (t/is (Double/isNaN (la/determinant [[]])))
    (t/is= 4.0 (la/determinant [[4.0]]))
    (t/is-approx= -2.0 (la/determinant [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10)
    (t/is-approx= 0.0 (la/determinant [[1.0 2.0] [2.0 4.0]]) :tolerance 1e-10)))

;;;MINORS AND COFACTORS
(t/deftest minor-test
  (t/with-instrument `la/minor
    (t/is-spec-check la/minor))
  (t/with-instrument :all
    (t/is= nil (la/minor [[1.0]] 0 0))
    (t/is= 4.0 (la/minor [[1.0 2.0] [3.0 4.0]] 0 0))
    (t/is= 3.0 (la/minor [[1.0 2.0] [3.0 4.0]] 0 1))
    (t/is= 2.0 (la/minor [[1.0 2.0] [3.0 4.0]] 1 0))
    (t/is= 1.0 (la/minor [[1.0 2.0] [3.0 4.0]] 1 1))
    (t/is-approx= -3.0
      (la/minor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]] 0 0) :tolerance 1e-10)))

(t/deftest cofactor-test
  (t/with-instrument `la/cofactor
    (t/is-spec-check la/cofactor))
  (t/with-instrument :all
    (t/is= nil (la/cofactor [[1.0]] 0 0))
    (t/is= 4.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 0 0))
    (t/is= -3.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 0 1))
    (t/is= -2.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 1 0))
    (t/is= 1.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 1 1))))

(t/deftest cofactor-matrix-test
  (t/with-instrument `la/cofactor-matrix
    (t/is-spec-check la/cofactor-matrix))
  (t/with-instrument :all
    (t/is= nil (la/cofactor-matrix [[]]))
    (t/is= [[1.0]] (la/cofactor-matrix [[5.0]]))
    (t/is-data-approx= [[4.0 -3.0] [-2.0 1.0]]
      (la/cofactor-matrix [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10)))

(t/deftest adjugate-test
  (t/with-instrument `la/adjugate
    (t/is-spec-check la/adjugate))
  (t/with-instrument :all
    (t/is= nil (la/adjugate [[]]))
    (t/is= [[1.0]] (la/adjugate [[5.0]]))
    (t/is-data-approx= [[4.0 -2.0] [-3.0 1.0]]
      (la/adjugate [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10)
    ;; For invertible matrices: A * adj(A) = det(A) * I
    (let [A [[1.0 2.0] [3.0 4.0]]
          adj (la/adjugate A)
          det (la/determinant A)
          product (mx/mx* A adj)]
      (t/is-data-approx= [[det 0.0] [0.0 det]] product :tolerance 1e-10))))

;;;INVERSE
(t/deftest inverse-from-lu-test
  (t/with-instrument `la/inverse-from-lu
    (t/is-spec-check la/inverse-from-lu))
  (t/with-instrument :all
    (let [lu (la/lu-decomposition [[4.0 7.0] [2.0 6.0]])
          inv (la/inverse-from-lu lu)
          expected [[0.6 -0.7] [-0.2 0.4]]]
      (t/is-data-approx= expected inv :tolerance 1e-10))
    ;; Singular matrix returns anomaly
    (let [lu (la/lu-decomposition [[1.0 2.0] [2.0 4.0]])
          result (la/inverse-from-lu lu)]
      (t/is= ::anom/no-solve (::anom/category result)))))

(t/deftest inverse-test
  (t/with-instrument `la/inverse
    (t/is-spec-check la/inverse))
  (t/with-instrument :all
    (t/is= [[]] (la/inverse [[]]))
    (t/is= [[0.25]] (la/inverse [[4.0]]))
    (let [m [[1.0 2.0] [3.0 4.0]]
          inv (la/inverse m)
          product (mx/mx* m inv)]
      (t/is-data-approx= [[1.0 0.0] [0.0 1.0]] product :tolerance 1e-10))
    (t/is= ::anom/no-solve (::anom/category (la/inverse [[1.0 2.0] [2.0 4.0]])))))

;;;SOLVE LINEAR SYSTEM
(t/deftest solve-test
  (t/with-instrument `la/solve
    (t/is-spec-check la/solve))
  (t/with-instrument :all
    (t/is= [] (la/solve [[]] []))
    (t/is-data-approx= [1.0 2.0] (la/solve [[1.0 0.0] [0.0 1.0]] [1.0 2.0]) :tolerance 1e-10)
    ;; Square system: 2x + y = 5, x + 3y = 7 => x = 1.6, y = 1.8
    (let [A [[2.0 1.0] [1.0 3.0]]
          b [5.0 7.0]
          x (la/solve A b)]
      (t/is-data-approx= [1.6 1.8] x :tolerance 1e-10))
    ;; Overdetermined system (least squares)
    (let [A [[1.0 1.0] [1.0 2.0] [1.0 3.0]]
          b [1.0 2.0 3.0]
          x (la/solve A b)]
      (t/is= 2 (count x))
      ;; Verify Ax is close to b (least squares sense)
      (let [Ax (mapv (fn [row] (reduce + (map * row x))) A)
            residual (reduce + (map #(m/sq (- %1 %2)) Ax b))]
        (t/is (< residual 0.5))))))

;;;CHOLESKY DECOMPOSITION
(t/deftest cholesky-decomposition-test
  (t/with-instrument `la/cholesky-decomposition
    (t/is-spec-check la/cholesky-decomposition))
  (t/with-instrument :all
    (t/is= nil (la/cholesky-decomposition [[]]))
    ;;Math CholeskyDecomposition[{{4}}] = {{2}}
    (t/is= {::la/cholesky-L [[2.0]] ::la/cholesky-LT [[2.0]]}
      (la/cholesky-decomposition [[4.0]]))
    ;;Math CholeskyDecomposition[{{1, 0.5}, {0.5, 3}}] -> L22 = Sqrt[11/4] = 1.6583123951776998
    (t/is-data-approx= {::la/cholesky-L  [[1.0 0.0] [0.5 1.6583123951777]]
                        ::la/cholesky-LT [[1.0 0.5] [0.0 1.6583123951777]]}
      (la/cholesky-decomposition [[1.0 0.5] [0.5 3.0]]) :tolerance 1e-10)
    ;; Reconstruction test
    (let [m [[4.0 2.0] [2.0 5.0]]
          result (la/cholesky-decomposition m)
          L (::la/cholesky-L result)
          reconstructed (mx/mx* L (mx/transpose L))]
      (t/is-data-approx= m reconstructed :tolerance 1e-10))))

(t/deftest rectangular-cholesky-decomposition-test
  (t/with-instrument `la/rectangular-cholesky-decomposition
    (t/is-spec-check la/rectangular-cholesky-decomposition {:num-tests 50}))
  (t/with-instrument :all
    ;; Empty matrix returns nil
    (t/is= nil (la/rectangular-cholesky-decomposition [[]] 1e-4))
    ;; Full rank 2x2
    (let [m [[1.0 0.5] [0.5 3.0]]
          result (la/rectangular-cholesky-decomposition m 1e-4)
          {::la/keys [rectangular-root rank]} result
          reconstructed (mx/mx* rectangular-root (mx/transpose rectangular-root))]
      (t/is= 2 rank)
      (t/is-data-approx= m reconstructed :tolerance 1e-10))
    ;; Rank-deficient 3x3 matrix (rank 2)
    (let [m [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]]
          result (la/rectangular-cholesky-decomposition m 1e-4)
          {::la/keys [rectangular-root rank]} result
          reconstructed (mx/mx* rectangular-root (mx/transpose rectangular-root))]
      (t/is= 2 rank)
      (t/is-data-approx= m reconstructed :tolerance 1e-6))
    ;; Rank 1 matrix
    (let [m [[0.08061440651728713 0.048368643910372044]
             [0.048368643910372044 0.029021186346223526]]
          result (la/rectangular-cholesky-decomposition m 1e-14)
          {::la/keys [rectangular-root rank]} result
          reconstructed (mx/mx* rectangular-root (mx/transpose rectangular-root))]
      (t/is= 1 rank)
      (t/is-data-approx= m reconstructed :tolerance 1e-10))))

;;;POSITIVE DEFINITE TESTS
(t/deftest pos-definite-matrix-finite?-test
  (t/with-instrument `la/pos-definite-matrix-finite?
    (t/is-spec-check la/pos-definite-matrix-finite?))
  (t/with-instrument :all
    (t/is (la/pos-definite-matrix-finite? [[1.0]]))
    (t/is (la/pos-definite-matrix-finite? [[4.0 2.0] [2.0 5.0]]))
    (t/is-not (la/pos-definite-matrix-finite? [[1.0 2.0] [2.0 1.0]]))))

(t/deftest pos-semidefinite-matrix-finite?-test
  (t/with-instrument `la/pos-semidefinite-matrix-finite?
    (t/is-spec-check la/pos-semidefinite-matrix-finite?))
  (t/with-instrument :all
    (t/is (la/pos-semidefinite-matrix-finite? [[1.0]] 1e-10))
    (t/is (la/pos-semidefinite-matrix-finite? [[4.0 2.0] [2.0 5.0]] 1e-10))
    (t/is (la/pos-semidefinite-matrix-finite? [[1.0 1.0] [1.0 1.0]] 1e-10))
    (t/is-not (la/pos-semidefinite-matrix-finite? [[1.0 2.0] [2.0 1.0]] 1e-10))))

(t/deftest pos-semidefinite-matrix-finite-by-squaring-test
  (t/with-instrument `la/pos-semidefinite-matrix-finite-by-squaring
    (t/is-spec-check la/pos-semidefinite-matrix-finite-by-squaring))
  (t/with-instrument :all
    (t/is-data-approx= [[5.0 11.0] [11.0 25.0]]
      (la/pos-semidefinite-matrix-finite-by-squaring [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10)
    ;; Result should be positive semi-definite
    (let [result (la/pos-semidefinite-matrix-finite-by-squaring [[1.0 2.0] [3.0 4.0]])]
      (t/is (la/pos-semidefinite-matrix-finite? result 1e-10)))))

(t/deftest pos-definite-matrix-finite-by-squaring-test
  (t/with-instrument `la/pos-definite-matrix-finite-by-squaring
    (t/is-spec-check la/pos-definite-matrix-finite-by-squaring))
  (t/with-instrument :all
    ;; Result should be positive definite
    (let [result (la/pos-definite-matrix-finite-by-squaring [[1.0 2.0] [3.0 4.0]])]
      (t/is (la/pos-definite-matrix-finite? result)))
    ;; With custom epsilon
    (let [result (la/pos-definite-matrix-finite-by-squaring [[1.0 2.0] [3.0 4.0]] 0.5)]
      (t/is (la/pos-definite-matrix-finite? result)))))

;;;CORRELATION MATRIX
(t/deftest correlation-matrix?-test
  (t/with-instrument `la/correlation-matrix?
    (t/is-spec-check la/correlation-matrix?))
  (t/with-instrument :all
    (t/is (la/correlation-matrix? [[1.0]] 1e-10))
    (t/is (la/correlation-matrix? [[1.0 0.5] [0.5 1.0]] 1e-10))
    (t/is (la/correlation-matrix? [[1.0 0.0] [0.0 1.0]] 1e-10))
    ;; Invalid: correlation > 1
    (t/is-not (la/correlation-matrix? [[1.0 1.5] [1.5 1.0]] 1e-10))
    ;; Invalid: not symmetric
    (t/is-not (la/correlation-matrix? [[1.0 0.5] [0.3 1.0]] 1e-10))
    ;; Invalid: diagonal not 1
    (t/is-not (la/correlation-matrix? [[2.0 0.5] [0.5 1.0]] 1e-10))))

(t/deftest covariance-matrix->correlation-matrix-test
  (t/with-instrument `la/covariance-matrix->correlation-matrix
    (t/is-spec-check la/covariance-matrix->correlation-matrix {:num-tests 300}))
  (t/with-instrument :all
    (let [cov [[4.0 2.0] [2.0 9.0]]
          result (la/covariance-matrix->correlation-matrix cov)]
      (t/is-data-approx= [[1.0 (/ 2.0 6.0)] [(/ 2.0 6.0) 1.0]] result :tolerance 1e-10))))

(t/deftest correlation-matrix->covariance-matrix-test
  (t/with-instrument `la/correlation-matrix->covariance-matrix
    (t/is-spec-check la/correlation-matrix->covariance-matrix {:num-tests 200}))
  (t/with-instrument :all
    (let [corr [[1.0 0.5] [0.5 1.0]]
          variances [4.0 9.0]
          result (la/correlation-matrix->covariance-matrix corr variances)]
      (t/is-data-approx= [[4.0 3.0] [3.0 9.0]] result :tolerance 1e-10))))

(t/deftest correlation-matrix-by-squaring-test
  (t/with-instrument `la/correlation-matrix-by-squaring
    (t/is-spec-check la/correlation-matrix-by-squaring))
  (t/with-instrument :all
    (t/is= nil (la/correlation-matrix-by-squaring [[]]))
    (let [result (la/correlation-matrix-by-squaring [[1.0 0.5] [0.3 1.0]])]
      (t/is (some? result))
      (t/is (la/correlation-matrix? result 1e-6)))))

(t/deftest rnd-pos-definite-matrix-finite!-test
  (t/with-instrument `la/rnd-pos-definite-matrix-finite!
    (t/is-spec-check la/rnd-pos-definite-matrix-finite!))
  (t/with-instrument :all
    (t/is= nil (la/rnd-pos-definite-matrix-finite! 0))
    (random/bind-seed 3
      (let [result (la/rnd-pos-definite-matrix-finite! 2)]
        (t/is (some? result))
        (t/is (la/pos-definite-matrix-finite? result))))))

(t/deftest rnd-correlation-matrix!-test
  (t/with-instrument `la/rnd-correlation-matrix!
    (t/is-spec-check la/rnd-correlation-matrix!))
  (t/with-instrument :all
    (t/is= nil (la/rnd-correlation-matrix! 0))
    (random/bind-seed 3
      (let [result (la/rnd-correlation-matrix! 2)]
        (when result
          (t/is (la/correlation-matrix? result 1e-6)))))))

;;;QR DECOMPOSITION
(t/deftest qr-decomposition-test
  (t/with-instrument `la/qr-decomposition
    (t/is-spec-check la/qr-decomposition))
  (t/with-instrument :all
    (t/is= nil (la/qr-decomposition [[]]))
    ;;Math QRDecomposition[{{1, 0.4, 0.2}, {0.6, 0.3, 0.9}}]
    ;;Math R11 = -Sqrt[1.36] = -1.16619037896906
    (let [m [[1.0 0.4 0.2] [0.6 0.3 0.9]]
          result (la/qr-decomposition m)
          {::la/keys [Q R]} result
          expected-Q [[-0.8574929257125441 0.5144957554275266]
                      [-0.5144957554275265 -0.8574929257125442]]
          expected-R [[-1.16619037896906 -0.4973458969132756 -0.6345447650272829]
                      [0.0 -0.051449575542752646 -0.6688444820557842]]]
      (t/is-data-approx= expected-Q Q :tolerance 1e-10)
      (t/is-data-approx= expected-R R :tolerance 1e-10))
    ;; Reconstruction test
    (let [m [[1.0 2.0] [3.0 4.0] [5.0 6.0]]
          result (la/qr-decomposition m)
          {::la/keys [Q R]} result
          reconstructed (mx/mx* Q R)]
      (t/is-data-approx= m reconstructed :tolerance 1e-10))
    ;; Orthogonality of Q: Q^T * Q = I
    (let [m [[1.0 2.0] [3.0 4.0] [5.0 6.0]]
          {::la/keys [Q]} (la/qr-decomposition m)
          QtQ (mx/mx* (mx/transpose Q) Q)]
      (t/is-data-approx= (mx/identity-matrix 3) QtQ :tolerance 1e-10))))

(t/deftest rank-revealing-qr-decomposition-test
  (t/with-instrument `la/rank-revealing-qr-decomposition
    (t/is-spec-check la/rank-revealing-qr-decomposition))
  (t/with-instrument :all
    ;; Empty matrix
    (t/is= nil (la/rank-revealing-qr-decomposition [[]] 1e-6))
    ;;Math MatrixRank[{{1, 0.5}, {2, 4}}] = 2
    (let [m [[1.0 0.5] [2.0 4.0]]
          result (la/rank-revealing-qr-decomposition m 1e-6)
          {::la/keys [Q R RRQR-permutation rank]} result
          AP (mx/mx* m RRQR-permutation)
          QR (mx/mx* Q R)
          expected-Q [[-0.12403473458920855 0.9922778767136677]
                      [-0.9922778767136677 -0.12403473458920833]]
          expected-R [[-4.031128874149275 -2.108590488016544] [0.0 0.7442084075352513]]]
      (t/is= 2 rank)
      (t/is= [[0.0 1.0] [1.0 0.0]] RRQR-permutation)
      (t/is-data-approx= expected-Q Q :tolerance 1e-10)
      (t/is-data-approx= expected-R R :tolerance 1e-10)
      (t/is-data-approx= AP QR :tolerance 1e-10))
    ;; Rank-deficient matrix (rank 2 out of 3)
    (let [m [[1 2 3] [4 5 6] [7 8 9]]
          result (la/rank-revealing-qr-decomposition m 1e-10)
          {::la/keys [rank]} result]
      (t/is= 2 rank))
    ;; Wide matrix (1x2)
    (let [m [[1.0 0.5]]
          result (la/rank-revealing-qr-decomposition m 1e-6)
          {::la/keys [RRQR-permutation rank]} result]
      (t/is= 1 rank)
      (t/is= [[1.0 0.0] [0.0 1.0]] RRQR-permutation))
    ;; Tall matrix (2x1)
    (let [m [[1.0] [0.5]]
          result (la/rank-revealing-qr-decomposition m 1e-6)
          {::la/keys [Q R RRQR-permutation rank]} result
          AP (mx/mx* m RRQR-permutation)
          QR (mx/mx* Q R)]
      (t/is= 1 rank)
      (t/is-data-approx= AP QR :tolerance 1e-10))))

;;;LEAST SQUARES
(t/deftest least-squares-test
  (t/with-instrument `la/least-squares
    (t/is-spec-check la/least-squares))
  (t/with-instrument :all
    (t/is= nil (la/least-squares [[]] []))
    ;; Fit line y = mx + c to points (1,1), (2,2), (3,4)
    ;; A = [[1 1] [2 1] [3 1]], b = [1 2 4]
    ;; Solution: m = 1.5, c = -2/3
    (let [A [[1.0 1.0] [2.0 1.0] [3.0 1.0]]
          b [1.0 2.0 4.0]
          x (la/least-squares A b)]
      (t/is-approx= 1.5 (first x) :tolerance 1e-10)
      (t/is-approx= -0.6666666666666666 (second x) :tolerance 1e-10))))

;;;SCHUR DECOMPOSITION
(t/deftest schur-decomposition-test
  (t/with-instrument `la/schur-decomposition
    (t/is-spec-check la/schur-decomposition {:num-tests 20}))
  (t/with-instrument :all
    (t/is= nil (la/schur-decomposition [[]]))
    ;; 1x1 case
    (let [m [[5.0]]
          result (la/schur-decomposition m)
          {::la/keys [schur-Q schur-T]} result]
      (t/is= [[1.0]] schur-Q)
      (t/is= [[5.0]] schur-T))
    ;; 2x2 with real eigenvalues
    ;; Matrix [[4, 1], [2, 3]] has eigenvalues 5 and 2
    (let [m [[4.0 1.0] [2.0 3.0]]
          result (la/schur-decomposition m)
          {::la/keys [schur-Q schur-T]} result
          ;; Verify Q is orthogonal: Q^T * Q = I
          QtQ (mx/mx* (mx/transpose schur-Q) schur-Q)
          ;; Verify reconstruction: A = Q * T * Q^T
          reconstructed (mx/mx* schur-Q (mx/mx* schur-T (mx/transpose schur-Q)))]
      (t/is-data-approx= [[1.0 0.0] [0.0 1.0]] QtQ :tolerance 1e-10)
      (t/is-data-approx= m reconstructed :tolerance 1e-10)
      ;; T should be upper triangular for real eigenvalues
      (t/is-approx= 0.0 (get-in schur-T [1 0]) :tolerance 1e-10))
    ;; 2x2 with complex eigenvalues (rotation matrix)
    ;; [[0, -1], [1, 0]] has eigenvalues i and -i
    (let [m [[0.0 -1.0] [1.0 0.0]]
          result (la/schur-decomposition m)
          {::la/keys [schur-Q schur-T]} result
          ;; Verify reconstruction
          reconstructed (mx/mx* schur-Q (mx/mx* schur-T (mx/transpose schur-Q)))]
      (t/is-data-approx= m reconstructed :tolerance 1e-10)
      ;; T should have a 2x2 block (non-zero subdiagonal) for complex eigenvalues
      (t/is (> (m/abs (get-in schur-T [1 0])) 0.1)))
    ;; 3x3 symmetric matrix
    (let [m [[2.0 1.0 0.0] [1.0 3.0 1.0] [0.0 1.0 2.0]]
          result (la/schur-decomposition m)
          {::la/keys [schur-Q schur-T]} result
          QtQ (mx/mx* (mx/transpose schur-Q) schur-Q)
          reconstructed (mx/mx* schur-Q (mx/mx* schur-T (mx/transpose schur-Q)))]
      (t/is-data-approx= [[1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]] QtQ :tolerance 1e-10)
      (t/is-data-approx= m reconstructed :tolerance 1e-10))))

;;;EIGENDECOMPOSITION
(t/deftest eigen-decomposition-test
  (t/with-instrument `la/eigen-decomposition
    (t/is-spec-check la/eigen-decomposition {:num-tests 20}))
  (t/with-instrument :all
    (t/is= nil (la/eigen-decomposition [[]]))
    ;; Diagonal matrix
    (let [m [[2.0 0.0] [0.0 3.0]]
          result (la/eigen-decomposition m)
          eigenvalues (::la/eigenvalues result)]
      (t/is= #{2.0 3.0} (set eigenvalues)))
    ;; Non-diagonal symmetric matrix
    (let [m [[2.0 1.0] [1.0 2.0]]
          result (la/eigen-decomposition m)
          {::la/keys [eigenvalues eigenvectors]} result]
      ;; Eigenvalues should be 3 and 1
      (t/is (some #(m/roughly? % 3.0 1e-10) eigenvalues))
      (t/is (some #(m/roughly? % 1.0 1e-10) eigenvalues))
      ;; Reconstruction: V * D * V^T = A
      (let [D (::la/eigenvalues-matrix result)
            V eigenvectors
            VT (mx/transpose V)
            reconstructed (mx/mx* V (mx/mx* D VT))]
        (t/is-data-approx= m reconstructed :tolerance 1e-10))
      ;; Orthogonality: V^T * V = I
      (let [V eigenvectors
            VtV (mx/mx* (mx/transpose V) V)]
        (t/is-data-approx= [[1.0 0.0] [0.0 1.0]] VtV :tolerance 1e-10)))))

;;;SVD (SINGULAR VALUE DECOMPOSITION)
(t/deftest sv-decomposition-test
  (t/with-instrument `la/sv-decomposition
    (t/is-spec-check la/sv-decomposition {:num-tests 30}))
  (t/with-instrument :all
    (t/is= nil (la/sv-decomposition [[]]))
    ;; Diagonal matrix
    (let [m [[3.0 0.0] [0.0 4.0]]
          result (la/sv-decomposition m)
          singular-values (::la/singular-values result)]
      (t/is-data-approx= [4.0 3.0] singular-values :tolerance 1e-10))
    ;; Non-diagonal square matrix - verify reconstruction
    (let [m [[1.0 2.0] [3.0 4.0]]
          result (la/sv-decomposition m)
          {::la/keys [svd-left singular-values-matrix svd-right]} result
          U svd-left
          S singular-values-matrix
          Vt svd-right
          reconstructed (mx/mx* U (mx/mx* S Vt))]
      (t/is-data-approx= m reconstructed :tolerance 1e-10))))

;;;CONDITION NUMBER
(t/deftest condition-number-from-svd-test
  (t/with-instrument `la/condition-number-from-svd
    (t/is-spec-check la/condition-number-from-svd))
  (t/with-instrument :all
    (let [svd (la/sv-decomposition [[1.0 0.0] [0.0 2.0]])]
      (t/is-approx= 2.0 (la/condition-number-from-svd svd) :tolerance 1e-10))
    (let [svd (la/sv-decomposition [[3.0 0.0] [0.0 4.0]])]
      (t/is-approx= (/ 4.0 3.0) (la/condition-number-from-svd svd) :tolerance 1e-10))))

(t/deftest condition-number-test
  (t/with-instrument `la/condition-number
    (t/is-spec-check la/condition-number {:num-tests 30}))
  (t/with-instrument :all
    (t/is (Double/isNaN (la/condition-number [[]])))
    (t/is-approx= 1.0 (la/condition-number [[2.0]]) :tolerance 1e-10)
    (t/is-approx= 1.0 (la/condition-number [[1.0 0.0] [0.0 1.0]]) :tolerance 1e-10)
    (t/is-approx= 2.0 (la/condition-number [[1.0 0.0] [0.0 2.0]]) :tolerance 1e-10)
    (t/is-approx= 4.0 (la/condition-number [[1.0 0.0] [0.0 4.0]]) :tolerance 1e-10)
    ;; Singular matrices have very high condition numbers
    (t/is (> (la/condition-number [[1.0 2.0] [2.0 4.0]]) 1e6))))

;;;MATRIX RANK
(t/deftest matrix-rank-test
  (t/with-instrument `la/matrix-rank
    (t/is-spec-check la/matrix-rank {:num-tests 40}))
  (t/with-instrument :all
    (t/is= 0 (la/matrix-rank [[]]))
    (t/is= 1 (la/matrix-rank [[1.0]]))
    (t/is= 2 (la/matrix-rank [[1.0 0.0] [0.0 1.0]]))
    (t/is= 1 (la/matrix-rank [[1.0 2.0] [2.0 4.0]]))))

;;;INDUCED MATRIX NORMS
(t/deftest norm-1-test
  (t/with-instrument `la/norm-1
    (t/is-spec-check la/norm-1))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-1 [[]]))
    (t/is= 1.0 (la/norm-1 [[1.0 0.0] [0.0 1.0]]))
    (t/is= 6.0 (la/norm-1 [[1.0 -2.0] [3.0 4.0]]))
    (t/is= 5.0 (la/norm-1 [[-5.0]]))))

(t/deftest norm-inf-matrix-test
  (t/with-instrument `la/norm-inf-matrix
    (t/is-spec-check la/norm-inf-matrix))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-inf-matrix [[]]))
    (t/is= 1.0 (la/norm-inf-matrix [[1.0 0.0] [0.0 1.0]]))
    (t/is= 7.0 (la/norm-inf-matrix [[1.0 -2.0] [3.0 4.0]]))
    (t/is= 5.0 (la/norm-inf-matrix [[-5.0]]))))

(t/deftest norm-spectral-from-svd-test
  (t/with-instrument `la/norm-spectral-from-svd
    (t/is-spec-check la/norm-spectral-from-svd))
  (t/with-instrument :all
    (let [svd (la/sv-decomposition [[3.0 0.0] [0.0 4.0]])]
      (t/is-approx= 4.0 (la/norm-spectral-from-svd svd) :tolerance 1e-10))
    (let [svd (la/sv-decomposition [[1.0 0.0] [0.0 1.0]])]
      (t/is-approx= 1.0 (la/norm-spectral-from-svd svd) :tolerance 1e-10))))

(t/deftest norm-spectral-test
  (t/with-instrument `la/norm-spectral
    (t/is-spec-check la/norm-spectral {:num-tests 30}))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-spectral [[]]))
    (t/is-approx= 1.0 (la/norm-spectral [[1.0 0.0] [0.0 1.0]]) :tolerance 1e-10)
    (t/is-approx= 4.0 (la/norm-spectral [[3.0 0.0] [0.0 4.0]]) :tolerance 1e-10)))

;;;PSEUDOINVERSE
(t/deftest pseudoinverse-from-svd-test
  (t/with-instrument `la/pseudoinverse-from-svd
    (t/is-spec-check la/pseudoinverse-from-svd {:num-tests 40}))
  (t/with-instrument :all
    (let [svd (la/sv-decomposition [[1.0 0.0] [0.0 2.0]] {:rank-tolerance 0.0})
          pinv (la/pseudoinverse-from-svd svd)]
      (t/is-data-approx= [[1.0 0.0] [0.0 0.5]] pinv :tolerance 1e-10))
    ;; Verify A * A+ * A = A
    (let [A [[1.0 2.0] [3.0 4.0]]
          svd (la/sv-decomposition A {:rank-tolerance 0.0})
          A+ (la/pseudoinverse-from-svd svd)
          result (mx/mx* A (mx/mx* A+ A))]
      (t/is-data-approx= A result :tolerance 1e-10))))

(t/deftest pseudoinverse-test
  (t/with-instrument `la/pseudoinverse
    (t/is-spec-check la/pseudoinverse {:num-tests 40}))
  (t/with-instrument :all
    (t/is= nil (la/pseudoinverse [[]]))
    (let [result (la/pseudoinverse [[1.0 0.0] [0.0 2.0]])]
      (t/is-data-approx= [[1.0 0.0] [0.0 0.5]] result :tolerance 1e-10))
    (let [A [[1.0 2.0] [3.0 4.0]]
          A+ (la/pseudoinverse A)
          result (mx/mx* A A+ A)]
      (t/is-data-approx= A result :tolerance 1e-10))
    (let [A [[1.0 2.0 3.0]]
          A+ (la/pseudoinverse A)]
      (t/is (some? A+))
      (t/is= 3 (mx/rows A+))
      (t/is= 1 (mx/columns A+)))))

;;;MATRIX POWER
(t/deftest matrix-power-test
  (t/with-instrument `la/matrix-power
    (t/is-spec-check la/matrix-power))
  (t/with-instrument :all
    ;; Empty matrix
    (t/is= [[]] (la/matrix-power [[]] 5))
    ;; Power of 0 -> identity
    (t/is= [[1.0 0.0] [0.0 1.0]] (la/matrix-power [[1.0 2.0] [3.0 4.0]] 0))
    ;; Power of 1 -> same matrix
    (t/is-data-approx= [[1.0 2.0] [3.0 4.0]]
      (la/matrix-power [[1.0 2.0] [3.0 4.0]] 1) :tolerance 1e-10)
    ;; Power of 2
    (t/is-data-approx= [[7.0 10.0] [15.0 22.0]]
      (la/matrix-power [[1.0 2.0] [3.0 4.0]] 2) :tolerance 1e-10)
    ;; Power of 3
    (t/is-data-approx= [[37.0 54.0] [81.0 118.0]]
      (la/matrix-power [[1.0 2.0] [3.0 4.0]] 3) :tolerance 1e-10)
    ;; Diagonal matrix power
    (t/is-data-approx= [[8.0 0.0] [0.0 27.0]]
      (la/matrix-power [[2.0 0.0] [0.0 3.0]] 3) :tolerance 1e-10)
    ;; Negative power (inverse)
    (let [A [[2.0 0.0] [0.0 4.0]]
          A-inv (la/matrix-power A -1)]
      (t/is-data-approx= [[0.5 0.0] [0.0 0.25]] A-inv :tolerance 1e-10))))

;;;MATRIX EXPONENTIAL
(t/deftest matrix-exp-test
  (t/with-instrument `la/matrix-exp
    (t/is-spec-check la/matrix-exp {:num-tests 50}))
  (t/with-instrument :all
    ;; Empty matrix returns nil
    (t/is= nil (la/matrix-exp [[]]))
    ;; Zero matrix -> identity
    (t/is-data-approx= [[1.0 0.0] [0.0 1.0]] (la/matrix-exp [[0.0 0.0] [0.0 0.0]]) :tolerance 1e-10)
    ;; Identity matrix -> e*I
    (let [result (la/matrix-exp [[1.0 0.0] [0.0 1.0]])]
      (t/is-approx= m/E (get-in result [0 0]) :tolerance 1e-6)
      (t/is-approx= 0.0 (get-in result [0 1]) :tolerance 1e-6)
      (t/is-approx= 0.0 (get-in result [1 0]) :tolerance 1e-6)
      (t/is-approx= m/E (get-in result [1 1]) :tolerance 1e-6))
    ;; Diagonal matrix: exp([[a 0][0 b]]) = [[e^a 0][0 e^b]]
    (let [result (la/matrix-exp [[2.0 0.0] [0.0 3.0]])]
      (t/is-approx= (m/exp 2.0) (get-in result [0 0]) :tolerance 1e-6)
      (t/is-approx= (m/exp 3.0) (get-in result [1 1]) :tolerance 1e-6))
    ;; Rotation matrix test: exp([[0 -t][t 0]]) = [[cos(t) -sin(t)][sin(t) cos(t)]]
    (let [t 0.5
          result (la/matrix-exp [[0.0 (- t)] [t 0.0]])]
      (t/is-approx= (m/cos t) (get-in result [0 0]) :tolerance 1e-4)
      (t/is-approx= (- (m/sin t)) (get-in result [0 1]) :tolerance 1e-4)
      (t/is-approx= (m/sin t) (get-in result [1 0]) :tolerance 1e-4)
      (t/is-approx= (m/cos t) (get-in result [1 1]) :tolerance 1e-4))))
