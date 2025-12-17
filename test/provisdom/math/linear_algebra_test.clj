(ns provisdom.math.linear-algebra-test
  (:require
    [clojure.test :refer :all]
    [provisdom.math.linear-algebra :as la]
    [provisdom.math.matrix :as mx]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anom]))

(set! *warn-on-reflection* true)

;;;INDUCED MATRIX NORMS
(deftest norm-1-test
  (t/with-instrument `la/norm-1
    (t/is (t/spec-check la/norm-1)))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-1 [[]]))
    (t/is= 1.0 (la/norm-1 [[1.0 0.0] [0.0 1.0]]))
    (t/is= 6.0 (la/norm-1 [[1.0 -2.0] [3.0 4.0]]))
    (t/is= 5.0 (la/norm-1 [[-5.0]]))))

(deftest norm-inf-matrix-test
  (t/with-instrument `la/norm-inf-matrix
    (t/is (t/spec-check la/norm-inf-matrix)))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-inf-matrix [[]]))
    (t/is= 1.0 (la/norm-inf-matrix [[1.0 0.0] [0.0 1.0]]))
    (t/is= 7.0 (la/norm-inf-matrix [[1.0 -2.0] [3.0 4.0]]))
    (t/is= 5.0 (la/norm-inf-matrix [[-5.0]]))))

(deftest norm-spectral-test
  (t/with-instrument `la/norm-spectral
    (t/is (t/spec-check la/norm-spectral)))
  (t/with-instrument :all
    (t/is= 0.0 (la/norm-spectral [[]]))
    (t/is-approx= 1.0 (la/norm-spectral [[1.0 0.0] [0.0 1.0]]) :tolerance 1e-10)
    (t/is-approx= 4.0 (la/norm-spectral [[3.0 0.0] [0.0 4.0]]) :tolerance 1e-10)))

;;;PSEUDOINVERSE
(deftest pseudoinverse-test
  (t/with-instrument `la/pseudoinverse
    (t/is (t/spec-check la/pseudoinverse {:num-tests 20}))
  (t/with-instrument :all
    (t/is= nil (la/pseudoinverse [[]]))
    (let [result (la/pseudoinverse [[1.0 0.0] [0.0 2.0]])]
      (t/is (t/data-approx= [[1.0 0.0] [0.0 0.5]] result :tolerance 1e-10)))
    (let [A [[1.0 2.0] [3.0 4.0]]
          A+ (la/pseudoinverse A)
          result (mx/mx* A A+ A)]
      (t/is (t/data-approx= A result :tolerance 1e-10)))
    (let [A [[1.0 2.0 3.0]]
          A+ (la/pseudoinverse A)]
      (t/is (some? A+))
      (t/is= 3 (mx/rows A+))
      (t/is= 1 (mx/columns A+)))))

;;;SOLVE
(deftest solve-test
  (t/with-instrument `la/solve
    (t/is (t/spec-check la/solve {:num-tests 20}))
  (t/with-instrument :all
    (t/is= [] (la/solve [[]] []))
    (t/is (t/data-approx= [1.0 2.0] (la/solve [[1.0 0.0] [0.0 1.0]] [1.0 2.0]) :tolerance 1e-10))
    (let [A [[1.0 1.0] [1.0 2.0] [1.0 3.0]]
          b [1.0 2.0 3.0]
          x (la/solve A b)]
      (t/is (some? x))
      (t/is= 2 (count x)))))

;;;INVERSE
(deftest inverse-test
  (t/with-instrument `la/inverse
    (t/is (t/spec-check la/inverse)))
  (t/with-instrument :all
    (t/is= [[]] (la/inverse [[]]))
    (t/is= [[0.25]] (la/inverse [[4.0]]))
    (let [m [[1.0 2.0] [3.0 4.0]]
          inv (la/inverse m)
          product (mx/mx* m inv)]
      (t/is (t/data-approx= [[1.0 0.0] [0.0 1.0]] product :tolerance 1e-10)))
    (t/is= ::anom/no-solve (::anom/category (la/inverse [[1.0 2.0] [2.0 4.0]])))))

;;;LU DECOMPOSITION
(deftest lu-decomposition-test
  (t/with-instrument `la/lu-decomposition
    (t/is (t/spec-check la/lu-decomposition {:num-tests 15}))
  (t/with-instrument :all
    (t/is= nil (la/lu-decomposition [[]]))
    (let [m [[4.0 3.0] [6.0 3.0]]
          result (la/lu-decomposition m)
          {::la/keys [L U LU-permutation singular?]} result
          reconstructed (mx/mx* LU-permutation (mx/mx* L U))]
      (t/is-not singular?)
      (t/is (t/data-approx= m reconstructed :tolerance 1e-10)))))

;;;CHOLESKY DECOMPOSITION
(deftest cholesky-decomposition-test
  (t/with-instrument `la/cholesky-decomposition
    (t/is (t/spec-check la/cholesky-decomposition {:num-tests 15}))
  (t/with-instrument :all
    (t/is= nil (la/cholesky-decomposition [[]]))
    (let [m [[4.0 2.0] [2.0 5.0]]
          result (la/cholesky-decomposition m)
          L (::la/cholesky-L result)
          reconstructed (mx/mx* L (mx/transpose L))]
      (t/is (t/data-approx= m reconstructed :tolerance 1e-10)))))

;;;QR DECOMPOSITION
(deftest qr-decomposition-test
  (t/with-instrument `la/qr-decomposition
    (t/is (t/spec-check la/qr-decomposition {:num-tests 15}))
  (t/with-instrument :all
    (t/is= nil (la/qr-decomposition [[]]))
    (let [m [[1.0 2.0] [3.0 4.0] [5.0 6.0]]
          result (la/qr-decomposition m)
          {::la/keys [Q R]} result
          reconstructed (mx/mx* Q R)]
      (t/is (t/data-approx= m reconstructed :tolerance 1e-10)))))

;;;EIGENDECOMPOSITION
(deftest eigen-decomposition-test
  (t/with-instrument `la/eigen-decomposition
    (t/is (t/spec-check la/eigen-decomposition {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/eigen-decomposition [[]]))
    (let [m [[2.0 0.0] [0.0 3.0]]
          result (la/eigen-decomposition m)
          eigenvalues (::la/eigenvalues result)]
      (t/is (some? result))
      (t/is (or (t/data-approx= [3.0 2.0] eigenvalues :tolerance 1e-10)
              (t/data-approx= [2.0 3.0] eigenvalues :tolerance 1e-10))))))

;;;SVD
(deftest sv-decomposition-test
  (t/with-instrument `la/sv-decomposition
    (t/is (t/spec-check la/sv-decomposition {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/sv-decomposition [[]]))
    (let [m [[3.0 0.0] [0.0 4.0]]
          result (la/sv-decomposition m)
          singular-values (::la/singular-values result)]
      (t/is (some? result))
      (t/is (t/data-approx= [4.0 3.0] singular-values :tolerance 1e-10)))))

;;;DETERMINANT
(deftest determinant-test
  (t/with-instrument `la/determinant
    (t/is (t/spec-check la/determinant)))
  (t/with-instrument :all
    (t/is (Double/isNaN (la/determinant [[]])))
    (t/is= 4.0 (la/determinant [[4.0]]))
    (t/is-approx= -2.0 (la/determinant [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10)
    (t/is-approx= 0.0 (la/determinant [[1.0 2.0] [2.0 4.0]]) :tolerance 1e-10)))

;;;CONDITION NUMBER
(deftest condition-number-test
  (t/with-instrument `la/condition-number
    (t/is (t/spec-check la/condition-number {:num-tests 15}))
  (t/with-instrument :all
    (t/is (Double/isNaN (la/condition-number [[]])))
    (t/is-approx= 1.0 (la/condition-number [[1.0 0.0] [0.0 1.0]]) :tolerance 1e-10)
    ;; Singular matrices have very high condition numbers
    (t/is (> (la/condition-number [[1.0 2.0] [2.0 4.0]]) 1e6))))

;;;MATRIX RANK
(deftest matrix-rank-test
  (t/with-instrument `la/matrix-rank
    (t/is (t/spec-check la/matrix-rank)))
  (t/with-instrument :all
    (t/is= 0 (la/matrix-rank [[]]))
    (t/is= 1 (la/matrix-rank [[1.0]]))
    (t/is= 2 (la/matrix-rank [[1.0 0.0] [0.0 1.0]]))
    (t/is= 1 (la/matrix-rank [[1.0 2.0] [2.0 4.0]]))))

;;;LEAST SQUARES
(deftest least-squares-test
  (t/with-instrument `la/least-squares
    (t/is (t/spec-check la/least-squares {:num-tests 15}))
  (t/with-instrument :all
    (t/is= nil (la/least-squares [[]] []))
    (let [A [[1.0 1.0] [1.0 2.0] [1.0 3.0]]
          b [1.0 2.0 3.0]
          x (la/least-squares A b)]
      (t/is (some? x))
      (t/is= 2 (count x)))))

;;;POSITIVE DEFINITE
(deftest pos-definite-matrix-finite?-test
  (t/with-instrument `la/pos-definite-matrix-finite?
    (t/is (t/spec-check la/pos-definite-matrix-finite?)))
  (t/with-instrument :all
    (t/is (la/pos-definite-matrix-finite? [[1.0]]))
    (t/is (la/pos-definite-matrix-finite? [[4.0 2.0] [2.0 5.0]]))
    (t/is-not (la/pos-definite-matrix-finite? [[1.0 2.0] [2.0 1.0]]))))

(deftest pos-semidefinite-matrix-finite?-test
  (t/with-instrument `la/pos-semidefinite-matrix-finite?
    (t/is (t/spec-check la/pos-semidefinite-matrix-finite?)))
  (t/with-instrument :all
    (t/is (la/pos-semidefinite-matrix-finite? [[1.0]] 1e-10))
    (t/is (la/pos-semidefinite-matrix-finite? [[4.0 2.0] [2.0 5.0]] 1e-10))
    (t/is (la/pos-semidefinite-matrix-finite? [[1.0 1.0] [1.0 1.0]] 1e-10))
    (t/is-not (la/pos-semidefinite-matrix-finite? [[1.0 2.0] [2.0 1.0]] 1e-10))))

;;;CORRELATION/COVARIANCE
(deftest covariance-matrix->correlation-matrix-test
  (t/with-instrument `la/covariance-matrix->correlation-matrix
    (t/is (t/spec-check la/covariance-matrix->correlation-matrix {:num-tests 10}))
  (t/with-instrument :all
    (let [cov [[4.0 2.0] [2.0 9.0]]
          result (la/covariance-matrix->correlation-matrix cov)]
      (t/is (t/data-approx= [[1.0 (/ 2.0 6.0)] [(/ 2.0 6.0) 1.0]] result :tolerance 1e-10)))))

(deftest correlation-matrix->covariance-matrix-test
  (t/with-instrument `la/correlation-matrix->covariance-matrix
    (t/is (t/spec-check la/correlation-matrix->covariance-matrix {:num-tests 10}))
  (t/with-instrument :all
    ;; variances argument is [var1 var2], not standard deviations
    (let [corr [[1.0 0.5] [0.5 1.0]]
          variances [4.0 9.0]
          result (la/correlation-matrix->covariance-matrix corr variances)]
      (t/is (t/data-approx= [[4.0 3.0] [3.0 9.0]] result :tolerance 1e-10)))))

;;;MINOR
(deftest minor-test
  (t/with-instrument `la/minor
    (t/is (t/spec-check la/minor {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/minor [[1.0]] 0 0))
    (t/is= 4.0 (la/minor [[1.0 2.0] [3.0 4.0]] 0 0))
    (t/is= 3.0 (la/minor [[1.0 2.0] [3.0 4.0]] 0 1))
    (t/is= 2.0 (la/minor [[1.0 2.0] [3.0 4.0]] 1 0))
    (t/is= 1.0 (la/minor [[1.0 2.0] [3.0 4.0]] 1 1))
    (t/is-approx= -3.0 (la/minor [[1.0 2.0 3.0] [4.0 5.0 6.0] [7.0 8.0 9.0]] 0 0) :tolerance 1e-10)))

;;;COFACTOR
(deftest cofactor-test
  (t/with-instrument `la/cofactor
    (t/is (t/spec-check la/cofactor {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/cofactor [[1.0]] 0 0))
    (t/is= 4.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 0 0))
    (t/is= -3.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 0 1))
    (t/is= -2.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 1 0))
    (t/is= 1.0 (la/cofactor [[1.0 2.0] [3.0 4.0]] 1 1))))

;;;COFACTOR-MATRIX
(deftest cofactor-matrix-test
  (t/with-instrument `la/cofactor-matrix
    (t/is (t/spec-check la/cofactor-matrix {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/cofactor-matrix [[]]))
    (t/is= [[0.0]] (la/cofactor-matrix [[1.0]]))
    (t/is (t/data-approx= [[4.0 -3.0] [-2.0 1.0]]
            (la/cofactor-matrix [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10))))

;;;ADJUGATE
(deftest adjugate-test
  (t/with-instrument `la/adjugate
    (t/is (t/spec-check la/adjugate {:num-tests 10}))
  (t/with-instrument :all
    (t/is= nil (la/adjugate [[]]))
    (t/is= [[0.0]] (la/adjugate [[1.0]]))
    (t/is (t/data-approx= [[4.0 -2.0] [-3.0 1.0]]
            (la/adjugate [[1.0 2.0] [3.0 4.0]]) :tolerance 1e-10))
    ;; For invertible matrices: A * adj(A) = det(A) * I
    (let [A [[1.0 2.0] [3.0 4.0]]
          adj (la/adjugate A)
          det (la/determinant A)
          product (mx/mx* A adj)]
      (t/is (t/data-approx= [[det 0.0] [0.0 det]] product :tolerance 1e-10)))))

;;;MATRIX POWER
(deftest matrix-power-test
  (t/with-instrument `la/matrix-power
    (t/is (t/spec-check la/matrix-power {:num-tests 10}))
  (t/with-instrument :all
    ;; Empty matrix
    (t/is= [[]] (la/matrix-power [[]] 5))
    ;; Power of 0 -> identity
    (t/is= [[1.0 0.0] [0.0 1.0]] (la/matrix-power [[1.0 2.0] [3.0 4.0]] 0))
    ;; Power of 1 -> same matrix
    (t/is (t/data-approx= [[1.0 2.0] [3.0 4.0]] (la/matrix-power [[1.0 2.0] [3.0 4.0]] 1) :tolerance 1e-10))
    ;; Power of 2
    (t/is (t/data-approx= [[7.0 10.0] [15.0 22.0]] (la/matrix-power [[1.0 2.0] [3.0 4.0]] 2) :tolerance 1e-10))
    ;; Power of 3
    (t/is (t/data-approx= [[37.0 54.0] [81.0 118.0]] (la/matrix-power [[1.0 2.0] [3.0 4.0]] 3) :tolerance 1e-10))
    ;; Diagonal matrix power
    (t/is (t/data-approx= [[8.0 0.0] [0.0 27.0]] (la/matrix-power [[2.0 0.0] [0.0 3.0]] 3) :tolerance 1e-10))
    ;; Negative power (inverse)
    (let [A [[2.0 0.0] [0.0 4.0]]
          A-inv (la/matrix-power A -1)]
      (t/is (t/data-approx= [[0.5 0.0] [0.0 0.25]] A-inv :tolerance 1e-10)))))

;;;MATRIX EXPONENTIAL
(deftest matrix-exp-test
  (t/with-instrument `la/matrix-exp
    (t/is (t/spec-check la/matrix-exp {:num-tests 5}))
  (t/with-instrument :all
    ;; Empty matrix returns nil
    (t/is= nil (la/matrix-exp [[]]))
    ;; Zero matrix -> identity
    (t/is (t/data-approx= [[1.0 0.0] [0.0 1.0]] (la/matrix-exp [[0.0 0.0] [0.0 0.0]]) :tolerance 1e-10))
    ;; Identity matrix -> e*I
    (let [result (la/matrix-exp [[1.0 0.0] [0.0 1.0]])]
      (t/is-approx= Math/E (get-in result [0 0]) :tolerance 1e-6)
      (t/is-approx= 0.0 (get-in result [0 1]) :tolerance 1e-6)
      (t/is-approx= 0.0 (get-in result [1 0]) :tolerance 1e-6)
      (t/is-approx= Math/E (get-in result [1 1]) :tolerance 1e-6))
    ;; Diagonal matrix: exp([[a 0][0 b]]) = [[e^a 0][0 e^b]]
    (let [result (la/matrix-exp [[2.0 0.0] [0.0 3.0]])]
      (t/is-approx= (Math/exp 2.0) (get-in result [0 0]) :tolerance 1e-6)
      (t/is-approx= (Math/exp 3.0) (get-in result [1 1]) :tolerance 1e-6))
    ;; Rotation matrix test: exp([[0 -t][t 0]]) = [[cos(t) -sin(t)][sin(t) cos(t)]]
    (let [t 0.5
          result (la/matrix-exp [[0.0 (- t)] [t 0.0]])]
      (t/is-approx= (Math/cos t) (get-in result [0 0]) :tolerance 1e-4)
      (t/is-approx= (- (Math/sin t)) (get-in result [0 1]) :tolerance 1e-4)
      (t/is-approx= (Math/sin t) (get-in result [1 0]) :tolerance 1e-4)
      (t/is-approx= (Math/cos t) (get-in result [1 1]) :tolerance 1e-4))))
