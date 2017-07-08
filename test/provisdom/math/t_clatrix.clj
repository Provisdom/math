(ns provisdom.math.t-clatrix
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.clatrix :as decomp]
            [provisdom.math.random2 :as random]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]))

(ost/instrument)

(deftest positive-matrix?-test
  (is-not (decomp/positive-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (decomp/positive-matrix? [[1.0 0.5] [0.5 1.0]]))
  (is-not (decomp/positive-matrix? [[1.0 -1.1] [-1.1 1.0]]))
  (is-not (decomp/positive-matrix? [[1.0 -1.0] [-1.0 1.0]]))
  (is (decomp/positive-matrix? [[1.0 -1.0] [-1.0 1.0]] 1e-32))) ;accuracy too strict

(deftest positive-matrix-with-unit-diagonal?-test
  (is-not (decomp/positive-matrix-with-unit-diagonal? [[1.0 0.5] [2.0 4.0]]))
  (is-not (decomp/positive-matrix-with-unit-diagonal? [[1.0 0.5] [0.5 2.0]]))
  (is-not (decomp/positive-matrix-with-unit-diagonal? [[1.0 -1.1] [-1.1 1.0]]))
  (is-not (decomp/positive-matrix-with-unit-diagonal? [[1.0 -1.0] [-1.0 1.0]]))
  (is (decomp/positive-matrix-with-unit-diagonal? [[1.0 -1.0] [-1.0 1.0]] 1e-32))) ;accuracy too strict

(deftest non-negative-matrix?-test
  (is-not (decomp/non-negative-matrix? [[1.0 0.5] [2.0 4.0]]))
  (is (decomp/non-negative-matrix? [[1.0 0.5] [0.5 2.0]]))
  (is-not (decomp/non-negative-matrix? [[1.0 -1.1] [-1.1 1.0]]))
  (is (decomp/non-negative-matrix? [[1.0 -1.0] [-1.0 1.0]]))
  (is-not (decomp/non-negative-matrix? [[1.0 -1.0001] [-1.0001 1.0]]))
  (is (decomp/non-negative-matrix? [[1.0 -1.0001] [-1.0001 1.0]] 1e-3))) ;accuracy too lax

(deftest type-tests
  (positive-matrix?-test)
  (positive-matrix-with-unit-diagonal?-test)
  (non-negative-matrix?-test))

(defspec-test test-positive-matrix? `decomp/positive-matrix?)
(defspec-test test-positive-matrix-with-unit-diagonal? `decomp/positive-matrix-with-unit-diagonal?)
(defspec-test test-non-negative-matrix? `decomp/non-negative-matrix?)

(deftest lu-decomposition-test
  (is= [] (decomp/lu-decomposition 1)))

(deftest decomposition-tests
  (lu-decomposition-test))

;(defspec-test test-lu-decomposition `decomp/lu-decomposition)



(comment

  (facts "decomposition"
         (fact "inverse"
               (inverse 2.0) => 0.5
               (inverse [2.0]) => [0.5]
               (inverse ap)
               => (apache-commons [[1.3333333333333333 -0.16666666666666663]
                                   [-0.6666666666666666 0.3333333333333333]])
               (inverse cl)
               => (clatrix [[1.3333333333333333 -0.16666666666666663]
                            [-0.6666666666666666 0.3333333333333333]])
               (inverse cl-row) => (throws)
               (inverse cl-col) => (throws)
               (inverse cl1D) => [1.0 2.0]
               (inverse ve) => [[1.3333333333333333 -0.16666666666666663]
                                [-0.6666666666666666 0.3333333333333333]]
               (inverse se) => [[1.3333333333333333 -0.16666666666666663]
                                [-0.6666666666666666 0.3333333333333333]])
         (fact "cholesky"
               (cholesky-decomposition ap) => (throws)
               (first (cholesky-decomposition
                        (apache-commons [[1.0 0.5] [0.5 3.0]])))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
               (second (cholesky-decomposition (clatrix [[1.0 0.5] [0.5 3.0]])))
               => (clatrix [[1.0 0.5] [0.0 1.6583123951777]])
               (first (cholesky-decomposition [[1.0 0.5] [0.5 3.0]]))
               => [[1.0 0.0] [0.5 1.6583123951777]]
               (cholesky-decomposition cl-row) => (throws))
         (fact "lower cholesky"
               (lower-cholesky (apache-commons [[1.0 0.5] [0.5 3.0]]))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
               (lower-cholesky [[1.0 0.5] [0.5 3.0]]) => [[1.0 0.0]
                                                          [0.5 1.6583123951777]])
         (fact "upper cholesky"
               (upper-cholesky (apache-commons [[1.0 0.5] [0.5 3.0]]))
               => (apache-commons [[1.0 0.5] [0.0 1.6583123951777]])
               (upper-cholesky [[1.0 0.5] [0.5 3.0]]) => [[1.0 0.5]
                                                          [0.0 1.6583123951777]])
         (fact "cholesky with positive-semi-definite"
               (second (cholesky-decomposition-semi-definite cl 1e-4))
               => (clatrix [[0.25 2.0] [0.9682458365518543 0.0]])
               (first (cholesky-decomposition
                        (apache-commons [[1.0 0.5] [0.5 3.0]])))
               => (apache-commons [[1.0 0.0] [0.5 1.6583123951777]])
               (first (cholesky-decomposition-semi-definite
                        (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                        1e-4))
               => (clatrix [[1.0 0.0 0.0] [0.5 1.6583123951777 0.0]
                            [1.0E-9 3.0151134457776366E-10
                             9.999999999994545E-4]])
               (second (cholesky-decomposition-semi-definite
                         (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                   [1e-9 1e-9 1e-6]]) 1e-4))
               => (clatrix [[1.0 0.5 1.0E-9]
                            [0.0 1.6583123951777 3.0151134457776366E-10]
                            [0.0 0.0 9.999999999994545E-4]])
               (first (cholesky-decomposition-semi-definite
                        [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]] 1e-4))
               => [[1.0 0.0 0.0] [0.5 1.6583123951777 0.0]
                   [1.0E-9 3.0151134457776366E-10 9.999999999994545E-4]]
               (second (cholesky-decomposition-semi-definite
                         [[0.08061440651728713 0.048368643910372044]
                          [0.048368643910372044 0.029021186346223526]] 1e-14))
               => [[0.28392676259431254 0.17035605755658673] [0.0 0.0]]
               (cholesky-decomposition-semi-definite cl-row) => (throws))
         (fact "cholesky rectangular"
               (:B (cholesky-rectangular ap 1e-4))
               => (apache-commons [[0.25 0.9682458365518543] [2.0 0.0]])
               (:B (cholesky-rectangular (apache-commons [[1.0 0.5] [0.5 3.0]])
                                         1e-4))
               => (apache-commons [[0.2886751345948129 0.9574271077563381]
                                   [1.7320508075688772 0.0]])
               (:B (cholesky-rectangular
                     (apache-commons [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                      [1e-9 1e-9 1e-6]]) 1e-4))
               => (apache-commons [[0.2886751345948129 0.9574271077563381]
                                   [1.7320508075688772 0.0]
                                   [5.773502691896259E-10
                                    8.703882797784892E-10]])
               (:B (cholesky-rectangular (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9]
                                                   [1e-9 1e-9 1e-6]]) 1e-4))
               => (clatrix [[0.2886751345948129 0.9574271077563381]
                            [1.7320508075688772 0.0]
                            [5.773502691896259E-10 8.703882797784892E-10]])
               (cholesky-rectangular
                 [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]] 1e-4)
               => {:B [[0.2886751345948129 0.9574271077563381]
                       [1.7320508075688772 0.0]
                       [5.773502691896259E-10 8.703882797784892E-10]], :rank 2}
               (cholesky-rectangular cl-row) => (throws))
         (fact "sv-decomposition with rank"
               (sv-decomposition-with-rank ve)
               => {:S [[4.562639046204302 0.0] [0.0 0.6575142082509742]],
                   :U [[0.20027709794089957 0.9797392939146471]
                       [0.979739293914647 -0.2002770979408996]],
                   :UT [[0.20027709794089957 0.979739293914647]
                        [0.9797392939146471 -0.2002770979408996]],
                   :V [[0.47335668324824287 0.8808708477547789]
                       [0.8808708477547789 -0.47335668324824287]],
                   :VT [[0.47335668324824287 0.8808708477547789]
                        [0.8808708477547789 -0.47335668324824287]], :rank 2}
               (sv-decomposition-with-rank cl)
               => {:S    (clatrix [[4.562639046204301 0.0]
                                   [0.0 0.6575142082509741]]),
                   :U    (clatrix [[-0.20027709794089957 -0.9797392939146469]
                                   [-0.9797392939146469 0.2002770979408998]]),
                   :UT   (clatrix [[-0.20027709794089957 -0.9797392939146469]
                                   [-0.9797392939146469 0.2002770979408998]]),
                   :V    (clatrix [[-0.4733566832482428 -0.8808708477547789]
                                   [-0.8808708477547789 0.4733566832482428]]),
                   :VT   (clatrix [[-0.4733566832482428 -0.8808708477547789]
                                   [-0.8808708477547789 0.4733566832482428]]),
                   :rank 2}
               (sv-decomposition-with-rank ve-row)
               => {:S  [[1.118033988749895]], :U [[-1.0]], :UT [[-1.0]],
                   :V  [[-0.8944271909999157] [-0.4472135954999579]],
                   :VT [[-0.8944271909999157 -0.4472135954999579]], :rank 1}
               (:S (sv-decomposition-with-rank cl-row))
               => (clatrix [[1.118033988749895]])
               (:U (sv-decomposition-with-rank cl-row)) => (clatrix [[1.0]])
               (:UT (sv-decomposition-with-rank cl-row)) => (clatrix [[1.0]])
               (:V (sv-decomposition-with-rank cl-row))
               => (clatrix [[0.8944271909999157] [0.4472135954999579]])
               (:VT (sv-decomposition-with-rank cl-row))
               => (clatrix [[0.8944271909999157 0.4472135954999579]])
               (sv-decomposition-with-rank ve-col)
               => {:S  [[1.118033988749895]],
                   :U  [[-0.8944271909999157] [-0.4472135954999579]],
                   :UT [[-0.8944271909999157 -0.4472135954999579]],
                   :V  [[-1.0]], :VT [[-1.0]], :rank 1}
               (sv-decomposition-with-rank ve1D) => (throws))
         (fact "sv-decomposition"
               (sv-decomposition ve)
               => [[[-0.20027709794089957 -0.9797392939146469]
                    [-0.9797392939146469 0.2002770979408998]],
                   [[4.562639046204301 0.0] [0.0 0.6575142082509741]],
                   [[-0.4733566832482428 -0.8808708477547789]
                    [-0.8808708477547789 0.4733566832482428]]]
               (sv-decomposition ap)
               => [(apache-commons [[0.20027709794089957 0.9797392939146471]
                                    [0.979739293914647 -0.2002770979408996]])
                   (apache-commons [[4.562639046204302 0.0]
                                    [0.0 0.6575142082509742]]),
                   (apache-commons [[0.47335668324824287 0.8808708477547789]
                                    [0.8808708477547789 -0.47335668324824287]])]
               (sv-decomposition cl)
               => [(clatrix [[-0.20027709794089957 -0.9797392939146469]
                             [-0.9797392939146469 0.2002770979408998]])
                   (clatrix [[4.562639046204301 0.0] [0.0 0.6575142082509741]]),
                   (clatrix [[-0.4733566832482428 -0.8808708477547789]
                             [-0.8808708477547789 0.4733566832482428]])])
         (fact "rank"
               (rank ve) => 2
               (rank ve 1e-6) => 2
               (rank (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                     1e-4) => 2
               (rank (clatrix [[1.0 0.5 1e-9] [0.5 3.0 1e-9] [1e-9 1e-9 1e-6]])
                     1e-10) => 3
               (rank ap) => 2
               (rank cl) => 2
               (rank ve-row) => 1
               (rank cl-col) => 1
               (rank ap1D) => (throws))
         (fact "condition"
               (def cm (diagonal (second (sv-decomposition ve))))
               (condition cm) => 6.939225021982696)
         (fact "lu-decomposition-with-permutation-matrix"
               (lu-decomposition-with-permutation-matrix ap)
               => {:L (apache-commons [[1.0 0.0] [0.5 1.0]]),
                   :P (apache-commons [[0.0 1.0] [1.0 0.0]]),
                   :U (apache-commons [[2.0 4.0] [0.0 -1.5]])}
               (lu-decomposition-with-permutation-matrix cl)
               => {:L (clatrix [[1.0 0.0] [0.5 1.0]]),
                   :P (clatrix [[0.0 1.0] [1.0 0.0]]),
                   :U (clatrix [[2.0 4.0] [0.0 -1.5]])}
               (lu-decomposition-with-permutation-matrix ve)
               => {:L [[1.0 0.0] [0.5 1.0]], :P [[0.0 1.0] [1.0 0.0]],
                   :U [[2.0 4.0] [0.0 -1.5]]}
               (lu-decomposition-with-permutation-matrix ve-col) => (throws)
               (lu-decomposition-with-permutation-matrix ap-row) => (throws))
         (fact "lu-decomposition"
               (lu-decomposition ap)
               => [(apache-commons [[1.0 0.0] [0.5 1.0]])
                   (apache-commons [[2.0 4.0] [0.0 -1.5]])]
               (lu-decomposition cl)
               => [(clatrix [[1.0 0.0] [0.5 1.0]])
                   (clatrix [[2.0 4.0] [0.0 -1.5]])]
               (lu-decomposition ve) => [[[1.0 0.0] [0.5 1.0]]
                                         [[2.0 4.0] [0.0 -1.5]]])
         (fact "qr-decomposition"
               (qr-decomposition ap)
               => [(apache-commons [[-0.44721359549995787 0.8944271909999159]
                                    [-0.8944271909999157 -0.447213595499958]])
                   (apache-commons [[-2.23606797749979 -3.801315561749642]
                                    [0.0 -1.341640786499874]])]
               (qr-decomposition cl)
               => [(clatrix [[-0.44721359549995787 -0.8944271909999157]
                             [-0.8944271909999157 0.4472135954999581]])
                   (clatrix [[-2.23606797749979 -3.801315561749642]
                             [0.0 1.341640786499874]])]
               (qr-decomposition ve)
               => [[[-0.44721359549995787 -0.8944271909999157]
                    [-0.8944271909999157 0.4472135954999581]]
                   [[-2.23606797749979 -3.801315561749642]
                    [0.0 1.341640786499874]]]
               (qr-decomposition ve-row) => [[[1.0]] [[1.0 0.5]]]
               (qr-decomposition ve-col)
               => [[[-0.8944271909999157 -0.4472135954999579]
                    [-0.4472135954999579 0.8944271909999159]]
                   [[-1.118033988749895] [0.0]]]
               (qr-decomposition ap-row)
               => [(apache-commons [[-1.0]])
                   (apache-commons [[-1.0 -0.5]])]
               (qr-decomposition ap-col)
               => [(apache-commons [[-0.894427190999916 -0.4472135954999579]
                                    [-0.4472135954999579 0.8944271909999159]])
                   (apache-commons [[-1.118033988749895] [0.0]])])
         (fact "rrqr-decomposition"
               (rrqr-decomposition ve 1e-6)
               => {:P [[0.0 1.0] [1.0 0.0]],
                   :Q [[-0.12403473458920855 0.9922778767136677]
                       [-0.9922778767136677 -0.12403473458920833]],
                   :QT [[-0.12403473458920855 -0.9922778767136677]
                        [0.9922778767136677 -0.12403473458920833]],
                   :R [[-4.031128874149275 -2.108590488016544]
                       [0.0 0.7442084075352513]], :rank 2}
               (rrqr-decomposition ve-row 1e-6)
               => {:P [[1.0 0.0] [0.0 1.0]], :Q [[-1.0]], :QT [[-1.0]],
                   :R [[-1.0 -0.5]], :rank 1}
               (rrqr-decomposition ve-col 1e-6)
               => {:P  [[1.0]],
                   :Q  [[-0.894427190999916 -0.4472135954999579]
                        [-0.4472135954999579 0.8944271909999159]],
                   :QT [[-0.894427190999916 -0.4472135954999579]
                        [-0.4472135954999579 0.8944271909999159]],
                   :R  [[-1.118033988749895] [0.0]], :rank 1})
         (fact "eigenvalues"
               (eigenvalues [[m/inf+ 0.0] [0.0 m/inf+]]) => [m/inf+ m/inf+]
               (eigenvalues ap) => [0.6972243622680055 4.302775637731996]
               (eigenvalues cl) => [0.6972243622680057 4.302775637731995]
               (eigenvalues ve-row) => (throws))
         (fact "eigen-decomposition"
               (eigen-decomposition cl)
               => [(clatrix [[-0.8553908861324309 -0.16211892282756657]
                             [0.5179830421177656 -1.0708848574604801]])
                   (clatrix [[0.6972243622680055 0.0] [0.0 4.302775637731996]])
                   (clatrix [[-0.8553908861324309 0.5179830421177656]
                             [-0.16211892282756657 -1.0708848574604801]])]
               (eigen-decomposition ve)
               => [[[-0.8553908861324309 -0.16211892282756657]
                    [0.5179830421177656 -1.0708848574604801]]
                   [[0.6972243622680055 0.0] [0.0 4.302775637731996]]
                   [[-0.8553908861324309 0.5179830421177656]
                    [-0.16211892282756657 -1.0708848574604801]]]
               (eigen-decomposition cl-row) => (throws)))

  (facts "solve"
         (fact "linear least squares"
               (linear-least-squares ap [7.0 9.0])
               => [7.833333333333333 -1.6666666666666665]
               (linear-least-squares [[1.0 0.4 0.2] [0.6 0.3 0.9]] [7.0 9.0])
               => [-24.999999999999996 80.0 0.0]
               (linear-least-squares [[1.0 0.4] [0.2 0.6] [0.3 0.9]]
                                     [7.0 9.0 12.0])
               => [1.6863905325443753 13.284023668639056]
               (linear-least-squares cl [7.0 9.0]) => [7.833333333333333
                                                       -1.6666666666666665]
               (linear-least-squares cl-col [7.0 9.0]) => [9.200000000000003]
               (linear-least-squares ap-row [7.0 9.0]) => (throws)
               (linear-least-squares ve1D [7.0 9.0]) => (throws))
         (fact "linear-least-squares-with-error-matrix"
               (linear-least-squares-with-error-matrix [[1.0 2.0]] [3.0])
               => (throws)
               (linear-least-squares-with-error-matrix ap [7.0 9.0])
               => {:E (apache-commons [[1.8055555555555545 -0.944444444444444]
                                       [-0.944444444444444 0.5555555555555554]]),
                   :S [7.833333333333333 -1.6666666666666665]}
               (linear-least-squares-with-error-matrix cl [7.0 9.0])
               => {:E (clatrix [[1.8055555555555545 -0.944444444444444]
                                [-0.944444444444444 0.5555555555555554]]),
                   :S [7.833333333333333 -1.6666666666666665]}
               (linear-least-squares-with-error-matrix cl-col [7.0 9.0])
               => {:E [0.7999999999999999], :S [9.200000000000003]}
               (linear-least-squares-with-error-matrix ap-row [7.0 9.0])
               => (throws)
               (linear-least-squares-with-error-matrix ve1D [7.0 9.0])
               => (throws))
         (fact "matrix-solve-iterative"
               (matrix-solve-iterative [[1.0 0.5] [0.5 3.0]] [7.0 9.0]
                                       :solver :symm)
               => [5.999999999999998 1.9999999999999984]
               (matrix-solve-iterative ap [7.0 9.0] :solver :symm) => (throws)))

  )

#_(ost/unstrument)