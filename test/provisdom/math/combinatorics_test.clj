(ns provisdom.math.combinatorics-test
  (:require
    [provisdom.math.combinatorics :as combo]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]))

;;20 seconds

(set! *warn-on-reflection* true)

;;;FACTORIALS
(t/deftest factorial-test
  (t/with-instrument `combo/factorial
    (t/is-spec-check combo/factorial))
  (t/with-instrument :all
    (t/is= 1.0 (combo/factorial 0))
    ;;SciPy 0.95135
    (t/is= 0.9513507698668731 (combo/factorial 0.1))
    ;;SciPy 0.88622
    (t/is= 0.886226925452758 (combo/factorial 0.5))
    ;;SciPy 0.96176
    (t/is= 0.9617658319073874 (combo/factorial 0.9))
    (t/is= 1.0 (combo/factorial 1))
    ;;SciPy 1.3293
    (t/is= 1.329340388179137 (combo/factorial 1.5))
    (t/is= 2.0 (combo/factorial 2.0))
    ;;Math 5.1090E19
    (t/is= 5.109094217170945E19 (combo/factorial 21))
    ;;Math 1.1240E21
    (t/is= 1.1240007277776072E21 (combo/factorial 22))
    ;;Math 2.5852E22
    (t/is= 2.585201673888498E22 (combo/factorial 23))
    ;;mpmath 4.7147236359920616E284 (JVM exp(lgamma) drifts at ~13th sig fig)
    (t/is-approx= 4.7147236359920616E284 (combo/factorial 160) :rel-tolerance 1e-12)))

(t/deftest factorial'-test
  (t/with-instrument `combo/factorial'
    (t/is-spec-check combo/factorial'))
  (t/with-instrument :all
    (t/is= 1 (combo/factorial' 0))))

(t/deftest log-factorial-test
  (t/with-instrument `combo/log-factorial
    (t/is-spec-check combo/log-factorial))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-factorial 0))
    ;;SciPy -0.049872
    (t/is= -0.04987244125983972 (combo/log-factorial 0.1))
    ;;SciPy -0.12078
    (t/is= -0.1207822376352452 (combo/log-factorial 0.5))
    ;;SciPy -0.038984
    (t/is= -0.03898427592308332 (combo/log-factorial 0.9))
    (t/is= 0.0 (combo/log-factorial 1))
    ;;SciPy 0.28468
    (t/is= 0.2846828704729192 (combo/log-factorial 1.5))
    ;;SciPy 0.69314
    (t/is= 0.6931471805599453 (combo/log-factorial 2.0))
    ;;SciPy 51.606
    (t/is= 51.60667556776438 (combo/log-factorial 23))
    ;;SciPy 655.48
    (t/is= 655.4848567108892 (combo/log-factorial 160))))

(t/deftest subfactorial-test
  (t/with-instrument `combo/subfactorial
    (t/is-spec-check combo/subfactorial))
  (t/with-instrument :all
    (t/is= 1 (combo/subfactorial 0))
    (t/is= 0 (combo/subfactorial 0.1))
    (t/is= 0 (combo/subfactorial 0.5))
    (t/is= 0 (combo/subfactorial 0.9))
    (t/is= 0 (combo/subfactorial 1))
    (t/is= 0 (combo/subfactorial 1.5))
    (t/is= 1 (combo/subfactorial 2.0))
    ;;Math 8.9501E17
    (t/is= 895014631192902121 (combo/subfactorial 20))
    ;;Math 1.8795E19
    (t/is= 18795307255050944540N (combo/subfactorial 21))
    ;;Math 4.1349E20
    (t/is= 4.134967596111206E20 (combo/subfactorial 22))
    ;;Math 9.5104E21
    (t/is= 9.510425471055779E21 (combo/subfactorial 23))))

(t/deftest double-factorial-test
  (t/with-instrument `combo/double-factorial
    (t/is-spec-check combo/double-factorial))
  (t/with-instrument :all
    (t/is= 1.0 (combo/double-factorial 0))
    (t/is= 1.0 (combo/double-factorial 1))
    (t/is= 2.0 (combo/double-factorial 2))
    (t/is= 3.0 (combo/double-factorial 3))
    (t/is= 8.0 (combo/double-factorial 4))
    (t/is= 15.0 (combo/double-factorial 5))
    (t/is= 48.0 (combo/double-factorial 6))
    (t/is= 105.0 (combo/double-factorial 7))
    (t/is= 1.0 (combo/double-factorial -1))))

(t/deftest double-factorial'-test
  (t/with-instrument `combo/double-factorial'
    (t/is-spec-check combo/double-factorial'))
  (t/with-instrument :all
    (t/is= 1 (combo/double-factorial' 0))
    (t/is= 105 (combo/double-factorial' 7))))

(t/deftest rising-factorial-test
  (t/with-instrument `combo/rising-factorial
    (t/is-spec-check combo/rising-factorial))
  (t/with-instrument :all
    ;;mpmath 1.0
    (t/is= 1.0 (combo/rising-factorial 5 0))
    ;;mpmath 5.0
    (t/is= 5.0 (combo/rising-factorial 5 1))
    ;;mpmath 30.0
    (t/is= 30.0 (combo/rising-factorial 5 2))               ;; 5 × 6
    ;;mpmath 360.0
    (t/is= 360.0 (combo/rising-factorial 3 4))              ;; 3 × 4 × 5 × 6
    ;;mpmath 120.0
    (t/is= 120.0 (combo/rising-factorial 1 5))              ;; same as 5!
    ;;mpmath 1.875
    (t/is= 1.875 (combo/rising-factorial 0.5 3))            ;; 0.5 × 1.5 × 2.5
    ;;mpmath 176310.36293506622
    (t/is= 176310.36293506622 (combo/rising-factorial 0.25 10))))

(t/deftest falling-factorial-test
  (t/with-instrument `combo/falling-factorial
    (t/is-spec-check combo/falling-factorial))
  (t/with-instrument :all
    ;;mpmath 1.0
    (t/is= 1.0 (combo/falling-factorial 5 0))
    ;;mpmath 5.0
    (t/is= 5.0 (combo/falling-factorial 5 1))
    ;;mpmath 20.0
    (t/is= 20.0 (combo/falling-factorial 5 2))              ;; 5 × 4
    ;;mpmath 60.0
    (t/is= 60.0 (combo/falling-factorial 5 3))              ;; 5 × 4 × 3
    ;;mpmath 120.0
    (t/is= 120.0 (combo/falling-factorial 5 5))             ;; same as 5!
    ;;mpmath 8.75
    (t/is= 8.75 (combo/falling-factorial 3.5 2))            ;; 3.5 × 2.5
    ;;mpmath 41333.90625
    (t/is= 41333.90625 (combo/falling-factorial 10.5 5))))

;;;CHOOSING
(t/deftest choose-k-from-n-test
  (t/with-instrument `combo/choose-k-from-n
    (t/is-spec-check combo/choose-k-from-n))
  (t/with-instrument :all
    (t/is= 1.0 (combo/choose-k-from-n 0 1))
    (t/is= 1.0 (combo/choose-k-from-n 0 0))
    (t/is= 1.0 (combo/choose-k-from-n 1 1))
    (t/is= 2.0 (combo/choose-k-from-n 1 2))
    (t/is= 4.0 (combo/choose-k-from-n 1 4))
    (t/is= 10.0 (combo/choose-k-from-n 2 5))
    ;;SciPy 1.2689E24
    (t/is= 1.268976952064044E24 (combo/choose-k-from-n 12 545))))

(t/deftest choose-k-from-n'-test
  (t/with-instrument `combo/choose-k-from-n'
    (t/is-spec-check combo/choose-k-from-n'))
  (t/with-instrument :all
    (t/is= 4 (combo/choose-k-from-n' 1 4))))

(t/deftest log-choose-k-from-n-test
  (t/with-instrument `combo/log-choose-k-from-n
    (t/is-spec-check combo/log-choose-k-from-n))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-choose-k-from-n 0 1))
    (t/is= 0.0 (combo/log-choose-k-from-n 0 0))
    (t/is= 0.0 (combo/log-choose-k-from-n 1 1))
    ;;SciPy 0.33647
    (t/is= 0.33647223662121284 (combo/log-choose-k-from-n 1 1.4))
    ;;mpmath 1.3862943611198906
    (t/is= 1.3862943611198906 (combo/log-choose-k-from-n 1 4))
    ;;mpmath 2.3025850929940459
    (t/is= 2.302585092994046 (combo/log-choose-k-from-n 2 5))
    ;;mpmath 55.500253258142337
    (t/is= 55.50025325814234 (combo/log-choose-k-from-n 12 545))
    ;;mpmath 52.77703558056906
    (t/is= 52.77703558056906 (combo/log-choose-k-from-n 5 100000))
    ;;mpmath 76.99448972101092
    (t/is= 76.99448972101092 (combo/log-choose-k-from-n 10 10000))
    ;; large n — direct summation avoids catastrophic cancellation
    ;; log C(n, 1) = log(n)
    (t/is= (m/log 1000000000000000) (combo/log-choose-k-from-n 1 1000000000000000))
    (t/is= (m/log 1000000000000000000) (combo/log-choose-k-from-n 1 1000000000000000000))
    ;; log C(n, 2) = log(n) + log(n-1) - log(2), n = 10^15 (exact doubles)
    (let [n 1000000000000000]
      (t/is= (- (+ (m/log n) (m/log (dec n))) (m/log 2))
        (combo/log-choose-k-from-n 2 n)))
    ;; log C(n, 5) via independent sum of logs, n = 10^15
    (let [n 1000000000000000]
      (t/is-approx= (- (+ (m/log n) (m/log (- n 1)) (m/log (- n 2))
                         (m/log (- n 3)) (m/log (- n 4)))
                      (m/log 120.0))
        (combo/log-choose-k-from-n 5 n)
        :tolerance 1e-10))
    ;; n = 10^18 — old log-factorial approach returned 0.0 (total precision loss)
    ;;mpmath 202.44516662668207
    (t/is= 202.44516662668207 (combo/log-choose-k-from-n 5 1000000000000000000))
    ;;mpmath 399.36090416585273
    (t/is= 399.36090416585273 (combo/log-choose-k-from-n 10 1000000000000000000))))

(t/deftest multinomial-coefficient-test
  (t/with-instrument `combo/multinomial-coefficient
    (t/is-spec-check combo/multinomial-coefficient))
  (t/with-instrument :all
    (t/is= 1.0 (combo/multinomial-coefficient [3]))
    (t/is= 6.0 (combo/multinomial-coefficient [2 2]))       ;; same as C(4,2)
    (t/is= 60.0 (combo/multinomial-coefficient [2 3 1]))    ;; 6!/(2!3!1!)
    (t/is= 6.0 (combo/multinomial-coefficient [1 1 1]))))   ;; 3!

(t/deftest log-multinomial-coefficient-test
  (t/with-instrument `combo/log-multinomial-coefficient
    ;;:num-tests reduced; large k values in ::m/non- collections cause
    ;;log-choose-k-from-n direct summation to dominate runtime -- ME
    (t/is-spec-check combo/log-multinomial-coefficient {:num-tests 200}))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-multinomial-coefficient [3]))
    ;;SciPy 4.0943
    (t/is-approx= 4.0943445622221 (combo/log-multinomial-coefficient [2 3 1]) :tolerance 1e-10)))

(t/deftest stirling-number-of-the-second-kind-test
  (t/with-instrument `combo/stirling-number-of-the-second-kind
    (t/is-spec-check combo/stirling-number-of-the-second-kind))
  (t/with-instrument :all
    ;;mpmath 0
    (t/is= 0.0 (combo/stirling-number-of-the-second-kind 0 1))
    ;;mpmath 1
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 0 0))
    ;;mpmath 1
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 1))
    ;;mpmath 1
    (t/is= 1.0 (combo/stirling-number-of-the-second-kind 1 4))
    ;;mpmath 15
    (t/is= 15.0 (combo/stirling-number-of-the-second-kind 2 5))
    ;;mpmath 42525
    (t/is= 42525.0 (combo/stirling-number-of-the-second-kind 5 10))
    ;;mpmath 1.4318980615233434E207
    (t/is= 1.4318980615233435E207 (combo/stirling-number-of-the-second-kind 12 200))))

(t/deftest stirling-number-of-the-second-kind'-test
  (t/with-instrument `combo/stirling-number-of-the-second-kind'
    (t/is-spec-check combo/stirling-number-of-the-second-kind'))
  (t/with-instrument :all
    (t/is= 0 (combo/stirling-number-of-the-second-kind' 0 1))))

(t/deftest log-stirling-number-of-the-second-kind-test
  (t/with-instrument `combo/log-stirling-number-of-the-second-kind
    (t/is-spec-check combo/log-stirling-number-of-the-second-kind))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-stirling-number-of-the-second-kind 0 0))
    (t/is= m/inf- (combo/log-stirling-number-of-the-second-kind 0 5))
    (t/is= 0.0 (combo/log-stirling-number-of-the-second-kind 5 5))
    (t/is= 0.0 (combo/log-stirling-number-of-the-second-kind 1 100))
    ;;mpmath 2.7080502011022101
    (t/is= 2.70805020110221 (combo/log-stirling-number-of-the-second-kind 2 5))
    ;;mpmath 3.2188758248682007
    (t/is= 3.2188758248682006 (combo/log-stirling-number-of-the-second-kind 3 5))
    ;; k > 170 — polynomial formula (exact, matches mpmath to all digits)
    ;;mpmath 1016.3446424254222
    (t/is= 1016.3446424254222 (combo/log-stirling-number-of-the-second-kind 171 342))
    ;;mpmath 1220.5075051108709
    (t/is= 1220.5075051108709 (combo/log-stirling-number-of-the-second-kind 200 400))
    ;;mpmath 749.7847782829931
    (t/is= 749.7847782829931 (combo/log-stirling-number-of-the-second-kind 180 300))
    ;; k ≤ 170 with large n — IE in log-space avoids overflow
    (t/is= 16089.591632598222 (combo/log-stirling-number-of-the-second-kind 5 10000))))

(t/deftest stirling-number-of-the-first-kind-test
  (t/with-instrument `combo/stirling-number-of-the-first-kind
    (t/is-spec-check combo/stirling-number-of-the-first-kind))
  (t/with-instrument :all
    ;;mpmath 1
    (t/is= 1.0 (combo/stirling-number-of-the-first-kind 0 0))
    ;;mpmath 0
    (t/is= 0.0 (combo/stirling-number-of-the-first-kind 0 3))
    ;;mpmath 1
    (t/is= 1.0 (combo/stirling-number-of-the-first-kind 4 4))
    ;;mpmath 6
    (t/is= 6.0 (combo/stirling-number-of-the-first-kind 1 4)) ;; (4-1)! = 6
    ;;mpmath 11
    (t/is= 11.0 (combo/stirling-number-of-the-first-kind 2 4))
    ;;mpmath 6
    (t/is= 6.0 (combo/stirling-number-of-the-first-kind 3 4))
    ;;mpmath 362880
    (t/is= 362880.0 (combo/stirling-number-of-the-first-kind 1 10))
    ;;mpmath 1026576
    (t/is= 1026576.0 (combo/stirling-number-of-the-first-kind 2 10))
    ;;mpmath 269325
    (t/is= 269325.0 (combo/stirling-number-of-the-first-kind 5 10))))

(t/deftest stirling-number-of-the-first-kind'-test
  (t/with-instrument `combo/stirling-number-of-the-first-kind'
    (t/is-spec-check combo/stirling-number-of-the-first-kind'))
  (t/with-instrument :all
    (t/is= 1 (combo/stirling-number-of-the-first-kind' 0 0))
    (t/is= 6 (combo/stirling-number-of-the-first-kind' 1 4))))

(t/deftest bell-number-test
  (t/with-instrument `combo/bell-number
    (t/is-spec-check combo/bell-number))
  (t/with-instrument :all
    (t/is= 1 (combo/bell-number 0))
    (t/is= 1 (combo/bell-number 1))
    (t/is= 2 (combo/bell-number 2))
    (t/is= 52 (combo/bell-number 5))
    ;;Math 4.9631E19
    (t/is= 49631246523618756274N (combo/bell-number 26))
    ;;Math 5.4571E20
    (t/is= 5.4571704793605997E20 (combo/bell-number 27))
    ;;Math 6.1605E21
    (t/is= 6.160539404599935E21 (combo/bell-number 28))))

(t/deftest catalan-number-test
  (t/with-instrument `combo/catalan-number
    (t/is-spec-check combo/catalan-number))
  (t/with-instrument :all
    (t/is= 1.0 (combo/catalan-number 0))
    (t/is= 1.0 (combo/catalan-number 1))
    (t/is= 2.0 (combo/catalan-number 2))
    (t/is= 5.0 (combo/catalan-number 3))
    ;;Math 14.000
    (t/is-approx= 14.0 (combo/catalan-number 4))
    ;;Math 42.000
    (t/is-approx= 42.0 (combo/catalan-number 5))
    ;;Math 16796
    (t/is-approx= 16796.0 (combo/catalan-number 10))))

(t/deftest catalan-number'-test
  (t/with-instrument `combo/catalan-number'
    (t/is-spec-check combo/catalan-number'))
  (t/with-instrument :all
    (t/is= 1 (combo/catalan-number' 0))
    (t/is= 42 (combo/catalan-number' 5))))

(t/deftest binomial-probability-test
  (t/with-instrument `combo/binomial-probability
    (t/is-spec-check combo/binomial-probability))
  (t/with-instrument :all
    (t/is= 1.0 (combo/binomial-probability 0 0 0.4))
    (t/is= 0.4 (combo/binomial-probability 1 1 0.4))
    (t/is= 0.48 (combo/binomial-probability 1 2 0.4))
    ;;SciPy 0.34560
    (t/is= 0.34559999999999996 (combo/binomial-probability 1 4 0.4))
    ;;SciPy 0.34559
    (t/is= 0.3456 (combo/binomial-probability 2 5 0.4))
    ;;SciPy 1.2100E-99
    (t/is= 1.2100131348406543E-99 (combo/binomial-probability 12 545 0.4))))

(t/deftest log-binomial-probability-test
  (t/with-instrument `combo/log-binomial-probability
    ;;:num-tests reduced; ::m/long-non- successes and ::m/finite-non- trials
    ;;drive log-choose-k-from-n direct summation up to 1e7 iterations -- ME
    (t/is-spec-check combo/log-binomial-probability {:num-tests 400}))
  (t/with-instrument :all
    (t/is= 0.0 (combo/log-binomial-probability 0 0 0.4))
    ;;SciPy -0.91629
    (t/is= -0.916290731874155 (combo/log-binomial-probability 1 1 0.4))
    ;;SciPy -0.78414
    (t/is= -0.7841487447593384 (combo/log-binomial-probability 1 1.4 0.4))
    ;;SciPy -1.0624
    (t/is= -1.0624732420522367 (combo/log-binomial-probability 1 4 0.4))
    ;;SciPy -1.0624
    (t/is= -1.0624732420522363 (combo/log-binomial-probability 2 5 0.4))
    ;;SciPy -227.76
    (t/is= -227.76529299162056 (combo/log-binomial-probability 12 545.0 0.4))))

;;;COUNTING
(t/deftest count-combinations-test
  (t/with-instrument `combo/count-combinations
    (t/is-spec-check combo/count-combinations))
  (t/with-instrument :all
    (t/is= 1.0 (combo/count-combinations 0 5))
    (t/is= 10.0 (combo/count-combinations 2 5))
    (t/is= 1.0 (combo/count-combinations 5 5))))

(t/deftest count-permutations-test
  (t/with-instrument `combo/count-permutations
    (t/is-spec-check combo/count-permutations))
  (t/with-instrument :all
    (t/is= 120.0 (combo/count-permutations 5))
    (t/is= 20.0 (combo/count-permutations 2 5))             ;; P(5,2) = 5 × 4
    (t/is= 120.0 (combo/count-permutations 5 5))))          ;; P(5,5) = 5!

;;;INTEGER PARTITIONS
(t/deftest integer-partitions-test
  (t/with-instrument `combo/integer-partitions
    (t/is-spec-check combo/integer-partitions))
  (t/with-instrument :all
    (t/is= '(()) (combo/integer-partitions 0))
    (t/is= '((1)) (combo/integer-partitions 1))
    (t/is= '((3) (2 1) (1 1 1)) (combo/integer-partitions 3))
    (t/is= '((4) (3 1) (2 2) (2 1 1) (1 1 1 1)) (combo/integer-partitions 4))
    ;; With max-part constraint
    (t/is= '((2 2) (2 1 1) (1 1 1 1)) (combo/integer-partitions 4 2))
    (t/is= '((3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1)) (combo/integer-partitions 5 3))))

(t/deftest count-integer-partitions-test
  (t/with-instrument `combo/count-integer-partitions
    (t/is-spec-check combo/count-integer-partitions))
  (t/with-instrument :all
    (t/is= 0 (combo/count-integer-partitions -1))
    (t/is= 1 (combo/count-integer-partitions 0))
    (t/is= 1 (combo/count-integer-partitions 1))
    (t/is= 3 (combo/count-integer-partitions 3))
    (t/is= 5 (combo/count-integer-partitions 4))
    (t/is= 7 (combo/count-integer-partitions 5))
    (t/is= 42 (combo/count-integer-partitions 10))
    ;;Math 1.9056E8
    (t/is= 190569292 (combo/count-integer-partitions 100))
    ;;p(405) is the largest value fitting in a long
    (t/is= 9147679068859117602 (combo/count-integer-partitions 405))
    ;;p(406) requires BigInt
    (t/is= 9725512513742021729N (combo/count-integer-partitions 406))
    (t/is= 2300165032574323995027N (combo/count-integer-partitions 500))))

;;;UNORDERED COMBINATIONS
(t/deftest combinations-test
  (t/with-instrument `combo/combinations
    (t/is-spec-check combo/combinations))
  (t/with-instrument :all
    (t/is= '(()) (combo/combinations [1 2 3] 0))
    (t/is= '((1 2) (1 3) (2 3)) (combo/combinations [1 2 3] 2))
    (t/is= '((1 2) (1 3) (2 3)) (combo/combinations '(1 2 3) 2))
    (t/is= '((1 3) (1 2) (3 2)) (combo/combinations #{1 2 3} 2))
    (t/is= '((1 1) (1 2) (1 2)) (combo/combinations [1 1 2] 2))
    (t/is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations [1 2 3]))
    (t/is= '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)) (combo/combinations '(1 2 3)))
    (t/is= '(() (1) (3) (2) (1 3) (1 2) (3 2) (1 3 2)) (combo/combinations #{1 2 3}))
    (t/is= '(() (1) (1) (2) (1 1) (1 2) (1 2) (1 1 2)) (combo/combinations [1 1 2]))))

(t/deftest combinations-with-complements-test
  (t/with-instrument `combo/combinations-with-complements
    (t/is-spec-check combo/combinations-with-complements))
  (t/with-instrument :all
    (t/is= '((() (1 2 3))
             ((1) (2 3))
             ((2) (1 3))
             ((3) (1 2))
             ((1 2) (3))
             ((1 3) (2))
             ((2 3) (1))
             ((1 2 3) ()))
      (combo/combinations-with-complements [1 2 3]))
    (t/is= '((() (1 2 3))
             ((1) (2 3))
             ((2) (1 3))
             ((3) (1 2))
             ((1 2) (3))
             ((1 3) (2))
             ((2 3) (1))
             ((1 2 3) ()))
      (combo/combinations-with-complements '(1 2 3)))
    (t/is= '((() (1 3 2))
             ((1) (3 2))
             ((3) (1 2))
             ((2) (1 3))
             ((1 3) (2))
             ((1 2) (3))
             ((3 2) (1))
             ((1 3 2) ()))
      (combo/combinations-with-complements #{1 2 3}))
    (t/is= '((() (1 1 2))
             ((1) (1 2))
             ((1) (1 2))
             ((2) (1 1))
             ((1 1) (2))
             ((1 2) (1))
             ((1 2) (1))
             ((1 1 2) ()))
      (combo/combinations-with-complements [1 1 2]))
    (t/is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1))) (combo/combinations-with-complements [1 2 3] 2))
    (t/is= '(((1 2) (3)) ((1 3) (2)) ((2 3) (1))) (combo/combinations-with-complements '(1 2 3) 2))
    (t/is= '(((1 3) (2)) ((1 2) (3)) ((3 2) (1))) (combo/combinations-with-complements #{1 2 3} 2))
    (t/is= '(((1 1) (2)) ((1 2) (1)) ((1 2) (1))) (combo/combinations-with-complements [1 1 2] 2))
    (t/is= '((() (1 2 3))) (combo/combinations-with-complements [1 2 3] 0))))

(t/deftest combinations-using-all-test
  (t/with-instrument `combo/combinations-using-all
    (t/is-spec-check combo/combinations-using-all))
  (t/with-instrument :all
    (t/is= '(((1 2) (3 4))
             ((1 3) (2 4))
             ((1 4) (2 3))
             ((2 3) (1 4))
             ((2 4) (1 3))
             ((3 4) (1 2)))
      (combo/combinations-using-all [1 2 3 4] [2 2]))
    (t/is= '(((1 2) (3 4))
             ((1 3) (2 4))
             ((1 4) (2 3))
             ((2 3) (1 4))
             ((2 4) (1 3))
             ((3 4) (1 2)))
      (combo/combinations-using-all '(1 2 3 4) [2 2]))
    (t/is= '(((1 4) (3 2))
             ((1 3) (4 2))
             ((1 2) (4 3))
             ((4 3) (1 2))
             ((4 2) (1 3))
             ((3 2) (1 4)))
      (combo/combinations-using-all #{1 2 3 4} [2 2]))
    (t/is= '(((1 1) (2 3))
             ((1 2) (1 3))
             ((1 3) (1 2))
             ((1 2) (1 3))
             ((1 3) (1 2))
             ((2 3) (1 1)))
      (combo/combinations-using-all [1 1 2 3] [2 2]))
    (t/is= '(((1 2 3) (4))
             ((1 2 4) (3))
             ((1 3 4) (2))
             ((2 3 4) (1)))
      (combo/combinations-using-all [1 2 3 4] [3 1]))))

(t/deftest distinct-combinations-with-replacement-test
  (t/with-instrument `combo/distinct-combinations-with-replacement
    (t/is-spec-check combo/distinct-combinations-with-replacement))
  (t/with-instrument :all
    (t/is= '(())
      (combo/distinct-combinations-with-replacement [1 2 3 4] 0))
    (t/is= '(() (1) (2) (3) (4))
      (combo/distinct-combinations-with-replacement [1 2 3 4] 1))
    (t/is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
      (combo/distinct-combinations-with-replacement [1 2 3 4] 2))
    (t/is= '(() (1) (2) (3) (4) (1 1) (1 2) (1 3) (1 4) (2 2) (2 3) (2 4) (3 3) (3 4) (4 4))
      (combo/distinct-combinations-with-replacement '(1 2 3 4) 2))
    (t/is= '(() (1) (4) (3) (2) (1 1) (1 4) (1 3) (1 2) (4 4) (4 3) (4 2) (3 3) (3 2) (2 2))
      (combo/distinct-combinations-with-replacement #{1 2 3 4} 2))
    (t/is= '(() (1) (2) (3) (1 1) (1 2) (1 3) (2 2) (2 3) (3 3))
      (combo/distinct-combinations-with-replacement [1 1 2 3] 2))
    (t/is= '(() (1) (2) (1 1) (1 2) (2 2) (1 1 1) (1 1 2) (1 2 2) (2 2 2))
      (combo/distinct-combinations-with-replacement [1 2] 3))))

;;;ORDERED COMBINATIONS
(t/deftest permutations-test
  (t/with-instrument `combo/permutations
    (t/is-spec-check combo/permutations))
  (t/with-instrument :all
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations '(1 2 3)))
    (t/is= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)) (combo/permutations [1 2 3]))
    (t/is= '((1 3 2) (1 2 3) (3 1 2) (3 2 1) (2 1 3) (2 3 1)) (combo/permutations #{1 2 3}))
    (t/is= '((1 1 2) (1 2 1) (1 1 2) (1 2 1) (2 1 1) (2 1 1)) (combo/permutations [1 1 2]))))

(t/deftest k-permutations-test
  (t/with-instrument `combo/k-permutations
    (t/is-spec-check combo/k-permutations))
  (t/with-instrument :all
    (t/is= '([]) (combo/k-permutations [1 2 3] 0))
    (t/is= '([1] [2] [3]) (combo/k-permutations [1 2 3] 1))
    (t/is= '([1 2] [2 1] [1 3] [3 1] [2 3] [3 2]) (combo/k-permutations [1 2 3] 2))
    (t/is= nil (combo/k-permutations [1 2] 3))))

(t/deftest cartesian-product-test
  (t/with-instrument `combo/cartesian-product
    (t/is-spec-check combo/cartesian-product))
  (t/with-instrument :all
    (t/is= '() (combo/cartesian-product '((1 2)) '()))
    (t/is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
      (combo/cartesian-product [1 2 3] [8.0 9.0]))
    (t/is= '((1 8.0) (1 9.0) (2 8.0) (2 9.0) (3 8.0) (3 9.0))
      (combo/cartesian-product '(1 2 3) [8.0 9.0]))
    (t/is= '((1 8.0) (1 9.0) (1 8.0) (1 9.0) (2 8.0) (2 9.0))
      (combo/cartesian-product [1 1 2] [8.0 9.0]))))

(t/deftest selections-test
  (t/with-instrument `combo/selections
    (t/is-spec-check combo/selections))
  (t/with-instrument :all
    (t/is= '(()) (combo/selections [1 2 3] 0))
    (t/is= '((1) (2) (3)) (combo/selections [1 2 3] 1))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections [1 2 3] 2))
    (t/is= '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3)) (combo/selections '(1 2 3) 2))
    (t/is= '((1 1) (1 3) (1 2) (3 1) (3 3) (3 2) (2 1) (2 3) (2 2)) (combo/selections #{1 2 3} 2))
    (t/is= '((1 1) (1 1) (1 2) (1 1) (1 1) (1 2) (2 1) (2 1) (2 2)) (combo/selections [1 1 2] 2))))

;;;DIRECT ACCESS
(t/deftest nth-combination-test
  (t/with-instrument `combo/nth-combination
    (t/is-spec-check combo/nth-combination))
  (t/with-instrument :all
    (t/is= '(1 2) (combo/nth-combination [1 2 3 4 5] 2 0))
    (t/is= '(4 5) (combo/nth-combination [1 2 3 4 5] 2 9))
    (t/is= nil (combo/nth-combination [1 2 3] 2 10))))

(t/deftest nth-permutation-test
  (t/with-instrument `combo/nth-permutation
    (t/is-spec-check combo/nth-permutation))
  (t/with-instrument :all
    (t/is= '(1 2 3) (combo/nth-permutation [1 2 3] 0))
    (t/is= '(3 2 1) (combo/nth-permutation [1 2 3] 5))
    (t/is= nil (combo/nth-permutation [1 2 3] 6))))

(t/deftest nth-k-permutation-test
  (t/with-instrument `combo/nth-k-permutation
    (t/is-spec-check combo/nth-k-permutation))
  (t/with-instrument :all
    (t/is= '(1 2) (combo/nth-k-permutation [1 2 3 4] 2 0))
    (t/is= '(4 3) (combo/nth-k-permutation [1 2 3 4] 2 11))
    (t/is= nil (combo/nth-k-permutation [1 2 3] 2 10))))
