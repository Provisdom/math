(ns provisdom.math.random-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.combinatorics :as cm]
    [provisdom.math.random :as random]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;20 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;HELPERS
(deftest random-long-test
  (is (spec-check random/random-long))
  (is= -1254378597012249600 (random/random-long 0.432))
  (is= 6 (random/random-long 0.432 [5 8])))

(deftest random-bool-test
  (is (spec-check random/random-bool))
  (is (random/random-bool 0.432))
  (is-not (random/random-bool 0.5223)))

(deftest random-normal-test
  (is (spec-check random/random-normal))
  (is= -0.17128458593150658 (random/random-normal 0.432)))

;;;IMMUTABLE RNG
(deftest rng-test
  (is (spec-check random/rng)))

(deftest rnd-test
  (is (spec-check random/rnd))
  (is= 0.11345034205715454 (random/rnd (random/rng 3)))
  (is= -0.4619863177138184 (random/rnd (random/rng 3) [-5.0 35.0])))

(deftest rnd-long-test
  (is (spec-check random/rnd-long))
  (is= 2092789425003139053 (random/rnd-long (random/rng 3)))
  (is= 8 (random/rnd-long (random/rng 3) [5 35])))

(deftest rnd-bool-test
  (is (spec-check random/rnd-bool))
  (is (random/rnd-bool (random/rng 3))))

(deftest rnd-normal-test
  (is (spec-check random/rnd-normal))
  (is= -1.2083811575795258 (random/rnd-normal (random/rng 3))))

(deftest rng-lazy-test
  (is (spec-check random/rng-lazy))
  (is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (map random/rnd (random/rng-lazy (random/rng 3))))))

(deftest rnd-lazy-test
  (is (spec-check random/rnd-lazy))
  (is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (random/rnd-lazy (random/rng 3)))))

(deftest rnd-long-lazy-test
  (is (spec-check random/rnd-long-lazy))
  (is= '(2092789425003139053 -7139356981108613887 3992596847233833366)
       (take 3 (random/rnd-long-lazy (random/rng 3)))))

;;;BOUND RNG
(deftest rng!-test
  (is (spec-check random/rng!))
  (is= 0.11345034205715454 (random/rnd (random/bind-seed 3 (random/rng!)))))

(deftest rnd!-test
  (is (spec-check random/rnd!))
  (is= 0.11345034205715454 (random/bind-seed 3 (random/rnd!)))
  (is= -0.4619863177138184 (random/bind-seed 3 (random/rnd! [-5.0 35.0])))
  (is= 0.11345034205715454 (do (random/set-seed! 3) (random/rnd!)))
  (is= 0.11345034205715454 (random/do-set-seed! 3 (random/rnd!))))

(deftest rnd-long!-test
  (is (spec-check random/rnd-long!))
  (is= 2092789425003139053 (random/bind-seed 3 (random/rnd-long!)))
  (is= 3 (random/bind-seed 3 (random/rnd-long! [3 6]))))

(deftest rnd-bool!-test
  (is (spec-check random/rnd-bool!))
  (is (random/bind-seed 3 (random/rnd-bool!))))

(deftest rnd-normal!-test
  (is (spec-check random/rnd-normal!))
  (is= -1.2083811575795258 (random/bind-seed 3 (random/rnd-normal!))))

(deftest rng-lazy!-test
  (is (spec-check random/rng-lazy!))
  (is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (map random/rnd
                    (random/bind-seed 3 (random/rng-lazy!))))))

(deftest rnd-lazy!-test
  (is (spec-check random/rnd-lazy!))
  (is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (random/bind-seed 3 (random/rnd-lazy!)))))

(deftest rnd-long-lazy!-test
  (is (spec-check random/rnd-long-lazy!))
  (is= '(2092789425003139053 -7139356981108613887 3992596847233833366)
       (take 3 (random/bind-seed 3 (random/rnd-long-lazy!)))))

;;;USE CLOCK
(deftest rng$-test
  (is (spec-check random/rng$))
  ;(is= 0.9790050362451599 (random/rnd (random/rng$)))
  )

(deftest seed$-test
  (is (spec-check random/seed$))
  ;(is= 3765021903556771769 (random/seed$))
  )

(deftest set-seed!$-test
  (is (spec-check random/set-seed!$)))

;;;APACHE RANDOM NUMBER GENERATORS
(deftest quasi-rnd-vector-lazy-test
  (is (spec-check random/quasi-rnd-vector-lazy))
  (is= '([0.0] [0.5] [0.75])
       (take 3 (random/quasi-rnd-vector-lazy 1)))
  (is= '([0.0 0.0] [0.5 0.5] [0.75 0.25])
       (take 3 (random/quasi-rnd-vector-lazy 2))))

(deftest secure-rnd-lazy-test
  (is (spec-check random/secure-rnd-lazy))
  (is= 0.26673862796330083 (first (random/secure-rnd-lazy 4)))
  (is= 0.9214463212165593 (first (random/secure-rnd-lazy 0))))

(deftest mersenne-rnd-lazy-test
  (is (spec-check random/mersenne-rnd-lazy))
  (is= 0.8335762378570932 (first (random/mersenne-rnd-lazy 4)))
  (is= 0.15071724896777527 (first (random/mersenne-rnd-lazy 0))))

#_(ost/unstrument)

(comment
  (def samplef (fn [r] (* 100 r)))
  (def sample-lazy-fn (fn [r] [(* 100 (first r)) (rest r)]))
  (fact "multi sample"
        (first (multi-sample sample-lazy-fn (mersenne-random) 3))
        => [83.35762378570932 11.249249636232017 85.02406979201282]
        (first (multi-sample (with-meta samplef {:r :rnd}) (mersenne-random) 3))
        => [83.35762378570932 11.249249636232017 85.02406979201282]
        (first (multi-sample
                 (with-meta sample-lazy-fn {:r :rnd-lazy}) (mersenne-random) 3))
        => [83.35762378570932 11.249249636232017 85.02406979201282])
  (fact "multi sample indexed"
        (first (multi-sample-indexed
                 (fn [i r] [(+ (* 100 i) (first r)) (rest r)])
                 (mersenne-random) 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013)
        (first (multi-sample-indexed
                 (with-meta (fn [i r] (+ (* 100 i) r)) {:r :rnd}) (mersenne-random)
                 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013)
        (first (multi-sample-indexed
                 (fn [i r] [(+ (* 100 i) (first r)) (rest r)])
                 (mersenne-random) 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013))
  (fact "fold random"
        (first (fold-random 1000 + (with-meta samplef {:r :rnd})
                            (mersenne-random)))
        => 50653.13952160504
        (defn reducef [tot e]
          [(+ (first tot) e) (* (second tot) e 0.0285)])
        (defn combinef
          ([] [0.0 1.0])
          ([tot1 tot2] [(+ (first tot1) (first tot2)),
                        (* (second tot1) (second tot2))]))
        (first (fold-random
                 1000 combinef reducef
                 (with-meta samplef {:r :rnd}) (mersenne-random)))
        => [50653.13952160504 39.987434644813305]
        (first (fold-random
                 300 4 combinef reducef
                 (with-meta samplef {:r :rnd}) (mersenne-random)))
        => [58986.355496151344 0.5303202033382911]
        (first (fold-random 1000 + sample-lazy-fn (mersenne-random)))
        => 51685.727620165926
        (first (fold-random
                 1000 combinef reducef sample-lazy-fn (mersenne-random)))
        => [51685.727620165926 2.6215627010170977E32]
        (first (fold-random
                 300 4 combinef reducef sample-lazy-fn (mersenne-random)))
        => [60626.00738574421 6.304745954380607E38]))