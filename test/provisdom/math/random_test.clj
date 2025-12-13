(ns provisdom.math.random-test
  (:require
    
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random])
  (:import (java.util UUID)))

;;1 seconds

(set! *warn-on-reflection* true)

;;;HELPERS
(deftest random-long-test
  (t/with-instrument `random/random-long
    (t/is (t/spec-check random/random-long)))
  (t/with-instrument :all)
  (t/is= -1254378597012249600 (random/random-long 0.432))
  (t/is= 6 (random/random-long 0.432 [5 8])))

(deftest random-bool-test
  (t/with-instrument `random/random-bool
    (t/is (t/spec-check random/random-bool)))
  (t/with-instrument :all)
  (t/is (random/random-bool 0.432))
  (t/is-not (random/random-bool 0.5223)))

(deftest random-normal-test
  (t/with-instrument `random/random-normal
    (t/is (t/spec-check random/random-normal)))
  (t/with-instrument :all)
  (t/is= -0.17128458593150658 (random/random-normal 0.432)))

(deftest random-uuid-test
  (t/with-instrument `random/random-uuid
    (t/is (t/spec-check random/random-uuid)))
  (t/with-instrument :all)
  (let [^UUID uuid (random/random-uuid 0.432 0.789)]
    (t/is (uuid? uuid))
    (t/is= 4 (.version uuid))
    (t/is= 2 (.variant uuid)))
  (t/is= (random/random-uuid 0.5 0.5) (random/random-uuid 0.5 0.5)))

;;;IMMUTABLE RNG
(deftest rng-test
  (t/with-instrument `random/rng
    (t/is (t/spec-check random/rng)))
  (t/with-instrument :all))

(deftest rnd-test
  (t/with-instrument `random/rnd
    (t/is (t/spec-check random/rnd)))
  (t/with-instrument :all)
  (t/is= 0.11345034205715454 (random/rnd (random/rng 3)))
  (t/is= -0.4619863177138184 (random/rnd (random/rng 3) [-5.0 35.0]))
  (t/is= -1.3897953327344593E308 (random/rnd (random/rng 3) [m/min-dbl m/max-dbl])))

(deftest rnd-long-test
  (t/with-instrument `random/rnd-long
    (t/is (t/spec-check random/rnd-long)))
  (t/with-instrument :all)
  (t/is= 2092789425003139053 (random/rnd-long (random/rng 3)))
  (t/is= 8 (random/rnd-long (random/rng 3) [5 35])))

(deftest rnd-bool-test
  (t/with-instrument `random/rnd-bool
    (t/is (t/spec-check random/rnd-bool)))
  (t/with-instrument :all)
  (t/is (random/rnd-bool (random/rng 3))))

(deftest rnd-normal-test
  (t/with-instrument `random/rnd-normal
    (t/is (t/spec-check random/rnd-normal)))
  (t/with-instrument :all)
  (t/is= -1.2083811575795258 (random/rnd-normal (random/rng 3))))

(deftest rnd-uuid-test
  (t/with-instrument `random/rnd-uuid
    (t/is (t/spec-check random/rnd-uuid)))
  (t/with-instrument :all)
  (let [^UUID uuid (random/rnd-uuid (random/rng 3))]
    (t/is (uuid? uuid))
    (t/is= 4 (.version uuid))
    (t/is= 2 (.variant uuid)))
  (t/is= (random/rnd-uuid (random/rng 3)) (random/rnd-uuid (random/rng 3))))

(deftest rng-lazy-test
  (t/with-instrument `random/rng-lazy
    (t/is (t/spec-check random/rng-lazy)))
  (t/with-instrument :all)
  (t/is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (map random/rnd (random/rng-lazy (random/rng 3))))))

(deftest rnd-lazy-test
  (t/with-instrument `random/rnd-lazy
    (t/is (t/spec-check random/rnd-lazy)))
  (t/with-instrument :all)
  (t/is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (random/rnd-lazy (random/rng 3)))))

(deftest rnd-long-lazy-test
  (t/with-instrument `random/rnd-long-lazy
    (t/is (t/spec-check random/rnd-long-lazy)))
  (t/with-instrument :all)
  (t/is= '(2092789425003139053 -7139356981108613887 3992596847233833366)
       (take 3 (random/rnd-long-lazy (random/rng 3)))))

;;;BOUND RNG
(deftest rng!-test
  (t/with-instrument `random/rng!
    (t/is (t/spec-check random/rng!)))
  (t/with-instrument :all)
  (t/is= 0.11345034205715454 (random/rnd (random/bind-seed 3 (random/rng!)))))

(deftest rnd!-test
  (t/with-instrument `random/rnd!
    (t/is (t/spec-check random/rnd!)))
  (t/with-instrument :all)
  (t/is= 0.11345034205715454 (random/bind-seed 3 (random/rnd!)))
  (t/is= -0.4619863177138184 (random/bind-seed 3 (random/rnd! [-5.0 35.0])))
  (t/is= 0.11345034205715454 (do (random/set-seed! 3) (random/rnd!)))
  (t/is= 0.11345034205715454 (random/do-set-seed! 3 (random/rnd!))))

(deftest rnd-long!-test
  (t/with-instrument `random/rnd-long!
    (t/is (t/spec-check random/rnd-long!)))
  (t/with-instrument :all)
  (t/is= 2092789425003139053 (random/bind-seed 3 (random/rnd-long!)))
  (t/is= 3 (random/bind-seed 3 (random/rnd-long! [3 6]))))

(deftest rnd-bool!-test
  (t/with-instrument `random/rnd-bool!
    (t/is (t/spec-check random/rnd-bool!)))
  (t/with-instrument :all)
  (t/is (random/bind-seed 3 (random/rnd-bool!))))

(deftest rnd-normal!-test
  (t/with-instrument `random/rnd-normal!
    (t/is (t/spec-check random/rnd-normal!)))
  (t/with-instrument :all)
  (t/is= -1.2083811575795258 (random/bind-seed 3 (random/rnd-normal!))))

(deftest rnd-uuid!-test
  (t/with-instrument `random/rnd-uuid!
    (t/is (t/spec-check random/rnd-uuid!)))
  (t/with-instrument :all)
  (let [^UUID uuid (random/bind-seed 3 (random/rnd-uuid!))]
    (t/is (uuid? uuid))
    (t/is= 4 (.version uuid))
    (t/is= 2 (.variant uuid))))

(deftest rng-lazy!-test
  (t/with-instrument `random/rng-lazy!
    (t/is (t/spec-check random/rng-lazy!)))
  (t/with-instrument :all)
  (t/is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (map random/rnd (random/bind-seed 3 (random/rng-lazy!))))))

(deftest rnd-lazy!-test
  (t/with-instrument `random/rnd-lazy!
    (t/is (t/spec-check random/rnd-lazy!)))
  (t/with-instrument :all)
  (t/is= '(0.11345034205715454 0.6129746825466243 0.21643910878148487)
       (take 3 (random/bind-seed 3 (random/rnd-lazy!)))))

(deftest rnd-long-lazy!-test
  (t/with-instrument `random/rnd-long-lazy!
    (t/is (t/spec-check random/rnd-long-lazy!)))
  (t/with-instrument :all)
  (t/is= '(2092789425003139053 -7139356981108613887 3992596847233833366)
       (take 3 (random/bind-seed 3 (random/rnd-long-lazy!)))))

;;;USE CLOCK
(deftest rng$-test
  (t/with-instrument `random/rng$
    (t/is (t/spec-check random/rng$)))
  (t/with-instrument :all))
  ;(t/is= 0.9790050362451599 (random/rnd (random/rng$)))

(deftest seed$-test
  (t/with-instrument `random/seed$
    (t/is (t/spec-check random/seed$)))
  (t/with-instrument :all))
  ;(t/is= 3765021903556771769 (random/seed$))

(deftest set-seed!$-test
  (t/with-instrument `random/set-seed!$
    (t/is (t/spec-check random/set-seed!$)))
  (t/with-instrument :all))

(comment "Might be needed in future"
  (def sample-f (fn [r] (* 100 r)))
  (def sample-lazy-fn (fn [r] [(* 100 (first r)) (rest r)]))
  ;;replace multi-sample with: (repeatedly n sample-fn!)
  (fact "multi sample indexed"
        (first (multi-sample-indexed
                 (fn [i r] [(+ (* 100 i) (first r)) (rest r)])
                 (random) 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013)
        (first (multi-sample-indexed
                 (with-meta (fn [i r] (+ (* 100 i) r)) {:r :rnd}) (random)
                 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013)
        (first (multi-sample-indexed
                 (fn [i r] [(+ (* 100 i) (first r)) (rest r)])
                 (random) 3))
        => '(0.8335762378570932 100.11249249636232 200.85024069792013))
  (fact "fold random"
        (first (fold-random 1000 + (with-meta sample-f {:r :rnd})
                            (random)))
        => 50653.13952160504
        (defn reduce-f [tot e]
          [(+ (first tot) e) (* (second tot) e 0.0285)])
        (defn combine-f
          ([] [0.0 1.0])
          ([tot1 tot2] [(+ (first tot1) (first tot2)),
                        (* (second tot1) (second tot2))]))
        (first (fold-random
                 1000 combine-f reduce-f
                 (with-meta sample-f {:r :rnd}) (random)))
        => [50653.13952160504 39.987434644813305]
        (first (fold-random
                 300 4 combine-f reduce-f
                 (with-meta sample-f {:r :rnd}) (random)))
        => [58986.355496151344 0.5303202033382911]
        (first (fold-random 1000 + sample-lazy-fn (random)))
        => 51685.727620165926
        (first (fold-random
                 1000 combine-f reduce-f sample-lazy-fn (random)))
        => [51685.727620165926 2.6215627010170977E32]
        (first (fold-random
                 300 4 combine-f reduce-f sample-lazy-fn (random)))
        => [60626.00738574421 6.304745954380607E38]))
