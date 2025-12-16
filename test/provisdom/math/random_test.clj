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
  (t/with-instrument :all)
  ;; Test algorithm selection
  (t/is= 0.17343824438608113 (random/rnd (random/rng 3)))
  (t/is= 0.17343824438608113 (random/rnd (random/rng 3 :default)))
  (t/is= 0.4469927121551721 (random/rnd (random/rng 3 :fast)))
  (t/is= 0.8775965774237054 (random/rnd (random/rng 3 :quality)))
  (t/is= 0.11345034205715454 (random/rnd (random/rng 3 :legacy)))
  ;; SecureRandom is non-deterministic, just test it works
  (t/is (number? (random/rnd (random/rng 3 :secure)))))

(deftest rnd-test
  (t/with-instrument `random/rnd
    (t/is (t/spec-check random/rnd)))
  (t/with-instrument :all)
  (t/is= 0.17343824438608113 (random/rnd (random/rng 3)))
  (t/is= 1.9375297754432452 (random/rnd (random/rng 3) [-5.0 35.0]))
  (t/is= -1.1741156523514546E308 (random/rnd (random/rng 3) [m/min-dbl m/max-dbl])))

(deftest rnd-long-test
  (t/with-instrument `random/rnd-long
    (t/is (t/spec-check random/rnd-long)))
  (t/with-instrument :all)
  (t/is= 3199370906783531226 (random/rnd-long (random/rng 3)))
  (t/is= 10 (random/rnd-long (random/rng 3) [5 35])))

(deftest rnd-bool-test
  (t/with-instrument `random/rnd-bool
    (t/is (t/spec-check random/rnd-bool)))
  (t/with-instrument :all)
  (t/is (random/rnd-bool (random/rng 3))))

(deftest rnd-normal-test
  (t/with-instrument `random/rnd-normal
    (t/is (t/spec-check random/rnd-normal)))
  (t/with-instrument :all)
  (t/is= -0.9406651398679139 (random/rnd-normal (random/rng 3))))

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
  (t/is= '(0.17343824438608113 0.5672348695804793 0.4407296095269242)
       (take 3 (map random/rnd (random/rng-lazy (random/rng 3))))))

(deftest rnd-lazy-test
  (t/with-instrument `random/rnd-lazy
    (t/is (t/spec-check random/rnd-lazy)))
  (t/with-instrument :all)
  (t/is= '(0.17343824438608113 0.5672348695804793 0.4407296095269242)
       (take 3 (random/rnd-lazy (random/rng 3)))))

(deftest rnd-long-lazy-test
  (t/with-instrument `random/rnd-long-lazy
    (t/is (t/spec-check random/rnd-long-lazy)))
  (t/with-instrument :all)
  (t/is= '(3199370906783531226 -7983107604874433585 8130026312649114387)
       (take 3 (random/rnd-long-lazy (random/rng 3)))))

;;;BOUND RNG
(deftest rng!-test
  (t/with-instrument `random/rng!
    (t/is (t/spec-check random/rng!)))
  (t/with-instrument :all)
  (t/is= 0.17343824438608113 (random/rnd (random/bind-seed 3 (random/rng!)))))

(deftest rnd!-test
  (t/with-instrument `random/rnd!
    (t/is (t/spec-check random/rnd!)))
  (t/with-instrument :all)
  (t/is= 0.17343824438608113 (random/bind-seed 3 (random/rnd!)))
  (t/is= 1.9375297754432452 (random/bind-seed 3 (random/rnd! [-5.0 35.0])))
  (t/is= 0.17343824438608113 (do (random/set-seed! 3) (random/rnd!)))
  (t/is= 0.17343824438608113 (random/do-set-seed! 3 (random/rnd!))))

(deftest rnd-long!-test
  (t/with-instrument `random/rnd-long!
    (t/is (t/spec-check random/rnd-long!)))
  (t/with-instrument :all)
  (t/is= 3199370906783531226 (random/bind-seed 3 (random/rnd-long!)))
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
  (t/is= -0.9406651398679139 (random/bind-seed 3 (random/rnd-normal!))))

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
  (t/is= '(0.17343824438608113 0.5672348695804793 0.4407296095269242)
       (take 3 (map random/rnd (random/bind-seed 3 (random/rng-lazy!))))))

(deftest rnd-lazy!-test
  (t/with-instrument `random/rnd-lazy!
    (t/is (t/spec-check random/rnd-lazy!)))
  (t/with-instrument :all)
  (t/is= '(0.17343824438608113 0.5672348695804793 0.4407296095269242)
       (take 3 (random/bind-seed 3 (random/rnd-lazy!)))))

(deftest rnd-long-lazy!-test
  (t/with-instrument `random/rnd-long-lazy!
    (t/is (t/spec-check random/rnd-long-lazy!)))
  (t/with-instrument :all)
  (t/is= '(3199370906783531226 -7983107604874433585 8130026312649114387)
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

;;;INTROSPECTION
(deftest rng-algorithm-test
  (t/with-instrument `random/rng-algorithm
    (t/is (t/spec-check random/rng-algorithm)))
  (t/with-instrument :all)
  (t/is= "L64X128MixRandom" (random/rng-algorithm (random/rng 3)))
  (t/is= "L64X128MixRandom" (random/rng-algorithm (random/rng 3 :default)))
  (t/is= "L32X64MixRandom" (random/rng-algorithm (random/rng 3 :fast)))
  (t/is= "L128X256MixRandom" (random/rng-algorithm (random/rng 3 :quality)))
  (t/is= "SplittableRandom" (random/rng-algorithm (random/rng 3 :legacy)))
  (t/is= :secure (random/rng-algorithm (random/rng 3 :secure))))

(deftest rng-seed-test
  (t/with-instrument `random/rng-seed
    (t/is (t/spec-check random/rng-seed)))
  (t/with-instrument :all)
  (t/is= 42 (random/rng-seed (random/rng 42)))
  (t/is= nil (random/rng-seed (random/rng 0 :secure))))

;;;BATCH GENERATION
(deftest rnd-doubles-test
  (t/with-instrument `random/rnd-doubles
    (t/is (t/spec-check random/rnd-doubles)))
  (t/with-instrument :all)
  (t/is= [0.17343824438608113 0.5672348695804793 0.4407296095269242]
         (random/rnd-doubles (random/rng 3) 3))
  (t/is= [] (random/rnd-doubles (random/rng 3) 0)))

(deftest rnd-doubles!-test
  (t/with-instrument `random/rnd-doubles!
    (t/is (t/spec-check random/rnd-doubles!)))
  (t/with-instrument :all)
  (t/is= [0.17343824438608113 0.5672348695804793 0.4407296095269242]
         (random/bind-seed 3 (random/rnd-doubles! 3))))

(deftest rnd-longs-test
  (t/with-instrument `random/rnd-longs
    (t/is (t/spec-check random/rnd-longs)))
  (t/with-instrument :all)
  (t/is= [3199370906783531226 -7983107604874433585 8130026312649114387]
         (random/rnd-longs (random/rng 3) 3)))

(deftest rnd-longs!-test
  (t/with-instrument `random/rnd-longs!
    (t/is (t/spec-check random/rnd-longs!)))
  (t/with-instrument :all)
  (t/is= [3199370906783531226 -7983107604874433585 8130026312649114387]
         (random/bind-seed 3 (random/rnd-longs! 3))))

(deftest rnd-normals-test
  (t/with-instrument `random/rnd-normals
    (t/is (t/spec-check random/rnd-normals)))
  (t/with-instrument :all)
  (t/is= [-0.9406651398679139 0.16933867009164844 -0.14911965219889575]
         (random/rnd-normals (random/rng 3) 3)))

(deftest rnd-normals!-test
  (t/with-instrument `random/rnd-normals!
    (t/is (t/spec-check random/rnd-normals!)))
  (t/with-instrument :all)
  (t/is= [-0.9406651398679139 0.16933867009164844 -0.14911965219889575]
         (random/bind-seed 3 (random/rnd-normals! 3))))

;;;CONVENIENCE FUNCTIONS
(deftest rnd-int-test
  (t/with-instrument `random/rnd-int
    (t/is (t/spec-check random/rnd-int)))
  (t/with-instrument :all)
  (t/is= 1 (random/rnd-int (random/rng 3) 10))
  (t/is= 2 (random/rnd-int (random/rng 0) 10)))

(deftest rnd-int!-test
  (t/with-instrument `random/rnd-int!
    (t/is (t/spec-check random/rnd-int!)))
  (t/with-instrument :all)
  (t/is= 1 (random/bind-seed 3 (random/rnd-int! 10))))

(deftest rnd-gaussian-test
  (t/with-instrument `random/rnd-gaussian
    (t/is (t/spec-check random/rnd-gaussian)))
  (t/with-instrument :all)
  (t/is= 3.1186697202641723 (random/rnd-gaussian (random/rng 3) 5.0 2.0)))

(deftest rnd-gaussian!-test
  (t/with-instrument `random/rnd-gaussian!
    (t/is (t/spec-check random/rnd-gaussian!)))
  (t/with-instrument :all)
  (t/is= 3.1186697202641723 (random/bind-seed 3 (random/rnd-gaussian! 5.0 2.0))))

;;;COLLECTION UTILITIES
(deftest rnd-choice-test
  (t/with-instrument `random/rnd-choice
    (t/is (t/spec-check random/rnd-choice)))
  (t/with-instrument :all)
  (t/is= :a (random/rnd-choice (random/rng 3) [:a :b :c :d :e]))
  (t/is= nil (random/rnd-choice (random/rng 3) [])))

(deftest rnd-choice!-test
  (t/with-instrument `random/rnd-choice!
    (t/is (t/spec-check random/rnd-choice!)))
  (t/with-instrument :all)
  (t/is= :a (random/bind-seed 3 (random/rnd-choice! [:a :b :c :d :e]))))

(deftest rnd-shuffle-test
  (t/with-instrument `random/rnd-shuffle
    (t/is (t/spec-check random/rnd-shuffle)))
  (t/with-instrument :all)
  (t/is= [4 5 2 3 1] (random/rnd-shuffle (random/rng 3) [1 2 3 4 5]))
  (t/is= [] (random/rnd-shuffle (random/rng 3) [])))

(deftest rnd-shuffle!-test
  (t/with-instrument `random/rnd-shuffle!
    (t/is (t/spec-check random/rnd-shuffle!)))
  (t/with-instrument :all)
  (t/is= [4 5 2 3 1] (random/bind-seed 3 (random/rnd-shuffle! [1 2 3 4 5]))))

(deftest rnd-sample-test
  (t/with-instrument `random/rnd-sample
    (t/is (t/spec-check random/rnd-sample)))
  (t/with-instrument :all)
  (t/is= [4 5 2] (random/rnd-sample (random/rng 3) 3 [1 2 3 4 5]))
  (t/is= [4 5 2 3 1] (random/rnd-sample (random/rng 3) 10 [1 2 3 4 5])))

(deftest rnd-sample!-test
  (t/with-instrument `random/rnd-sample!
    (t/is (t/spec-check random/rnd-sample!)))
  (t/with-instrument :all)
  (t/is= [4 5 2] (random/bind-seed 3 (random/rnd-sample! 3 [1 2 3 4 5]))))

(deftest rnd-weighted-choice-test
  (t/with-instrument `random/rnd-weighted-choice
    (t/is (t/spec-check random/rnd-weighted-choice)))
  (t/with-instrument :all)
  (t/is= :b (random/rnd-weighted-choice (random/rng 3) [0.1 0.2 0.7] [:a :b :c])))

(deftest rnd-weighted-choice!-test
  (t/with-instrument `random/rnd-weighted-choice!
    (t/is (t/spec-check random/rnd-weighted-choice!)))
  (t/with-instrument :all)
  (t/is= :b (random/bind-seed 3 (random/rnd-weighted-choice! [0.1 0.2 0.7] [:a :b :c]))))

;;;PARALLEL UTILITIES
(deftest parallel-sample-test
  (t/with-instrument `random/parallel-sample
    (t/is (t/spec-check random/parallel-sample {:num-tests 10})))
  (t/with-instrument :all)
  (let [samples (random/parallel-sample (random/rng 3) #(random/rnd-int % 100) 5)]
    (t/is= 5 (count samples))
    (t/is (every? #(and (int? %) (<= 0 % 99)) samples))))

(deftest parallel-fold-test
  (t/with-instrument :all)
  (let [result (random/parallel-fold (random/rng 3) 100 (constantly 0) + #(random/rnd-int % 100))]
    (t/is (int? result))
    (t/is (pos? result))))
