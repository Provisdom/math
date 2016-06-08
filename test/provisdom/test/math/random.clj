(ns provisdom.test.math.random
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.random :refer :all]
            [clojure.test :refer [deftest is]]))

(facts "random"
       (fact "random draws"
             (take 3 (quasi-random 1)) => [[0.0] [0.5] [0.75]]
             (take 3 (quasi-random 2)) => [[0.0 0.0] [0.5 0.5] [0.75 0.25]]
             (first (secure-random)) => 0.26673862796330083
             (first (secure-random 0)) => 0.9214463212165593
             (first (random)) => 0.8335762378570932
             (first (random 0)) => 0.15071724896777527
             (rnd-long 0.432) => -1254378597012249600
             (rnd-long 0.432 8 5) => (throws)
             (rnd-long 0.432 5 8) => 6
             (rnd-boolean 0.432) => true 
             (rnd-boolean 0.5223) => false
             (rnd-normal 0.432) => -0.17128458593150658
             (def my-rnd (random$))
             (first (random (second my-rnd))) => (ffirst my-rnd))
       (fact "split random"
             (map first (split-random (random))) 
             => [0.11249249636232017 0.20562722505260633]
             (first (ffirst (split-random-lazy (random)))) 
             => 0.5326103872028469
             (first (second (split-random-lazy (random)))) 
             => 0.20562722505260633
             (map first (take 5 (first (split-random-lazy (random))))) 
             => [0.5326103872028469 0.5779202565232506 0.18462626379965785 
                 0.46312152907066695 0.04609959292577681])
       (def samplef (fn [r] (* 100 r)))
       (def sample-lazy-fn (fn [r] [(* 100 (first r)) (rest r)]))
       (fact "multi sample"
             (first (multi-sample sample-lazy-fn (random) 3)) 
             => [83.35762378570932 11.249249636232017 85.02406979201282]
             (first (multi-sample (with-meta samplef {:r :rnd}) (random) 3)) 
             => [83.35762378570932 11.249249636232017 85.02406979201282]
             (first (multi-sample 
                      (with-meta sample-lazy-fn {:r :rnd-lazy}) (random) 3)) 
             => [83.35762378570932 11.249249636232017 85.02406979201282])
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
             (first (fold-random 1000 + (with-meta samplef {:r :rnd}) 
                                 (random))) 
             => 50653.13952160504
             (defn reducef [tot e] 
               [(+ (first tot) e) (* (second tot) e 0.0285)])
             (defn combinef
               ([] [0.0 1.0])
               ([tot1 tot2] [(+ (first tot1) (first tot2)),
                             (* (second tot1) (second tot2))]))
             (first (fold-random
                      1000 combinef reducef 
                      (with-meta samplef {:r :rnd}) (random))) 
             => [50653.13952160504 39.987434644813305]
             (first (fold-random 
                      300 4 combinef reducef 
                      (with-meta samplef {:r :rnd}) (random)))
             => [58986.355496151344 0.5303202033382911] 
             (first (fold-random 1000 + sample-lazy-fn (random)))
             => 51685.727620165926
             (first (fold-random
                      1000 combinef reducef sample-lazy-fn (random))) 
             => [51685.727620165926 2.6215627010170977E32]
             (first (fold-random
                      300 4 combinef reducef sample-lazy-fn (random))) 
             => [60626.00738574421 6.304745954380607E38]))




