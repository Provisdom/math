(ns provisdom.math.t-core
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :refer :all]))

(deftest long-able?-test
  (is-not (long-able? 3.3))
  (is (long-able? 3))
  (is (long-able? 3.0))
  (is-not (long-able? "A"))
  (is (long-able? 3.4E15))
  (is-not (long-able? 3.3E30))
  (is-not (long-able? -3.3E30))
  (is-not (long-able? nan)))

(deftest long-able+?-test
  (is-not (long-able+? 3.3))
  (is-not (long-able+? 0.0))
  (is (long-able+? 1.0))
  (is-not (long-able+? -1.0))
  (is-not (long-able+? -3.3)))

(deftest long-able-?-test
  (is-not (long-able-? 3.3))
  (is-not (long-able-? 0.0))
  (is-not (long-able-? 1.0))
  (is (long-able-? -1.0))
  (is-not (long-able-? -3.3)))

(deftest long-able-non+?-test
  (is-not (long-able-non+? 3.3))
  (is (long-able-non+? 0.0))
  (is-not (long-able-non+? 1.0))
  (is (long-able-non+? -1.0))
  (is-not (long-able-non+? -3.3)))

(deftest long-able-non-?-test
  (is-not (long-able-non-? 3.3))
  (is (long-able-non-? 0.0))
  (is (long-able-non-? 1.0))
  (is-not (long-able-non-? -1.0))
  (is-not (long-able-non-? -3.3)))

(deftest maybe-long-able-test
  (is (zero? (maybe-long-able 0.0)))
  (is (zero? (maybe-long-able 0)))
  (is= 23423423 (maybe-long-able 2.3423423E7))
  (is= 234234324234234234234N (maybe-long-able 234234324234234234234N))
  (is= 2.3423432423423423E20 (maybe-long-able 2.3423432423423423E20))
  (is= inf+ (maybe-long-able inf+))
  (is= inf- (maybe-long-able inf-))
  (is (nan? (maybe-long-able nan)))
  (is (nil? (maybe-long-able nil))))

(deftest int?-test
  (is-not (int? 3.3))
  (is (int? 3))
  (is-not (int? 3.0))
  (is-not (int? "A"))
  (is-not (int? 3400.0))
  (is-not (int? 3.4E15))
  (is-not (int? 3.3E30))
  (is-not (int? -3.3E30))
  (is-not (int? nan)))

(deftest inf+?-test
  (is-not (inf+? 3.3))
  (is-not (inf+? "A"))
  (is (inf+? inf+))
  (is-not (inf+? inf-))
  (is-not (inf+? nan)))

(deftest inf-?-test
  (is-not (inf-? 3.3))
  (is-not (inf-? "A"))
  (is-not (inf-? inf+))
  (is (inf-? inf-))
  (is-not (inf-? nan)))

(deftest inf?-test
  (is-not (inf? 3.3))
  (is-not (inf? "A"))
  (is (inf? inf+))
  (is (inf? inf-))
  (is-not (inf? nan)))

(deftest nan?-test
  (is-not (nan? 3.3))
  (is-not (nan? "A"))
  (is-not (nan? inf+))
  (is-not (nan? inf-))
  (is (nan? nan)))

(deftest one?-test
  (is (one? 1))
  (is (one? 1.0))
  (is-not (one? "A"))
  (is-not (one? -1))
  (is-not (one? nan)))

(deftest non-?-test
  (is (non-? 0))
  (is (non-? 1))
  (is-not (non-? -1))
  (is (non-? inf+))
  (is-not (non-? inf-))
  (is-not (non-? nan))
  (is-not (non-? "A")))

(deftest non+?-test
  (is (non+? 0))
  (is-not (non+? 1))
  (is (non+? -1))
  (is-not (non+? inf+))
  (is (non+? inf-))
  (is-not (non+? nan))
  (is-not (non+? "A")))

(deftest prob?-test
  (is-not (prob? -0.5))
  (is (prob? 0))
  (is (prob? 0.5))
  (is (prob? 1))
  (is-not (prob? 1.5))
  (is-not (prob? inf+))
  (is-not (prob? inf-))
  (is-not (prob? "A"))
  (is-not (prob? nan)))

(deftest open-prob?-test
  (is-not (open-prob? -0.5))
  (is-not (open-prob? 0))
  (is (open-prob? 0.5))
  (is-not (open-prob? 1))
  (is-not (open-prob? 1.5))
  (is-not (open-prob? inf+))
  (is-not (open-prob? inf-))
  (is-not (open-prob? nan))
  (is-not (open-prob? "A")))

(deftest corr?-test
  (is (corr? -0.5))
  (is (corr? 0))
  (is (corr? 0.5))
  (is (corr? 1))
  (is-not (corr? 1.5))
  (is (corr? -1))
  (is-not (corr? -1.5))
  (is-not (corr? inf+))
  (is-not (corr? inf-))
  (is-not (corr? nan))
  (is-not (corr? "A")))

(deftest open-corr?-test
  (is (open-corr? -0.5))
  (is (open-corr? 0))
  (is (open-corr? 0.5))
  (is-not (open-corr? 1))
  (is-not (open-corr? 1.5))
  (is-not (open-corr? -1))
  (is-not (open-corr? -1.5))
  (is-not (open-corr? inf+))
  (is-not (open-corr? inf-))
  (is-not (open-corr? nan))
  (is-not (open-corr? "A")))

(deftest type-tests-test
  long-able?-test
  long-able+?-test
  long-able-?-test
  long-able-non+?-test
  long-able-non-?-test
  maybe-long-able-test
  int?-test
  inf+?-test
  inf-?-test
  inf?-test
  nan?-test
  one?-test
  non-?-test
  non+?-test
  prob?-test
  open-prob?-test
  corr?-test
  open-corr?-test)

(deftest rev-1-x-test
  (is= -2 (one- 3))
  (is= 4 (one- -3))
  (is (nan? (one- nan)))
  (is= inf- (one- inf+))
  (is= inf+ (one- inf-))
  (is= 4.0 (one- -3.0))
  (is (thrown? Exception (one- nil))))

(deftest sq-test
  (is= 9 (sq 3))
  (is= 9 (sq -3))
  (is (nan? (sq nan)))
  (is= inf+ (sq inf+))
  (is= inf+ (sq inf-))
  (is= 9.0 (sq -3.0))
  (is (thrown? Exception (sq nil))))

(deftest cube-test
  (is= 27 (cube 3))
  (is (nan? (cube nan)))
  (is= inf+ (cube inf+))
  (is= inf- (cube inf-))
  (is= -27 (cube -3))
  (is= -27.0 (cube -3.0))
  (is (thrown? Exception (cube nil))))

(deftest sgn-test
  (is= 1 (sgn 3))
  (is= -1 (sgn -3))
  (is (nan? (sgn nan)))
  (is= 1 (sgn inf+))
  (is= -1 (sgn inf-))
  (is (zero? (sgn 0)))
  (is (zero? (sgn 0.0)))
  (is= -1 (sgn -3.0))
  (is (thrown? Exception (sgn nil))))

(deftest log2-test
  (is= 1.5849625007211563 (log2 3))
  (is= inf- (log2 0))
  (is= inf+ (log2 inf+))
  (is (nan? (log2 nan)))
  (is (nan? (log2 -3.0)))
  (is= 0.0 (log2 1.0))
  (is= -0.15200309344504997 (log2 0.9))
  (is (thrown? Exception (log2 nil))))

(deftest logn-test
  (is= 1.0 (logn 3 3))
  (is= inf- (logn 0 3))
  (is= inf+ (logn inf+ 3))
  (is (nan? (logn -3.0 3)))
  (is (nan? (logn nan 3)))
  (is= 0.0 (logn 1.0 3))
  (is= -0.09590327428938458 (logn 0.9 3))
  (is (thrown? Exception (logn nil 3)))
  (is= inf- (logn 0.9 1))
  (is= 0.15200309344504997 (logn 0.9 0.5))
  (is= 0.0 (logn 0.9 0))
  (is= -0.0 (logn 0.9 inf+)))

(deftest abs-returns-long-if-possible-test
  (is= 3.3 (abs' -3.3))
  (is= 3 (abs' -3))
  (is= 300000000 (abs' 3.0E8))
  (is (zero? (abs' 0)))
  (is (zero? (abs' 0.0)))
  (is= inf+ (abs' inf+))
  (is= inf+ (abs' inf-))
  (is (nan? (abs' nan)))
  (is (thrown? Exception (abs' nil))))

(deftest cbrt-test
  (is= 0.0 (cbrt 0.0))
  (is= 1.0 (cbrt 1.0))
  (is= -1.0 (cbrt -1.0))
  (is= -2.0 (cbrt -8))
  (is= inf+ (cbrt inf+))
  (is= inf- (cbrt inf-))
  (is (nan? (cbrt nan)))
  (is (thrown? Exception (cbrt nil))))

(deftest basic-math-test
  rev-1-x-test
  sq-test
  cube-test
  sgn-test
  log2-test
  logn-test
  abs-returns-long-if-possible-test
  cbrt-test)

(deftest asinh-inverse-hyperbolic-sine-test
  (is= 0.0 (asinh 0.0))
  (is= 0.48121182505960347 (asinh 0.5))
  (is= -0.8813735870195428 (asinh -1.0))
  (is= 0.8813735870195429 (asinh 1.0))
  (is= -1.4436354751788099 (asinh -2.0))
  (is= 1.4436354751788103 (asinh 2.0))
  (is= inf+ (asinh inf+))
  (is (nan? (asinh inf-)))
  (is (nan? (asinh nan)))
  (is (thrown? Exception (asinh nil))))

(deftest acosh-x->=-1-test
  (is (nan? (acosh 0.0)))
  (is= 0.0 (acosh 1.0))
  (is= 1.3169578969248166 (acosh 2.0))
  (is= inf+ (acosh inf+))
  (is (nan? (acosh nan)))
  (is (thrown? Exception (acosh nil))))

(deftest atanh-must-be-a-corr-test
  (is= 0.0 (atanh 0.0))
  (is= -0.2027325540540822 (atanh 0.5))
  (is= inf- (atanh -1.0))
  (is= inf+ (atanh 1.0))
  (is (nan? (atanh -2.0)))
  (is (nan? (atanh nan)))
  (is (thrown? Exception (atanh nil))))

(deftest trigonometry-test
  asinh-inverse-hyperbolic-sine-test
  acosh-x->=-1-test
  atanh-must-be-a-corr-test)

(deftest round-test
  (is= 1 (round 0.5))
  (is= 2.342342342342342E22 (round 2.342342342342342E22))
  (is (zero? (round -0.5)))
  (is= -1 (round -0.5 :type :down))
  (is= -1 (round -0.5 :type :away))
  (is (zero? (round -0.5 :type :toward)))
  (is (zero? (round 0.5 :type :down)))
  (is= 1 (round 0.5 :type :away))
  (is (zero? (round 0.5 :type :toward)))
  (is= inf+ (round inf+))
  (is= inf- (round inf-))
  (is (nan? (round nan)))
  (is (thrown? Exception (round nil))))

(deftest floor-test
  (is (zero? (floor 0.4)))
  (is= 2.3423423423423425E26 (floor 234234234234234234234343242N))
  (is= -1 (floor -0.4))
  (is= inf+ (floor inf+))
  (is= inf- (floor inf-))
  (is (nan? (floor nan)))
  (is (thrown? Exception (floor nil))))

(deftest ceil-test
  (is= 1 (ceil 0.4))
  (is= 2.3423423423423425E26 (ceil 234234234234234234234343242N))
  (is (zero? (ceil -0.4)))
  (is= inf+ (ceil inf+))
  (is= inf- (ceil inf-))
  (is (nan? (ceil nan)))
  (is (thrown? Exception (ceil nil))))

(deftest roughly-floor-test
  (is= 1 (roughly-floor 0.99 0.02))
  (is (thrown? Exception (roughly-floor 0.99 -0.02)))
  (is (zero? (roughly-floor 0.99 0.005)))
  (is=
    2.3423423423423425E26
    (roughly-floor 234234234234234234234343242N 0.02))
  (is=
    2.3423423423423425E26
    (roughly-floor 2.3423423423423425E26 0.02))
  (is (zero? (roughly-floor -0.01 0.02)))
  (is= inf+ (roughly-floor inf+ 0.02))
  (is= inf- (roughly-floor inf- 0.02))
  (is (nan? (roughly-floor nan 0.02)))
  (is (thrown? Exception (roughly-floor nil 0.02))))

(deftest roughly-ceil-test
  (is (zero? (roughly-ceil 0.01 0.02)))
  (is (thrown? Exception (roughly-ceil 0.01 -0.02)))
  (is= 1 (roughly-ceil 0.01 0.005))
  (is=
    2.3423423423423425E26
    (roughly-ceil 234234234234234234234343242N 0.02))
  (is= 2.3423423423423425E26 (roughly-ceil 2.3423423423423425E26 0.02))
  (is= -1 (roughly-ceil -0.99 0.02))
  (is= inf+ (roughly-ceil inf+ 0.02))
  (is= inf- (roughly-ceil inf- 0.02))
  (is (nan? (roughly-ceil nan 0.02)))
  (is (thrown? Exception (roughly-ceil nil 0.02))))

(deftest roughly?-test
  (is-not (roughly? 0.01 0.02 0.005))
  (is (roughly? 0.01 0.02 0.01))
  (is (roughly? 0.01 0.02 0.02))
  (is (roughly? 2.3423423423423425E26 2.3423423423423425E26 0.03))
  (is (roughly? 2.3423423423423425E26 2.3423423423423425E26 0.005))
  (is-not (roughly? inf+ inf+ 0.01))
  (is (roughly? inf- 0.02 inf+))
  (is-not (roughly? nan 0.02 0.01))
  (is (thrown? Exception (roughly? nan 0.02 -0.01)))
  (is (thrown? Exception (roughly? nil 0.02 0.01))))

(deftest roughly-round?-test
  (is (thrown? Exception (roughly-round? 0.01 -0.3)))
  (is (roughly-round? 0.01 0.02))
  (is-not (roughly-round? 0.01 0.005))
  (is (roughly-round? 2.3423423423423425E26 0.03))
  (is (roughly-round? 2.3423423423423425E26 0.005))
  (is (roughly-round? inf+ inf+))
  (is-not (roughly-round? inf- 0.4))
  (is-not (roughly-round? nan 0.01))
  (is (thrown? Exception (roughly-round? nil 0.02))))

(deftest roughly-round-non-?-test
  (is (roughly-round-non-? 0 0.02))
  (is-not (roughly-round-non-? -0.01 0.02))
  (is (thrown? Exception (roughly-round-non-? 0.01 -0.3)))
  (is (roughly-round-non-? 0.01 0.02))
  (is-not (roughly-round-non-? 0.01 0.005))
  (is (roughly-round-non-? 2.3423423423423425E26 0.03))
  (is (roughly-round-non-? 2.3423423423423425E26 0.005))
  (is (roughly-round-non-? inf+ inf+))
  (is-not (roughly-round-non-? inf- inf+))
  (is-not (roughly-round-non-? inf+ 0.4))
  (is-not (roughly-round-non-? nan 0.01))
  (is (thrown? Exception (roughly-round-non-? nil 0.02))))

(deftest roughly-round-non+?-test
  (is (roughly-round-non+? 0 0.02))
  (is-not (roughly-round-non+? 0.01 0.02))
  (is (thrown? Exception (roughly-round-non+? -0.01 -0.3)))
  (is (roughly-round-non+? -0.01 0.02))
  (is-not (roughly-round-non+? -0.01 0.005))
  (is (roughly-round-non+? -2.3423423423423425E26 0.03))
  (is (roughly-round-non+? -2.3423423423423425E26 0.005))
  (is-not (roughly-round-non+? inf+ inf+))
  (is (roughly-round-non+? inf- inf+))
  (is-not (roughly-round-non+? inf- 0.4))
  (is-not (roughly-round-non+? nan 0.01))
  (is (thrown? Exception (roughly-round-non+? nil 0.02))))

(deftest roughly-round+?-test
  (is-not (roughly-round+? 0 0.02))
  (is-not (roughly-round+? -0.01 0.02))
  (is (thrown? Exception (roughly-round+? 0.01 -0.3)))
  (is (roughly-round+? 0.01 0.02))
  (is-not (roughly-round+? 0.01 0.005))
  (is (roughly-round+? 2.3423423423423425E26 0.03))
  (is (roughly-round+? 2.3423423423423425E26 0.005))
  (is (roughly-round+? inf+ inf+))
  (is-not (roughly-round+? inf- inf+))
  (is-not (roughly-round+? inf+ 0.4))
  (is-not (roughly-round+? nan 0.01))
  (is (thrown? Exception (roughly-round+? nil 0.02))))

(deftest roughly-round-?-test
  (is-not (roughly-round-? 0 0.02))
  (is-not (roughly-round-? 0.01 0.02))
  (is (thrown? Exception (roughly-round-? -0.01 -0.3)))
  (is (roughly-round-? -0.01 0.02))
  (is-not (roughly-round-? -0.01 0.005))
  (is (roughly-round-? -2.3423423423423425E26 0.03))
  (is (roughly-round-? -2.3423423423423425E26 0.005))
  (is-not (roughly-round-? inf+ inf+))
  (is (roughly-round-? inf- inf+))
  (is-not (roughly-round-? inf- 0.4))
  (is-not (roughly-round-? nan 0.01))
  (is (thrown? Exception (roughly-round-? nil 0.02))))

(deftest roughly-non-?-test
  (is (thrown? Exception (roughly-non-? 0.01 -0.005)))
  (is-not (roughly-non-? -0.01 0.005))
  (is (roughly-non-? -0.02 0.02))
  (is (roughly-non-? 0.01 0.001))
  (is (roughly-non-? inf+ inf+))
  (is (roughly-non-? inf- inf+))
  (is-not (roughly-non-? inf- 0.4))
  (is (roughly-non-? inf+ 0.4))
  (is-not (roughly-non-? nan 0.01))
  (is (thrown? Exception (roughly-non-? nil 0.02))))

(deftest roughly-non+?-test
  (is (thrown? Exception (roughly-non+? -0.01 -0.005)))
  (is-not (roughly-non+? 0.01 0.005))
  (is (roughly-non+? 0.02 0.02))
  (is (roughly-non+? -0.01 0.001))
  (is (roughly-non+? inf+ inf+))
  (is (roughly-non+? inf- inf+))
  (is (roughly-non+? inf- 0.4))
  (is-not (roughly-non+? inf+ 0.4))
  (is-not (roughly-non+? nan 0.01))
  (is (thrown? Exception (roughly-non+? nil 0.02))))

(deftest roughly-prob?-test
  (is (thrown? Exception (roughly-prob? -0.01 -0.005)))
  (is (roughly-prob? 0.01 0.005))
  (is (roughly-prob? 0.02 0.02))
  (is-not (roughly-prob? -0.01 0.001))
  (is (roughly-prob? 1.01 0.01))
  (is (roughly-prob? 1.01 0.01))
  (is (roughly-prob? inf+ inf+))
  (is (roughly-prob? inf- inf+))
  (is-not (roughly-prob? inf- 0.4))
  (is-not (roughly-prob? inf+ 0.4))
  (is-not (roughly-prob? nan 0.01))
  (is (thrown? Exception (roughly-prob? nil 0.02))))

(deftest roughly-corr?-test
  (is (thrown? Exception (roughly-corr? -1.01 -0.005)))
  (is-not (roughly-corr? -1.01 0.005))
  (is (roughly-corr? -1.02 0.02))
  (is-not (roughly-corr? -1.01 0.001))
  (is (roughly-corr? 1.01 0.01))
  (is (roughly-corr? 1.01 0.01))
  (is (roughly-corr? inf+ inf+))
  (is (roughly-corr? inf- inf+))
  (is-not (roughly-corr? inf- 0.4))
  (is-not (roughly-corr? inf+ 0.4))
  (is-not (roughly-corr? nan 0.01))
  (is (thrown? Exception (roughly-corr? nil 0.02))))

(deftest rounding-test
  round-test
  floor-test
  ceil-test
  roughly-floor-test
  roughly-ceil-test
  roughly?-test
  roughly-round?-test
  roughly-round-non-?-test
  roughly-round-non+?-test
  roughly-round+?-test
  roughly-round-?-test
  roughly-non-?-test
  roughly-non+?-test
  roughly-prob?-test
  roughly-corr?-test)

(deftest quot-returns-long-if-possible-test
  (is= 1 (quot' 3 2))
  (is= -1 (quot' -3 2))
  (is= -1 (quot' 3 -2))
  (is= 1 (quot' -3 -2))
  (is= 1 (quot' 3.0 2))
  (is= 1 (quot' 3 2.0))
  (is= 1 (quot' 3 2.12))
  (is= 1.4150943396226415E40 (quot' 3.0E40 2.12))
  (is= inf+ (quot' inf+ 3))
  (is= inf- (quot' inf- 4))
  (is= inf- (quot' inf+ -3))
  (is= inf+ (quot' inf- -4))
  (is (nan? (quot' nan 2)))
  (is (zero? (quot' 3 inf+)))
  (is (zero? (quot' 4 inf-)))
  (is (nan? (quot' 2 nan)))
  (is (thrown? Exception (quot' nil -2))))

(deftest mod-returns-long-if-possible-test
  (is= 1 (mod' 3 2))
  (is= 1 (mod' -3 2))
  (is= -1 (mod' 3 -2))
  (is= -1 (mod' -3 -2))
  (is= 1 (mod' 3.0 2))
  (is= 1 (mod' 3 2.0))
  (is= 0.8799999999999999 (mod' 3 2.12))
  (is (zero? (mod' 3.0E40 2.12)))
  (is (nan? (mod' inf+ 3)))
  (is (nan? (mod' inf- 4)))
  (is (nan? (mod' nan 2)))
  (is= 3 (mod' 3 inf+))
  (is= inf- (mod' 4 inf-))
  (is= inf+ (mod' -3 inf+))
  (is= -4 (mod' -4 inf-))
  (is (nan? (mod' 2 nan)))
  (is (thrown? Exception (mod' nil -2))))

(deftest rem-returns-long-if-possible-test
  (is= 1 (rem' 3 2))
  (is= -1 (rem' -3 2))
  (is= 1 (rem' 3 -2))
  (is= -1 (rem' -3 -2))
  (is= 1 (rem' 3.0 2))
  (is= 1 (rem' 3 2.0))
  (is= 0.8799999999999999 (rem' 3 2.12))
  (is (zero? (rem' 3.0E40 2.12)))
  (is (nan? (rem' inf+ 3)))
  (is (nan? (rem' inf- 4)))
  (is (nan? (rem' nan 2)))
  (is= 3 (rem' 3 inf+))
  (is= 4 (rem' 4 inf-))
  (is= -3 (rem' -3 inf+))
  (is= -4 (rem' -4 inf-))
  (is (nan? (rem' 2 nan)))
  (is (thrown? Exception (rem' nil -2))))

(deftest quot-and-rem-test
  (is= [4 0] (quot-and-rem 16 4))
  (is= [1 1] (quot-and-rem 3 2))
  (is= [-1 -1] (quot-and-rem -3 2))
  (is= [-1 1] (quot-and-rem 3 -2))
  (is= [1 -1] (quot-and-rem -3 -2))
  (is= [0 3] (quot-and-rem 3 4))
  (is= [0 -3] (quot-and-rem -3 4))
  (is= [0 3] (quot-and-rem 3 -4))
  (is= [0 -3] (quot-and-rem -3 -4))
  (is= [1 1] (quot-and-rem 3.0 2))
  (is= [1 1] (quot-and-rem 3 2.0))
  (is= [1 0.8799999999999999] (quot-and-rem 3 2.12))
  (is= [1.4150943396226415E40 0] (quot-and-rem 3.0E40 2.12))
  (is (just [inf+ nan?] (quot-and-rem inf+ 3)))
  (is (just [inf- nan?] (quot-and-rem inf- 4)))
  (is (just [nan? nan?] (quot-and-rem nan 2)))
  (is= [0 3] (quot-and-rem 3 inf+))
  (is= [0 4] (quot-and-rem 4 inf-))
  (is= [0 -3] (quot-and-rem -3 inf+))
  (is= [0 -4] (quot-and-rem -4 inf-))
  (is (just [nan? nan?] (quot-and-rem 2 nan)))
  (is (thrown? Exception (quot-and-rem nil -2))))

(deftest quot-and-mod-this-is-the-quotient-associated-with-the-mod-test
  (is= [4 0] (quot-and-mod 16 4))
  (is= [0 0] (quot-and-mod 0 4))
  (is= [0 0] (quot-and-mod 0 -4))
  (is (thrown? Exception (quot-and-mod 4 0)))
  (is (thrown? Exception (quot-and-mod -4 0)))
  (is (thrown? Exception (quot-and-mod 0 0)))
  (is= [1 1] (quot-and-mod 3 2))
  (is= [-2 1] (quot-and-mod -3 2))
  (is= [-2 -1] (quot-and-mod 3 -2))
  (is= [1 -1] (quot-and-mod -3 -2))
  (is= [0 3] (quot-and-mod 3 4))
  (is= [-1 1] (quot-and-mod -3 4))
  (is= [-1 -1] (quot-and-mod 3 -4))
  (is= [0 -3] (quot-and-mod -3 -4))
  (is= [1 1] (quot-and-mod 3.0 2))
  (is= [1 1] (quot-and-mod 3 2.0))
  (is= [1 0.8799999999999999] (quot-and-mod 3 2.12))
  (is= [1.4150943396226415E40 0] (quot-and-mod 3.0E40 2.12))
  (is (just [inf+ nan?] (quot-and-mod inf+ 3)))
  (is (just [inf- nan?] (quot-and-mod inf- 4)))
  (is (just [nan? nan?] (quot-and-mod nan 2)))
  (is= [0 3] (quot-and-mod 3 inf+))
  (is= [-1 inf-] (quot-and-mod 4 inf-))
  (is= [-1 inf+] (quot-and-mod -3 inf+))
  (is= [0 -4] (quot-and-mod -4 inf-))
  (is (just [nan? nan?] (quot-and-mod 2 nan)))
  (is (thrown? Exception (quot-and-mod nil -2))))

(deftest quotients-test
  quot-returns-long-if-possible-test
  mod-returns-long-if-possible-test
  rem-returns-long-if-possible-test
  quot-and-rem-test
  quot-and-mod-this-is-the-quotient-associated-with-the-mod-test)

(deftest reduce-angle-test
  (is= 30.4 (reduce-angle 30.4))
  (is= 350.2 (reduce-angle -9.8))
  (is= 118 (reduce-angle 478.0))
  (is= 26 (reduce-angle -8399494))
  (is (nan? (reduce-angle nan)))
  (is (nan? (reduce-angle inf+)))
  (is (nan? (reduce-angle inf-)))
  (is (thrown? Exception (reduce-angle nil))))

(deftest reduce-radians-test
  (is= 5.267258771281654 (reduce-radians 30.4))
  (is (zero? (reduce-radians two-pi)))
  (is= PI (reduce-radians PI))
  (is= 0.06552912132908517 (reduce-radians -8399494))
  (is (nan? (reduce-radians nan)))
  (is (nan? (reduce-radians inf+)))
  (is (nan? (reduce-radians inf-)))
  (is (thrown? Exception (reduce-radians nil))))

(deftest radians->angle-test
  (is (zero? (radians->angle 0)))
  (is= 194.8056503444799 (radians->angle 3.4))
  (is (zero? (radians->angle two-pi)))
  (is= 165.1943496555201 (radians->angle -3.4))
  (is= 58.31007808870436 (radians->angle 45))
  (is (nan? (radians->angle nan)))
  (is= inf+ (radians->angle inf+))
  (is= inf- (radians->angle inf-))
  (is (thrown? Exception (radians->angle nil))))

(deftest angle->radians-test
  (is (zero? (angle->radians 0)))
  (is= 0.059341194567807204 (angle->radians 3.4))
  (is= 0.002777777777777778 (angle->radians inv-two-pi))
  (is= 6.223844112611779 (angle->radians -3.4))
  (is= 0.7853981633974483 (angle->radians 45))
  (is (nan? (angle->radians nan)))
  (is= inf+ (angle->radians inf+))
  (is= inf- (angle->radians inf-))
  (is (thrown? Exception (angle->radians nil))))

(deftest angles-test
  reduce-angle-test
  reduce-radians-test
  radians->angle-test
  angle->radians-test)

