(ns provisdom.test.math.core
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :refer :all]))

;;;REDUNDANCY
;;;exp, log, log10, sqrt, pow, abs, sin, asin, cos, acos, tan, atan, atan2, 
;;;hypot

(facts "type tests"
       (fact "long-able?"
             (long-able? 3.3) => false
             (long-able? 3) => true
             (long-able? 3.0) => true
             (long-able? "A") => false 
             (long-able? 3.4E15) => true
             (long-able? 3.3E30) => false
             (long-able? -3.3E30) => false
             (long-able? nan) => false)
       (fact "long-able+?"
             (long-able+? 3.3) => false
             (long-able+? 0.0) => false
             (long-able+? 1.0) => true
             (long-able+? -1.0) => false
             (long-able+? -3.3) => false)
       (fact "long-able-?"
             (long-able-? 3.3) => false
             (long-able-? 0.0) => false
             (long-able-? 1.0) => false
             (long-able-? -1.0) => true
             (long-able-? -3.3) => false)
       (fact "long-able-non+?"
             (long-able-non+? 3.3) => false
             (long-able-non+? 0.0) => true
             (long-able-non+? 1.0) => false
             (long-able-non+? -1.0) => true
             (long-able-non+? -3.3) => false)
       (fact "long-able-non-?"
             (long-able-non-? 3.3) => false
             (long-able-non-? 0.0) => true
             (long-able-non-? 1.0) => true
             (long-able-non-? -1.0) => false
             (long-able-non-? -3.3) => false)
       (fact "maybe-long-able"
             (maybe-long-able 0.0) => 0
             (maybe-long-able 0) => 0
             (maybe-long-able 23423423.00) => 23423423
             (maybe-long-able 234234324234234234234) => 234234324234234234234
             (maybe-long-able 234234324234234234234.0) 
             => 234234324234234234234.0
             (maybe-long-able inf+) => inf+
             (maybe-long-able inf-) => inf-
             (maybe-long-able nan) => nan?
             (maybe-long-able nil) => nil)
       (fact "int?"
             (int? 3.3) => false
             (int? 3) => true
             (int? 3.0) => false
             (int? "A") => false 
             (int? 3.4E3) => false
             (int? 3.4E15) => false
             (int? 3.3E30) => false
             (int? -3.3E30) => false
             (int? nan) => false)
       (fact "inf+?"
             (inf+? 3.3) => false
             (inf+? "A") => false
             (inf+? inf+) => true
             (inf+? inf-) => false
             (inf+? nan) => false)
       (fact "inf-?"
             (inf-? 3.3) => false
             (inf-? "A") => false
             (inf-? inf+) => false
             (inf-? inf-) => true
             (inf-? nan) => false)
       (fact "inf?"
             (inf? 3.3) => false
             (inf? "A") => false
             (inf? inf+) => true
             (inf? inf-) => true
             (inf? nan) => false)
       (fact "nan?"
             (nan? 3.3) => false
             (nan? "A") => false
             (nan? inf+) => false
             (nan? inf-) => false
             (nan? nan) => true)
       (fact "one?"
             (one? 1) => true
             (one? 1.0) => true
             (one? "A") => false
             (one? -1) => false
             (one? nan) => false)
       (fact "non-?"
             (non-? 0) => true
             (non-? 1) => true
             (non-? -1) => false
             (non-? inf+) => true 
             (non-? inf-) => false
             (non-? nan) => false
             (non-? "A") => false)
       (fact "non+?"
             (non+? 0) => true
             (non+? 1) => false
             (non+? -1) => true
             (non+? inf+) => false 
             (non+? inf-) => true
             (non+? nan) => false
             (non+? "A") => false)
       (fact "prob?"
             (prob? -0.5) => false
             (prob? 0) => true
             (prob? 0.5) => true
             (prob? 1) => true
             (prob? 1.5) => false
             (prob? inf+) => false 
             (prob? inf-) => false 
             (prob? "A") => false
             (prob? nan) => false)
       (fact "open prob?"
             (open-prob? -0.5) => false
             (open-prob? 0) => false
             (open-prob? 0.5) => true
             (open-prob? 1) => false
             (open-prob? 1.5) => false
             (open-prob? inf+) => false 
             (open-prob? inf-) => false
             (open-prob? nan) => false
             (open-prob? "A") => false)
       (fact "corr?"
             (corr? -0.5) => true
             (corr? 0) => true
             (corr? 0.5) => true
             (corr? 1) => true
             (corr? 1.5) => false
             (corr? -1) => true
             (corr? -1.5) => false
             (corr? inf+) => false 
             (corr? inf-) => false 
             (corr? nan) => false
             (corr? "A") => false)
       (fact "open corr?"
             (open-corr? -0.5) => true
             (open-corr? 0) => true
             (open-corr? 0.5) => true
             (open-corr? 1) => false
             (open-corr? 1.5) => false
             (open-corr? -1) => false
             (open-corr? -1.5) => false
             (open-corr? inf+) => false 
             (open-corr? inf-) => false
             (open-corr? nan) => false
             (open-corr? "A") => false))
       
(facts "basic math"
       (fact "rev -- (1 - x)"
             (rev 3) => -2
             (rev -3) => 4
             (rev nan) => nan?
             (rev inf+) => inf-
             (rev inf-) => inf+
             (rev -3.0) => 4.0
             (rev nil) => (throws))
       (fact "sq"
             (sq 3) => 9
             (sq -3) => 9
             (sq nan) => nan?
             (sq inf+) => inf+
             (sq inf-) => inf+
             (sq -3.0) => 9.0
             (sq nil) => (throws))
       (fact "cube"
             (cube 3) => 27
             (cube nan) => nan?
             (cube inf+) => inf+
             (cube inf-) => inf-
             (cube -3) => -27
             (cube -3.0) => -27.0
             (cube nil) => (throws))
       (fact "sgn"
             (sgn 3) => 1
             (sgn -3) => -1
             (sgn nan) => nan?
             (sgn inf+) => 1
             (sgn inf-) => -1
             (sgn 0) => 0
             (sgn 0.0) => 0
             (sgn -3.0) => -1
             (sgn nil) => (throws))
       (fact "log2"
             (log2 3) => 1.5849625007211563
             (log2 0) => inf-
             (log2 inf+) => inf+
             (log2 nan) => nan?
             (log2 -3.0) => nan?
             (log2 1.0) => 0.0
             (log2 0.9) => -0.15200309344504997
             (log2 nil) => (throws))
       (fact "logn"
             (logn 3 3) => 1.0
             (logn 0 3) => inf-
             (logn inf+ 3) => inf+
             (logn -3.0 3) => nan?
             (logn nan 3) => nan?
             (logn 1.0 3) => 0.0
             (logn 0.9 3) => -0.09590327428938458
             (logn nil 3) => (throws)
             (logn 0.9 1) => inf-
             (logn 0.9 0.5) => 0.15200309344504997
             (logn 0.9 0) => 0.0
             (logn 0.9 inf+) => -0.0)
       (fact "abs' -- returns long if possible"
             (abs' -3.3) => 3.3
             (abs' -3) => 3
             (abs' 3e8) => 300000000
             (abs' 0) => 0
             (abs' 0.0) => 0
             (abs' inf+) => inf+
             (abs' inf-) => inf+
             (abs' nan) => nan?
             (abs' nil) => (throws))
       (fact "cbrt"
             (cbrt 0.0) => 0.0
             (cbrt 1.0) => 1.0
             (cbrt -1.0) => -1.0
             (cbrt -8) => -2.0
             (cbrt inf+) => inf+
             (cbrt inf-) => inf-
             (cbrt nan) => nan?
             (cbrt nil) => (throws)))

(facts "trigonometry"
       (fact "asinh -- inverse hyperbolic sine"
             (asinh 0.0) => 0.0
             (asinh 0.5) => 0.48121182505960347
             (asinh -1.0) => -0.8813735870195428
             (asinh 1.0) => 0.8813735870195429
             (asinh -2.0) => -1.4436354751788099 
             (asinh 2.0) => 1.4436354751788103
             (asinh inf+) => inf+
             (asinh inf-) => nan?
             (asinh nan) => nan?
             (asinh nil) => (throws))
       (fact "acosh -- x >= 1"
             (acosh 0.0) => nan?
             (acosh 1.0) => 0.0 
             (acosh 2.0) => 1.3169578969248166
             (acosh inf+) => inf+
             (acosh nan) => nan?
             (acosh nil) => (throws))
       (fact "atanh -- must be a corr"
             (atanh 0.0) => 0.0
             (atanh 0.5) => -0.2027325540540822
             (atanh -1.0) => inf-
             (atanh 1.0) => inf+
             (atanh -2.0) => nan?
             (atanh nan) => nan? 
             (atanh nil) => (throws))) 

(facts "rounding"
       (fact "round"
             (round 0.5) => 1
             ;;returns value if not able to create a long
             (round 23423423423423423234344.5) => 23423423423423423234344.5
             (round -0.5) => 0
             (round -0.5 :type :down) => -1 
             (round -0.5 :type :away) => -1 
             (round -0.5 :type :toward) => 0
             (round 0.5 :type :down) => 0
             (round 0.5 :type :away) => 1
             (round 0.5 :type :toward) => 0
             (round inf+) => inf+
             (round inf-) => inf- 
             (round nan) => nan?
             (round nil) => (throws))
       (fact "floor"
             (floor 0.4) => 0
             ;;returns a double
             (floor 234234234234234234234343242) => 2.3423423423423425E26
             (floor -0.4) => -1
             (floor inf+) => inf+
             (floor inf-) => inf- 
             (floor nan) => nan?
             (floor nil) => (throws))
       (fact "ceil"
             (ceil 0.4) => 1
             ;;returns a double
             (ceil 234234234234234234234343242) => 2.3423423423423425E26
             (ceil -0.4) => 0
             (ceil inf+) => inf+
             (ceil inf-) => inf- 
             (ceil nan) => nan?
             (ceil nil) => (throws))
       (fact "roughly-floor"
             (roughly-floor 0.99 0.02) => 1
             (roughly-floor 0.99 -0.02) => (throws)
             (roughly-floor 0.99 0.005) => 0
             ;;returns double
             (roughly-floor 234234234234234234234343242 0.02) 
             => 2.3423423423423425E26
             (roughly-floor 234234234234234234234343242.99 0.02) 
             => 2.3423423423423425E26
             (roughly-floor -0.01 0.02) => 0
             (roughly-floor inf+ 0.02) => inf+
             (roughly-floor inf- 0.02) => inf- 
             (roughly-floor nan 0.02) => nan?
             (roughly-floor nil 0.02) => (throws))
       (fact "roughly-ceil"
             (roughly-ceil 0.01 0.02) => 0
             (roughly-ceil 0.01 -0.02) => (throws)
             (roughly-ceil 0.01 0.005) => 1
             ;;returns double
             (roughly-ceil 234234234234234234234343242 0.02) 
             => 2.3423423423423425E26
             (roughly-ceil 234234234234234234234343242.01 0.02) 
             => 2.3423423423423425E26
             (roughly-ceil -0.99 0.02) => -1
             (roughly-ceil inf+ 0.02) => inf+
             (roughly-ceil inf- 0.02) => inf- 
             (roughly-ceil nan 0.02) => nan?
             (roughly-ceil nil 0.02) => (throws))
       (fact "roughly?"
             (roughly? 0.01 0.02 0.005) => false
             (roughly? 0.01 0.02 0.01) => true
             (roughly? 0.01 0.02 0.02) => true
             (roughly? 234234234234234234234343242.01 
                       234234234234234234234343242.03 0.03) => true
             ;;within double accuracy
             (roughly? 234234234234234234234343242.01 
                       234234234234234234234343242.03 0.005) => true
             (roughly? inf+ inf+ 0.01) => false
             (roughly? inf- 0.02 inf+) => true
             (roughly? nan 0.02 0.01) => false
             (roughly? nan 0.02 -0.01) => (throws)
             (roughly? nil 0.02 0.01) => (throws))
       (fact "roughly-round?"
             (roughly-round? 0.01 -0.3) => (throws)
             (roughly-round? 0.01 0.02) => true
             (roughly-round? 0.01 0.005) => false
             (roughly-round? 234234234234234234234343242.01 0.03) => true
             ;;within double accuracy
             (roughly-round? 234234234234234234234343242.01 0.005) => true
             (roughly-round? inf+ inf+) => true
             (roughly-round? inf- 0.4) => false
             (roughly-round? nan 0.01) => false
             (roughly-round? nil 0.02) => (throws))
       (fact "roughly-round-non-?"
             (roughly-round-non-? 0 0.02) => true
             (roughly-round-non-? -0.01 0.02) => false
             (roughly-round-non-? 0.01 -0.3) => (throws)
             (roughly-round-non-? 0.01 0.02) => true
             (roughly-round-non-? 0.01 0.005) => false
             (roughly-round-non-? 234234234234234234234343242.01 0.03) => true
             ;;within double accuracy
             (roughly-round-non-? 234234234234234234234343242.01 0.005) 
             => true
             (roughly-round-non-? inf+ inf+) => true
             (roughly-round-non-? inf- inf+) => false
             (roughly-round-non-? inf+ 0.4) => false
             (roughly-round-non-? nan 0.01) => false
             (roughly-round-non-? nil 0.02) => (throws))
       (fact "roughly-round-non+?"
             (roughly-round-non+? 0 0.02) => true
             (roughly-round-non+? 0.01 0.02) => false
             (roughly-round-non+? -0.01 -0.3) => (throws)
             (roughly-round-non+? -0.01 0.02) => true
             (roughly-round-non+? -0.01 0.005) => false
             (roughly-round-non+? -234234234234234234234343242.01 0.03) => true
             ;;within double accuracy
             (roughly-round-non+? -234234234234234234234343242.01 0.005) 
             => true
             (roughly-round-non+? inf+ inf+) => false
             (roughly-round-non+? inf- inf+) => true
             (roughly-round-non+? inf- 0.4) => false
             (roughly-round-non+? nan 0.01) => false
             (roughly-round-non+? nil 0.02) => (throws))
       (fact "roughly-round+?"
             (roughly-round+? 0 0.02) => false
             (roughly-round+? -0.01 0.02) => false
             (roughly-round+? 0.01 -0.3) => (throws)
             (roughly-round+? 0.01 0.02) => true
             (roughly-round+? 0.01 0.005) => false
             (roughly-round+? 234234234234234234234343242.01 0.03) => true
             ;;within double accuracy
             (roughly-round+? 234234234234234234234343242.01 0.005) => true
             (roughly-round+? inf+ inf+) => true
             (roughly-round+? inf- inf+) => false
             (roughly-round+? inf+ 0.4) => false
             (roughly-round+? nan 0.01) => false
             (roughly-round+? nil 0.02) => (throws))
       (fact "roughly-round-?"
             (roughly-round-? 0 0.02) => false
             (roughly-round-? 0.01 0.02) => false
             (roughly-round-? -0.01 -0.3) => (throws)
             (roughly-round-? -0.01 0.02) => true
             (roughly-round-? -0.01 0.005) => false
             (roughly-round-? -234234234234234234234343242.01 0.03) => true
             ;;within double accuracy
             (roughly-round-? -234234234234234234234343242.01 0.005) => true
             (roughly-round-? inf+ inf+) => false
             (roughly-round-? inf- inf+) => true
             (roughly-round-? inf- 0.4) => false
             (roughly-round-? nan 0.01) => false
             (roughly-round-? nil 0.02) => (throws))
       (fact "roughly-non-?"
             (roughly-non-? 0.01 -0.005) => (throws)
             (roughly-non-? -0.01 0.005) => false
             (roughly-non-? -0.02 0.02) => true
             (roughly-non-? 0.01 0.001) => true 
             (roughly-non-? inf+ inf+) => true
             (roughly-non-? inf- inf+) => true
             (roughly-non-? inf- 0.4) => false
             (roughly-non-? inf+ 0.4) => true
             (roughly-non-? nan 0.01) => false
             (roughly-non-? nil 0.02) => (throws))
       (fact "roughly-non+?"
             (roughly-non+? -0.01 -0.005) => (throws)
             (roughly-non+? 0.01 0.005) => false
             (roughly-non+? 0.02 0.02) => true
             (roughly-non+? -0.01 0.001) => true 
             (roughly-non+? inf+ inf+) => true
             (roughly-non+? inf- inf+) => true
             (roughly-non+? inf- 0.4) => true
             (roughly-non+? inf+ 0.4) => false
             (roughly-non+? nan 0.01) => false
             (roughly-non+? nil 0.02) => (throws))
       (fact "roughly-prob?"
             (roughly-prob? -0.01 -0.005) => (throws)
             (roughly-prob? 0.01 0.005) => true
             (roughly-prob? 0.02 0.02) => true
             (roughly-prob? -0.01 0.001) => false
             (roughly-prob? 1.01 0.01) => true
             (roughly-prob? 1.01 0.01) => true
             (roughly-prob? inf+ inf+) => true
             (roughly-prob? inf- inf+) => true
             (roughly-prob? inf- 0.4) => false
             (roughly-prob? inf+ 0.4) => false
             (roughly-prob? nan 0.01) => false
             (roughly-prob? nil 0.02) => (throws))
       (fact "roughly-corr?"
             (roughly-corr? -1.01 -0.005) => (throws)
             (roughly-corr? -1.01 0.005) => false
             (roughly-corr? -1.02 0.02) => true
             (roughly-corr? -1.01 0.001) => false
             (roughly-corr? 1.01 0.01) => true
             (roughly-corr? 1.01 0.01) => true
             (roughly-corr? inf+ inf+) => true
             (roughly-corr? inf- inf+) => true
             (roughly-corr? inf- 0.4) => false
             (roughly-corr? inf+ 0.4) => false
             (roughly-corr? nan 0.01) => false
             (roughly-corr? nil 0.02) => (throws)))

(facts "quotients"
       (fact "quot' -- returns long if possible"
             (quot' 3 2) => 1
             (quot' -3 2) => -1
             (quot' 3 -2) => -1
             (quot' -3 -2) => 1
             (quot' 3.0 2) => 1
             (quot' 3 2.0) => 1
             (quot' 3 2.12) => 1
             (quot' 3E+40 2.12) => 1.4150943396226415E40
             (quot' inf+ 3) => inf+
             (quot' inf- 4) => inf-
             (quot' inf+ -3) => inf-
             (quot' inf- -4) => inf+
             (quot' nan 2) => nan?
             (quot' 3 inf+) => 0
             (quot' 4 inf-) => 0
             (quot' 2 nan) => nan?
             (quot' nil -2) => (throws))
       (fact "mod' -- returns long if possible"
             (mod' 3 2) => 1
             (mod' -3 2) => 1
             (mod' 3 -2) => -1
             (mod' -3 -2) => -1
             (mod' 3.0 2) => 1
             (mod' 3 2.0) => 1
             (mod' 3 2.12) => 0.8799999999999999
             (mod' 3E+40 2.12) => 0
             (mod' inf+ 3) => nan?
             (mod' inf- 4) => nan?
             (mod' nan 2) => nan?
             (mod' 3 inf+) => 3
             (mod' 4 inf-) => inf-
             (mod' -3 inf+) => inf+
             (mod' -4 inf-) => -4
             (mod' 2 nan) => nan?
             (mod' nil -2) => (throws))
       (fact "rem' -- returns long if possible"
             (rem' 3 2) => 1
             (rem' -3 2) => -1
             (rem' 3 -2) => 1
             (rem' -3 -2) => -1
             (rem' 3.0 2) => 1
             (rem' 3 2.0) => 1
             (rem' 3 2.12) => 0.8799999999999999
             (rem' 3E+40 2.12) => 0
             (rem' inf+ 3) => nan?
             (rem' inf- 4) => nan?
             (rem' nan 2) => nan?
             (rem' 3 inf+) => 3
             (rem' 4 inf-) => 4
             (rem' -3 inf+) => -3
             (rem' -4 inf-) => -4
             (rem' 2 nan) => nan?
             (rem' nil -2) => (throws))
       (fact "quot and rem"
             (quot-and-rem 16 4) => [4 0]
             (quot-and-rem 3 2) => [1 1]
             (quot-and-rem -3 2) => [-1 -1]
             (quot-and-rem 3 -2) => [-1 1]
             (quot-and-rem -3 -2) => [1 -1]
             (quot-and-rem 3 4) => [0 3]
             (quot-and-rem -3 4) => [0 -3]
             (quot-and-rem 3 -4) => [0 3]
             (quot-and-rem -3 -4) => [0 -3]
             (quot-and-rem 3.0 2) => [1 1]
             (quot-and-rem 3 2.0) => [1 1]
             (quot-and-rem 3 2.12) => [1 0.8799999999999999]
             (quot-and-rem 3E+40 2.12) => [1.4150943396226415E40 0]
             (quot-and-rem inf+ 3) => (just [inf+ nan?])
             (quot-and-rem inf- 4) => (just [inf- nan?])
             (quot-and-rem nan 2) => (just [nan? nan?])
             (quot-and-rem 3 inf+) => [0 3]
             (quot-and-rem 4 inf-) => [0 4]
             (quot-and-rem -3 inf+) => [0 -3]
             (quot-and-rem -4 inf-) => [0 -4]
             (quot-and-rem 2 nan) => (just [nan? nan?])
             (quot-and-rem nil -2) => (throws))
       (fact "quot and mod -- this is the quotient associated with the mod"
             (quot-and-mod 16 4) => [4 0]
             (quot-and-mod 0 4) => [0 0]
             (quot-and-mod 0 -4) => [0 0]
             (quot-and-mod 4 0) => (throws)
             (quot-and-mod -4 0) => (throws)
             (quot-and-mod 0 0) => (throws)
             (quot-and-mod 3 2) => [1 1]
             (quot-and-mod -3 2) => [-2 1]
             (quot-and-mod 3 -2) => [-2 -1]
             (quot-and-mod -3 -2) => [1 -1]
             (quot-and-mod 3 4) => [0 3]
             (quot-and-mod -3 4) => [-1 1]
             (quot-and-mod 3 -4) => [-1 -1]
             (quot-and-mod -3 -4) => [0 -3]
             (quot-and-mod 3.0 2) => [1 1]
             (quot-and-mod 3 2.0) => [1 1]
             (quot-and-mod 3 2.12) => [1 0.8799999999999999]
             (quot-and-mod 3E+40 2.12) => [1.4150943396226415E40 0]
             (quot-and-mod inf+ 3) => (just [inf+ nan?])
             (quot-and-mod inf- 4) => (just [inf- nan?])
             (quot-and-mod nan 2) => (just [nan? nan?])
             (quot-and-mod 3 inf+) => [0 3]
             (quot-and-mod 4 inf-) => [-1 inf-]
             (quot-and-mod -3 inf+) => [-1 inf+]
             (quot-and-mod -4 inf-) => [0 -4]
             (quot-and-mod 2 nan) => (just [nan? nan?])
             (quot-and-mod nil -2) => (throws)))

(facts "angles"
       (fact "reduce-angle" 
             (reduce-angle 30.4) => 30.4
             (reduce-angle -9.8) => 350.2
             (reduce-angle 478.0) => 118
             (reduce-angle -8399494) => 26
             (reduce-angle nan) => nan? 
             (reduce-angle inf+) => nan?
             (reduce-angle inf-) => nan?
             (reduce-angle nil) => (throws))
       (fact "reduce-radians" 
             (reduce-radians 30.4) => 5.267258771281654
             (reduce-radians two-pi) => 0
             (reduce-radians PI) => PI
             (reduce-radians -8399494) => 0.06552912132908517
             (reduce-radians nan) => nan?
             (reduce-radians inf+) => nan?
             (reduce-radians inf-) => nan?
             (reduce-radians nil) => (throws))
       (fact "radians->angle"
             (radians->angle 0) => 0
             (radians->angle 3.4) => 194.8056503444799
             (radians->angle two-pi) => 0
             (radians->angle -3.4) => 165.1943496555201
             (radians->angle 45) => 58.31007808870436 
             (radians->angle nan) => nan? 
             (radians->angle inf+) => inf+
             (radians->angle inf-) => inf-
             (radians->angle nil) => (throws))
       (fact "angle->radians"
             (angle->radians 0) => 0
             (angle->radians 3.4) => 0.059341194567807204
             (angle->radians inv-two-pi) => 0.002777777777777778
             (angle->radians -3.4) => 6.223844112611779
             (angle->radians 45) => 0.7853981633974483 
             (angle->radians nan) => nan? 
             (angle->radians inf+) => inf+
             (angle->radians inf-) => inf-
             (angle->radians nil) => (throws)))
       

