(ns provisdom.test.math.special-functions
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.special-functions :refer :all]
            [provisdom.math.calculus :as ca]
            [provisdom.utility-belt.core :as co]
            [provisdom.math [core :as m]]))

(facts "erf"
       (fact "erf"
       ;;Math 0.999977909503001414558627223870417679620152292912600750342761
             (erf 3.0) => 0.9999779095030014 
       ;;Math 0.842700792949714869341220635082609259296066997966302908459937      
             (erf 1.0) => 0.842700792949715 
       ;;Math 0.428392355046668455103603845320172444121862928522590383495086      
             (erf 0.4) => 0.4283923550466685 
             (erf 0) => 0.0
       ;;Math -0.11246291601828489220327507174396838322169629915970254753449      
             (erf -0.1) => -0.11246291601828491 
             (erf 1.0 1.0) => 0.0
             (erf 1.0 2.0) => 0.15262147206923762
             (erf -1.0 1.0) => 1.6854015858994302
             (erf 0.0 1.0) =>  0.8427007929497151) 
       (fact "erf-derivative"
             (erf-derivative 0.4) => 0.9615412988393078
             (erf-derivative 0) => 1.1283791670955126)
       (fact "erfc"
       ;;Math 0.571607644953331544896396154679827555878137071477409616504913
             (erfc 0.4) => 0.5716076449533315) 
       (fact "normally return the inv-erf" 
             (inv-erf 0.0) => 0.0
       ;;Math 0.5951160814499948500193003601681082534396168862798484
             (inv-erf 0.6) => 0.5951160814499948 
       ;;Math -1.64497635713318705017720343524951162466534303628880
             (inv-erf -0.98) => -1.644976357133187 
       ;;Math 1.6449763571331870501772034352495116246653430362888071
             (inv-erf 0.98) => 1.644976357133187 
       ;;Math 1.3859038243496779452779737236901775216912369580866617
             (inv-erf 0.95) => 1.3859038243496777 
       ;;Math 1.1630871536766740867262542605629475934779325500020816
             (inv-erf 0.9) => 1.1630871536766743 
       ;;Math 0.9061938024368232200711627030956628666508668747462206
             (inv-erf 0.8) => 0.9061938024368233 
       ;;Math 0.3708071585935579290582494775224491386043048831629311
             (inv-erf 0.4) => 0.37080715859355795 
             (inv-erf 1.0) => m/inf+
             (inv-erf -1.0) => m/inf-
             (inv-erf 2.0) => (throws)
             (inv-erf -2.0) => (throws))
       (fact "inv-erfc"
             (inv-erfc 0.6) => 0.37080715859355795
             (inv-erfc 2.0) => m/inf-
             (inv-erfc 0.0) => m/inf+
             (inv-erfc 2.01) => (throws)
             (inv-erfc -0.02) => (throws))
       (fact "inv-cdf-standard-normal" 
             (inv-cdf-standard-normal -0.1) => (throws)
             (inv-cdf-standard-normal 0.0) => m/inf-
             (inv-cdf-standard-normal 0.3) => -0.5244005127080409
             (inv-cdf-standard-normal 0.5) => 0.0
             (inv-cdf-standard-normal 1.0) => m/inf+
             (inv-cdf-standard-normal 1.01) => (throws)))

(facts "gamma functions"
       (fact "gamma"
       ;;Math 0.951350769866873183629248717726540219255057862608837734305000      
             (gamma 1.1) => 0.9513507698668734 
             (gamma 1) => 1.0 ;Math 1.0
             (gamma 0.0) => (throws)
       ;;Math 1.298055332647557785681171179152811617784141170553946247921645      
             (gamma 0.7) => 1.2980553326475581 
       ;;;Math -10.6862870211931935489730533569448077816983878506097317904937 
             (gamma -0.1) => -10.686287021193193) 
       (fact "lower-gamma"
             (lower-gamma 1 1.1) => 0.6346736081242847
             (lower-gamma 1 1) => 0.6321205588285577
             (lower-gamma 1 0) => (throws)
             (lower-gamma 1 -0.1) => (throws)
             (lower-gamma 0.1 1) => 0.09516258196404048
             (lower-gamma -0.1 1) => (throws)
             (lower-gamma 0.0 1) => 0.0)
       (fact "upper-gamma"
             (upper-gamma 1 1.1) => 0.3166771617425887
             (upper-gamma 1 1) => 0.36787944117144233
             (upper-gamma 1 0) => (throws)
             (upper-gamma 1 -0.1) => (throws)
             (upper-gamma 0.1 1) => 0.9048374180359595
             (upper-gamma -0.1 1) => (throws)
             (upper-gamma 0.0 1) => 1.0)
       (fact "upper gamma derivative c"
             (upper-gamma-derivative-c 1 1.1) => -0.36787944117144233
             (upper-gamma-derivative-c 1 1) => -0.36787944117144233
             (upper-gamma-derivative-c 1 0) => (throws)
             (upper-gamma-derivative-c 1 -0.1) => (throws)
             (upper-gamma-derivative-c 0.1 1) => -0.9048374180359595
             (upper-gamma-derivative-c -0.1 1) => (throws)
             (upper-gamma-derivative-c 0.0 1) => -1.0)
       (fact "regularized-gamma-p"
             (regularized-gamma-p 1 1.1) => 0.6671289163019202
             (regularized-gamma-p 1 1) => 0.6321205588285578
             (regularized-gamma-p 1 0) => (throws)
             (regularized-gamma-p 1 -0.1) => (throws)
             (regularized-gamma-p 0.1 1) => 0.9758726562736726
             (regularized-gamma-p -0.1 1) => (throws)
             (regularized-gamma-p 0.0 1) => 0.0)
       (fact "regularized-gamma-q"
             (regularized-gamma-q 1 1.1) => 0.33287108369807983
             (regularized-gamma-q 1 1) => 0.3678794411714422
             (regularized-gamma-q 1 0) => (throws)
             (regularized-gamma-q 1 -0.1) => (throws)
             (regularized-gamma-q 0.1 1) => 0.02412734372632741
             (regularized-gamma-q -0.1 1) => (throws)
             (regularized-gamma-q 0.0 1) => 1.0)
       (fact "log gamma"
             (log-gamma 1.1) => -0.049872441259839764 
             (log-gamma 1) => 0.0 ;0.0
             (log-gamma 0.0) => (throws)
             (log-gamma 0.7) => 0.2608672465316666
             (log-gamma -0.1) => (throws))
       (fact "log gamma derivative"
             (log-gamma-derivative 1.1) => -0.42375494327813756
             (log-gamma-derivative 1) => -0.5772156677920679
             (log-gamma-derivative 0.0) => (throws)
             (log-gamma-derivative 0.7) => -1.2200235564290474
             (log-gamma-derivative -0.1) => (throws))
       (fact "digamma -- same as log gamma derivative"
             (digamma 2.5) => 0.7031566378697297
             (digamma 1.0) => -0.5772156677920679)
       (fact "gamma derivative"
             (gamma-derivative 1.1) => -0.4031395915225494
             (gamma-derivative 1) => -0.5772156677920679
             (gamma-derivative 0.0) => (throws)
             (gamma-derivative 0.7) => -1.583658083378364
             (gamma-derivative -0.1) => (throws))
       (fact "trigamma"
             (trigamma 1.1) => 1.4332991507926893
             (trigamma 1) => 1.6449340668481562
             (trigamma 0.0) => m/inf+
             (trigamma 0.7) => 2.8340491566945474
             (trigamma -0.1) => 101.92253995947712)
       (fact "multivariate gamma"
             (multivariate-gamma 0 2) => (throws)
             (multivariate-gamma 1.1 -1) => (throws)
             (multivariate-gamma 1.1 0) => 1.0
             (multivariate-gamma 1.1 1) => 0.9513507698668734
             (multivariate-gamma 1.1 1.0) => 0.9513507698668734
             (multivariate-gamma 1.1 2) => 2.511113699545877
             (multivariate-gamma 1.1 3) 
             => (test-roughly 75.05107616754486 1e-14))
       (fact "multivariate log-gamma"
             (multivariate-log-gamma 0 2) => (throws)
             (multivariate-log-gamma 1.1 -1) => (throws)
             (multivariate-log-gamma 1.1 0) => 0.0
             (multivariate-log-gamma 1.1 1) => -0.049872441259839764
             (multivariate-log-gamma 1.1 2) => 0.9207263597340951
             (multivariate-log-gamma 1.1 3) => 4.318168897317701))

(facts "beta functions"
       (fact "beta"
             (beta 1 1) => 1.0
             (beta 0 1) => (throws)
             (beta 1 0) => (throws)
             (beta -0.1 1) => -9.999999999999998
             (beta 1 -0.1) => -9.999999999999998
             (beta 1 0.1) => 9.999999999999998
             (beta 1.1 1) => 0.9090909090909091
             (beta 1 1.1) => 0.9090909090909091)
       (fact "log beta"
             (log-beta 1 1) => 0.0
             (log-beta 0 1) => (throws)
             (log-beta 1 0) => (throws)
             (log-beta -0.1 1) => (throws)
             (log-beta 1 -0.1) => (throws)
             (log-beta 1 0.1) => 2.302585092994046
             (log-beta 1.1 1) => -0.09531017980432493
             (log-beta 1 1.1) => -0.09531017980432493)
       (fact "regularized beta"
             (regularized-beta 1 1 1) => 1.0
             (regularized-beta 1 1) => (throws)
             (regularized-beta 1 0) => (throws)
             (regularized-beta 1 -0.1 1) => (throws)
             (regularized-beta 1 1 -0.1) => (throws)
             (regularized-beta 1 1 0.1) => 1.0
             (regularized-beta 1 1.1 1) => 1.0
             (regularized-beta 1 1 1.1) => 1.0
             (regularized-beta 0.5 0.5 0.5) => 0.5000000000000001
             (regularized-beta 0 1 1) => 0.0
             (regularized-beta -0.1 1 1) => (throws)
             (regularized-beta 1.1 1 1) => (throws))
       (fact "incomplete beta"
             (incomplete-beta 1 1 1) => 1.0
             (incomplete-beta 1 1) => (throws)
             (incomplete-beta 1 0) => (throws)
             (incomplete-beta 1 -0.1 1) => (throws)
             (incomplete-beta 1 1 -0.1) => (throws)
             (incomplete-beta 1 1 0.1) => 9.999999999999998
             (incomplete-beta 1 1.1 1) => 0.9090909090909091
             (incomplete-beta 1 1 1.1) => 0.9090909090909091
             (incomplete-beta 0.5 0.5 0.5) => 1.5707963267948968
             (incomplete-beta 0 1 1) => 0.0
             (incomplete-beta -0.1 1 1) => (throws)
             (incomplete-beta 1.1 1 1) => (throws)))