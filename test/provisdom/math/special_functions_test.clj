(ns provisdom.math.special-functions-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.special-functions :as special-fns]))

;;9 seconds

(set! *warn-on-reflection* true)

;;;LOG-SUM-EXP
(deftest log-sum-exp-test
  (t/with-instrument `special-fns/log-sum-exp
    (t/is (t/spec-check special-fns/log-sum-exp)))
  (t/with-instrument :all
    (t/is= -1199.9999546011009 (special-fns/log-sum-exp [-1200.0 -1210.0]))
    (t/is= 1210.0000453988991 (special-fns/log-sum-exp [1200.0 1210.0]))
    (t/is= 1210.0 (special-fns/log-sum-exp [-1200.0 1210.0]))))

;;;ERROR FUNCTIONS
(deftest erf-test
  (t/with-instrument `special-fns/erf
    (t/is (t/spec-check special-fns/erf)))
  (t/with-instrument :all
    (t/is= -1.0 (special-fns/erf m/inf-))
    (t/is= 1.0 (special-fns/erf m/inf+))
    (t/is= 0.0 (special-fns/erf 0.0))
    (t/is= -0.997020533343667 (special-fns/erf -2.1))
    ;;Math 0.842700792949714869341220635082609259296066997966302908459937
    (t/is= 0.842700792949715 (special-fns/erf 1.0))
    ;;Math 0.999977909503001414558627223870417679620152292912600750342761
    (t/is= 0.9999779095030014 (special-fns/erf 3.0))
    ;;Math 0.428392355046668455103603845320172444121862928522590383495086
    (t/is= 0.4283923550466685 (special-fns/erf 0.4))
    ;;Math -0.11246291601828489220327507174396838322169629915970254753449
    (t/is= -0.11246291601828491 (special-fns/erf -0.1))))

(deftest erf-diff-test
  (t/with-instrument `special-fns/erf-diff
    (t/is (t/spec-check special-fns/erf-diff)))
  (t/with-instrument :all
    (t/is= 2.0 (special-fns/erf-diff m/inf- m/inf+))
    (t/is= 0.0 (special-fns/erf-diff m/inf+ m/inf+))
    (t/is= -1.0 (special-fns/erf-diff m/inf+ 0.0))
    (t/is= 1.839721326293382 (special-fns/erf-diff -2.1 1.0))
    (t/is= -1.8380230579686678 (special-fns/erf-diff 1.0 -2.0))
    (t/is= 0.0 (special-fns/erf-diff 1.0 1.0))
    (t/is= 0.1526214720692377 (special-fns/erf-diff 1.0 2.0))
    (t/is= 1.68540158589943 (special-fns/erf-diff -1.0 1.0))
    (t/is= 0.842700792949715 (special-fns/erf-diff 0.0 1.0))))

(deftest erf-derivative-test
  (t/with-instrument `special-fns/erf-derivative
    (t/is (t/spec-check special-fns/erf-derivative)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/erf-derivative m/inf-))
    (t/is= 0.0 (special-fns/erf-derivative m/inf+))
    (t/is= 1.1283791670955126 (special-fns/erf-derivative 0.0))
    (t/is= 0.4151074974205947 (special-fns/erf-derivative 1.0))
    (t/is= 0.013715649999806838 (special-fns/erf-derivative -2.1))
    (t/is= 0.9615412988393078 (special-fns/erf-derivative 0.4))))

(deftest erfc-test
  (t/with-instrument `special-fns/erfc
    (t/is (t/spec-check special-fns/erfc)))
  (t/with-instrument :all
    (t/is= 2.0 (special-fns/erfc m/inf-))
    (t/is= 0.0 (special-fns/erfc m/inf+))
    (t/is= 1.0 (special-fns/erfc 0.0))
    (t/is= 0.157299207050285 (special-fns/erfc 1.0))
    (t/is= 1.997020533343667 (special-fns/erfc -2.1))
    ;;Math 0.571607644953331544896396154679827555878137071477409616504913
    (t/is= 0.5716076449533315 (special-fns/erfc 0.4))))

(deftest inv-erf-test
  (t/with-instrument `special-fns/inv-erf
    (t/is (t/spec-check special-fns/inv-erf)))
  (t/with-instrument :all
    (t/is= m/inf- (special-fns/inv-erf -1.0))
    (t/is= m/inf+ (special-fns/inv-erf 1.0))
    (t/is= 0.0 (special-fns/inv-erf 0.0))
    (t/is= 1.0000000000000002 (special-fns/inv-erf 0.842700792949715))
    (t/is= -2.100000000000001 (special-fns/inv-erf -0.997020533343667))
    ;;Math 0.5951160814499948500193003601681082534396168862798484
    (t/is= 0.5951160814499948 (special-fns/inv-erf 0.6))
    ;;Math -1.64497635713318705017720343524951162466534303628880
    (t/is= -1.644976357133187 (special-fns/inv-erf -0.98))
    ;;Math 1.6449763571331870501772034352495116246653430362888071
    (t/is= 1.644976357133187 (special-fns/inv-erf 0.98))
    ;;Math 1.3859038243496779452779737236901775216912369580866617
    (t/is= 1.3859038243496777 (special-fns/inv-erf 0.95))
    ;;Math 1.1630871536766740867262542605629475934779325500020816
    (t/is= 1.1630871536766743 (special-fns/inv-erf 0.9))
    ;;Math 0.9061938024368232200711627030956628666508668747462206
    (t/is= 0.9061938024368233 (special-fns/inv-erf 0.8))
    ;;Math 0.3708071585935579290582494775224491386043048831629311
    (t/is= 0.37080715859355795 (special-fns/inv-erf 0.4))))

(deftest inv-erfc-test
  (t/with-instrument `special-fns/inv-erfc
    (t/is (t/spec-check special-fns/inv-erfc)))
  (t/with-instrument :all
    (t/is= m/inf- (special-fns/inv-erfc 2.0))
    (t/is= m/inf+ (special-fns/inv-erfc 0.0))
    (t/is= 0.37080715859355795 (special-fns/inv-erfc 0.6))
    (t/is= 0.0 (special-fns/inv-erfc 1.0))
    (t/is= 1.0000000000000002 (special-fns/inv-erfc 0.157299207050285))
    (t/is= -2.100000000000001 (special-fns/inv-erfc 1.997020533343667))))

;;;SIGMOID FUNCTIONS
(deftest inv-cdf-standard-normal-test
  (t/with-instrument `special-fns/inv-cdf-standard-normal
    (t/is (t/spec-check special-fns/inv-cdf-standard-normal)))
  (t/with-instrument :all
    (t/is= m/inf- (special-fns/inv-cdf-standard-normal 0.0))
    (t/is= -0.5244005127080409 (special-fns/inv-cdf-standard-normal 0.3))
    (t/is= 0.0 (special-fns/inv-cdf-standard-normal 0.5))
    (t/is= m/inf+ (special-fns/inv-cdf-standard-normal 1.0))
    (t/is= -1.0056199694085204 (special-fns/inv-cdf-standard-normal 0.157299207050285))
    (t/is= 2.750032615602772 (special-fns/inv-cdf-standard-normal 0.997020533343667))))

(deftest cdf-standard-normal-test
  (t/with-instrument `special-fns/cdf-standard-normal
    (t/is (t/spec-check special-fns/cdf-standard-normal)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/cdf-standard-normal m/inf-))
    (t/is= 0.5 (special-fns/cdf-standard-normal 0.0))
    (t/is= 1.0 (special-fns/cdf-standard-normal m/inf+))
    (t/is= 0.15729920705028516 (special-fns/cdf-standard-normal -1.0056199694085204))
    (t/is= 0.997020533343667 (special-fns/cdf-standard-normal 2.750032615602772))))

(deftest logistic-test
  (t/with-instrument `special-fns/logistic
    (t/is (t/spec-check special-fns/logistic)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/logistic m/inf-))
    (t/is= 1.0 (special-fns/logistic m/inf+))
    (t/is= 0.5 (special-fns/logistic 0.0))
    (t/is= 0.10909682119561293 (special-fns/logistic -2.1))))

(deftest logistic-derivative-test
  (t/with-instrument `special-fns/logistic-derivative
    (t/is (t/spec-check special-fns/logistic-derivative)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/logistic-derivative m/inf-))
    (t/is= 0.0 (special-fns/logistic-derivative m/inf+))
    (t/is= 0.25 (special-fns/logistic-derivative 0.0))
    (t/is= 0.09719470480062539 (special-fns/logistic-derivative -2.1))))

(deftest logit-test
  (t/with-instrument `special-fns/logit
    (t/is (t/spec-check special-fns/logit)))
  (t/with-instrument :all
    (t/is= m/inf- (special-fns/logit 0.0))
    (t/is= m/inf+ (special-fns/logit 1.0))
    (t/is= -0.4054651081081643 (special-fns/logit 0.4))
    (t/is= 0.0 (special-fns/logit 0.5))))

(deftest logit-derivative-test
  (t/with-instrument `special-fns/logistic-derivative
    (t/is (t/spec-check special-fns/logistic-derivative)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/logit-derivative 0.0))
    (t/is= m/inf+ (special-fns/logit-derivative 1.0))
    (t/is= 4.166666666666667 (special-fns/logit-derivative 0.4))
    (t/is= 4.0 (special-fns/logit-derivative 0.5))))

;;;GAMMA
(deftest gamma-test
  (t/with-instrument `special-fns/gamma
    (t/is (t/spec-check special-fns/gamma)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/gamma m/inf-))
    (t/is= m/inf+ (special-fns/gamma m/inf+))
    (t/is= 9.51350769866873 (special-fns/gamma 0.1))
    (t/is= 1.0 (special-fns/gamma 1.0))
    (t/is= -4.626098277572806 (special-fns/gamma -2.1))
    ;;Math 0.951350769866873183629248717726540219255057862608837734305000
    (t/is= 0.9513507698668731 (special-fns/gamma 1.1))
    ;;Math 1.298055332647557785681171179152811617784141170553946247921645
    (t/is= 1.298055332647558 (special-fns/gamma 0.7))
    ;;;Math -10.6862870211931935489730533569448077816983878506097317904937
    (t/is= -10.686287021193191 (special-fns/gamma -0.1))
    #_(t/is= -10.686287021193193 (Gamma/gamma -0.1))        ;;Gamma was Apache
    (t/is= 9.332621544394415E155 (special-fns/gamma 100.0))
    #_(t/is= 9.332621544394412E155 (Gamma/gamma 100.0))
    (t/is= 7.050810550405615E242 (special-fns/gamma 141.8))
    (t/is= 7.257415615308058E306 (special-fns/gamma 171.0))
    (t/is= m/inf+ (special-fns/gamma 172.0))
    #_(t/is= m/inf+ (Gamma/gamma 141.8))
    (t/is= 4.297739720709703E242 (special-fns/gamma 141.7))
    #_(t/is= 4.297739720709703E242 (Gamma/gamma 141.7))
    (t/is= -6.3764985844038365E-245 (special-fns/gamma -141.7))
    #_(t/is= 6.3764985844038365E-245 (Gamma/gamma -141.7))))

(deftest lower-gamma-test
  (t/with-instrument `special-fns/lower-gamma
    (t/is (t/spec-check special-fns/lower-gamma)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/lower-gamma m/inf+ 0.0))
    (t/is= 0.0 (special-fns/lower-gamma 0.1 0.0))
    (t/is= 0.8775435717470181 (special-fns/lower-gamma 1.0 2.1))
    (t/is= 9.51350769866873 (special-fns/lower-gamma 0.1 m/inf+))
    (t/is= 0.6671289163019205 (special-fns/lower-gamma 1 1.1))
    (t/is= 0.6321205588285577 (special-fns/lower-gamma 1 1))
    (t/is= 0.0 (special-fns/lower-gamma 1 0))
    (t/is= 9.283972028379884 (special-fns/lower-gamma 0.1 1))
    (t/is (m/nan? (special-fns/lower-gamma 1e150 1e150)))
    (t/is= 1.0 (special-fns/lower-gamma 1 1e150))
    (t/is (m/nan? (special-fns/lower-gamma 1e300 1e150)))))

(deftest upper-gamma-test
  (t/with-instrument `special-fns/upper-gamma
    (t/is (t/spec-check special-fns/upper-gamma)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/upper-gamma m/inf+ 0.0))
    (t/is= 9.51350769866873 (special-fns/upper-gamma 0.1 0.0))
    (t/is= 0.1224564282529819 (special-fns/upper-gamma 1.0 2.1))
    (t/is (m/nan? (special-fns/upper-gamma 0.1 m/inf+)))
    (t/is= 0.33287108369807955 (special-fns/upper-gamma 1 1.1))
    (t/is= 0.36787944117144233 (special-fns/upper-gamma 1 1))
    (t/is= 1.0 (special-fns/upper-gamma 1 0))
    (t/is= 0.22953567028884675 (special-fns/upper-gamma 0.1 1))))

(deftest upper-gamma-derivative-x-test
  (t/with-instrument `special-fns/upper-gamma-derivative-x
    (t/is (t/spec-check special-fns/upper-gamma-derivative-x)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/upper-gamma-derivative-x m/inf+ 0.0))
    (t/is (m/nan? (special-fns/upper-gamma-derivative-x m/inf+ m/inf+)))
    (t/is (m/nan? (special-fns/upper-gamma-derivative-x m/inf+ 1.0)))
    (t/is= m/inf+ (special-fns/upper-gamma-derivative-x 0.1 0.0))
    (t/is= 0.1224564282529819 (special-fns/upper-gamma-derivative-x 1.0 2.1))
    (t/is= 0.0 (special-fns/upper-gamma-derivative-x 0.1 m/inf+))
    (t/is= 0.33287108369807955 (special-fns/upper-gamma-derivative-x 1 1.1))
    (t/is= 0.36787944117144233 (special-fns/upper-gamma-derivative-x 1 1))
    (t/is= 1.0 (special-fns/upper-gamma-derivative-x 1 0))
    (t/is= 0.03866916944030238 (special-fns/upper-gamma-derivative-x 0.1 1))))

(deftest regularized-gamma-p-test
  (t/with-instrument `special-fns/regularized-gamma-p
    (t/is (t/spec-check special-fns/regularized-gamma-p)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/regularized-gamma-p m/inf+ 0.0))
    (t/is= 0.0 (special-fns/regularized-gamma-p 0.1 0.0))
    (t/is= 0.8775435717470181 (special-fns/regularized-gamma-p 1.0 2.1))
    (t/is (m/nan? (special-fns/regularized-gamma-p 0.1 m/inf+)))
    (t/is= 0.6671289163019202 (special-fns/regularized-gamma-p 1 1.1))
    (t/is= 0.6321205588285578 (special-fns/regularized-gamma-p 1 1))
    (t/is= 0.0 (special-fns/regularized-gamma-p 1 0))
    (t/is= 0.9758726562736721 (special-fns/regularized-gamma-p 0.1 1))
    #_(t/is= 0.9758726562736726 (Gamma/regularizedGammaP 0.1 1))
    (t/is= 0.5000049341877514 (special-fns/regularized-gamma-p 1e10 1e10))
    #_(t/is= 0.5000049341877536 (Gamma/regularizedGammaP 1e10 1e10))
    (t/is (m/nan? (special-fns/regularized-gamma-p 1e149 1e149)))))

(deftest regularized-gamma-q-test
  (t/with-instrument `special-fns/regularized-gamma-q
    (t/is (t/spec-check special-fns/regularized-gamma-q)))
  (t/with-instrument :all
    (t/is= 1.0 (special-fns/regularized-gamma-q m/inf+ 0.0))
    (t/is= 1.0 (special-fns/regularized-gamma-q 0.1 0.0))
    (t/is= 0.1224564282529819 (special-fns/regularized-gamma-q 1.0 2.1))
    ;;;Math 2.20904969985854413727761295823203798477070873992E-5
    (t/is= 2.209049699858544E-5 (special-fns/regularized-gamma-q 0.5 9.0))
    (t/is (m/nan? (special-fns/regularized-gamma-q 0.1 m/inf+)))
    (t/is= 0.33287108369807983 (special-fns/regularized-gamma-q 1 1.1))
    (t/is= 0.3678794411714422 (special-fns/regularized-gamma-q 1 1))
    (t/is= 1.0 (special-fns/regularized-gamma-q 1 0))
    (t/is= 0.024127343726327855 (special-fns/regularized-gamma-q 0.1 1))
    #_(t/is= 0.02412734372632741 (Gamma/regularizedGammaQ 0.1 1))
    (t/is= 0.4999950658122486 (special-fns/regularized-gamma-q 1e10 1e10))
    #_(t/is= 0.4999950658122464 (Gamma/regularizedGammaQ 1e10 1e10))
    (t/is (m/nan? (special-fns/regularized-gamma-q 1e149 1e149)))))

(deftest log-gamma-test
  (t/with-instrument `special-fns/log-gamma
    (t/is (t/spec-check special-fns/log-gamma)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/log-gamma m/inf+))
    (t/is= 2.252712651734206 (special-fns/log-gamma 0.1))
    ;;;Math 0.57236494292470008707
    (t/is= 0.5723649429247001 (special-fns/log-gamma 0.5))
    (t/is= 0.04543773854448517 (special-fns/log-gamma 2.1))
    (t/is= -0.04987244125983987 (special-fns/log-gamma 1.1))
    (t/is= 0.0 (special-fns/log-gamma 1))
    (t/is= 0.26086724653166665 (special-fns/log-gamma 0.7))
    #_(t/is= 0.2608672465316666 (Gamma/logGamma 0.7))))

(deftest log-gamma-inc-test
  (t/with-instrument `special-fns/log-gamma-inc
    (t/is (t/spec-check special-fns/log-gamma-inc)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/log-gamma-inc m/inf+))
    (t/is= -5.772156649015329E-18 (special-fns/log-gamma-inc 1e-17))))

(deftest log-gamma-derivative-test                          ;same as digamma
  (t/with-instrument `special-fns/log-gamma-derivative
    (t/is (t/spec-check special-fns/log-gamma-derivative)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/log-gamma-derivative m/inf+))
    (t/is= -10.423754943278134 (special-fns/log-gamma-derivative 0.1))
    (t/is= -0.5772156677920671 (special-fns/log-gamma-derivative 1))
    (t/is= 0.48533596581277155 (special-fns/log-gamma-derivative 2.1))
    (t/is= -0.4237549432781376 (special-fns/log-gamma-derivative 1.1))
    (t/is= -1.2200235564290471 (special-fns/log-gamma-derivative 0.7))
    (t/is= 0.7031566378697294 (special-fns/digamma 2.5))
    (t/is= 1.1031566378697286 (special-fns/digamma -2.5))
    #_(t/is= 1.1031566378697297 (Gamma/digamma -2.5))
    (t/is= m/inf- (special-fns/digamma -2.0))
    (t/is= (special-fns/log-gamma-derivative 1.0) (special-fns/digamma 1.0))))

(deftest gamma-derivative-test
  (t/with-instrument `special-fns/gamma-derivative
    (t/is (t/spec-check special-fns/gamma-derivative)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/gamma-derivative m/inf+))
    (t/is= -99.16647290191275 (special-fns/gamma-derivative 0.1))
    (t/is= -0.5772156677920671 (special-fns/gamma-derivative 1))
    (t/is= 0.5078972191920689 (special-fns/gamma-derivative 2.1))
    (t/is= -0.40313959152254936 (special-fns/gamma-derivative 1.1))
    (t/is= -1.0428235898368972 (special-fns/gamma-derivative -2.5))
    (t/is= -1.5836580833783633 (special-fns/gamma-derivative 0.7))))

(deftest trigamma-test
  (t/with-instrument `special-fns/trigamma
    (t/is (t/spec-check special-fns/trigamma)))
  (t/with-instrument :all
    (t/is= m/inf+ (special-fns/trigamma m/inf+))
    (t/is= 101.43329914974142 (special-fns/trigamma 0.1))
    (t/is= 0.6068528687496855 (special-fns/trigamma 2.1))
    (t/is= 1.4332991497414205 (special-fns/trigamma 1.1))
    (t/is= 1.6449340657861162 (special-fns/trigamma 1))
    (t/is= m/inf+ (special-fns/trigamma 0.0))
    (t/is= 2.8340491557052214 (special-fns/trigamma 0.7))
    (t/is= 101.9225399585074 (special-fns/trigamma -0.1))
    #_(t/is= 101.922539959477124 (Gamma/trigamma -0.1))
    (t/is= m/inf+ (special-fns/trigamma -2.0))))

(deftest multivariate-gamma-test
  (t/with-instrument `special-fns/multivariate-gamma
    (t/is (t/spec-check special-fns/multivariate-gamma)))
  (t/with-instrument :all
    (t/is= 1.0 (special-fns/multivariate-gamma m/inf+ 0))
    (t/is= 1.0 (special-fns/multivariate-gamma 0.1 0))
    (t/is= 1.0 (special-fns/multivariate-gamma 1.1 0))
    (t/is= 0.9513507698668731 (special-fns/multivariate-gamma 1.1 1))
    (t/is= 2.511113699545875 (special-fns/multivariate-gamma 1.1 2))
    (t/is= 75.05107616754478 (special-fns/multivariate-gamma 1.1 3))))

(deftest multivariate-log-gamma-test
  (t/with-instrument `special-fns/multivariate-log-gamma
    (t/is (t/spec-check special-fns/multivariate-log-gamma)))
  (t/with-instrument :all
    (t/is= 0.0 (special-fns/multivariate-log-gamma m/inf+ 0))
    (t/is= 0.0 (special-fns/multivariate-log-gamma 0.1 0))
    (t/is= 0.0 (special-fns/multivariate-log-gamma 1.1 0))
    (t/is= -0.04987244125983987 (special-fns/multivariate-log-gamma 1.1 1))
    (t/is= 0.9207263597340951 (special-fns/multivariate-log-gamma 1.1 2))))

;;;BETA
(deftest beta-test
  (t/with-instrument `special-fns/beta
    (t/is (t/spec-check special-fns/beta)))
  (t/with-instrument :all
    (t/is= 0.47619047619047605 (special-fns/beta 1.0 2.1))
    (t/is (m/nan? (special-fns/beta 0.1 m/inf+)))
    (t/is= 0.909090909090909 (special-fns/beta 1 1.1))
    (t/is= 0.909090909090909 (special-fns/beta 1.1 1))
    (t/is= 1.0 (special-fns/beta 1 1))
    (t/is= 10.000000000000002 (special-fns/beta 0.1 1))
    (t/is= 10.000000000000002 (special-fns/beta 1 0.1))))

(deftest log-beta-test
  (t/with-instrument `special-fns/log-beta
    (t/is (t/spec-check special-fns/log-beta)))
  (t/with-instrument :all
    (t/is= -0.7419373447293776 (special-fns/log-beta 1.0 2.1))
    (t/is (m/nan? (special-fns/log-beta 0.1 m/inf+)))
    (t/is= -0.09531017980432505 (special-fns/log-beta 1 1.1))
    (t/is= -0.09531017980432505 (special-fns/log-beta 1.1 1))
    (t/is= 0.0 (special-fns/log-beta 1 1))
    (t/is= 2.302585092994046 (special-fns/log-beta 0.1 1))
    (t/is= 2.302585092994046 (special-fns/log-beta 1 0.1))
    #_(t/is= 2.302585092994046 (Beta/logBeta 1.0 0.1))
    (t/is= -1.386294362144632E10 (special-fns/log-beta 1e10 1e10))
    #_(t/is= -1.386294362144632E10 (Beta/logBeta 1e10 1e10))))

(deftest regularized-beta-test
  (t/with-instrument `special-fns/regularized-beta
    (t/is (t/spec-check special-fns/regularized-beta)))
  (t/with-instrument :all
    (t/is= 0.766741752115798 (special-fns/regularized-beta 0.5 1.0 2.1))
    (t/is= 1.0 (special-fns/regularized-beta 1 1 1.1))
    (t/is= 1.0 (special-fns/regularized-beta 1 1.1 1))
    (t/is= 1.0 (special-fns/regularized-beta 1 1 1))
    (t/is= 0.886568150565213 (special-fns/regularized-beta 0.3 0.1 1))
    (t/is= 0.9330329915368075 (special-fns/regularized-beta 0.5 0.1 1))
    (t/is= 0.9649610951198176 (special-fns/regularized-beta 0.7 0.1 1))
    #_(t/is= 0.9649610951198176 (Beta/regularizedBeta 0.7 0.1 1.0)) ;;old Apache
    (t/is= 1.0 (special-fns/regularized-beta 1 1 0.1))
    (t/is= 0.49999999999999983 (special-fns/regularized-beta 0.5 0.5 0.5))
    #_(t/is= 0.49999999999999983 (Beta/regularizedBeta 0.5 0.5 0.5))
    (t/is= 0.0 (special-fns/regularized-beta 0 1 1))
    #_(t/is= 0.0 (Beta/regularizedBeta 0.0 1.0 1.0))
    (t/is= 0.5000004638553182 (special-fns/regularized-beta 0.5 1e10 1e10))
    #_(t/is= 0.500000463837539 (Beta/regularizedBeta 0.5 1e10 1e10))
    #_(t/is= 0.5 (Beta/regularizedBeta 0.5 1 1))
    (t/is= 0.49999999999999983 (special-fns/regularized-beta 0.5 0.5 0.5))))

(deftest incomplete-beta-test
  (t/with-instrument `special-fns/incomplete-beta
    (t/is (t/spec-check special-fns/incomplete-beta)))
  (t/with-instrument :all
    (t/is= 0.3651151200551418 (special-fns/incomplete-beta 0.5 1.0 2.1))
    (t/is= 0.909090909090909 (special-fns/incomplete-beta 1 1 1.1))
    (t/is= 0.909090909090909 (special-fns/incomplete-beta 1 1.1 1))
    (t/is= 1.0 (special-fns/incomplete-beta 1 1 1))
    (t/is= 8.865681505652132 (special-fns/incomplete-beta 0.3 0.1 1))
    (t/is= 9.330329915368077 (special-fns/incomplete-beta 0.5 0.1 1))
    (t/is= 9.649610951198179 (special-fns/incomplete-beta 0.7 0.1 1))
    (t/is= 10.000000000000002 (special-fns/incomplete-beta 1 1 0.1))
    (t/is= 1.5707963267948966 (special-fns/incomplete-beta 0.5 0.5 0.5))
    (t/is= 0.0 (special-fns/incomplete-beta 0 1 1))))
