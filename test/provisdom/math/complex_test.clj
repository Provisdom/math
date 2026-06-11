(ns provisdom.math.complex-test
  (:require
    [provisdom.math.complex :as complex]
    [provisdom.math.core :as m]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;3 seconds

(set! *warn-on-reflection* true)

;;;PREDICATES
(t/deftest complex?-test
  (t/with-instrument `complex/complex?
    (t/is-spec-check complex/complex?))
  (t/with-instrument :all
    (t/is (complex/complex? {::complex/im 1.0 ::complex/re 0.0}))
    (t/is (complex/complex? complex/i))
    (t/is (complex/complex? complex/one))
    (t/is (complex/complex? complex/zero))
    (t/is-not (complex/complex? {::complex/im 1.0}))
    (t/is-not (complex/complex? {::complex/im 1.0 ::complex/re m/nan}))
    (t/is-not (complex/complex? "not-a-complex"))))

(t/deftest zero?-test
  (t/with-instrument `complex/zero?
    (t/is-spec-check complex/zero?))
  (t/with-instrument :all
    (t/is (complex/zero? complex/zero))
    (t/is-not (complex/zero? complex/one))
    (t/is-not (complex/zero? complex/i))))

;;;CONSTRUCTORS AND ACCESSORS
(t/deftest ->complex-test
  (t/with-instrument `complex/->complex
    (t/is-spec-check complex/->complex))
  (t/with-instrument :all
    (t/is= {::complex/im 4.0 ::complex/re 3.0} (complex/->complex 3.0 4.0))
    (t/is= complex/zero (complex/->complex 0.0 0.0))
    (t/is= complex/i (complex/->complex 0.0 1.0))))

(t/deftest real-test
  (t/with-instrument `complex/real
    (t/is-spec-check complex/real))
  (t/with-instrument :all
    (t/is= 3.0 (complex/real (complex/->complex 3.0 4.0)))
    (t/is= 0.0 (complex/real complex/zero))
    (t/is= 1.0 (complex/real complex/one))))

(t/deftest imaginary-test
  (t/with-instrument `complex/imaginary
    (t/is-spec-check complex/imaginary))
  (t/with-instrument :all
    (t/is= 4.0 (complex/imaginary (complex/->complex 3.0 4.0)))
    (t/is= 0.0 (complex/imaginary complex/zero))
    (t/is= 1.0 (complex/imaginary complex/i))))

(t/deftest ->polar-test
  (t/with-instrument `complex/->polar
    (t/is-spec-check complex/->polar))
  (t/with-instrument :all
    (t/is-data-approx= {::complex/im 0.0 ::complex/re 1.0}
      (complex/->polar 1.0 0.0) :tolerance 1e-12)
    (t/is-data-approx= {::complex/im 1.0 ::complex/re 0.0}
      (complex/->polar 1.0 (* 0.5 m/PI)) :tolerance 1e-12)
    (t/is-data-approx= {::complex/im 0.0 ::complex/re -1.0}
      (complex/->polar 1.0 m/PI) :tolerance 1e-12)))

(t/deftest ->rectangular-test
  (t/with-instrument `complex/->rectangular
    (t/is-spec-check complex/->rectangular))
  (t/with-instrument :all
    (t/is-data-approx= {::complex/im 0.0 ::complex/re 1.0}
      (complex/->rectangular 1.0 0.0) :tolerance 1e-12)
    (t/is-data-approx= {::complex/im 2.0 ::complex/re 0.0}
      (complex/->rectangular 2.0 (* 0.5 m/PI)) :tolerance 1e-12)))

;;;ARITHMETIC
(t/deftest add-test
  (t/with-instrument `complex/add
    (t/is-spec-check complex/add))
  (t/with-instrument :all
    (t/is= {::complex/im 6.0 ::complex/re 4.0}
      (complex/add (complex/->complex 1.0 2.0) (complex/->complex 3.0 4.0)))
    (t/is= complex/one (complex/add complex/zero complex/one))
    (t/is= {::complex/im 1.0 ::complex/re 1.0} (complex/add complex/one complex/i))))

(t/deftest sub-test
  (t/with-instrument `complex/sub
    (t/is-spec-check complex/sub))
  (t/with-instrument :all
    (t/is= {::complex/im -2.0 ::complex/re -2.0}
      (complex/sub (complex/->complex 1.0 2.0) (complex/->complex 3.0 4.0)))
    (t/is= complex/zero (complex/sub complex/one complex/one))
    (t/is= {::complex/im -1.0 ::complex/re 1.0} (complex/sub complex/one complex/i))))

(t/deftest mul-test
  (t/with-instrument `complex/mul
    (t/is-spec-check complex/mul))
  (t/with-instrument :all
    ;; i * i = -1
    (t/is-data-approx= {::complex/im 0.0 ::complex/re -1.0}
      (complex/mul complex/i complex/i) :tolerance 1e-12)
    (t/is= complex/zero (complex/mul complex/zero complex/i))
    (t/is= complex/i (complex/mul complex/one complex/i))
    ;; (1+2i)(3+4i) = (3-8) + (4+6)i = -5 + 10i
    (t/is= {::complex/im 10.0 ::complex/re -5.0}
      (complex/mul (complex/->complex 1.0 2.0) (complex/->complex 3.0 4.0)))))

(t/deftest conjugate-test
  (t/with-instrument `complex/conjugate
    (t/is-spec-check complex/conjugate))
  (t/with-instrument :all
    (t/is= {::complex/im -4.0 ::complex/re 3.0} (complex/conjugate (complex/->complex 3.0 4.0)))
    (t/is= complex/zero (complex/conjugate complex/zero))
    ;; negating the 0.0 imaginary part of `one` yields -0.0
    (t/is= {::complex/im -0.0 ::complex/re 1.0} (complex/conjugate complex/one))
    (t/is= {::complex/im -1.0 ::complex/re 0.0} (complex/conjugate complex/i))))

(t/deftest div-test
  (t/with-instrument `complex/div
    (t/is-spec-check complex/div))
  (t/with-instrument :all
    ;; (1 + 0i) / (1 + 0i) = 1
    (t/is-data-approx= complex/one (complex/div complex/one complex/one) :tolerance 1e-12)
    ;; i / i = 1
    (t/is-data-approx= complex/one (complex/div complex/i complex/i) :tolerance 1e-12)
    ;; 1 / i = -i
    (t/is-data-approx= {::complex/im -1.0 ::complex/re 0.0}
      (complex/div complex/one complex/i) :tolerance 1e-12)
    ;; (3+4i) / (1+2i) = (3+4i)(1-2i) / 5 = (3+8 + (4-6)i)/5 = (11 - 2i)/5
    (t/is-data-approx= {::complex/im -0.4 ::complex/re 2.2}
      (complex/div (complex/->complex 3.0 4.0) (complex/->complex 1.0 2.0)) :tolerance 1e-12)
    ;; Division by zero -> anomaly
    (t/is= {::anomalies/category ::anomalies/no-solve
            ::anomalies/fn       (var complex/div)
            ::anomalies/message  "Division by complex zero."}
      (complex/div complex/one complex/zero))))

(t/deftest abs-test
  (t/with-instrument `complex/abs
    (t/is-spec-check complex/abs))
  (t/with-instrument :all
    (t/is= 0.0 (complex/abs complex/zero))
    (t/is= 1.0 (complex/abs complex/one))
    (t/is= 1.0 (complex/abs complex/i))
    (t/is= 5.0 (complex/abs (complex/->complex 3.0 4.0)))
    (t/is-approx= (m/sqrt 2.0) (complex/abs (complex/->complex 1.0 1.0)) :tolerance 1e-12)))

(t/deftest magnitude-test
  (t/with-instrument `complex/magnitude
    (t/is-spec-check complex/magnitude))
  (t/with-instrument :all
    (t/is= 0.0 (complex/magnitude complex/zero))
    (t/is= 1.0 (complex/magnitude complex/one))
    (t/is= 5.0 (complex/magnitude (complex/->complex 3.0 4.0)))))

(t/deftest arg-test
  (t/with-instrument `complex/arg
    (t/is-spec-check complex/arg))
  (t/with-instrument :all
    (t/is= 0.0 (complex/arg complex/one))
    (t/is-approx= (* 0.5 m/PI) (complex/arg complex/i) :tolerance 1e-12)
    (t/is-approx= m/PI (complex/arg (complex/->complex -1.0 0.0)) :tolerance 1e-12)
    (t/is-approx= (* -0.5 m/PI) (complex/arg (complex/->complex 0.0 -1.0)) :tolerance 1e-12)))

(t/deftest angle-test
  (t/with-instrument `complex/angle
    (t/is-spec-check complex/angle))
  (t/with-instrument :all
    (t/is= 0.0 (complex/angle complex/one))
    (t/is-approx= (* 0.5 m/PI) (complex/angle complex/i) :tolerance 1e-12)
    (t/is-approx= m/PI (complex/angle (complex/->complex -1.0 0.0)) :tolerance 1e-12)))

(t/deftest exp-test
  (t/with-instrument `complex/exp
    (t/is-spec-check complex/exp))
  (t/with-instrument :all
    ;; e^0 = 1
    (t/is-data-approx= complex/one (complex/exp complex/zero) :tolerance 1e-12)
    ;; Euler's identity: e^(i*pi) = -1
    (t/is-data-approx= {::complex/im 0.0 ::complex/re -1.0}
      (complex/exp (complex/->complex 0.0 m/PI)) :tolerance 1e-12)
    ;; e^(i*pi/2) = i
    (t/is-data-approx= {::complex/im 1.0 ::complex/re 0.0}
      (complex/exp (complex/->complex 0.0 (* 0.5 m/PI))) :tolerance 1e-12)))

(t/deftest log-test
  (t/with-instrument `complex/log
    (t/is-spec-check complex/log))
  (t/with-instrument :all
    ;; log(1) = 0
    (t/is-data-approx= complex/zero (complex/log complex/one) :tolerance 1e-12)
    ;; log(i) = i*pi/2
    (t/is-data-approx= {::complex/im (* 0.5 m/PI) ::complex/re 0.0}
      (complex/log complex/i) :tolerance 1e-12)
    ;; log(-1) = i*pi
    (t/is-data-approx= {::complex/im m/PI ::complex/re 0.0}
      (complex/log (complex/->complex -1.0 0.0)) :tolerance 1e-12)
    ;; log(0) -> anomaly
    (t/is= {::anomalies/category ::anomalies/no-solve
            ::anomalies/fn       (var complex/log)
            ::anomalies/message  "log of complex zero is undefined."}
      (complex/log complex/zero))))
