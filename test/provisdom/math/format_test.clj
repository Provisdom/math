(ns provisdom.math.format-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.format :as format]))

;;1 seconds

(set! *warn-on-reflection* true)

;;;NUMBERS
(deftest trim-number-as-string-test
  (t/with-instrument `format/trim-number-as-string
    (t/is (t/spec-check format/trim-number-as-string)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "-.003" (format/trim-number-as-string "-00.00300"))
    (t/is= "-.003" (format/trim-number-as-string "-.00300"))
    (t/is= ".003" (format/trim-number-as-string "00.00300"))
    (t/is= "-300" (format/trim-number-as-string "-00300"))
    (t/is= "300" (format/trim-number-as-string "0000300"))))

(deftest format-as-float-test
  (t/with-instrument `format/format-as-float
    (t/is (t/spec-check format/format-as-float)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "123" (format/format-as-float 123.456 0))
    (t/is= "123.4560" (format/format-as-float 123.456 4))
    (t/is= "0.0000" (format/format-as-float 1.23456E-5 4))
    (t/is= "-0.0000" (format/format-as-float -1.23456E-5 4))
    (t/is= "123.0000" (format/format-as-float 123 4))
    (t/is= "323234893849384900000.000" (format/format-as-float 3.232348938493849E20 3))))

(deftest format-as-exponential-test
  (t/with-instrument `format/format-as-exponential
    (t/is (t/spec-check format/format-as-exponential)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "2E+0" (format/format-as-exponential 2.0 {::format/digits 1}))
    (t/is= "2.3432E+4" (format/format-as-exponential 23432))
    (t/is= "2E+4" (format/format-as-exponential 23432 {::format/digits 1}))
    (t/is= "2E-2" (format/format-as-exponential 0.023432 {::format/digits 1}))
    (t/is= "2.343200E+4" (format/format-as-exponential 23432 {::format/digits 7}))
    (t/is= "-2.343200E+4" (format/format-as-exponential -23432 {::format/digits 7}))
    (t/is= "4.5E+101" (format/format-as-exponential 4.5E101 {::format/digits 2}))
    (t/is= "3.23E+20" (format/format-as-exponential 3.232348938493849E20 {::format/digits 3}))
    (t/is= "3.232348938493849E+20" (format/format-as-exponential 3.232348938493849E20))
    (t/is= "3.2323489384938490000E+20"
      (format/format-as-exponential 3.232348938493849E20 {::format/digits 20}))))

(deftest format-number-test
  (t/with-instrument `format/format-number
    (t/is (t/spec-check format/format-number)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "23" (format/format-number 23.33 1))
    (t/is= "1.2E+6" (format/format-number 1234567 6))
    (t/is= "1234567" (format/format-number 1234567 8))
    (t/is= "123.46" (format/format-number 123.4567 6))
    (t/is= "123.4567" (format/format-number 123.4567 9))
    (t/is= "123" (format/format-number 123 2))
    (t/is= "0" (format/format-number 0.023432 1))
    (t/is= "0.0" (format/format-number 0.023432 3))
    (t/is= "0.02" (format/format-number 0.023432 4))
    (t/is= "2E-5" (format/format-number 2.3432E-5 5))
    (t/is= "23423.42" (format/format-number 23423.4234 9 {::format/max-decimal-places 2}))
    (t/is= "23423.4" (format/format-number 23423.4234 8 {::format/max-decimal-places 1}))
    (t/is= "23423" (format/format-number 23423.4234 5 {::format/max-decimal-places 0}))
    (t/is= "23423" (format/format-number 23423.4234 4 {::format/max-decimal-places 0}))
    (t/is= "2E+5" (format/format-number 234233.4234 4 {::format/max-decimal-places 0}))
    (t/is= "2.34231E+11"
      (format/format-number 2.342311111114234E11 11 {::format/max-decimal-places 1}))
    (t/is= "2.3423E+11" (format/format-number 2.342311111114234E11 11 {::format/max-digits 5}))
    (t/is= "0E+0" (format/format-number 2.34231E-7 11 {::format/max-decimal-places 3}))
    (t/is= "3E+20" (format/format-number 3.232348938493849E20 3))
    (t/is= "-23" (format/format-number -23.33 2))
    (t/is= "-1.2E+6" (format/format-number -1234567 7))
    (t/is= "-1234567" (format/format-number -1234567 9))
    (t/is= "-123.46" (format/format-number -123.4567 7))
    (t/is= "-123.4567" (format/format-number -123.4567 10))
    (t/is= "-123" (format/format-number -123 3))
    (t/is= "-0" (format/format-number -0.023432 1))
    (t/is= "-0.0" (format/format-number -0.023432 4))
    (t/is= "-0.02" (format/format-number -0.023432 5))
    (t/is= "-2E-5" (format/format-number -2.3432E-5 6))
    (t/is= "-23423.42" (format/format-number -23423.4234 10 {::format/max-decimal-places 2}))
    (t/is= "-23423.4" (format/format-number -23423.4234 9 {::format/max-decimal-places 1}))
    (t/is= "-23423" (format/format-number -23423.4234 6 {::format/max-decimal-places 0}))
    (t/is= "-23423" (format/format-number -23423.4234 5 {::format/max-decimal-places 0}))
    (t/is= "-2E+5" (format/format-number -234233.4234 5 {::format/max-decimal-places 0}))
    (t/is= "-2.34231E+11"
      (format/format-number -2.342311111114234E11 12 {::format/max-decimal-places 1}))
    (t/is= "-2.3423E+11" (format/format-number -2.342311111114234E11 12 {::format/max-digits 5}))
    (t/is= "-0E+0" (format/format-number -2.34231E-7 12 {::format/max-decimal-places 3}))))

;;;SHORTHAND
(deftest unparse-shorthand-test
  (t/with-instrument `format/unparse-shorthand
    (t/is (t/spec-check format/unparse-shorthand)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= "3E+20" (format/unparse-shorthand 3.232348938493849E20 3))
    (t/is= "3T" (format/unparse-shorthand 3232394349923 3))
    (t/is= "32B" (format/unparse-shorthand 32323943499 3))
    (t/is= "323M" (format/unparse-shorthand 323234324 3))
    (t/is= "32K" (format/unparse-shorthand 32323 2))
    (t/is= "3K" (format/unparse-shorthand 3232 1))
    (t/is= "323.23432412M"
      (format/unparse-shorthand 3.23234324123124E8 13 {::format/max-decimal-places 2}))
    (t/is= "320M" (format/unparse-shorthand 3.23234324123124E8 13 {::format/max-digits 2}))
    (t/is= "3E-5" (format/unparse-shorthand 3.232E-5 1))
    (t/is= "NaN" (format/unparse-shorthand m/nan 3))
    (t/is= "Inf" (format/unparse-shorthand m/inf+ 3))
    (t/is= "-Inf" (format/unparse-shorthand m/inf- 3))
    (t/is= "Inf" (format/unparse-shorthand m/inf+ 3 {::format/money? true}))
    (t/is= "$323M" (format/unparse-shorthand 323234324 3 {::format/money? true}))
    (t/is= "-$323M" (format/unparse-shorthand -323234324 3 {::format/money? true}))))

(deftest parse-shorthand-test
  (t/with-instrument `format/parse-shorthand
    (t/is (t/spec-check format/parse-shorthand)))
  (t/with-instrument (st/instrumentable-syms)
    (t/is= 2.3432343E16 (format/parse-shorthand "23432.343T"))
    (t/is= 2.3432343E13 (format/parse-shorthand "23432.343B"))
    (t/is= 2.3432343E10 (format/parse-shorthand "23432.343M"))
    (t/is (m/nan? (format/parse-shorthand "NaN")))
    (t/is= m/inf+ (format/parse-shorthand "Inf"))
    (t/is= m/inf- (format/parse-shorthand "-Inf"))
    (t/is= 2.3432343E16 (format/parse-shorthand "$23432.343T"))
    (t/is= -2.3432343E16 (format/parse-shorthand "-$23432.343T"))))


