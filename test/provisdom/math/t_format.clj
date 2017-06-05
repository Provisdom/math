(ns provisdom.math.t-format
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.format :as format]
            [provisdom.math.core :as m]
            [clojure.spec.test.alpha :as sta]
            [orchestra.spec.test :as st]))

(deftest trim-number-test
  (is= "-.003" (format/trim-number "-00.00300"))
  (is= "-.003" (format/trim-number "-.00300"))
  (is= ".003" (format/trim-number "00.00300"))
  (is= "-300" (format/trim-number "-00300"))
  (is= "300" (format/trim-number "0000300")))

(deftest format-float-test
  (is (thrown? Exception (format/format-float 123.456 -1)))
  (is= "123" (format/format-float 123.456 0))
  (is= "123.4560" (format/format-float 123.456 4))
  (is= "0.0000" (format/format-float 1.23456E-5 4))
  (is= "-0.0000" (format/format-float -1.23456E-5 4))
  (is= "123.0000" (format/format-float 123 4))
  (is= "323234893849384900000.000" (format/format-float 3.232348938493849E20 3)))

(deftest format-exponential-test
  (is (thrown? Exception (format/format-exponential 23432 0)))
  (is= "2E+0" (format/format-exponential 2.0 1))
  (is= "2.3432E+4" (format/format-exponential 23432))
  (is= "2E+4" (format/format-exponential 23432 1))
  (is= "2E-2" (format/format-exponential 0.023432 1))
  (is= "2.343200E+4" (format/format-exponential 23432 7))
  (is= "-2.343200E+4" (format/format-exponential -23432 7))
  (is= "4.5E+101" (format/format-exponential 4.5E101 2))
  (is= "3.23E+20" (format/format-exponential 3.232348938493849E20 3))
  (is= "3.232348938493849E+20" (format/format-exponential 3.232348938493849E20))
  (is= "3.2323489384938490000E+20" (format/format-exponential 3.232348938493849E20 20)))

(deftest format-number-test
  (is (thrown? Exception (format/format-number 23.33 -1)))
  (is= "23" (format/format-number 23.33 1))
  (is= "1.2E+6" (format/format-number 1234567 6))
  (is= "1234567" (format/format-number 1234567 8))
  (is= "123.46" (format/format-number 123.4567 6))
  (is= "123.4567" (format/format-number 123.4567 9))
  (is= "123" (format/format-number 123 2))
  (is= "0" (format/format-number 0.023432 0))
  (is= "0.0" (format/format-number 0.023432 3))
  (is= "0.02" (format/format-number 0.023432 4))
  (is= "2E-5" (format/format-number 2.3432E-5 5))
  (is= "23423.42" (format/format-number 23423.4234 9 :max-decimal-places 2))
  (is= "23423.4" (format/format-number 23423.4234 8 :max-decimal-places 1))
  (is= "23423" (format/format-number 23423.4234 5 :max-decimal-places 0))
  (is= "23423" (format/format-number 23423.4234 4 :max-decimal-places 0))
  (is= "2E+5" (format/format-number 234233.4234 4 :max-decimal-places 0))
  (is= "2.34231E+11" (format/format-number 2.342311111114234E11 11 :max-decimal-places 1))
  (is= "2.3423E+11" (format/format-number 2.342311111114234E11 11 :max-digits 5))
  (is= "0E+0" (format/format-number 2.34231E-7 11 :max-decimal-places 3))
  (is= "3E+20" (format/format-number 3.232348938493849E20 3))
  (is= "-23" (format/format-number -23.33 2))
  (is= "-1.2E+6" (format/format-number -1234567 7))
  (is= "-1234567" (format/format-number -1234567 9))
  (is= "-123.46" (format/format-number -123.4567 7))
  (is= "-123.4567" (format/format-number -123.4567 10))
  (is= "-123" (format/format-number -123 3))
  (is= "-0" (format/format-number -0.023432 0))
  (is= "-0.0" (format/format-number -0.023432 4))
  (is= "-0.02" (format/format-number -0.023432 5))
  (is= "-2E-5" (format/format-number -2.3432E-5 6))
  (is= "-23423.42" (format/format-number -23423.4234 10 :max-decimal-places 2))
  (is= "-23423.4" (format/format-number -23423.4234 9 :max-decimal-places 1))
  (is= "-23423" (format/format-number -23423.4234 6 :max-decimal-places 0))
  (is= "-23423" (format/format-number -23423.4234 5 :max-decimal-places 0))
  (is= "-2E+5" (format/format-number -234233.4234 5 :max-decimal-places 0))
  (is= "-2.34231E+11" (format/format-number -2.342311111114234E11 12 :max-decimal-places 1))
  (is= "-2.3423E+11" (format/format-number -2.342311111114234E11 12 :max-digits 5))
  (is= "-0E+0" (format/format-number -2.34231E-7 12 :max-decimal-places 3)))

(deftest number-manipulation-test
  (trim-number-test)
  (format-float-test)
  (format-exponential-test)
  (format-number-test))

(deftest unparse-test
  (is= "3E+20" (format/unparse-shorthand 3.232348938493849E20 3))
  (is= "3T" (format/unparse-shorthand 3232394349923 3))
  (is= "32B" (format/unparse-shorthand 32323943499 3))
  (is= "323M" (format/unparse-shorthand 323234324 3))
  (is= "32K" (format/unparse-shorthand 32323 2))
  (is= "3K" (format/unparse-shorthand 3232 1))
  (is= "323.23432412M" (format/unparse-shorthand 3.23234324123124E8 13 :max-decimal-places 2))
  (is= "320M" (format/unparse-shorthand 3.23234324123124E8 13 :max-digits 2))
  (is= "3E-5" (format/unparse-shorthand 3.232E-5 1))
  (is= "NaN" (format/unparse-shorthand m/nan 3))
  (is= "Inf" (format/unparse-shorthand m/inf+ 3))
  (is= "-Inf" (format/unparse-shorthand m/inf- 3))
  (is= "Inf" (format/unparse-shorthand m/inf+ 3 :money? true))
  (is= "$323M" (format/unparse-shorthand 323234324 3 :money? true))
  (is= "-$323M" (format/unparse-shorthand -323234324 3 :money? true)))

(deftest parse-test
  (is= 2.3432343E16 (format/parse-shorthand "23432.343T"))
  (is= 2.3432343E13 (format/parse-shorthand "23432.343B"))
  (is= 2.3432343E10 (format/parse-shorthand "23432.343M"))
  (is (thrown? Exception (format/parse-shorthand "23432.343BT")))
  (is (m/nan? (format/parse-shorthand "NaN")))
  (is= m/inf+ (format/parse-shorthand "Inf"))
  (is= m/inf- (format/parse-shorthand "-Inf"))
  (is= 2.3432343E16 (format/parse-shorthand "$23432.343T"))
  (is= -2.3432343E16 (format/parse-shorthand "-$23432.343T")))

(deftest shorthand-test
  (unparse-test)
  (parse-test))

