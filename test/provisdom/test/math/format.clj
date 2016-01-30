(ns provisdom.test.math.format
  (:require [midje.sweet :refer :all]
            [criterium.core :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.utility-belt.format :refer :all]
            [provisdom.math.format :refer :all]
            [provisdom.math.core :as m]))

;;;REDUNDANCY from clojure.string
;;;replace-string (replace), replace-first, join, split

(facts "number manipulation"
       (fact  "trim-number"
              (trim-number "-00.00300") => "-.003"
              (trim-number "-.00300") => "-.003"
              (trim-number "00.00300") => ".003"
              (trim-number "-00300") => "-300"
              (trim-number "0000300") => "300")
       (fact "format-float"
             (format-float 123.456 -1) => (throws)
             (format-float 123.456 0) => "123"
             (format-float 123.456 4) => "123.4560"
             (format-float 0.0000123456 4) => "0.0000"
             (format-float -0.0000123456 4) => "-0.0000"
             (format-float 123 4) => "123.0000"
             (format-float 3.232348938493849E20 3)
             => "323234893849384900000.000")
       (fact "format exponential"
             (format-exponential 23432 0) => (throws)
             (format-exponential 2.0 1) => "2E+0"
             (format-exponential 23432) => "2.3432E+4"
             (format-exponential 23432 1) => "2E+4"
             (format-exponential 0.023432 1) => "2E-2"
             (format-exponential 23432 7) => "2.343200E+4"
             (format-exponential -23432 7) => "-2.343200E+4"
             (format-exponential 4.5E101 2) => "4.5E+101"
             (format-exponential 3.2323489384938491E20 3) => "3.23E+20"
             (format-exponential 3.2323489384938491E20)
             => "3.232348938493849E+20"
             (format-exponential 3.2323489384938491E20 20)
             => "3.2323489384938490000E+20")
       (fact "format-number"
             (format-number 23.33 -1) => (throws)
             (format-number 23.33 1) => "23"
             (format-number 1234567 6) => "1.2E+6"
             (format-number 1234567 8) => "1234567"
             (format-number 123.4567 6) => "123.46"
             (format-number 123.4567 9) => "123.4567"
             (format-number 123 2) => "123"
             (format-number 0.023432 0) => "0"
             (format-number 0.023432 3) => "0.0"
             (format-number 0.023432 4) => "0.02"
             (format-number 0.000023432 5) => "2E-5"
             (format-number 23423.4234 9 :max-decimal-places 2) => "23423.42"
             (format-number 23423.4234 8 :max-decimal-places 1) => "23423.4"
             (format-number 23423.4234 5 :max-decimal-places 0) => "23423"
             (format-number 23423.4234 4 :max-decimal-places 0) => "23423"
             (format-number 234233.4234 4 :max-decimal-places 0) => "2E+5"
             (format-number 234231111111.4234 11 :max-decimal-places 1)
             => "2.34231E+11"
             (format-number 234231111111.4234 11 :max-digits 5) => "2.3423E+11"
             (format-number 0.000000234231 11 :max-decimal-places 3) => "0E+0"
             (format-number 3.232348938493849E20 3) => "3E+20"
             (format-number -23.33 2) => "-23"
             (format-number -1234567 7) => "-1.2E+6"
             (format-number -1234567 9) => "-1234567"
             (format-number -123.4567 7) => "-123.46"
             (format-number -123.4567 10) => "-123.4567"
             (format-number -123 3) => "-123"
             (format-number -0.023432 0) => "-0"
             (format-number -0.023432 4) => "-0.0"
             (format-number -0.023432 5) => "-0.02"
             (format-number -0.000023432 6) => "-2E-5"
             (format-number -23423.4234 10 :max-decimal-places 2)
             => "-23423.42"
             (format-number -23423.4234 9 :max-decimal-places 1) => "-23423.4"
             (format-number -23423.4234 6 :max-decimal-places 0) => "-23423"
             (format-number -23423.4234 5 :max-decimal-places 0) => "-23423"
             (format-number -234233.4234 5 :max-decimal-places 0) => "-2E+5"
             (format-number -234231111111.4234 12 :max-decimal-places 1)
             => "-2.34231E+11"
             (format-number -234231111111.4234 12 :max-digits 5)
             => "-2.3423E+11"
             (format-number -0.000000234231 12 :max-decimal-places 3)
             => "-0E+0"))

(facts "shorthand"
       (fact "unparse"
             (unparse-shorthand 323234893849384903840.0 3) => "3E+20"
             (unparse-shorthand 3232394349923 3) => "3T"
             (unparse-shorthand 32323943499 3) => "32B"
             (unparse-shorthand 323234324 3) => "323M"
             (unparse-shorthand 32323 2) => "32K"
             (unparse-shorthand 3232 1) => "3K"
             (unparse-shorthand 323234324.123124 13 :max-decimal-places 2)
             => "323.23432412M"
             (unparse-shorthand 323234324.123124 13 :max-digits 2)
             => "320M"
             (unparse-shorthand 0.00003232 1) => "3E-5"
             (unparse-shorthand m/nan 3) => "NaN"
             (unparse-shorthand m/inf+ 3) => "Inf"
             (unparse-shorthand m/inf- 3) => "-Inf"
             (unparse-shorthand m/inf+ 3 :money? true) => "Inf"
             (unparse-shorthand 323234324 3 :money? true) => "$323M"
             (unparse-shorthand -323234324 3 :money? true) => "-$323M")
       (fact "parse"
             (parse-shorthand "23432.343T") => 2.3432343E16
             (parse-shorthand "23432.343B") => 2.3432343E13
             (parse-shorthand "23432.343M") => 2.3432343E10
             (parse-shorthand "23432.343BT") => (throws)
             (parse-shorthand "NaN") => m/nan?
             (parse-shorthand "Inf") => m/inf+
             (parse-shorthand "-Inf") => m/inf-
             (parse-shorthand "$23432.343T") => 2.3432343E16
             (parse-shorthand "-$23432.343T") => -2.3432343E16))