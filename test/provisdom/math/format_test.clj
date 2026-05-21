(ns provisdom.math.format-test
  (:require
    [provisdom.math.core :as m]
    [provisdom.math.format :as format]
    [provisdom.test.core :as t]
    [provisdom.utility-belt.anomalies :as anomalies]))

;;4 seconds

(set! *warn-on-reflection* true)

;;;PRIMITIVE FORMATTERS
(t/deftest trim-number-as-string-test
  (t/with-instrument `format/trim-number-as-string
    (t/is-spec-check format/trim-number-as-string))
  (t/with-instrument :all
    (t/is= "-.003" (format/trim-number-as-string "-00.00300"))
    (t/is= "-.003" (format/trim-number-as-string "-.00300"))
    (t/is= ".003" (format/trim-number-as-string "00.00300"))
    (t/is= "-300" (format/trim-number-as-string "-00300"))
    (t/is= "300" (format/trim-number-as-string "0000300"))))

(t/deftest format-as-float-test
  (t/with-instrument `format/format-as-float
    (t/is-spec-check format/format-as-float))
  (t/with-instrument :all
    (t/is= "123" (format/format-as-float 123.456 0))
    (t/is= "123.4560" (format/format-as-float 123.456 4))
    (t/is= "0.0000" (format/format-as-float 1.23456E-5 4))
    (t/is= "-0.0000" (format/format-as-float -1.23456E-5 4))
    (t/is= "123.0000" (format/format-as-float 123 4))
    (t/is= "323234893849384900000.000" (format/format-as-float 3.232348938493849E20 3))))

(t/deftest format-as-exponential-test
  (t/with-instrument `format/format-as-exponential
    (t/is-spec-check format/format-as-exponential))
  (t/with-instrument :all
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

(t/deftest format-as-engineering-test
  (t/with-instrument `format/format-as-engineering
    (t/is-spec-check format/format-as-engineering))
  (t/with-instrument :all
    (t/is= "12.35E+3" (format/format-as-engineering 12345))
    (t/is= "1.23E-3" (format/format-as-engineering 0.00123))
    (t/is= "123.46E+6" (format/format-as-engineering 123456789))
    (t/is= "1.235E+6" (format/format-as-engineering 1234567 {::format/digits 4}))
    (t/is= "0E+0" (format/format-as-engineering 0))
    (t/is= "-12.35E+3" (format/format-as-engineering -12345))
    (t/is= "1.00E+0" (format/format-as-engineering 1))))

;;;NUMBER FORMATTING
(t/deftest format-number-test
  (t/with-instrument `format/format-number
    (t/is-spec-check format/format-number))
  (t/with-instrument :all
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
    (t/is= "-0E+0" (format/format-number -2.34231E-7 12 {::format/max-decimal-places 3}))
    ;; max-length edge cases: max-length of 1 still produces something, large max-length OK
    (t/is= "1E+5" (format/format-number 123456 1))
    (t/is= "123456.0" (format/format-number 123456 1000))))

(t/deftest format-number-extended-test
  (t/with-instrument `format/format-number-extended
    (t/is-spec-check format/format-number-extended))
  (t/with-instrument :all
    ;; Thousand separators (note: integers formatted as doubles have .0 suffix)
    (t/is= "1,234,567.0" (format/format-number-extended 1234567 15 {::format/thousands-sep? true}))
    (t/is= "-1,234,567.0"
      (format/format-number-extended -1234567 15 {::format/thousands-sep? true}))
    ;; With decimal places specified (format-number still adds .0 for round numbers)
    (t/is= "1,234,567.0"
      (format/format-number-extended 1234567 15 {::format/max-decimal-places 0
                                                 ::format/thousands-sep?     true}))
    ;; Engineering notation
    (t/is= "12.35E+3" (format/format-number-extended 12345 10 {::format/engineering? true}))
    ;; Localization (note: must escape decimal first to avoid double replacement)
    (t/is= "1.234,56" (format/format-number-extended 1234.56 15
                        {::format/decimal-symbol  \,
                         ::format/grouping-symbol \.
                         ::format/thousands-sep?  true}))
    ;; Special values
    (t/is= "NaN" (format/format-number-extended m/nan 5))
    (t/is= "Inf" (format/format-number-extended m/inf+ 5))
    (t/is= "-Inf" (format/format-number-extended m/inf- 5))))

;;;PERCENT
(t/deftest format-percent-test
  (t/with-instrument `format/format-percent
    (t/is-spec-check format/format-percent))
  (t/with-instrument :all
    (t/is= "15%" (format/format-percent 0.15))
    (t/is= "16%" (format/format-percent 0.156 {::format/precision 0}))
    (t/is= "15.6%" (format/format-percent 0.156 {::format/precision 1}))
    (t/is= "150%" (format/format-percent 1.5))
    (t/is= "-25%" (format/format-percent -0.25))
    (t/is= "0%" (format/format-percent 0))
    (t/is= "NaN" (format/format-percent m/nan))
    (t/is= "Inf" (format/format-percent m/inf+))
    (t/is= "-Inf" (format/format-percent m/inf-))))

;;;SHORTHAND
(t/deftest format-shorthand-test
  (t/with-instrument `format/format-shorthand
    (t/is-spec-check format/format-shorthand))
  (t/with-instrument :all
    (t/is= "3E+20" (format/format-shorthand 3.232348938493849E20 3))
    (t/is= "3T" (format/format-shorthand 3232394349923 3))
    (t/is= "32B" (format/format-shorthand 32323943499 3))
    (t/is= "323M" (format/format-shorthand 323234324 3))
    (t/is= "32K" (format/format-shorthand 32323 2))
    (t/is= "3K" (format/format-shorthand 3232 1))
    (t/is= "323.23432412M"
      (format/format-shorthand 3.23234324123124E8 13 {::format/max-decimal-places 2}))
    (t/is= "320M" (format/format-shorthand 3.23234324123124E8 13 {::format/max-digits 2}))
    (t/is= "3E-5" (format/format-shorthand 3.232E-5 1))
    (t/is= "NaN" (format/format-shorthand m/nan 3))
    (t/is= "Inf" (format/format-shorthand m/inf+ 3))
    (t/is= "-Inf" (format/format-shorthand m/inf- 3))
    (t/is= "Inf" (format/format-shorthand m/inf+ 3 {::format/money? true}))
    (t/is= "$323M" (format/format-shorthand 323234324 3 {::format/money? true}))
    (t/is= "-$323M" (format/format-shorthand -323234324 3 {::format/money? true}))
    ;; Boundaries: no-suffix/K and K/M
    (t/is= "999.0" (format/format-shorthand 999 5))
    (t/is= "1K" (format/format-shorthand 1000 5))
    (t/is= "999K" (format/format-shorthand 999000 5))
    (t/is= "1M" (format/format-shorthand 1000000 5))
    ;; Format side of format/parse roundtrip values
    (t/is= "1.234K" (format/format-shorthand 1234 10))
    (t/is= "1.234567M" (format/format-shorthand 1234567 10))
    (t/is= "1.2345679B" (format/format-shorthand 1234567890 10))
    (t/is= "1.2345679T" (format/format-shorthand 1234567890123 10))
    (t/is= "-5.678K" (format/format-shorthand -5678 10))))

(t/deftest parse-shorthand-test
  (t/with-instrument `format/parse-shorthand
    (t/is-spec-check format/parse-shorthand))
  (t/with-instrument :all
    (t/is= 2.3432343E16 (format/parse-shorthand "23432.343T"))
    (t/is= 2.3432343E13 (format/parse-shorthand "23432.343B"))
    (t/is= 2.3432343E10 (format/parse-shorthand "23432.343M"))
    (t/is-approx= m/nan (format/parse-shorthand "NaN") :nan-equal? true)
    (t/is= m/inf+ (format/parse-shorthand "Inf"))
    (t/is= m/inf- (format/parse-shorthand "-Inf"))
    (t/is= 2.3432343E16 (format/parse-shorthand "$23432.343T"))
    (t/is= -2.3432343E16 (format/parse-shorthand "-$23432.343T"))
    ;; Scientific notation parsing
    (t/is= 12300.0 (format/parse-shorthand "1.23E+4"))
    (t/is= 1.23E-4 (format/parse-shorthand "1.23E-4"))
    (t/is= 12300.0 (format/parse-shorthand "1.23e+4"))
    (t/is= -12300.0 (format/parse-shorthand "-1.23E+4"))
    (t/is= 12300.0 (format/parse-shorthand "$1.23E+4"))
    ;; Empty / blank input returns anomaly with full map
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input ""}
            ::anomalies/message  "Cannot parse empty or blank string"}
      (format/parse-shorthand ""))
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input "   "}
            ::anomalies/message  "Cannot parse empty or blank string"}
      (format/parse-shorthand "   "))
    ;; Invalid input returns anomaly
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input "invalid"}
            ::anomalies/message  "Failed to parse number string"}
      (format/parse-shorthand "invalid"))
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input "abc123"}
            ::anomalies/message  "Failed to parse number string"}
      (format/parse-shorthand "abc123"))
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input ":keyword"}
            ::anomalies/message  "Failed to parse number string"}
      (format/parse-shorthand ":keyword"))
    ;; Security: code-execution payloads return anomalies, not eval results
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input "#=(+ 1 1)"}
            ::anomalies/message  "Failed to parse number string"}
      (format/parse-shorthand "#=(+ 1 1)"))
    (t/is= {::anomalies/category ::anomalies/incorrect
            ::anomalies/data     {:input "(println \"test\")"}
            ::anomalies/message  "Failed to parse number string"}
      (format/parse-shorthand "(println \"test\")"))
    ;; Parse side of format/parse roundtrip values
    (t/is= 1234.0 (format/parse-shorthand "1.234K"))
    (t/is= 1234567.0 (format/parse-shorthand "1.234567M"))
    (t/is= 1.2345679E9 (format/parse-shorthand "1.2345679B"))
    (t/is= 1.2345679E12 (format/parse-shorthand "1.2345679T"))
    (t/is= -5678.0 (format/parse-shorthand "-5.678K"))))

(t/deftest format-shorthand-custom-test
  (t/with-instrument `format/format-shorthand-custom
    (t/is-spec-check format/format-shorthand-custom))
  (t/with-instrument :all
    ;; Custom letters
    (t/is= "1.5k" (format/format-shorthand-custom 1500 5 {::format/shorthand-letters {"k" 1000}}))
    ;; Default behavior matches format-shorthand
    (t/is= "1.5K" (format/format-shorthand-custom 1500 5))
    ;; Money option
    (t/is= "$1.5K" (format/format-shorthand-custom 1500 5 {::format/money? true}))
    ;; Special values
    (t/is= "NaN" (format/format-shorthand-custom m/nan 5))
    (t/is= "Inf" (format/format-shorthand-custom m/inf+ 5))))
