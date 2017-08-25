(ns provisdom.math.t-combinatorics
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.combinatorics :as combo]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

(ost/instrument)

(set! *warn-on-reflection* true)

;;;FACTORIALS
(deftest factorial-test
  (is= 1.0 (combo/factorial 0))
  (is= 0.9513507698668734 (combo/factorial 0.1))
  (is= 0.8862269254527579 (combo/factorial 0.5))
  (is= 0.9617658319073873 (combo/factorial 0.9))
  (is= 1.0 (combo/factorial 1))
  (is= 1.329340388179137 (combo/factorial 1.5))
  (is= 2.0 (combo/factorial 2.0))
  (is= 5.109094217170944E19 (combo/factorial 21))
  (is= 1.1240007277776077E21 (combo/factorial 22))
  (is= 2.585201673888498E22 (combo/factorial 23)))

(deftest factorial'-test
  (is= 1 (combo/factorial' 0)))

(deftest log-factorial-test
  (is= 0.0 (combo/log-factorial 0))
  (is= -0.049872441259839764 (combo/log-factorial 0.1))
  (is= -0.1207822376352452 (combo/log-factorial 0.5))
  (is= -0.03898427592308336 (combo/log-factorial 0.9))
  (is= 0.0 (combo/log-factorial 1))
  (is= 0.2846828704729192 (combo/log-factorial 1.5))
  (is= 0.6931471805599453 (combo/log-factorial 2.0))
  (is= 51.60667556776437 (combo/log-factorial 23)))

(deftest subfactorial-test
  (is= 1 (combo/subfactorial 0))
  (is= 0 (combo/subfactorial 0.1))
  (is= 0 (combo/subfactorial 0.5))
  (is= 0 (combo/subfactorial 0.9))
  (is= 0 (combo/subfactorial 1))
  (is= 0 (combo/subfactorial 1.5))
  (is= 1 (combo/subfactorial 2.0))
  (is= 895014631192902121 (combo/subfactorial 20))
  (is= 18795307255050944540N (combo/subfactorial 21))
  (is= 4.134967596111208E20 (combo/subfactorial 22))
  (is= 9.510425471055779E21 (combo/subfactorial 23)))

(defspec-test test-factorial `combo/factorial)
(defspec-test test-log-factorial `combo/log-factorial)
(defspec-test test-subfactorial `combo/subfactorial)

;;;CHOOSING
(deftest choose-k-from-n-test
  (is= 0 (combo/choose-k-from-n -1 1))
  (is= 1 (combo/choose-k-from-n 0 1))
  (is= 1 (combo/choose-k-from-n 0 0))
  (is= 1 (combo/choose-k-from-n 0 -1))
  (is= 0.9 (combo/choose-k-from-n 1 0.9))
  (is= 0 (combo/choose-k-from-n 1 0))
  (is= 1 (combo/choose-k-from-n 1 1))
  (is= 1.4 (combo/choose-k-from-n 1 1.4))
  (is= 0.2799999999999999 (combo/choose-k-from-n 2 1.4))
  (is= 4 (combo/choose-k-from-n 1.0 4))
  (is= 10 (combo/choose-k-from-n 2 5))
  (is= 1.2689769520640436E24 (combo/choose-k-from-n 12 545.0)))

(defspec-test test-choose-k-from-n `combo/choose-k-from-n)

#_(ost/unstrument)