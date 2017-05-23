(ns provisdom.math.t-apache
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :as m]
            [provisdom.math.apache :as ap]
            [clojure.spec.test :as st]))

(st/instrument)

(def fgb0 [identity -5.0 [-100.0 100.0]])
(def fgb1 [(fn [v] (- (* v v) (* 20 v))) -5.0 [-10.0 10.0]])
(def fgb2 [(fn [v] (- (m/exp v) (* 2000 v))) -3.0 [-5.0 5.0]])
(def fgb3 [(comp inc m/sq) -5.0 [-10.0 10.0]])
(deftest root-solver-test
  ;;test 0
  (is= 0.0 (ap/root-solver fgb0))
  ;;test 1 -- 0.0
  (is= -4.992965308883406E-7 (ap/root-solver fgb1 ::ap/root-solver :brent))
  (is= -2.980232238769531E-7 (ap/root-solver fgb1 ::ap/root-solver :bisection))
  (is= 0.0 (ap/root-solver fgb1 ::ap/root-solver :bracketing-brent))
  (is= 0.0 (ap/root-solver fgb1 ::ap/root-solver :illinois))
  (is= 0.0 (ap/root-solver fgb1 ::ap/root-solver :muller))
  (is= 2.0E-6 (ap/root-solver fgb1 ::ap/root-solver :muller2))
  (is= -2.7947626310537565E-16 (ap/root-solver fgb1 ::ap/root-solver :pegasus))
  (is= 4.441642203583786E-17 (ap/root-solver fgb1 ::ap/root-solver :regula))
  (is= 0.0 (ap/root-solver fgb1 ::ap/root-solver :ridders))
  (is= 1.1992433948198355E-15 (ap/root-solver fgb1 ::ap/root-solver :secant))
  (is= 7.812805199624811E-4 (ap/root-solver fgb1 ::ap/root-solver :newton-raphson))
  ;;test 2 -- 5.002501876668296E-4
  (is= 5.002501514765274E-4 (ap/root-solver fgb2 ::ap/root-solver :brent))
  (is= 5.003809928894043E-4 (ap/root-solver fgb2 ::ap/root-solver :bisection))
  (is= 5.002502003152998E-4 (ap/root-solver fgb2 ::ap/root-solver :bracketing-brent))
  (is= 5.002501846562727E-4 (ap/root-solver fgb2 ::ap/root-solver :illinois))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 ::ap/root-solver :muller))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 ::ap/root-solver :muller2))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 ::ap/root-solver :pegasus))
  (is= 5.002501876668299E-4 (ap/root-solver fgb2 ::ap/root-solver :regula))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 ::ap/root-solver :ridders))
  (is= 5.002501876675032E-4 (ap/root-solver fgb2 ::ap/root-solver :secant))
  (is= 5.04165457653621E-4 (ap/root-solver fgb2 ::ap/root-solver :newton-raphson))
  ;;test 3
  (is (instance? Exception (ap/root-solver fgb3))))

(deftest apache-test
  (root-solver-test))

(defspec-test test-root-solver `ap/root-solver)

#_(st/unstrument)