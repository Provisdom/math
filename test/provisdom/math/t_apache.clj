(ns provisdom.math.t-apache
  (:require [clojure.test :refer :all]
            [provisdom.test.core :refer :all]
            [provisdom.math.core :as m]
            [provisdom.math.apache :as ap]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.apache-matrix :as apache-mx]))

(set! *warn-on-reflection* true)

(ost/instrument)

(def fgb0 {::ap/root-f identity ::ap/guess -5.0 ::ap/bounds [-100.0 100.0]})
(def fgb1 {::ap/root-f (fn [v] (- (* v v) (* 20 v))) ::ap/guess -5.0 ::ap/bounds [-10.0 10.0]})
(def fgb2 {::ap/root-f (fn [v] (- (m/exp v) (* 2000 v))) ::ap/guess -3.0 ::ap/bounds [-5.0 5.0]})
(def fgb3 {::ap/root-f (comp inc m/sq) ::ap/guess -5.0 ::ap/bounds [-10.0 10.0]})

(deftest root-solver-test
  ;;test 0
  (is= 0.0 (ap/root-solver fgb0))
  ;;test 1 -- 0.0
  (is= -4.992965308883406E-7 (ap/root-solver fgb1 {::ap/root-solver :brent}))
  (is= -2.980232238769531E-7 (ap/root-solver fgb1 {::ap/root-solver :bisection}))
  (is= 0.0 (ap/root-solver fgb1 {::ap/root-solver :bracketing-brent}))
  (is= 0.0 (ap/root-solver fgb1 {::ap/root-solver :illinois}))
  (is= 0.0 (ap/root-solver fgb1 {::ap/root-solver :muller}))
  (is= 2.0E-6 (ap/root-solver fgb1 {::ap/root-solver :muller2}))
  (is= -2.7947626310537565E-16 (ap/root-solver fgb1 {::ap/root-solver :pegasus}))
  (is= 4.441642203583786E-17 (ap/root-solver fgb1 {::ap/root-solver :regula}))
  (is= 0.0 (ap/root-solver fgb1 {::ap/root-solver :ridders}))
  (is= 1.1992433948198355E-15 (ap/root-solver fgb1 {::ap/root-solver :secant}))
  (is= 7.812805199624811E-4 (ap/root-solver fgb1 {::ap/root-solver :newton-raphson}))
  ;;test 2 -- 5.002501876668296E-4
  (is= 5.002501514765274E-4 (ap/root-solver fgb2 {::ap/root-solver :brent}))
  (is= 5.003809928894043E-4 (ap/root-solver fgb2 {::ap/root-solver :bisection}))
  (is= 5.002502003152998E-4 (ap/root-solver fgb2 {::ap/root-solver :bracketing-brent}))
  (is= 5.002501846562727E-4 (ap/root-solver fgb2 {::ap/root-solver :illinois}))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 {::ap/root-solver :muller}))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 {::ap/root-solver :muller2}))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 {::ap/root-solver :pegasus}))
  (is= 5.002501876668299E-4 (ap/root-solver fgb2 {::ap/root-solver :regula}))
  (is= 5.002501876668296E-4 (ap/root-solver fgb2 {::ap/root-solver :ridders}))
  (is= 5.002501876675032E-4 (ap/root-solver fgb2 {::ap/root-solver :secant}))
  (is= 5.04165457653621E-4 (ap/root-solver fgb2 {::ap/root-solver :newton-raphson}))
  ;;test 3
  (is (instance? Exception (ap/root-solver fgb3))))

(comment
  (deftest matrix-solve-iterative-test
    (is= [5.999999999999998 1.9999999999999984]
         (ap/matrix-solve-iterative (apache-mx/apache-matrix [[1.0 0.5] [0.5 3.0]])
                                    [7.0 9.0]
                                    {::ap/iterative-solver ::ap/symm-lq}))
    (is= nil
         (ap/matrix-solve-iterative (apache-mx/apache-matrix [[1.0 0.5] [2.0 4.0]])
                                    [7.0 9.0]
                                    {::ap/iterative-solver ::ap/symm-lq})))
  )

(defn- constraints-fn [[a b]] [(- a b 0.5) (- (m/sq a) (m/sq b) 0.5)])
(def n-cons 2)
(defn- jacobian-fn [[a b]] [[1.0 -1] [(* 2 a) (* 2 b)]])
(def guesses [1.0 1.0])

(deftest nonlinear-least-squares-test
  (is= {::ap/errors [0.0 -6.801038283654748E-7]
        ::ap/point  [0.7500006801038284 0.25000068010382837]}
       (ap/nonlinear-least-squares
         {::ap/constraints-fn constraints-fn
          ::ap/n-cons         n-cons
          ::ap/jacobian-fn    jacobian-fn
          ::ap/guesses        guesses})))

(deftest apache-test
  (root-solver-test)
  ;(matrix-solve-iterative-test)
  (nonlinear-least-squares-test))

(defspec-test test-root-solver `ap/root-solver)
;(defspec-test test-matrix-solve-iterative `ap/matrix-solve-iterative)
(defspec-test test-nonlinear-least-squares-test `ap/nonlinear-least-squares)

#_(ost/unstrument)