(ns provisdom.math.intervals-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.intervals :as intervals]
    [provisdom.math.core :as m]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]))

;;3 seconds

(set! *warn-on-reflection* true)

(ost/instrument)

;;;INTERVAL TEST
(deftest in-interval?-test
  (is (spec-check intervals/in-interval?))
  (is (intervals/in-interval? [0.0 1.0] 0.5))
  (is (intervals/in-interval? [0.0 1.0] 0.0))
  (is (intervals/in-interval? [0.0 1.0] 1.0))
  (is-not (intervals/in-interval? [0.0 1.0] -0.5)))

(deftest in-interval-roughly?-test
  (is (spec-check intervals/in-interval-roughly?))
  (is (intervals/in-interval-roughly? [0.0 1.0] 0.5 0.001))
  (is (intervals/in-interval-roughly? [0.0 1.0] 0.0 0.001))
  (is (intervals/in-interval-roughly? [0.0 1.0] 1.0 0.001))
  (is (intervals/in-interval-roughly? [0.0 1.0] -0.001 0.001))
  (is (intervals/in-interval-roughly? [0.0 1.0] 1.001 0.001))
  (is-not (intervals/in-interval-roughly? [0.0 1.0] -0.5 0.001)))

;;;BOUNDS TEST
(deftest in-bounds?-test
  (is (spec-check intervals/in-bounds?))
  (is (intervals/in-bounds? {::intervals/lower       0.0
                             ::intervals/upper       1.0
                             ::intervals/open-lower? true
                             ::intervals/open-upper? false}
                            0.5))
  (is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 0.0))
  (is (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 1.0))
  (is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) -0.5)))

;;;BOUNDS CONSTRUCTORS
(deftest bounds-test
  (is (spec-check intervals/bounds))
  (is= {::intervals/lower       0.0
        ::intervals/upper       1.0
        ::intervals/open-lower? true
        ::intervals/open-upper? false}
       (intervals/bounds 0.0 1.0 true false))
  (is= {::intervals/lower       m/inf-
        ::intervals/upper       m/inf+
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       (intervals/bounds))
  (is= {::intervals/lower       -1.0
        ::intervals/upper       1.0
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       (intervals/bounds -1.0 1.0))
  (is= {::intervals/lower       -1.0
        ::intervals/upper       1.0
        ::intervals/open-lower? true
        ::intervals/open-upper? true}
       (intervals/bounds -1.0 1.0 true true))
  (is= {::intervals/lower       m/inf-
        ::intervals/upper       m/inf+
        ::intervals/open-lower? true
        ::intervals/open-upper? true}
       intervals/bounds-finite)
  (is= {::intervals/lower       0.0
        ::intervals/upper       m/inf+
        ::intervals/open-lower? true
        ::intervals/open-upper? false}
       intervals/bounds+)
  (is= {::intervals/lower       0.0
        ::intervals/upper       m/inf+
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       intervals/bounds-non-)
  (is= {::intervals/lower       0.0
        ::intervals/upper       1.0
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       intervals/bounds-prob)
  (is= {::intervals/lower       0.0
        ::intervals/upper       1.0
        ::intervals/open-lower? true
        ::intervals/open-upper? true}
       intervals/bounds-open-prob)
  (is= {::intervals/lower       0
        ::intervals/upper       m/max-long
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       intervals/bounds-long-non-))

(deftest vector-bounds-test
  (is (spec-check intervals/vector-bounds))
  (is= [{::intervals/lower       m/inf-
         ::intervals/upper       m/inf+
         ::intervals/open-lower? false
         ::intervals/open-upper? false}
        {::intervals/lower       m/inf-
         ::intervals/upper       m/inf+
         ::intervals/open-lower? false
         ::intervals/open-upper? false}
        {::intervals/lower       m/inf-
         ::intervals/upper       m/inf+
         ::intervals/open-lower? false
         ::intervals/open-upper? false}]
       (intervals/vector-bounds 3))
  (is= [{::intervals/lower       0.0
         ::intervals/upper       m/inf+
         ::intervals/open-lower? true
         ::intervals/open-upper? false}
        {::intervals/lower       0.0
         ::intervals/upper       m/inf+
         ::intervals/open-lower? true
         ::intervals/open-upper? false}]
       (intervals/vector-bounds 2 intervals/bounds+)))

(deftest positive-definite-matrix-bounds-test
  (is (spec-check intervals/positive-definite-matrix-bounds))
  (is= [{::intervals/lower       0.0
         ::intervals/upper       m/inf+
         ::intervals/open-lower? true
         ::intervals/open-upper? false}
        {::intervals/lower       m/inf-
         ::intervals/upper       m/inf+
         ::intervals/open-lower? false
         ::intervals/open-upper? false}
        {::intervals/lower       0.0
         ::intervals/upper       m/inf+
         ::intervals/open-lower? true
         ::intervals/open-upper? false}]
       (intervals/positive-definite-matrix-bounds 2)))

(deftest get-interval-test
  (is (spec-check intervals/get-interval))
  (is= [1.0 2.0]
       (intervals/get-interval (intervals/bounds 1.0 2.0 true false))))

;;;BOUNDS MANIPULATION
(deftest sort-bounds-test
  (is (spec-check intervals/sort-bounds))
  (is= [{::intervals/lower       -2.0
         ::intervals/open-lower? true
         ::intervals/open-upper? true
         ::intervals/upper       0.0}
        {::intervals/lower       -1.0
         ::intervals/open-lower? false
         ::intervals/open-upper? false
         ::intervals/upper       1.0}]
       (intervals/sort-bounds [(intervals/bounds -1.0 1.0)
                               (intervals/bounds -2.0 0.0 true true)]))
  (is= [{::intervals/lower       -1.0
         ::intervals/open-lower? false
         ::intervals/open-upper? false
         ::intervals/upper       1.0}
        {::intervals/lower       -2.0
         ::intervals/open-lower? true
         ::intervals/open-upper? true
         ::intervals/upper       0.0}]
       (intervals/sort-bounds [(intervals/bounds -1.0 1.0)
                               (intervals/bounds -2.0 0.0 true true)]
                              {::intervals/by-upper? true})))

(deftest intersection-test
  (is (spec-check intervals/intersection))
  (is= {::intervals/lower       -1.0
        ::intervals/upper       0.0
        ::intervals/open-lower? false
        ::intervals/open-upper? true}
       (intervals/intersection [(intervals/bounds -1.0 1.0)
                                (intervals/bounds -2.0 0.0 true true)]))
  (is= nil
       (intervals/intersection [(intervals/bounds -3.0 -1.0 true true)
                                (intervals/bounds 1.0 3.0)]))
  (is= {::intervals/lower       -3.0
        ::intervals/upper       -1.0
        ::intervals/open-lower? false
        ::intervals/open-upper? true}
       (intervals/intersection [(intervals/bounds -3.0 -1.0 true true)
                                (intervals/bounds -3.0 1.0 false false)])))

(deftest union-test
  (is (spec-check intervals/union))
  (is= [{::intervals/lower       -1.0
         ::intervals/open-lower? false
         ::intervals/open-upper? true
         ::intervals/upper       0.0}]
       (intervals/union [(intervals/bounds -1.0 1.0)
                         (intervals/bounds -2.0 0.0 true true)]))
  (is= [{::intervals/lower       -3.0
         ::intervals/open-lower? true
         ::intervals/open-upper? true
         ::intervals/upper       -1.0}
        {::intervals/lower       1.0
         ::intervals/open-lower? false
         ::intervals/open-upper? false
         ::intervals/upper       3.0}]
       (intervals/union [(intervals/bounds -3.0 -1.0 true true)
                         (intervals/bounds 1.0 3.0)]))
  (is= [{::intervals/lower       -3.0
         ::intervals/open-lower? false
         ::intervals/open-upper? true
         ::intervals/upper       -1.0}]
       (intervals/union [(intervals/bounds -3.0 -1.0 true true)
                         (intervals/bounds -3.0 1.0 false false)])))

(deftest encompassing-bounds-test
  (is (spec-check intervals/encompassing-bounds))
  (is= {::intervals/lower       -2.0
        ::intervals/upper       1.0
        ::intervals/open-lower? true
        ::intervals/open-upper? false}
       (intervals/encompassing-bounds [(intervals/bounds -1.0 1.0)
                                       (intervals/bounds -2.0 0.0 true true)]))
  (is= {::intervals/lower       -3.0
        ::intervals/upper       3.0
        ::intervals/open-lower? true
        ::intervals/open-upper? false}
       (intervals/encompassing-bounds [(intervals/bounds -3.0 -1.0 true true)
                                       (intervals/bounds 1.0 3.0)]))
  (is= {::intervals/lower       -3.0
        ::intervals/upper       1.0
        ::intervals/open-lower? false
        ::intervals/open-upper? false}
       (intervals/encompassing-bounds [(intervals/bounds -3.0 -1.0 true true)
                                       (intervals/bounds -3.0 1.0 false false)])))