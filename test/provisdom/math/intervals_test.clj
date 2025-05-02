(ns provisdom.math.intervals-test
  (:require
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [provisdom.test.core :refer :all]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]))

;;1 seconds

(set! *warn-on-reflection* true)

;;;INTERVAL TEST
(deftest in-interval?-test
  (with-instrument `intervals/in-interval?
    (is (spec-check intervals/in-interval?)))
  (with-instrument (st/instrumentable-syms)
    (is (intervals/in-interval? [0.0 1.0] 0.5))
    (is (intervals/in-interval? [0.0 1.0] 0.0))
    (is (intervals/in-interval? [0.0 1.0] 1.0))
    (is-not (intervals/in-interval? [0.0 1.0] -0.5))))

(deftest in-interval-roughly?-test
  (with-instrument `intervals/in-interval-roughly?
    (is (spec-check intervals/in-interval-roughly?)))
  (with-instrument (st/instrumentable-syms)
    (is (intervals/in-interval-roughly? [0.0 1.0] 0.5 0.001))
    (is (intervals/in-interval-roughly? [0.0 1.0] 0.0 0.001))
    (is (intervals/in-interval-roughly? [0.0 1.0] 1.0 0.001))
    (is (intervals/in-interval-roughly? [0.0 1.0] -0.001 0.001))
    (is (intervals/in-interval-roughly? [0.0 1.0] 1.001 0.001))
    (is-not (intervals/in-interval-roughly? [0.0 1.0] -0.5 0.001))))

(deftest bound-by-interval-test
  (with-instrument `intervals/bound-by-interval
    (is (spec-check intervals/bound-by-interval)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.5 (intervals/bound-by-interval [0.0 1.0] 0.5))
    (is= 0.0 (intervals/bound-by-interval [0.0 1.0] 0.0))
    (is= 1.0 (intervals/bound-by-interval [0.0 1.0] 1.5))
    (is= 0.0 (intervals/bound-by-interval [0.0 1.0] -0.5))))

(deftest bound-by-strict-interval-test
  (with-instrument `intervals/bound-by-strict-interval
    (is (spec-check intervals/bound-by-strict-interval)))
  (with-instrument (st/instrumentable-syms)
    (is= 0.5 (intervals/bound-by-strict-interval [0.0 1.0] 0.5))
    (is= 4.9E-324 (intervals/bound-by-strict-interval [0.0 1.0] 0.0))
    (is= (m/one- 1e-16) (intervals/bound-by-strict-interval [0.0 1.0] 1.5))
    (is= 4.9E-324 (intervals/bound-by-strict-interval [0.0 1.0] -0.5))))

;;;BOUNDS TEST
(deftest in-bounds?-test
  (with-instrument `intervals/in-bounds?
    (is (spec-check intervals/in-bounds?)))
  (with-instrument (st/instrumentable-syms)
    (is (intervals/in-bounds? {::intervals/lower       0.0
                               ::intervals/upper       1.0
                               ::intervals/open-lower? true
                               ::intervals/open-upper? false}
          0.5))
    (is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 0.0))
    (is (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 1.0))
    (is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) -0.5))))

;;;BOUNDS CONSTRUCTORS
(deftest bounds-test
  (with-instrument `intervals/bounds
    (is (spec-check intervals/bounds)))
  (with-instrument (st/instrumentable-syms)
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
      intervals/bounds-long-non-)))

(deftest vector-bounds-test
  (with-instrument `intervals/vector-bounds
    (is (spec-check intervals/vector-bounds)))
  (with-instrument (st/instrumentable-syms)
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
      (intervals/vector-bounds 2 intervals/bounds+))))

(deftest pos-definite-matrix-bounds-test
  (with-instrument `intervals/pos-definite-matrix-bounds
    (is (spec-check intervals/pos-definite-matrix-bounds)))
  (with-instrument (st/instrumentable-syms)
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
      (intervals/pos-definite-matrix-bounds 2))))

(deftest finite-pos-definite-matrix-bounds-test
  (with-instrument `intervals/finite-pos-definite-matrix-bounds
    (is (spec-check intervals/finite-pos-definite-matrix-bounds)))
  (with-instrument (st/instrumentable-syms)
    (is= [{::intervals/lower       0.0
           ::intervals/upper       m/inf+
           ::intervals/open-lower? true
           ::intervals/open-upper? true}
          {::intervals/lower       m/inf-
           ::intervals/upper       m/inf+
           ::intervals/open-lower? true
           ::intervals/open-upper? true}
          {::intervals/lower       0.0
           ::intervals/upper       m/inf+
           ::intervals/open-lower? true
           ::intervals/open-upper? true}]
      (intervals/finite-pos-definite-matrix-bounds 2))))

(deftest get-interval-test
  (with-instrument `intervals/get-interval
    (is (spec-check intervals/get-interval)))
  (with-instrument (st/instrumentable-syms)
    (is= [1.0000000000000002 2.0]
      (intervals/get-interval (intervals/bounds 1.0 2.0 true false)))))

;;;BOUNDS MANIPULATION
(deftest sort-bounds-test
  (with-instrument `intervals/sort-bounds
    (is (spec-check intervals/sort-bounds)))
  (with-instrument (st/instrumentable-syms)
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
        {::intervals/by-upper? true}))))

(deftest intersection-test
  (with-instrument `intervals/intersection
    (is (spec-check intervals/intersection)))
  (with-instrument (st/instrumentable-syms)
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
                               (intervals/bounds -3.0 1.0 false false)]))))

(deftest union-test
  (with-instrument `intervals/union
    (is (spec-check intervals/union)))
  (with-instrument (st/instrumentable-syms)
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
                        (intervals/bounds -3.0 1.0 false false)]))))

(deftest encompassing-bounds-test
  (with-instrument `intervals/encompassing-bounds
    (is (spec-check intervals/encompassing-bounds)))
  (with-instrument (st/instrumentable-syms)
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
      (intervals/encompassing-bounds
        [(intervals/bounds -3.0 -1.0 true true)
         (intervals/bounds -3.0 1.0 false false)]))))
