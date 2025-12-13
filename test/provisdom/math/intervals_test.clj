(ns provisdom.math.intervals-test
  (:require
    [clojure.test :refer :all]
    [provisdom.test.core :as t]
    [provisdom.math.core :as m]
    [provisdom.math.intervals :as intervals]))
;;1 seconds

(set! *warn-on-reflection* true)

;;;INTERVAL TEST
(deftest in-interval?-test
  (t/with-instrument `intervals/in-interval?
    (t/is (t/spec-check intervals/in-interval?)))
  (t/with-instrument :all
    (t/is (intervals/in-interval? [0.0 1.0] 0.5))
    (t/is (intervals/in-interval? [0.0 1.0] 0.0))
    (t/is (intervals/in-interval? [0.0 1.0] 1.0))
    (t/is-not (intervals/in-interval? [0.0 1.0] -0.5))))

(deftest in-interval-roughly?-test
  (t/with-instrument `intervals/in-interval-roughly?
    (t/is (t/spec-check intervals/in-interval-roughly?)))
  (t/with-instrument :all
    (t/is (intervals/in-interval-roughly? [0.0 1.0] 0.5 0.001))
    (t/is (intervals/in-interval-roughly? [0.0 1.0] 0.0 0.001))
    (t/is (intervals/in-interval-roughly? [0.0 1.0] 1.0 0.001))
    (t/is (intervals/in-interval-roughly? [0.0 1.0] -0.001 0.001))
    (t/is (intervals/in-interval-roughly? [0.0 1.0] 1.001 0.001))
    (t/is-not (intervals/in-interval-roughly? [0.0 1.0] -0.5 0.001))))

(deftest bound-by-interval-test
  (t/with-instrument `intervals/bound-by-interval
    (t/is (t/spec-check intervals/bound-by-interval)))
  (t/with-instrument :all
    (t/is= 0.5 (intervals/bound-by-interval [0.0 1.0] 0.5))
    (t/is= 0.0 (intervals/bound-by-interval [0.0 1.0] 0.0))
    (t/is= 1.0 (intervals/bound-by-interval [0.0 1.0] 1.5))
    (t/is= 0.0 (intervals/bound-by-interval [0.0 1.0] -0.5))))

(deftest bound-by-strict-interval-test
  (t/with-instrument `intervals/bound-by-strict-interval
    (t/is (t/spec-check intervals/bound-by-strict-interval)))
  (t/with-instrument :all
    (t/is= 0.5 (intervals/bound-by-strict-interval [0.0 1.0] 0.5))
    (t/is= 4.9E-324 (intervals/bound-by-strict-interval [0.0 1.0] 0.0))
    (t/is= (m/one- 1e-16) (intervals/bound-by-strict-interval [0.0 1.0] 1.5))
    (t/is= 4.9E-324 (intervals/bound-by-strict-interval [0.0 1.0] -0.5))))

;;;BOUNDS TEST
(deftest in-bounds?-test
  (t/with-instrument `intervals/in-bounds?
    (t/is (t/spec-check intervals/in-bounds?)))
  (t/with-instrument :all
    (t/is (intervals/in-bounds? {::intervals/lower       0.0
                               ::intervals/upper       1.0
                               ::intervals/open-lower? true
                               ::intervals/open-upper? false}
          0.5))
    (t/is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 0.0))
    (t/is (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) 1.0))
    (t/is-not (intervals/in-bounds? (intervals/bounds 0.0 1.0 true false) -0.5))))

;;;BOUNDS CONSTRUCTORS
(deftest bounds-test
  (t/with-instrument `intervals/bounds
    (t/is (t/spec-check intervals/bounds)))
  (t/with-instrument :all
    (t/is= {::intervals/lower       0.0
          ::intervals/upper       1.0
          ::intervals/open-lower? true
          ::intervals/open-upper? false}
      (intervals/bounds 0.0 1.0 true false))
    (t/is= {::intervals/lower       m/inf-
          ::intervals/upper       m/inf+
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      (intervals/bounds))
    (t/is= {::intervals/lower       -1.0
          ::intervals/upper       1.0
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      (intervals/bounds -1.0 1.0))
    (t/is= {::intervals/lower       -1.0
          ::intervals/upper       1.0
          ::intervals/open-lower? true
          ::intervals/open-upper? true}
      (intervals/bounds -1.0 1.0 true true))
    (t/is= {::intervals/lower       m/inf-
          ::intervals/upper       m/inf+
          ::intervals/open-lower? true
          ::intervals/open-upper? true}
      intervals/bounds-finite)
    (t/is= {::intervals/lower       0.0
          ::intervals/upper       m/inf+
          ::intervals/open-lower? true
          ::intervals/open-upper? false}
      intervals/bounds+)
    (t/is= {::intervals/lower       0.0
          ::intervals/upper       m/inf+
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      intervals/bounds-non-)
    (t/is= {::intervals/lower       0.0
          ::intervals/upper       1.0
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      intervals/bounds-prob)
    (t/is= {::intervals/lower       0.0
          ::intervals/upper       1.0
          ::intervals/open-lower? true
          ::intervals/open-upper? true}
      intervals/bounds-open-prob)
    (t/is= {::intervals/lower       0
          ::intervals/upper       m/max-long
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      intervals/bounds-long-non-)))

(deftest vector-bounds-test
  (t/with-instrument `intervals/vector-bounds
    (t/is (t/spec-check intervals/vector-bounds)))
  (t/with-instrument :all
    (t/is= [{::intervals/lower       m/inf-
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
    (t/is= [{::intervals/lower       0.0
           ::intervals/upper       m/inf+
           ::intervals/open-lower? true
           ::intervals/open-upper? false}
          {::intervals/lower       0.0
           ::intervals/upper       m/inf+
           ::intervals/open-lower? true
           ::intervals/open-upper? false}]
      (intervals/vector-bounds 2 intervals/bounds+))))

(deftest pos-definite-matrix-bounds-test
  (t/with-instrument `intervals/pos-definite-matrix-bounds
    (t/is (t/spec-check intervals/pos-definite-matrix-bounds)))
  (t/with-instrument :all
    (t/is= [{::intervals/lower       0.0
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
  (t/with-instrument `intervals/finite-pos-definite-matrix-bounds
    (t/is (t/spec-check intervals/finite-pos-definite-matrix-bounds)))
  (t/with-instrument :all
    (t/is= [{::intervals/lower       0.0
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
  (t/with-instrument `intervals/get-interval
    (t/is (t/spec-check intervals/get-interval)))
  (t/with-instrument :all
    (t/is= [1.0000000000000002 2.0]
      (intervals/get-interval (intervals/bounds 1.0 2.0 true false)))))

;;;BOUNDS MANIPULATION
(deftest sort-bounds-test
  (t/with-instrument `intervals/sort-bounds
    (t/is (t/spec-check intervals/sort-bounds)))
  (t/with-instrument :all
    (t/is= [{::intervals/lower       -2.0
           ::intervals/open-lower? true
           ::intervals/open-upper? true
           ::intervals/upper       0.0}
          {::intervals/lower       -1.0
           ::intervals/open-lower? false
           ::intervals/open-upper? false
           ::intervals/upper       1.0}]
      (intervals/sort-bounds [(intervals/bounds -1.0 1.0)
                              (intervals/bounds -2.0 0.0 true true)]))
    (t/is= [{::intervals/lower       -1.0
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
  (t/with-instrument `intervals/intersection
    (t/is (t/spec-check intervals/intersection)))
  (t/with-instrument :all
    (t/is= {::intervals/lower       -1.0
          ::intervals/upper       0.0
          ::intervals/open-lower? false
          ::intervals/open-upper? true}
      (intervals/intersection [(intervals/bounds -1.0 1.0)
                               (intervals/bounds -2.0 0.0 true true)]))
    (t/is= nil
      (intervals/intersection [(intervals/bounds -3.0 -1.0 true true)
                               (intervals/bounds 1.0 3.0)]))
    (t/is= {::intervals/lower       -3.0
          ::intervals/upper       -1.0
          ::intervals/open-lower? false
          ::intervals/open-upper? true}
      (intervals/intersection [(intervals/bounds -3.0 -1.0 true true)
                               (intervals/bounds -3.0 1.0 false false)]))))

(deftest union-test
  (t/with-instrument `intervals/union
    (t/is (t/spec-check intervals/union)))
  (t/with-instrument :all
    (t/is= [{::intervals/lower       -1.0
           ::intervals/open-lower? false
           ::intervals/open-upper? true
           ::intervals/upper       0.0}]
      (intervals/union [(intervals/bounds -1.0 1.0)
                        (intervals/bounds -2.0 0.0 true true)]))
    (t/is= [{::intervals/lower       -3.0
           ::intervals/open-lower? true
           ::intervals/open-upper? true
           ::intervals/upper       -1.0}
          {::intervals/lower       1.0
           ::intervals/open-lower? false
           ::intervals/open-upper? false
           ::intervals/upper       3.0}]
      (intervals/union [(intervals/bounds -3.0 -1.0 true true)
                        (intervals/bounds 1.0 3.0)]))
    (t/is= [{::intervals/lower       -3.0
           ::intervals/open-lower? false
           ::intervals/open-upper? true
           ::intervals/upper       -1.0}]
      (intervals/union [(intervals/bounds -3.0 -1.0 true true)
                        (intervals/bounds -3.0 1.0 false false)]))))

(deftest encompassing-bounds-test
  (t/with-instrument `intervals/encompassing-bounds
    (t/is (t/spec-check intervals/encompassing-bounds)))
  (t/with-instrument :all
    (t/is= {::intervals/lower       -2.0
          ::intervals/upper       1.0
          ::intervals/open-lower? true
          ::intervals/open-upper? false}
      (intervals/encompassing-bounds [(intervals/bounds -1.0 1.0)
                                      (intervals/bounds -2.0 0.0 true true)]))
    (t/is= {::intervals/lower       -3.0
          ::intervals/upper       3.0
          ::intervals/open-lower? true
          ::intervals/open-upper? false}
      (intervals/encompassing-bounds [(intervals/bounds -3.0 -1.0 true true)
                                      (intervals/bounds 1.0 3.0)]))
    (t/is= {::intervals/lower       -3.0
          ::intervals/upper       1.0
          ::intervals/open-lower? false
          ::intervals/open-upper? false}
      (intervals/encompassing-bounds
        [(intervals/bounds -3.0 -1.0 true true)
         (intervals/bounds -3.0 1.0 false false)]))))
