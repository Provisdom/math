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

(deftest interval-width-test
  (t/with-instrument `intervals/interval-width
    (t/is (t/spec-check intervals/interval-width)))
  (t/with-instrument :all
    (t/is= 10.0 (intervals/interval-width [0.0 10.0]))
    (t/is= 10.0 (intervals/interval-width [-5.0 5.0]))
    (t/is= 0.0 (intervals/interval-width [3.0 3.0]))))

(deftest interval-midpoint-test
  (t/with-instrument `intervals/interval-midpoint
    (t/is (t/spec-check intervals/interval-midpoint)))
  (t/with-instrument :all
    (t/is= 5.0 (intervals/interval-midpoint [0.0 10.0]))
    (t/is= 0.0 (intervals/interval-midpoint [-5.0 5.0]))
    (t/is= 5.0 (intervals/interval-midpoint [2.0 8.0]))))

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

(deftest bound-by-bounds-test
  (t/with-instrument `intervals/bound-by-bounds
    (t/is (t/spec-check intervals/bound-by-bounds)))
  (t/with-instrument :all
    (t/is= 5.0 (intervals/bound-by-bounds (intervals/bounds 0.0 10.0) 5.0))
    (t/is= 10.0 (intervals/bound-by-bounds (intervals/bounds 0.0 10.0) 15.0))
    (t/is= 0.0 (intervals/bound-by-bounds (intervals/bounds 0.0 10.0) -5.0))
    ;; Open bounds
    (t/is= 4.9E-324 (intervals/bound-by-bounds (intervals/bounds 0.0 10.0 true false) -1.0))
    (t/is= (m/next-down 10.0) (intervals/bound-by-bounds (intervals/bounds 0.0 10.0 false true) 15.0))))

(deftest bounds-width-test
  (t/with-instrument `intervals/bounds-width
    (t/is (t/spec-check intervals/bounds-width)))
  (t/with-instrument :all
    (t/is= 10.0 (intervals/bounds-width (intervals/bounds 0.0 10.0)))
    (t/is= 10.0 (intervals/bounds-width (intervals/bounds -5.0 5.0)))))

(deftest bounds-midpoint-test
  (t/with-instrument `intervals/bounds-midpoint
    (t/is (t/spec-check intervals/bounds-midpoint)))
  (t/with-instrument :all
    (t/is= 5.0 (intervals/bounds-midpoint (intervals/bounds 0.0 10.0)))
    (t/is= 0.0 (intervals/bounds-midpoint (intervals/bounds -5.0 5.0)))))

(deftest overlaps?-test
  (t/with-instrument `intervals/overlaps?
    (t/is (t/spec-check intervals/overlaps?)))
  (t/with-instrument :all
    (t/is (intervals/overlaps? (intervals/bounds 0.0 5.0) (intervals/bounds 3.0 8.0)))
    (t/is-not (intervals/overlaps? (intervals/bounds 0.0 5.0) (intervals/bounds 6.0 10.0)))
    (t/is (intervals/overlaps? (intervals/bounds 0.0 5.0) (intervals/bounds 5.0 10.0)))
    ;; Open at touching point - no overlap
    (t/is-not (intervals/overlaps? (intervals/bounds 0.0 5.0 false true) (intervals/bounds 5.0 10.0 true false)))))

(deftest contains-bounds?-test
  (t/with-instrument `intervals/contains-bounds?
    (t/is (t/spec-check intervals/contains-bounds?)))
  (t/with-instrument :all
    (t/is (intervals/contains-bounds? (intervals/bounds 0.0 10.0) (intervals/bounds 2.0 8.0)))
    (t/is-not (intervals/contains-bounds? (intervals/bounds 0.0 10.0) (intervals/bounds -1.0 8.0)))
    (t/is-not (intervals/contains-bounds? (intervals/bounds 0.0 10.0) (intervals/bounds 2.0 12.0)))
    ;; Same bounds - contains itself
    (t/is (intervals/contains-bounds? (intervals/bounds 0.0 10.0) (intervals/bounds 0.0 10.0)))
    ;; Open outer doesn't contain closed inner at same endpoint
    (t/is-not (intervals/contains-bounds? (intervals/bounds 0.0 10.0 true true) (intervals/bounds 0.0 10.0)))
    ;; Closed outer contains open inner at same endpoint
    (t/is (intervals/contains-bounds? (intervals/bounds 0.0 10.0) (intervals/bounds 0.0 10.0 true true)))))

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
(deftest min-bound-test
  (t/with-instrument `intervals/min-bound
    (t/is (t/spec-check intervals/min-bound)))
  (t/with-instrument :all
    (t/is= [3 true] (intervals/min-bound [[5 false] [3 true] [7 false]]))
    (t/is= [3 false] (intervals/min-bound [[3 false] [3 true]]))
    (t/is= [3 true] (intervals/min-bound [[3 true] [3 true]]))
    (t/is= [m/inf+ false] (intervals/min-bound []))))

(deftest max-bound-test
  (t/with-instrument `intervals/max-bound
    (t/is (t/spec-check intervals/max-bound)))
  (t/with-instrument :all
    (t/is= [7 false] (intervals/max-bound [[5 false] [3 true] [7 false]]))
    (t/is= [7 false] (intervals/max-bound [[7 false] [7 true]]))
    (t/is= [7 true] (intervals/max-bound [[7 true] [7 true]]))
    (t/is= [m/inf- false] (intervals/max-bound []))))

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
    ;; Overlapping bounds merge into encompassing bounds
    (t/is= [{::intervals/lower       -2.0
             ::intervals/open-lower? true
             ::intervals/open-upper? false
             ::intervals/upper       1.0}]
      (intervals/union [(intervals/bounds -1.0 1.0)
                        (intervals/bounds -2.0 0.0 true true)]))
    ;; Non-overlapping bounds stay separate
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
    ;; Overlapping at same lower bound merge
    (t/is= [{::intervals/lower       -3.0
             ::intervals/open-lower? false
             ::intervals/open-upper? false
             ::intervals/upper       1.0}]
      (intervals/union [(intervals/bounds -3.0 -1.0 true true)
                        (intervals/bounds -3.0 1.0 false false)]))
    ;; Empty input returns empty
    (t/is= [] (intervals/union []))
    ;; Single bounds returns itself
    (t/is= [(intervals/bounds 0 5)]
      (intervals/union [(intervals/bounds 0 5)]))
    ;; Multiple overlapping bounds merge progressively
    (t/is= [{::intervals/lower       0.0
             ::intervals/open-lower? false
             ::intervals/open-upper? false
             ::intervals/upper       8.0}]
      (intervals/union [(intervals/bounds 0.0 3.0)
                        (intervals/bounds 2.0 5.0)
                        (intervals/bounds 4.0 8.0)]))))

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
