(ns provisdom.math.t-arrays
  (:require [clojure.test :refer :all]
            [criterium.core :as perf]
            [provisdom.test.core :refer :all]
            [provisdom.math.arrays :as a]
            [clojure.spec.test.alpha :as sta]
            [orchestra.spec.test :as st]))

(deftest array?-test
  (is (a/array? (a/jagged-2D-array :long [[1 2 3]])))
  (is-not (a/array? [1]))
  (is-not (a/array? 1.0))
  (is-not (a/array? nil)))

(deftest array->seq-test
  (is= (a/array->seq (a/avec [1 2 3])) '(1.0 2.0 3.0))
  (is= (a/array->seq (a/jagged-2D-array :long [[1 2 3] [4 5 6]])) '((1 2 3) (4 5 6)))
  (is= (a/array->seq nil) nil)
  (is= (a/array->seq 1) 1))

(deftest type->obj-test
  (is= (a/type->obj :long) Long/TYPE)
  (is= (a/type->obj :boolean) Boolean/TYPE)
  (is= (a/type->obj :char) Character/TYPE)
  (is= (a/type->obj :double) Double/TYPE)
  (is= (a/type->obj nil) Double/TYPE))

(deftest block-array-one-dim-test
  (let [arr (a/block-array :long 3)]
    (is (a/array? arr))
    (is= 3 (count arr))
    (is (instance? Long (aget arr 0)))))

(deftest block-array-two-dim-test
  (let [arr (a/block-array :double 2 2)
        arr1 (aget arr 0)
        arr2 (aget arr 1)]
    (is (a/array? arr))
    (is= 2 (count arr))
    (is= 2 (count arr1))
    (is= 2 (count arr2))
    (is (instance? Double (aget arr1 0)))
    (is (instance? Double (aget arr2 0)))))

(deftest jagged-2d-array-test
  (let [arr (a/jagged-2D-array :char [[\a \b \c]])]
    (is (a/array? arr))
    (is= 1 (count arr))
    (is (a/array? (aget arr 0)))
    (is (instance? Character (aget arr 0 0)))
    (is= \a (aget arr 0 0)))
  (is (thrown? Exception (a/jagged-2D-array :char [[[]]])))
  (is (thrown? AssertionError (a/jagged-2D-array :char []))))

(deftest jagged-3d-array-test
  (let [arr (a/jagged-3D-array :double [[[1.0 2.0 3.0] [4.0]]])]
    (is (a/array? arr))
    (is= 1 (count arr))
    (is (a/array? (aget arr 0)))
    (is (instance? Double (aget arr 0 0 0)))
    (is= 1.0 (aget arr 0 0 0)))
  (is (thrown? Exception (a/jagged-3D-array :char [[[[]]]])))
  (is (thrown? AssertionError (a/jagged-3D-array :char []))))

(deftest aset!-test
  (let [og (a/avec [1.0 2.0 3.0])
        arr (a/block-array :double 3)]
    (is= '(0.0 0.0 0.0) (a/array->seq arr))
    (is= '(1.0 2.0 3.0) (a/array->seq (a/aset! arr og))))
  )