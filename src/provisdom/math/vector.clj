(ns provisdom.math.vector
  (:refer-clojure :exclude [vector?])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.random2 :as random]
            [provisdom.math.tensor :as tensor]))

(set! *warn-on-reflection* true)

(declare)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::size (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))
(s/def ::number ::m/number)
(s/def ::numbers ::tensor/numbers)
(s/def ::vector ::tensor/tensor1D)
(s/def ::vector-2D (s/with-gen (s/coll-of ::number :kind clojure.core/vector? :into [] :min-count 2 :max-count 2)
                               #(gen/vector (s/gen ::number) 2)))
(s/def ::vector-3D (s/with-gen (s/coll-of ::number :kind clojure.core/vector? :into [] :min-count 3 :max-count 3)
                               #(gen/vector (s/gen ::number) 3)))
(s/def ::vector-num (s/with-gen (s/coll-of ::m/num :kind clojure.core/vector? :into [])
                                #(gen/vector (s/gen ::m/num) 0 mdl)))
(s/def ::vector-finite (s/with-gen (s/coll-of ::m/finite :kind clojure.core/vector? :into [])
                                   #(gen/vector (s/gen ::m/finite) 0 mdl)))
(s/def ::sparse-vector
  (s/with-gen (s/coll-of (s/tuple ::m/int-non- ::number))
              #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                                    (gen/large-integer* {:min 0 :max mdl}))
                         (fn [[i j]] (gen/vector
                                       (gen/tuple (gen/large-integer* {:min 0 :max (max 0 (dec (max i j)))})
                                                  (s/gen ::number))
                                       (min i j))))))

;(s/def ::accu ::tensor/accu)
(s/def ::index ::tensor/index)

;;;VECTOR TYPES
(defn numbers?
  "Returns true if the parameter is a collection of numbers (i.e., dimensionality is 1 and contains numbers only)."
  [x] (and (sequential? x) (every? number? x)))

(s/fdef numbers?
        :args (s/cat :x any?)
        :ret boolean?)

(defn vector?
  "Returns true if a vector (i.e., numbers only)."
  [x] (and (numbers? x) (clojure.core/vector? x)))

(s/fdef vector?
        :args (s/cat :x any?)
        :ret boolean?)

;;;VECTOR CONSTRUCTORS
(defn to-vector
  "Creates a vector representing the flattened numbers of `x` if possible.
  Otherwise, returns nil."
  [x]
  (let [ret (cond (number? x) [x]
                  (sequential? x) (let [flat (flatten x)] (when (every? number? flat) (vec flat)))
                  :else nil)]
    ret))

(s/fdef to-vector
        :args (s/cat :x any?)
        :ret (s/nilable ::vector))

(defn compute-vector
  "`f` takes an `index` and returns a number."
  [size f] (mapv f (range 0 size)))

(s/fdef compute-vector
        :args (s/cat :size ::size
                     :f (s/fspec :args (s/cat :index ::index)
                                 :ret ::number))
        :ret ::vector)

(defn rnd-vector!
  "Returns vector `v` of `size` with random doubles."
  [size] (vec (take size (random/rand-double-lazy!))))

(s/fdef rnd-vector!
        :args (s/cat :size ::size)
        :ret ::vector)

(defn sparse->vector
  "Builds a vector using a sparse representation and an existing vector `v` (often a zero-vector).
  `sparse` is a collection of tuples of `[index number]`.
  Later values will override prior overlapping values."
  [sparse v]
  (let [s (count v)]
    (vec (reduce (fn [new-v [i x]]
                   (if (or (>= i s) (neg? i))
                     new-v
                     (assoc new-v i x)))
                 v
                 sparse))))

(s/fdef sparse->vector
        :args (s/cat :sparse ::sparse-vector :v ::vector)
        :ret ::vector)

;;;VECTOR INFO
(defn filter-kv
  "Returns a vector of the items in `v` for which (`pred` item) returns true.
  `pred` must be free of side-effects."
  [pred v] (persistent! (reduce-kv #(if (pred %2 %3) (conj! %1 %3) %1) (transient []) v)))

(s/fdef filter-kv
        :args (s/cat :pred (s/fspec :args (s/cat :index ::index :number ::number) :ret boolean?)
                     :v ::vector)
        :ret ::vector)

(defn some-kv
  "Returns the first logical true value of (`pred` index number) for any number in `v`, else nil."
  [pred v]
  (loop [i 0, s v]
    (when (sequential? s)
      (let [h (first s)]
        (when h
          (if (pred i h)
            h
            (recur (inc i) (next s))))))))

(s/fdef some-kv
        :args (s/cat :pred (s/fspec :args (s/cat :index ::index :number ::number) :ret boolean?)
                     :v ::vector)
        :ret (s/nilable ::number))

;;;VECTOR MANIPULATION
(defn insertv
  "Returns a vector with the new value inserted into index."
  [v index number]
  (when (<= index (count v))
    (let [f (subvec v 0 index)
          l (subvec v index)]
      (vec (concat f [number] l)))))

(s/fdef insertv
        :args (s/cat :v ::vector :index ::index :number ::number)
        :ret (s/nilable ::vector))

(defn removev
  "Returns a vector with the value in the index removed."
  [v index]
  (if (<= (inc index) (count v))
    (let [f (subvec v 0 index)
          l (subvec v (inc index))]
      (vec (concat f l)))
    v))

(s/fdef removev
        :args (s/cat :v ::vector :index ::index)
        :ret ::vector)

(defn replace-nan
  "Takes a collection of `numbers` and returns the collection with any NaN replaced with `replacement-number`.
  Note that clojure.core/replace doesn't work with NaN."
  [replacement-number numbers]
  (if (vector? numbers)
    (reduce (fn [v i]
              (if (m/nan? (nth v i))
                (assoc v i replacement-number)
                v))
            numbers (range (count numbers)))
    (map #(if (m/nan? %) replacement-number %) numbers)))

(s/fdef replace-nan
        :args (s/cat :replacement-number ::number :numbers ::numbers)
        :ret ::numbers)

;;;VECTOR MATH
(defn kahan-sum
  "Kahan Summation algorithm -- for greater floating-point summation accuracy,
  as fast alternative to bigDecimal"
  [numbers]
  (loop [[h & t] numbers, sum 0.0, carry 0.0]
    (if-not h
      sum
      (if (m/inf? h)
        (apply + numbers)
        (let [y (- h carry)
              new-sum (+ y sum)]
          (recur t new-sum (- new-sum sum y)))))))

(s/fdef kahan-sum
        :args (s/cat :numbers ::numbers)
        :ret ::number)

(defn dot-product
  "The dot product is the sum of the products of the corresponding entries of two vectors.
  Geometrically, the dot product is the product of the Euclidean magnitudes of the two vectors and the cosine of
  the angle between them.
  Also called [[inner-product]]."
  [v1 v2] (apply + (map (fn [a b] (* (double a) b)) v1 v2)))

(s/fdef dot-product
        :args (s/and (s/cat :v1 ::vector :v2 ::vector)
                     #(= (count (:v1 %)) (count (:v2 %))))
        :ret ::number)

(defn cross-product
  "Given two linearly independent 3D vectors v1 and v2, the cross product, v1 × v2,
  is a vector that is perpendicular to both v1 and v2.
  For 2D vectors, the cross product has an analog result, which is a number.
  Only defined for 2D and 3D vectors."
  [v1 v2]
  (let [v10 (double (get v1 0))
        v20 (double (get v2 0))
        v11 (double (get v1 1))
        v21 (double (get v2 1))
        t (- (* v10 v21) (* v20 v11))]
    (cond
      (= (count v1) (count v2) 3) (let [v12 (get v1 2), v22 (get v2 2)]
                                    [(- (* v11 v22) (* v21 v12))
                                     (- (* v12 v20) (* v22 v10))
                                     t])
      (= (count v1) (count v2) 2) t
      :else nil)))

(s/fdef cross-product
        :args (s/and (s/cat :v1 (s/or :vector-2D ::vector-2D :vector-3D ::vector-3D)
                            :v2 (s/or :vector-2D ::vector-2D :vector-3D ::vector-3D))
                     #(or (and (= (first (:v1 %)) :vector-2D)
                               (= (first (:v2 %)) :vector-2D))
                          (and (= (first (:v1 %)) :vector-3D)
                               (= (first (:v2 %)) :vector-3D))))
        :ret (s/or :number ::number :v ::vector))

(defn projection
  "Returns vector of v1 projected onto v2."
  [v1 v2]
  (let [s (m/div (dot-product v1 v2) (apply + (map m/sq v2)))]
    (mapv #(* s %) v2)))

(s/fdef projection
        :args (s/and (s/cat :v1 ::vector :v2 ::vector)
                     #(= (count (:v1 %)) (count (:v2 %))))
        :ret ::vector)