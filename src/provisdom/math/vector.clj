(ns provisdom.math.vector
  (:refer-clojure :exclude [vector?])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]
    [provisdom.math.tensor :as tensor]))

(declare kahan-sum open-probs? probs?)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::size
  (s/with-gen ::m/int-non-
    #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::vector ::tensor/tensor1D)

(s/def ::vector-2D
  (s/with-gen
    (s/coll-of ::m/number
               :kind clojure.core/vector?
               :into []
               :min-count 2
               :max-count 2)
    #(gen/vector (s/gen ::m/number) 2)))

(s/def ::vector-3D
  (s/with-gen
    (s/coll-of ::m/number
               :kind clojure.core/vector?
               :into []
               :min-count 3
               :max-count 3)
    #(gen/vector (s/gen ::m/number) 3)))

(s/def ::vector-num
  (s/with-gen
    (s/coll-of ::m/num
               :kind clojure.core/vector?
               :into [])
    #(gen/vector (s/gen ::m/num) 0 mdl)))

(s/def ::vector-finite
  (s/with-gen
    (s/coll-of ::m/finite
               :kind clojure.core/vector?
               :into [])
    #(gen/vector (s/gen ::m/finite) 0 mdl)))

(defmacro vector-of-spec
  [{d?        :distinct?
    max-count :max-count
    min-count :min-count
    pred      :pred
    :or       {d?        false
               pred      ::m/number
               min-count 0}}]
  `(s/with-gen
     (s/coll-of ~pred
                :distinct? ~d?
                :into []
                :kind clojure.core/vector?
                :max-count (if ~max-count
                             (max ~max-count ~min-count)
                             m/max-int)
                :min-count ~min-count)
     #(if ~d?
        (gen/vector-distinct (s/gen ~pred)
                             {:max-elements (if ~max-count
                                              (max ~max-count ~min-count)
                                              (+ ~min-count mdl))
                              :min-elements ~min-count})
        (gen/vector (s/gen ~pred)
                    ~min-count
                    (if ~max-count
                      (max ~max-count ~min-count)
                      (+ ~min-count mdl))))))

(defmacro vector-finite-spec
  [{d?        :distinct?
    m1        :min
    m2        :max
    max-count :max-count
    min-count :min-count
    :or       {d?        false
               m1        m/min-dbl
               m2        m/max-dbl
               min-count 0}}]
  `(s/with-gen
     (s/coll-of ::m/finite
                :distinct? ~d?
                :into []
                :kind clojure.core/vector?
                :max-count (if ~max-count
                             (max ~max-count ~min-count)
                             m/max-int)
                :min-count ~min-count)
     #(if ~d? (gen/vector-distinct (s/gen ::m/finite)
                                   {:max-elements (if ~max-count
                                                    (max ~max-count ~min-count)
                                                    (+ ~min-count mdl))
                                    :min-elements ~min-count})
              (gen/vector (s/gen (m/finite-spec {:min ~m1 :max ~m2}))
                          ~min-count
                          (if ~max-count
                            (max ~max-count ~min-count)
                            (+ ~min-count mdl))))))

(s/def ::vector-finite+
  (s/with-gen
    (s/coll-of ::m/finite+
               :kind clojure.core/vector?
               :into [])
    #(gen/vector (s/gen ::m/finite+) 0 mdl)))

(s/def ::vector-non-
  (s/with-gen
    (s/coll-of ::m/non-
      :kind clojure.core/vector?
      :into [])
    #(gen/vector (s/gen ::m/non-) 0 mdl)))

(s/def ::vector-pos
  (s/with-gen
    (s/coll-of ::m/pos
      :kind clojure.core/vector?
      :into [])
    #(gen/vector (s/gen ::m/pos) 0 mdl)))

(s/def ::vector-prob
  (s/with-gen
    (s/coll-of ::m/prob
               :kind clojure.core/vector?
               :into [])
    #(gen/vector (s/gen ::m/prob) 0 mdl)))

(s/def ::vector-probs
  (s/with-gen
    (s/and (s/coll-of ::m/prob
                      :kind clojure.core/vector?
                      :into []
                      :min-count 1)
           #(probs? % 1e-8))
    (fn []
      (gen/fmap
        (fn [weights]
          (let [total (kahan-sum weights)
                probs (mapv #(/ % total) weights)]
            probs))
        (gen/vector (s/gen ::m/finite+) 1 mdl)))))

(s/def ::vector-open-probs
  (s/with-gen
    (s/and (s/coll-of ::m/open-prob
                      :kind clojure.core/vector?
                      :into []
                      :min-count 2)
           #(open-probs? % 1e-8))
    (fn []
      (gen/fmap
        (fn [weights]
          (let [total (kahan-sum weights)
                probs (mapv #(/ % total) weights)]
            probs))
        (gen/vector (s/gen ::m/finite+) 2 mdl)))))

(s/def ::sparse-vector
  (s/with-gen
    (s/coll-of (s/tuple ::m/int-non- ::m/number))
    #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                          (gen/large-integer* {:min 0 :max mdl}))
       (fn [[i j]]
         (gen/vector
           (gen/tuple
             (gen/large-integer* {:min 0 :max (max 0 (dec (max i j)))})
             (s/gen ::m/number))
           (min i j))))))

(s/def ::index->number
  (s/fspec :args (s/cat :index ::tensor/index)
           :ret ::m/number))

(s/def ::index+number->bool
  (s/fspec :args (s/cat :index ::tensor/index :number ::m/number)
           :ret boolean?))

;;;VECTOR TYPES
(defn vector?
  "Returns true if a vector (i.e., numbers only)."
  [x]
  (and (m/numbers? x) (clojure.core/vector? x)))

(s/fdef vector?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-prob?
  "Returns true if a vector of probs only."
  [x]
  (and (vector? x)
       (every? m/prob? x)))

(s/fdef vector-prob?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-open-prob?
  "Returns true if a vector of open probs only."
  [x]
  (and (vector? x)
       (every? m/open-prob? x)))

(s/fdef vector-open-prob?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-roughly-prob?
  "Returns true if a vector of roughly probs only."
  [x accu]
  (and (vector? x)
       (every? (fn [number]
                 (m/roughly-prob? number accu))
         x)))

(s/fdef vector-roughly-prob?
  :args (s/cat :x any?
               :accu ::m/accu)
  :ret boolean?)

(defn probs?
  "Returns true if a vector of probs that sums to one within `sum-accu`."
  [x sum-accu]
  (and (vector-prob? x)
       (m/roughly? 1.0 (kahan-sum x) sum-accu)))

(s/fdef probs?
  :args (s/cat :x any?
               :sum-accu ::m/accu)
  :ret boolean?)

(defn open-probs?
  "Returns true if a vector of open probs that sums to one within `sum-accu`."
  [x sum-accu]
  (and (vector-open-prob? x)
       (m/roughly? 1.0 (kahan-sum x) sum-accu)))

(s/fdef open-probs?
  :args (s/cat :x any?
               :sum-accu ::m/accu)
  :ret boolean?)

(defn roughly-probs?
  "Returns true if a vector of roughly probs within `accu` that sums to one
  within `sum-accu`."
  [x accu sum-accu]
  (and (vector-roughly-prob? x accu)
       (m/roughly? 1.0 (kahan-sum x) sum-accu)))

(s/fdef roughly-probs?
  :args (s/cat :x any?
               :accu ::m/accu
               :sum-accu ::m/accu)
  :ret boolean?)

;;;VECTOR CONSTRUCTORS
(defn to-vector
  "Creates a vector representing the flattened numbers of `x` if possible.
  Otherwise, returns nil."
  [x]
  (let [ret (cond (number? x) [x]
                  (sequential? x) (let [flat (flatten x)]
                                    (when (every? number? flat) (vec flat)))
                  :else nil)]
    ret))

(s/fdef to-vector
  :args (s/cat :x any?)
  :ret (s/nilable ::vector))

(defn compute-vector
  "Function `index->number` takes an `index` and returns a number."
  [size index->number]
  (mapv index->number (range 0 size)))

(s/fdef compute-vector
  :args (s/cat :size ::size :index->number ::index->number)
  :ret ::vector)

(defn compute-coll
  "Function `index->any` takes an `index`."
  [size index->any]
  (map index->any (range 0 size)))

(s/fdef compute-coll
  :args (s/cat :size ::size
               :index->any (s/fspec :args (s/cat :index ::tensor/index)
                                    :ret any?))
  :ret coll?)

(defn rnd-vector!
  "Returns vector `v` of `size` with random doubles."
  [size]
  (vec (take size (random/rnd-lazy!))))

(s/fdef rnd-vector!
  :args (s/cat :size ::size)
  :ret ::vector)

(defn sparse->vector
  "Builds a vector using a sparse representation and an existing vector `v`
  (often a zero-vector). `sparse` is a collection of tuples of `[index number]`.
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
(defn indexes-of
  "Returns a vector of the indexes in `v` that contain 'number'."
  [number v]
  (vec (keep-indexed (fn [i n]
                       (when (= n number) i))
                     v)))

(s/fdef indexes-of
  :args (s/cat :number ::m/number :v ::vector)
  :ret ::vector)

(defn filter-kv
  "Returns a vector of the items in `v` for which function `index+number->bool`
  returns true. `index+number->bool` must be free of side effects."
  [index+number->bool v]
  (persistent!
    (reduce-kv (fn [tot index number]
                 (if (index+number->bool index number)
                   (conj! tot number)
                   tot))
      (transient [])
      v)))

(s/fdef filter-kv
  :args (s/cat :index+number->bool ::index+number->bool :v ::vector)
  :ret ::vector)

(defn some-kv
  "Returns the first logical true value of function `index+number->bool` for any
  number in `v`, else nil."
  [index+number->bool v]
  (loop [i 0
         s v]
    (when (sequential? s)
      (let [h (first s)]
        (when h
          (if (index+number->bool i h)
            h
            (recur (inc i) (next s))))))))

(s/fdef some-kv
  :args (s/cat :index+number->bool ::index+number->bool :v ::vector)
  :ret (s/nilable ::m/number))

;;;VECTOR MANIPULATION
(defn insertv
  "Returns a vector with the new `number` inserted into `index`."
  [v index number]
  (when (<= index (count v))
    (let [f (subvec v 0 index)
          l (subvec v index)]
      (vec (concat f [number] l)))))

(s/fdef insertv
  :args (s/cat :v ::vector
               :index ::tensor/index
               :number ::m/number)
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
  :args (s/cat :v ::vector :index ::tensor/index)
  :ret ::vector)

(defn concat-by-index
  "Returns a lazy sequence constructed by concatenating two collections, `coll1`
  and `coll2` with `coll2` beginning at index `i`. Preference goes to `coll2`
  and empty spaces are filled with nil."
  [coll1 coll2 i]
  (lazy-seq
    (cond
      (and (empty? coll1) (empty? coll2)) coll2

      (zero? i)
      (if (empty? coll2)
        (cons (first coll1) (concat-by-index (rest coll1) '() 0))
        (cons (first coll2) (concat-by-index (rest coll1) (rest coll2) i)))

      (neg? i) (cons (first coll2) (concat-by-index coll1 (rest coll2) (inc i)))

      (pos? i)
      (cons (first coll1) (concat-by-index (rest coll1) coll2 (dec i))))))

(s/fdef concat-by-index
  :args (s/cat :coll1 (s/coll-of any?)
               :coll2 (s/coll-of any?)
               :i (s/with-gen ::m/int
                    #(gen/large-integer* {:min (- mdl) :max mdl})))
  :ret coll?)

(defn replace-nan
  "Takes a collection of `numbers` and returns the collection with any NaN
  replaced with `replacement-number`. Note that clojure.core/replace doesn't
  work with NaN."
  [replacement-number numbers]
  (if (vector? numbers)
    (reduce (fn [v i]
              (if (m/nan? (nth v i))
                (assoc v i replacement-number)
                v))
      numbers (range (count numbers)))
    (map (fn [number]
           (if (m/nan? number)
             replacement-number
             number))
         numbers)))

(s/fdef replace-nan
  :args (s/cat :replacement-number ::m/number :numbers ::m/numbers)
  :ret ::m/numbers)

(defn round-roughly-vector-prob
  "Rounds any probs that are roughly probs."
  [v accu]
  (mapv (fn [p]
          (if (m/roughly-prob? p accu)
            (cond (> p 1.0) 1.0
                  (neg? p) 0.0
                  :else p)
            p))
    v))

(s/fdef round-roughly-vector-prob
  :args (s/cat :v ::vector
               :accu ::m/accu)
  :ret ::vector)

(defn rnd-shuffle-vector!
  "Randomly shuffles vector."
  [v]
  (loop [v' (transient v)
         n (count v')]
    (let [i (max 0 (dec n))
          j (random/rnd-long! [0 i])]
      (if (zero? i)
        (persistent! v')
        (let [x (v' i)
              y (v' j)]
          (recur (-> v'
                     (assoc! j x)
                     (assoc! i y))
                 (dec n)))))))

(s/fdef rnd-shuffle-vector!
  :args (s/cat :v ::vector)
  :ret ::vector)

;;;VECTOR MATH
(defn kahan-sum
  "Kahan Summation algorithm -- for greater floating-point summation accuracy,
   as a fast alternative to bigDecimal."
  [numbers]
  (loop [[h & t] numbers
         sum 0.0
         carry 0.0]
    (if-not h
      sum
      (if (m/inf? h)
        (apply + sum h t)
        (let [y (- h carry)
              new-sum (+ y sum)]
          (recur t new-sum (- new-sum sum y)))))))

(s/fdef kahan-sum
  :args (s/cat :numbers ::m/numbers)
  :ret ::m/number)

(defn dot-product
  "The dot product is the sum of the products of the corresponding entries of
  two vectors. Geometrically, the dot product is the product of the Euclidean
  magnitudes of the two vectors and the cosine of the angle between them. Also
  called [[inner-product]]."
  [v1 v2]
  (apply + (map (fn [a b]
                  (* (double a) b))
                v1
                v2)))

(s/fdef dot-product
  :args (s/and (s/cat :v1 ::vector
                      :v2 ::vector)
               (fn [{:keys [v1 v2]}]
                 (= (count v1) (count v2))))
  :ret ::m/number)

(defn cross-product
  "Given two linearly independent 3D vectors `v1` and `v2`, the cross-product,
  `v1` × `v2`, is a vector that is perpendicular to both `v1` and `v2`. For 2D
  vectors, the cross-product has an analog result, which is a number. Only
  defined for 2D and 3D vectors."
  [v1 v2]
  (let [v10 (double (get v1 0))
        v20 (double (get v2 0))
        v11 (double (get v1 1))
        v21 (double (get v2 1))
        t (- (* v10 v21) (* v20 v11))]
    (cond
      (= (count v1) (count v2) 3) (let [v12 (get v1 2)
                                        v22 (get v2 2)]
                                    [(- (* v11 v22) (* v21 v12))
                                     (- (* v12 v20) (* v22 v10))
                                     t])
      (= (count v1) (count v2) 2) t
      :else nil)))

(s/fdef cross-product
  :args (s/and (s/cat :v1 (s/or :vector-2D ::vector-2D
                                :vector-3D ::vector-3D)
                      :v2 (s/or :vector-2D ::vector-2D
                                :vector-3D ::vector-3D))
               (fn [{:keys [v1 v2]}]
                 (let [v1-type (first v1)
                       v2-type (first v2)]
                   (or (and (= v1-type :vector-2D) (= v2-type :vector-2D))
                       (and (= v1-type :vector-3D) (= v2-type :vector-3D))))))
  :ret (s/or :number ::m/number :v ::vector))

(defn projection
  "Returns vector of `v1` projected onto `v2`."
  [v1 v2]
  (let [s (m/div (dot-product v1 v2) (apply + (map m/sq v2)))]
    (mapv #(* s %) v2)))

(s/fdef projection
  :args (s/and (s/cat :v1 ::vector :v2 ::vector)
               (fn [{:keys [v1 v2]}]
                 (= (count v1) (count v2))))
  :ret ::vector)
