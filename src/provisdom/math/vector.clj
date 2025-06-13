(ns provisdom.math.vector
  "Vector operations and linear algebra primitives.
  
  Specialized vector functionality built on tensor operations:
  - Vector arithmetic (addition, subtraction, scaling)
  - Dot products, cross products, and norms
  - Vector projections and orthogonalization  
  - Probability vector operations and validation
  - Kahan summation for improved numerical accuracy
  - Statistical operations (mean, variance, etc.)
  
  Vectors are 1D tensors with specialized operations for linear algebra
  and numerical stability."
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
  "Returns true if `x` is a valid vector.
  
  A vector is a Clojure vector containing only numbers.
  
  Examples:
    (vector? [1 2 3]) ;=> true
    (vector? []) ;=> true
    (vector? [1 \"a\"]) ;=> false
    (vector? '(1 2 3)) ;=> false (list, not vector)"
  [x]
  (and (m/numbers? x) (clojure.core/vector? x)))

(s/fdef vector?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-prob?
  "Returns true if `x` is a vector containing only probability values.
  
  All elements must be numbers in the range [0, 1].
  
  Examples:
    (vector-prob? [0.3 0.7]) ;=> true
    (vector-prob? [0.0 1.0 0.5]) ;=> true
    (vector-prob? [0.3 1.2]) ;=> false (1.2 > 1)"
  [x]
  (and (vector? x)
       (every? m/prob? x)))

(s/fdef vector-prob?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-open-prob?
  "Returns true if `x` is a vector containing only open probability values.
  
  All elements must be numbers in the range (0, 1), excluding 0 and 1.
  
  Examples:
    (vector-open-prob? [0.3 0.7]) ;=> true
    (vector-open-prob? [0.0 0.5]) ;=> false (contains 0)
    (vector-open-prob? [0.3 1.0]) ;=> false (contains 1)"
  [x]
  (and (vector? x)
       (every? m/open-prob? x)))

(s/fdef vector-open-prob?
  :args (s/cat :x any?)
  :ret boolean?)

(defn vector-roughly-prob?
  "Returns true if `x` is a vector of values that are approximately probabilities.
  
  Elements are considered probability values if they are within the
  specified accuracy tolerance `accu` of the range [0, 1].
  
  Examples:
    (vector-roughly-prob? [0.3 1.01] 0.02) ;=> true (1.01 is close to 1)
    (vector-roughly-prob? [0.3 1.1] 0.02) ;=> false (1.1 too far from 1)"
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
  "Returns true if `x` is a valid probability distribution vector.
  
  A probability distribution must contain only probability values and
  sum to 1.0 within the specified tolerance `sum-accu`.
  
  Examples:
    (probs? [0.3 0.7] 1e-8) ;=> true
    (probs? [0.5 0.5] 1e-8) ;=> true
    (probs? [0.3 0.6] 1e-8) ;=> false (sums to 0.9)"
  [x sum-accu]
  (and (vector-prob? x)
       (m/roughly? 1.0 (kahan-sum x) sum-accu)))

(s/fdef probs?
  :args (s/cat :x any?
               :sum-accu ::m/accu)
  :ret boolean?)

(defn open-probs?
  "Returns true if `x` is a valid open probability distribution vector.
  
  Like probs? but requires all elements to be in (0, 1), excluding
  boundary values 0 and 1. Uses tolerance `sum-accu`.
  
  Examples:
    (open-probs? [0.3 0.7] 1e-8) ;=> true
    (open-probs? [0.0 1.0] 1e-8) ;=> false (contains boundary values)"
  [x sum-accu]
  (and (vector-open-prob? x)
       (m/roughly? 1.0 (kahan-sum x) sum-accu)))

(s/fdef open-probs?
  :args (s/cat :x any?
               :sum-accu ::m/accu)
  :ret boolean?)

(defn roughly-probs?
  "Returns true if `x` is approximately a probability distribution vector.
  
  Elements must be approximately in [0, 1] within `accu` tolerance,
  and the sum must be approximately 1.0 within `sum-accu` tolerance.
  
  Examples:
    (roughly-probs? [0.31 0.69] 0.01 1e-8) ;=> true
    (roughly-probs? [-0.01 1.01] 0.02 1e-8) ;=> true (values close to [0,1])"
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
  "Converts nested structure `x` to a flat vector of numbers.
  
  Flattens any nested sequences and converts to a vector.
  Returns nil if any non-numeric values are encountered.
  
  Examples:
    (to-vector [[1 2] [3 4]]) ;=> [1 2 3 4]
    (to-vector 5) ;=> [5]
    (to-vector [1 \"a\"]) ;=> nil (contains non-number)"
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
  "Creates a vector of `size` by computing elements using function `index->number`.
  
  The function receives the index (0-based) and should return the
  value for that position.
  
  Examples:
    (compute-vector 3 identity) ;=> [0 1 2]
    (compute-vector 4 #(* % %)) ;=> [0 1 4 9]"
  [size index->number]
  (mapv index->number (range 0 size)))

(s/fdef compute-vector
  :args (s/cat :size ::size :index->number ::index->number)
  :ret ::vector)

(defn compute-coll
  "Creates a lazy collection of `size` by computing elements using function `index->any`.
  
  Similar to compute-vector but returns a lazy sequence and allows
  any return type from the function.
  
  Examples:
    (compute-coll 3 str) ;=> (\"0\" \"1\" \"2\")
    (compute-coll 2 #(* % 10)) ;=> (0 10)"
  [size index->any]
  (map index->any (range 0 size)))

(s/fdef compute-coll
  :args (s/cat :size ::size
               :index->any (s/fspec :args (s/cat :index ::tensor/index)
                                    :ret any?))
  :ret coll?)

(defn rnd-vector!
  "Creates a vector of `size` with random numbers.
  
  Generates a vector of the specified size filled with uniformly
  distributed random doubles between 0 and 1.
  
  Examples:
    (rnd-vector! 3) ;=> [0.123 0.456 0.789] (example values)"
  [size]
  (vec (take size (random/rnd-lazy!))))

(s/fdef rnd-vector!
  :args (s/cat :size ::size)
  :ret ::vector)

(defn sparse->vector
  "Constructs a vector from sparse representation `sparse` applied to vector `v`.
  
  Takes a collection of [index value] pairs and applies them to an
  existing vector. Later values override earlier ones for the same index.
  
  Examples:
    (sparse->vector [[0 5] [2 7]] [1 1 1]) ;=> [5 1 7]
    (sparse->vector [[1 10]] [0 0 0]) ;=> [0 10 0]"
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
  "Returns a vector of indices where `number` appears in vector `v`.
  
  Searches through the vector and returns all positions where
  the exact number is found.
  
  Examples:
    (indexes-of 2 [1 2 3 2]) ;=> [1 3]
    (indexes-of 5 [1 2 3]) ;=> []"
  [number v]
  (vec (keep-indexed (fn [i n]
                       (when (= n number) i))
                     v)))

(s/fdef indexes-of
  :args (s/cat :number ::m/number :v ::vector)
  :ret ::vector)

(defn filter-kv
  "Filters vector `v` elements based on predicate `index+number->bool`.
  
  The predicate function receives (index value) and should return true
  to include the value in the result.
  
  Examples:
    (filter-kv (fn [i v] (even? i)) [10 20 30 40]) ;=> [10 30]
    (filter-kv (fn [i v] (> v i)) [0 0 3 1]) ;=> [3]"
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
  "Finds the first element in vector `v` where predicate `index+number->bool` returns a truthy value.
  
  The predicate receives (index value) and the function returns the
  first value for which the predicate is truthy, or nil if none found.
  
  Examples:
    (some-kv (fn [i v] (when (> v 5) v)) [1 3 7 2]) ;=> 7
    (some-kv (fn [i v] (when (> v 10) v)) [1 3 7]) ;=> nil"
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
  "Inserts `number` into vector `v` at the specified `index`, shifting existing elements.
  
  Returns a new vector with the number inserted at the given position.
  All elements at and after the index are shifted right.
  
  Examples:
    (insertv [1 2 3] 1 99) ;=> [1 99 2 3]
    (insertv [1 2] 0 99) ;=> [99 1 2]"
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
  "Removes the element at `index` from vector `v`.
  
  Returns a new vector with the element at the given index removed.
  All elements after the index are shifted left.
  
  Examples:
    (removev [1 2 3] 1) ;=> [1 3]
    (removev [1 2 3] 0) ;=> [2 3]"
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
  "Concatenates collections `coll1` and `coll2` with the second starting at index `i`.
  
  Overlays `coll2` onto `coll1` starting at index `i`. Values from `coll2` take
  precedence over `coll1`. Gaps are filled with nil.
  
  Examples:
    (concat-by-index [1 2 3] [8 9] 1) ;=> (1 8 9)
    (concat-by-index [1 2] [8 9] 3) ;=> (1 2 nil 8 9)"
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
  "Replaces all NaN values in collection `numbers` with `replacement-number`.
  
  Works correctly with NaN values (unlike clojure.core/replace).
  Preserves the original collection type.
  
  Examples:
    (replace-nan 0 [1 ##NaN 3]) ;=> [1 0 3]
    (replace-nan -1 [##NaN ##NaN]) ;=> [-1 -1]"
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
  "Rounds values in vector `v` that are approximately valid probabilities within tolerance `accu`.
  
  Values close to 0 are rounded to 0, values close to 1 are rounded to 1,
  within the specified accuracy tolerance.
  
  Examples:
    (round-roughly-vector-prob [1.01 -0.01 0.5] 0.02) ;=> [1.0 0.0 0.5]
    (round-roughly-vector-prob [0.99 0.01] 0.02) ;=> [1.0 0.0]"
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
  "Returns a randomly shuffled copy of vector `v`.
  
  Uses the Fisher-Yates shuffle algorithm for uniform random permutation.
  
  Examples:
    (rnd-shuffle-vector! [1 2 3 4]) ;=> [3 1 4 2] (example result)"
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
  "Computes the sum of `numbers` using Kahan summation algorithm for improved accuracy.
  
  Reduces floating-point errors that accumulate in naive summation,
  providing better precision than regular addition for large sequences.
  
  Examples:
    (kahan-sum [0.1 0.1 0.1]) ;=> 0.30000000000000004 (more accurate than +)
    (kahan-sum [1e16 1 -1e16]) ;=> 1.0 (preserves small values)"
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
  "Computes the dot product (inner product) of vectors `v1` and `v2`.
  
  Calculates ∑(`v1`[i] * `v2`[i]). Geometrically, this equals |`v1`| |`v2`| cos(θ)
  where θ is the angle between the vectors.
  
  Properties:
  - Commutative: v1 · v2 = v2 · v1
  - Zero when vectors are orthogonal
  - Positive when angle < 90°, negative when angle > 90°
  
  Examples:
    (dot-product [1 2 3] [4 5 6]) ;=> 32 (1*4 + 2*5 + 3*6)
    (dot-product [1 0] [0 1]) ;=> 0 (orthogonal vectors)"
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
  "Computes the cross product of vectors `v1` and `v2`.
  
  For 3D vectors: returns a vector perpendicular to both inputs with
  magnitude |`v1`| |`v2`| sin(θ). Direction follows right-hand rule.
  
  For 2D vectors: returns the z-component of the 3D cross product,
  which is the signed area of the parallelogram formed by the vectors.
  
  Examples:
    (cross-product [1 0 0] [0 1 0]) ;=> [0 0 1] (right-hand rule)
    (cross-product [3 4] [1 2]) ;=> 2 (3*2 - 4*1)"
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
  "Computes the vector projection of `v1` onto `v2`.
  
  Returns the component of `v1` that lies in the direction of `v2`.
  The result is parallel to `v2` with length |`v1`| cos(θ).
  
  Formula: proj_`v2`(`v1`) = ((`v1` · `v2`) / |`v2`|²) * `v2`
  
  Examples:
    (projection [3 4] [1 0]) ;=> [3.0 0.0] (projection onto x-axis)
    (projection [1 1] [2 0]) ;=> [1.0 0.0] (projection onto direction [2 0])"
  [v1 v2]
  (let [s (m/div (dot-product v1 v2) (apply + (map m/sq v2)))]
    (mapv #(* s %) v2)))

(s/fdef projection
  :args (s/and (s/cat :v1 ::vector :v2 ::vector)
               (fn [{:keys [v1 v2]}]
                 (= (count v1) (count v2))))
  :ret ::vector)
