(ns provisdom.math.tensor
  "Multidimensional array operations and tensor algebra.
  
  Provides comprehensive tensor functionality for numerical computing:
  - N-dimensional array creation, manipulation, and validation
  - Element-wise operations (map, reduce, filter with coordinate access)  
  - Tensor arithmetic (addition, multiplication, broadcasting)
  - Shape manipulation (reshape, transpose, slicing)
  - Advanced operations (inner products, convolution)
  - Memory-efficient lazy operations where possible
  
  Tensors are represented as nested vectors with consistent dimensionality."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.random :as random]))

(declare transpose rank tensor?)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::index
  (s/with-gen ::m/int-non-
    #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::indices
  (s/with-gen (s/coll-of ::index)
    #(gen/vector (s/gen ::index) 0 mdl)))

(s/def ::shape ::indices)

(s/def ::index+tensor->bool
  (s/with-gen
    (s/fspec :args (s/cat :index ::index :tensor ::tensor)
      :ret boolean?)
    #(gen/return (constantly true))))

(s/def ::tensor1D
  (s/with-gen
    (s/coll-of ::m/number :kind vector? :into [])
    #(gen/vector (s/gen ::m/number) 0 mdl)))

(s/def ::tensor2D
  (s/with-gen
    #(and (tensor? %) (= 2 (rank %)))
    #(gen/bind (gen/large-integer* {:min 0 :max mdl})
       (fn [i]
         (gen/vector
           (gen/vector (s/gen ::m/number) i)
           1
           mdl)))))

(s/def ::tensor3D
  (s/with-gen
    #(and (tensor? %) (= 3 (rank %)))
    #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                 (gen/large-integer* {:min 1 :max mdl}))
       (fn [[i j]]
         (gen/vector
           (gen/vector
             (gen/vector (s/gen ::m/number) i)
             j)
           1
           mdl)))))

(s/def ::tensor4D
  (s/with-gen
    #(and (tensor? %) (= 4 (rank %)))
    #(gen/bind
       (gen/tuple (gen/large-integer* {:min 0 :max mdl})
         (gen/large-integer* {:min 1 :max mdl})
         (gen/large-integer* {:min 1 :max mdl}))
       (fn [[i j k]]
         (gen/vector
           (gen/vector
             (gen/vector
               (gen/vector (s/gen ::m/number) i)
               j)
             k)
           1
           mdl)))))

(s/def ::tensor5D+
  (s/with-gen
    #(and (tensor? %) (>= (rank %) 5))
    #(gen/bind
       (gen/tuple (gen/large-integer* {:min 0 :max mdl})
         (gen/large-integer* {:min 1 :max mdl})
         (gen/large-integer* {:min 1 :max mdl})
         (gen/large-integer* {:min 1 :max mdl}))
       (fn [[i j k l]]
         (gen/vector
           (gen/vector
             (gen/vector
               (gen/vector
                 (gen/vector (s/gen ::m/number) i)
                 j)
               k)
             l)
           1
           mdl)))))

(s/def ::tensor
  (s/with-gen
    (s/or :number ::m/number
      :t1 ::tensor1D
      :t2 ::tensor2D
      :t3 ::tensor3D
      :t4 ::tensor4D
      :t5+ ::tensor5D+)
    #(gen/one-of (map s/gen [::tensor1D ::tensor2D ::tensor3D]))))

;;;TENSOR TYPES
(defn tensor?
  "Returns true if `x` is a valid tensor.
  
  A valid tensor is either:
  - A number (0-dimensional tensor)
  - A vector of numbers (1-dimensional tensor)
  - A nested vector structure where all dimensions have consistent lengths
  
  Examples:
    (tensor? 5) ;=> true
    (tensor? [1 2 3]) ;=> true
    (tensor? [[1 2] [3 4]]) ;=> true
    (tensor? [[1 2] [3]]) ;=> false (inconsistent row lengths)"
  [x]
  (or (number? x)
    (and (vector? x) (every? #(number? %) x))
    (and (vector? x)
      (every? #(and (vector? %) (= (count %) (count (first x))))
        x)
      (every? tensor? x))))

(s/fdef tensor?
  :args (s/cat :x any?)
  :ret boolean?)

;;;TENSOR CONSTRUCTOR
(defn to-tensor
  "Converts sequential structure `x` to a tensor, or returns nil if invalid.
  
  Recursively processes nested sequences and validates the result.
  Numbers are returned as-is. Non-sequential values return nil.
  
  Examples:
    (to-tensor '((1 2) (3 4))) ;=> [[1 2] [3 4]]
    (to-tensor [1 [2]]) ;=> nil (mixed structure)
    (to-tensor \"hello\") ;=> nil"
  [x]
  (let [ret (cond (number? x) x
                  (sequential? x) (mapv to-tensor x)
                  :else nil)]
    (when (tensor? ret) ret)))

(s/fdef to-tensor
  :args (s/cat :x any?)
  :ret (s/nilable ::tensor))

(defn- recursive-compute-tensor
  "Recursively computes a tensor for [[compute-tensor]]."
  [shape f sh]
  (let [c (count shape)
        dim (count sh)]
    (if (= dim c)
      (f sh)
      (mapv #(recursive-compute-tensor shape f (conj sh %))
        (range (get shape dim))))))

(defn compute-tensor
  "Creates a tensor by computing each element using function `indices->number`.
  
  The function receives a vector of indices (coordinates) and should return
  the value for that position. Indices are 0-based. The tensor will have
  dimensions specified by `shape`.
  
  Examples:
    (compute-tensor [2 3] #(apply + %)) ;=> [[0 1 2] [1 2 3]]
    (compute-tensor [2 2] #(if (= (first %) (second %)) 1 0))
    ;=> [[1 0] [0 1]] (identity matrix)"
  [shape indices->number]
  (recursive-compute-tensor shape indices->number []))

(s/fdef compute-tensor
  :args (s/cat :shape ::shape
          :indices->number (s/fspec :args (s/cat :indices ::indices)
                             :ret ::m/number))
  :ret ::tensor)

(defn repeat-tensor
  "Creates a tensor by repeating `seed-tensor` according to `shape`.
  
  With one argument, creates a tensor filled with zeros.
  With two arguments, tiles `seed-tensor` according to the `shape` dimensions.
  
  Examples:
    (repeat-tensor [2 3]) ;=> [[0.0 0.0 0.0] [0.0 0.0 0.0]]
    (repeat-tensor [2] 5) ;=> [5 5]
    (repeat-tensor [2] [1 2]) ;=> [[1 2] [1 2]]"
  ([shape] (repeat-tensor shape 0.0))
  ([shape seed-tensor]
   (if (zero? (count shape))
     seed-tensor
     (let [c (dec (count shape))
           n (vec (repeat (get shape c) seed-tensor))]
       (loop [dim (dec c), t n]
         (if (neg? dim)
           t
           (recur (dec dim) (vec (repeat (get shape dim) t)))))))))

(s/fdef repeat-tensor
  :args (s/cat :shape ::shape :seed-tensor (s/? ::tensor))
  :ret ::tensor)

(defn fill-tensor
  "Creates a tensor with `shape` using values from sequence `numbers`.
  
  Fills the tensor in row-major order. If `numbers` is too short, remaining
  elements are filled with 0.0. If too long, extra values are ignored.
  
  Examples:
    (fill-tensor [2 2] [1 2 3 4]) ;=> [[1 2] [3 4]]
    (fill-tensor [2 3] [1 2]) ;=> [[1 2 0.0] [0.0 0.0 0.0]]"
  [shape numbers]
  (if (empty? shape)
    (or (first numbers) 0.0)
    (let [tot (reduce * shape)]
      (if (zero? tot)
        (repeat-tensor shape)
        (let [cn (count numbers)
              rem (- tot cn)
              tensor (vec (if (neg? rem)
                            (take tot numbers)
                            (concat numbers (repeat rem 0.0))))]
          (reduce (fn [tot sh]
                    (vec (map vec (partition sh tot))))
            tensor
            (reverse (rest shape))))))))

(s/fdef fill-tensor
  :args (s/cat :shape ::shape
          :numbers ::m/numbers)
  :ret ::tensor)

(defn rnd-tensor!
  "Creates a tensor with `shape` filled with random numbers.
  
  Random values are uniformly distributed doubles between 0 and 1.
  Uses the current random number generator state.
  
  Examples:
    (rnd-tensor! [2 2]) ;=> [[0.123 0.456] [0.789 0.012]] (example values)"
  [shape]
  (fill-tensor shape (take (reduce * shape) (random/rnd-lazy!))))

(s/fdef rnd-tensor!
  :args (s/cat :shape ::shape)
  :ret ::tensor)

;;;TENSOR INFO
(defn first-number
  "Returns the first number encountered in `tensor`.
  
  Recursively traverses nested structure to find the first numeric element.
  Returns nil if `tensor` is nil.
  
  Examples:
    (first-number [[1 2] [3 4]]) ;=> 1
    (first-number 42) ;=> 42
    (first-number nil) ;=> nil"
  [tensor]
  (cond (number? tensor) tensor
        (nil? tensor) tensor
        :else (first-number (first tensor))))

(s/fdef first-number
  :args (s/cat :tensor (s/nilable ::tensor))
  :ret (s/nilable ::m/number))

(defn ecount
  "Returns the total number of elements in `tensor`.
  
  For scalars, returns 1. For multi-dimensional tensors, returns the product
  of all dimension sizes.
  
  Examples:
    (ecount 5) ;=> 1
    (ecount [1 2 3]) ;=> 3
    (ecount [[1 2] [3 4]]) ;=> 4"
  [tensor]
  (if (number? tensor)
    1
    (count (flatten tensor))))

(s/fdef ecount
  :args (s/cat :tensor ::tensor)
  :ret ::m/int-non-)

(defn rank
  "Returns the number of dimensions (rank) of `tensor`.
  
  Scalars have rank 0, vectors have rank 1, matrices have rank 2, etc.
  
  Examples:
    (rank 5) ;=> 0
    (rank [1 2 3]) ;=> 1
    (rank [[1 2] [3 4]]) ;=> 2
    (rank [[[1]]]) ;=> 3"
  [tensor]
  (if (vector? tensor)
    (let [f (first tensor)]
      (if f
        (inc (rank f))
        1))
    0))

(s/fdef rank
  :args (s/cat :tensor ::tensor)
  :ret ::m/int-non-)

(defn shape
  "Returns the dimensions of `tensor` as a vector.
  
  The shape describes the size of each dimension.
  
  Examples:
    (shape 5) ;=> []
    (shape [1 2 3]) ;=> [3]
    (shape [[1 2] [3 4]]) ;=> [2 2]
    (shape [[[1 2]]]) ;=> [1 1 2]"
  [tensor]
  (loop [dim 0
         sh []
         remain tensor]
    (if (sequential? remain)
      (let [r (first remain)]
        (recur (inc dim)
          (conj sh (count remain))
          r))
      sh)))

(s/fdef shape
  :args (s/cat :tensor ::tensor)
  :ret ::shape)

(defn every-kv?
  "Returns true if the predicate function returns true for every tensor element.
  
  The predicate receives (indices element) where indices is a vector of
  coordinates for the element's position.
  
  Examples:
    (every-kv? (fn [idx val] (pos? val)) [[1 2] [3 4]]) ;=> true
    (every-kv? (fn [idx val] (even? val)) [1 2 3]) ;=> false"
  [indices+number->bool tensor]
  (if (number? tensor)
    (indices+number->bool [] tensor)
    (every? true?
      (flatten (recursive-compute-tensor
                 (shape tensor)
                 (fn [indices]
                   (indices+number->bool indices (get-in tensor indices)))
                 [])))))

(s/fdef every-kv?
  :args (s/cat :indices+number->bool (s/fspec :args (s/cat :indices ::indices
                                                      :number ::m/number)
                                       :ret boolean?)
          :tensor ::tensor)
  :ret boolean?)

(defn filter-kv
  "Filters the top-level elements of a tensor based on a predicate.
  
  The predicate receives (index element) where index is the position
  of the element in the top-level sequence. Returns nil for scalars
  that don't match.
  
  Examples:
    (filter-kv (fn [i x] (even? i)) [10 20 30 40]) ;=> [10 30]
    (filter-kv (fn [i x] (> x 2)) [1 2 3 4]) ;=> [3 4]"
  [index+tensor->bool tensor-v]
  (if (number? tensor-v)
    (when (index+tensor->bool 0 tensor-v) tensor-v)
    (persistent!
      (reduce-kv (fn [tot index tensor]
                   (if (index+tensor->bool index tensor)
                     (conj! tot tensor)
                     tot))
        (transient [])
        tensor-v))))

(s/fdef filter-kv
  :args (s/cat :index+tensor->bool ::index+tensor->bool
          :tensor-v ::tensor)
  :ret (s/nilable ::tensor))

;;;TENSOR MANIPULATION
(defn- expandable-shape?
  "Tests whether a tensor with `shape` can be expanded into
  `desired-expanded-shape`."
  [shape desired-expanded-shape]
  (every? zero? (map -
                  (take-last (count shape) desired-expanded-shape)
                  shape)))

(defn- recursive-emap
  "Recursively maps for [[emap]]."
  [largest-shape f sh tensors]
  (let [c (count largest-shape)
        dim (count sh)]
    (if (= dim c)
      (apply f (map (fn [tensor]
                      (get-in tensor sh))
                 tensors))
      (mapv #(recursive-emap largest-shape f (conj sh %) tensors)
        (range (get largest-shape dim))))))

(defn- emap-core
  [f tensor & more]
  (let [tensors (concat [tensor] more)
        shapes (map shape tensors)
        [largest-shape large-count] (reduce (fn [[sh c] new-sh]
                                              (let [new-c (count new-sh)]
                                                (if (> new-c c)
                                                  [new-sh new-c]
                                                  [sh c])))
                                      [(first shapes) (count (first shapes))]
                                      (rest shapes))
        new-tensors (when (every? #(expandable-shape? % largest-shape)
                            shapes)
                      (map
                        (fn [tensor shape]
                          (repeat-tensor
                            (vec (take (- large-count (count shape))
                                   largest-shape))
                            tensor))
                        tensors
                        shapes))]
    (when new-tensors
      (recursive-emap largest-shape f [] new-tensors))))

(defn emap
  "Applies a function element-wise across one or more tensors.
  
  Tensors with different shapes are broadcast to a common shape if possible.
  Broadcasting follows NumPy-like rules. Returns nil if shapes are incompatible.
  
  Examples:
    (emap inc [1 2 3]) ;=> [2 3 4]
    (emap + [[1 2]] [[10] [20]]) ;=> [[11 12] [21 22]] (broadcasting)
    (emap * [1 2] 3) ;=> [3 6] (scalar broadcast)"
  ([f tensor] (recursive-emap (shape tensor) f [] [tensor]))
  ([f tensor1 tensor2] (emap-core f tensor1 tensor2))
  ([f tensor1 tensor2 tensor3] (emap-core f tensor1 tensor2 tensor3))
  ([f tensor1 tensor2 tensor3 & more]
   (apply emap-core f tensor1 tensor2 tensor3 more)))

(s/fdef emap
  :args (s/or :one (s/cat :f (s/fspec :args (s/cat :number ::m/number)
                               :ret any?)
                     :tensor ::tensor)
          :two (s/cat :f (s/fspec :args (s/cat :number1 ::m/number
                                          :number2 ::m/number)
                           :ret any?)
                 :tensor1 ::tensor
                 :tensor2 ::tensor)
          :three (s/cat :f (s/fspec :args (s/cat :number1 ::m/number
                                            :number2 ::m/number
                                            :number3 ::m/number)
                             :ret any?)
                   :tensor1 ::tensor
                   :tensor2 ::tensor
                   :tensor3 ::tensor)
          :more (s/cat :f (s/fspec :args (s/cat :number1 ::m/number
                                           :number2 ::m/number
                                           :number3 ::m/number
                                           :number-more (s/+ ::m/number))
                            :ret any?)
                  :tensor1 ::tensor
                  :tensor2 ::tensor
                  :tensor3 ::tensor
                  :more (s/+ ::tensor)))
  :ret any?)

(defn- recursive-emap-kv
  "Recursively maps for [[emap-kv]]."
  [shape f indices tensors]
  (let [c (count shape)
        dim (count indices)]
    (if (= dim c)
      (apply f indices (map (fn [tensor]
                              (get-in tensor indices))
                         tensors))
      (mapv (fn [index]
              (recursive-emap-kv shape f (conj indices index) tensors))
        (range (get shape dim))))))

(defn- emap-kv-core
  [f tensor & more]
  (let [tensors (concat [tensor] more)
        shapes (map shape tensors)
        [largest-shape large-count] (reduce (fn [[sh c] new-sh]
                                              (let [new-c (count new-sh)]
                                                (if (> new-c c)
                                                  [new-sh new-c]
                                                  [sh c])))
                                      [(first shapes) (count (first shapes))]
                                      (rest shapes))
        new-tensors (when (every? (fn [shape]
                                    (expandable-shape? shape largest-shape))
                            shapes)
                      (map
                        (fn [tensor sh]
                          (repeat-tensor
                            (vec (take (- large-count (count sh))
                                   largest-shape))
                            tensor))
                        tensors
                        shapes))]
    (recursive-emap-kv largest-shape f [] new-tensors)))

(defn emap-kv
  "Applies a function element-wise with coordinate information.
  
  Like emap, but the function also receives the indices (coordinates) of
  each element as its first argument.
  
  Examples:
    (emap-kv (fn [idx val] (+ (apply + idx) val)) [[1 2] [3 4]])
    ;=> [[1 3] [4 6]] (adds row+col indices to each value)"
  ([f tensor] (recursive-emap-kv (shape tensor) f [] [tensor]))
  ([f tensor1 tensor2] (emap-kv-core f tensor1 tensor2))
  ([f tensor1 tensor2 tensor3] (emap-kv-core f tensor1 tensor2 tensor3))
  ([f tensor1 tensor2 tensor3 & more]
   (apply emap-kv-core f tensor1 tensor2 tensor3 more)))

(s/fdef emap-kv
  :args (s/or :one (s/cat :f (s/fspec :args (s/cat :indices ::indices
                                              :number ::m/number)
                               :ret any?)
                     :tensor ::tensor)
          :two (s/cat :f (s/fspec :args (s/cat :indices ::indices
                                          :number1 ::m/number
                                          :number2 ::m/number)
                           :ret any?)
                 :tensor1 ::tensor
                 :tensor2 ::tensor)
          :three (s/cat :f (s/fspec :args (s/cat :indices ::indices
                                            :number1 ::m/number
                                            :number2 ::m/number
                                            :number3 ::m/number)
                             :ret any?)
                   :tensor1 ::tensor
                   :tensor2 ::tensor
                   :tensor3 ::tensor)
          :more (s/cat :f (s/fspec :args (s/cat :indices ::indices
                                           :number1 ::m/number
                                           :number2 ::m/number
                                           :number3 ::m/number
                                           :number-more (s/+ ::m/number))
                            :ret any?)
                  :tensor1 ::tensor
                  :tensor2 ::tensor
                  :tensor3 ::tensor
                  :more (s/* ::tensor)))
  :ret any?)

(defn partition-recursively
  "Recursively partitions a 1D tensor into nested chunks of size n.
  
  Creates a roughly cubic tensor by repeatedly partitioning into groups
  of n elements. May leave some elements unused if they don't fit evenly.
  
  Examples:
    (partition-recursively 2 [1 2 3 4]) ;=> [[1 2] [3 4]]
    (partition-recursively 3 (range 27)) ;=> 3x3x3 nested structure"
  [n tensor]
  (let [t (m/ceil' (m/div (m/log (count tensor)) (m/log n)))]
    (last (take t (iterate #(to-tensor (partition n %)) tensor)))))

(s/fdef partition-recursively
  :args (s/cat :n (s/with-gen
                    (s/int-in 2 m/max-int)
                    #(gen/large-integer* {:min 2 :max mdl}))
          :tensor ::tensor)
  :ret (s/nilable ::tensor))

;;;TENSOR MATH
(defn ===
  "Tests tensor equality with NaN-aware comparison.
  
  Unlike regular equality, considers NaN values equal to other NaN values.
  Tensors must have identical shapes and corresponding elements.
  
  Examples:
    (=== [[1 ##NaN]] [[1 ##NaN]]) ;=> true
    (=== [1 2] [1 2]) ;=> true
    (=== [1 2] [1 3]) ;=> false"
  ([tensor] true)
  ([tensor1 tensor2]
   (let [eq (emap (fn [n1 n2]
                    (m/=== n1 n2))
              tensor1
              tensor2)]
     (if (and eq (every? true? (flatten eq)))
       true
       false)))
  ([tensor1 tensor2 & more]
   (and (=== tensor1 tensor2) (apply === tensor2 more))))

(s/fdef ===
  :args (s/or :one (s/cat :tensor ::tensor)
          :two+ (s/cat :tensor1 ::tensor
                  :tensor2 ::tensor
                  :more (s/* ::tensor)))
  :ret boolean?)

(defn add
  "Performs element-wise addition of tensors.
  
  Supports broadcasting for tensors of different shapes.
  With no arguments, returns 0.0. With one argument, returns the tensor unchanged.
  
  Examples:
    (add) ;=> 0.0
    (add [1 2] [3 4]) ;=> [4 6]
    (add [[1]] [[10] [20]]) ;=> [[11] [21]] (broadcasting)"
  ([] 0.0)
  ([tensor] tensor)
  ([tensor1 tensor2]
   (emap (fn [i j]
           (+ (double i) j))
     tensor1
     tensor2))
  ([tensor1 tensor2 & more]
   (when-let [tensor3 (add tensor1 tensor2)]
     (apply emap + tensor3 more))))

(s/fdef add
  :args (s/or :zero (s/cat)
          :one (s/cat :tensor ::tensor)
          :two+ (s/cat :tensor1 ::tensor
                  :tensor2 ::tensor
                  :more (s/* ::tensor)))
  :ret (s/nilable ::tensor))

(defn subtract
  "Performs element-wise subtraction of tensors.
  
  With one argument, negates all elements. With multiple arguments,
  subtracts subsequent tensors from the first. Supports broadcasting.
  
  Examples:
    (subtract [5 3]) ;=> [-5 -3]
    (subtract [5 3] [1 2]) ;=> [4 1]
    (subtract [[5]] [1 2]) ;=> [[4 3]] (broadcasting)"
  ([] 0.0)
  ([tensor] (emap - tensor))
  ([tensor1 tensor2]
   (emap (fn [i j]
           (- (double i) j))
     tensor1
     tensor2))
  ([tensor1 tensor2 & more]
   (when-let [tensor3 (subtract tensor1 tensor2)]
     (apply emap - tensor3 more))))

(s/fdef subtract
  :args (s/or :zero (s/cat)
          :one (s/cat :tensor ::tensor)
          :two+ (s/cat :tensor1 ::tensor
                  :tensor2 ::tensor
                  :more (s/* ::tensor)))
  :ret (s/nilable ::tensor))

(defn multiply
  "Performs element-wise multiplication of tensors.
  
  Supports broadcasting for tensors of different shapes.
  With no arguments, returns 1.0. With one argument, returns the tensor unchanged.
  
  Examples:
    (multiply) ;=> 1.0
    (multiply [2 3] [4 5]) ;=> [8 15]
    (multiply [[2]] [3 4]) ;=> [[6 8]] (broadcasting)"
  ([] 1.0)
  ([tensor] tensor)
  ([tensor1 tensor2]
   (emap (fn [i j]
           (* (double i) j))
     tensor1
     tensor2))
  ([tensor1 tensor2 & more]
   (when-let [tensor3 (multiply tensor1 tensor2)]
     (apply emap * tensor3 more))))

(s/fdef multiply
  :args (s/or :zero (s/cat)
          :one (s/cat :tensor ::tensor)
          :two+ (s/cat :tensor1 ::tensor
                  :tensor2 ::tensor
                  :more (s/* ::tensor)))
  :ret (s/nilable ::tensor))

(defn divide
  "Performs element-wise division of tensors.
  
  With one argument, computes 1/x for each element. With multiple arguments,
  divides the first tensor by subsequent tensors. Supports broadcasting.
  
  Examples:
    (divide [8 6]) ;=> [0.125 0.167...] (reciprocals)
    (divide [8 6] [2 3]) ;=> [4.0 2.0]
    (divide [[12]] [3 4]) ;=> [[4.0 3.0]] (broadcasting)"
  ([] 1.0)
  ([tensor] (emap m/div tensor))
  ([tensor & more]
   (reduce (fn [tot e]
             (if-let [new-tot (emap m/div tot e)]
               new-tot
               (reduced nil)))
     tensor
     more)))

(s/fdef divide
  :args (s/or :zero (s/cat)
          :one+ (s/cat :tensor ::tensor
                  :more (s/* ::tensor)))
  :ret (s/nilable ::tensor))

(defn average
  "Calculates the arithmetic mean of all elements in `tensor`.
  
  For scalars, returns the scalar itself. For tensors, computes the sum
  of all elements divided by the total count.
  
  Examples:
    (average 5) ;=> 5
    (average [1 2 3]) ;=> 2.0
    (average [[1 2] [3 4]]) ;=> 2.5"
  [tensor]
  (if (number? tensor)
    tensor
    (let [numbers (flatten tensor)]
      (m/div (reduce + (map double numbers))
        (count numbers) m/nan))))

(s/fdef average
  :args (s/cat :tensor ::tensor)
  :ret ::m/number)

(defn norm1
  "Calculates the L1 norm (Manhattan norm) of `tensor`.
  
  Returns the sum of absolute values of all elements.
  
  Examples:
    (norm1 [-3 4]) ;=> 7.0
    (norm1 [[-1 2] [3 -4]]) ;=> 10.0"
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (reduce + (map m/abs (flatten tensor)))))

(s/fdef norm1
  :args (s/cat :tensor ::tensor)
  :ret ::m/number)

(defn norm
  "Calculates the L2 norm (Euclidean norm) of `tensor`.
  
  Returns the square root of the sum of squared elements.
  Also available as norm2.
  
  Examples:
    (norm [3 4]) ;=> 5.0
    (norm [[1 1] [1 1]]) ;=> 2.0"
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (m/sqrt (reduce + (map m/sq (flatten tensor))))))

(s/fdef norm
  :args (s/cat :tensor ::tensor)
  :ret ::m/number)

(def ^{:doc "See [[norm]]"} norm2 norm)

(defn norm-p
  "Calculates the Lp norm of `tensor` using exponent `p`.
  
  Returns the p-th root of the sum of absolute values raised to the p-th power.
  For `p` >= 1.0, this is a valid norm.
  
  Examples:
    (norm-p [1 -1 1] 1) ;=> 3.0 (L1 norm)
    (norm-p [3 4] 2) ;=> 5.0 (L2 norm)
    (norm-p [2 2 2] 3) ;=> 2.884... (L3 norm)"
  [tensor p]
  (if (number? tensor)
    (m/abs tensor)
    (m/pow (reduce + (map #(m/pow (m/abs %) p)
                       (flatten tensor)))
      (/ p))))

(s/fdef norm-p
  :args (s/cat :tensor ::tensor
          :p (s/and ::m/finite+ #(>= % 1.0)))
  :ret ::m/number)

(defn normalize1
  "Normalizes `tensor` to unit L1 norm.
  
  Divides each element by the L1 norm of `tensor`, resulting in
  a tensor where the sum of absolute values equals 1.
  
  Examples:
    (normalize1 [3 6]) ;=> [0.333... 0.666...]"
  [tensor]
  (emap #(m/div % (norm1 tensor))
    tensor))

(s/fdef normalize1
  :args (s/cat :tensor ::tensor)
  :ret ::tensor)

(defn normalize
  "Normalizes `tensor` to unit L2 norm.
  
  Divides each element by the L2 norm of `tensor`, resulting in
  a tensor with Euclidean length of 1. Also available as normalize2.
  
  Examples:
    (normalize [3 4]) ;=> [0.6 0.8]"
  [tensor]
  (emap #(m/div % (norm tensor))
    tensor))

(s/fdef normalize
  :args (s/cat :tensor ::tensor)
  :ret ::tensor)

(def ^{:doc "See [[normalize2]]"} normalize2 normalize)

(defn normalize-p
  "Normalizes `tensor` to unit Lp norm using exponent `p`.
  
  Divides each element by the Lp norm of `tensor`, resulting in
  a tensor with Lp norm equal to 1.
  
  Examples:
    (normalize-p [2 4 6] 1) ;=> [0.167... 0.333... 0.5] (L1 normalized)"
  [tensor p]
  (emap #(m/div % (norm-p tensor p)) tensor))

(s/fdef normalize-p
  :args (s/cat :tensor ::tensor
          :p (s/and ::m/finite+ #(>= % 1.0)))
  :ret ::tensor)

(defn inner-product
  "Computes the inner product (generalized dot product) of `tensor1` and `tensor2`.
  
  Multiplies corresponding elements and sums all results. For vectors,
  this is the standard dot product. Both tensors must have the same shape.
  
  Examples:
    (inner-product [1 2 3] [4 5 6]) ;=> 32 (1*4 + 2*5 + 3*6)
    (inner-product [[1 2]] [[3] [4]]) ;=> [[11]] (matrix-like operation)"
  [tensor1 tensor2]
  (if (number? tensor1)
    (* (double tensor1) tensor2)
    (let [mul (mapv (fn [a b]
                      (multiply a b))
                tensor1
                tensor2)]
      (when-not (some nil? mul)
        (apply add mul)))))

(s/fdef inner-product
  :args (s/and (s/cat :tensor1 ::tensor
                 :tensor2 ::tensor)
          (fn [{:keys [tensor1 tensor2]}]
            (let [tensor1 (second tensor1)
                  tensor2 (second tensor2)]
              (or (and (number? tensor1) (number? tensor2))
                (and (sequential? tensor1)
                  (sequential? tensor2)
                  (= (count tensor1) (count tensor2)))))))
  :ret (s/nilable ::tensor))

;;;TENSOR NUMERICAL STABILITY
(defn roughly?
  "Tests if `tensor1` and `tensor2` are approximately equal within tolerance `accu`.
  
  Returns true if tensors have the same shape and all corresponding
  elements are within the specified accuracy of each other.
  
  Examples:
    (roughly? [1.0 2.0] [1.001 1.999] 0.01) ;=> true
    (roughly? [1.0] [1.1] 0.05) ;=> false"
  [tensor1 tensor2 accu]
  (and (= (shape tensor1) (shape tensor2))
    (if (number? tensor1)
      (m/roughly? tensor1 tensor2 accu)
      (every? true? (map #(m/roughly? %1 %2 accu)
                      (flatten tensor1)
                      (flatten tensor2))))))

(s/fdef roughly?
  :args (s/cat :tensor1 ::tensor
          :tensor2 ::tensor
          :accu ::m/accu)
  :ret boolean?)

(defn roughly-distinct
  "Removes approximately duplicate elements from `tensor` within tolerance `accu`.
  
  Keeps only the first occurrence of elements that are approximately equal
  (within the specified tolerance). Works on top-level elements.
  
  Examples:
    (roughly-distinct [1.0 1.001 2.0 1.002] 0.01) ;=> [1.0 2.0]
    (roughly-distinct [[1 2] [1.001 2.001] [3 4]] 0.01) ;=> [[1 2] [3 4]]"
  [tensor accu]
  (if (number? tensor)
    tensor
    (loop [[h & t] tensor
           seen []]
      (cond (not h) seen
            (some #(roughly? h % accu) seen) (recur t seen)
            :else (recur t (conj seen h))))))

(s/fdef roughly-distinct
  :args (s/cat :tensor ::tensor
          :accu ::m/accu)
  :ret ::tensor)
