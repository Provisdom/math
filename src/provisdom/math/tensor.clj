(ns provisdom.math.tensor
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
  "Returns true if a tensor (contains numbers only, and each dimension has rows
  of equal lengths)."
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
  "Tries to convert any `x` to tensor, otherwise returns nil."
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
  "`indices->number` is a function that takes a vector of `indices` and returns
  a number."
  [shape indices->number]
  (recursive-compute-tensor shape indices->number []))

(s/fdef compute-tensor
  :args (s/cat :shape ::shape
          :indices->number (s/fspec :args (s/cat :indices ::indices)
                             :ret ::m/number))
  :ret ::tensor)

(defn repeat-tensor
  "Constructs a new tensor of zeros (doubles) with the given `shape`, or
  constructs a new tensor with a new shape that is the concat of `shape` and the
  shape of the `seed-tensor` by placing copies of `seed-tensor` into `shape`."
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
  "Creates a new tensor with a given `shape` from a sequence of `numbers`.
  If `numbers` doesn't fill tensor, will fill remaining elements with 0.0."
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
  "Creates a new tensor with a given `shape` with random doubles."
  [shape]
  (fill-tensor shape (take (reduce * shape) (random/rnd-lazy!))))

(s/fdef rnd-tensor!
  :args (s/cat :shape ::shape)
  :ret ::tensor)

;;;TENSOR INFO
(defn first-number
  "Returns the first number in the tensor."
  [tensor]
  (cond (number? tensor) tensor
        (nil? tensor) tensor
        :else (first-number (first tensor))))

(s/fdef first-number
  :args (s/cat :tensor (s/nilable ::tensor))
  :ret (s/nilable ::m/number))

(defn ecount
  "Returns the total count of elements."
  [tensor]
  (if (number? tensor)
    1
    (count (flatten tensor))))

(s/fdef ecount
  :args (s/cat :tensor ::tensor)
  :ret ::m/int-non-)

(defn rank
  "Returns the rank of a `tensor`. The rank is equal to the number of dimensions
  in the `tensor`'s shape."
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
  "Returns the shape of the `tensor`."
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
  "Returns true if function `indices+number->bool` is true for every element in
  `tensor`, else false."
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
  "Returns a vector of tensors of the tensors in `tensor-v` for which function
  `index+tensor->bool` returns true. `index+tensor->bool` must be free of
  side effects."
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
  "Element-wise mapping over all elements of one or more tensors. Returns nil if
  tensors can't be expanded as necessary."
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
  "Element-wise mapping over all elements of one or more tensors. Function `f`
  takes the indices and one number per tensor, and returns a number."
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
  "Partitions recursively in sets of 'n' elements. There may be unused elements.
  For example, a 1000-element tensor could be partitioned into 10x10x10."
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
  "Tensor equality that works with NaN."
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
  "Performs element-wise addition for one or more tensors."
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
  "Performs element-wise subtraction for one or more tensors."
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
  "Performs element-wise multiplication for one or more tensors."
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
  "Performs element-wise division for one or more tensors."
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
  "The average of the values of the elements."
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
  "The sum of the absolute values of the elements."
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (reduce + (map m/abs (flatten tensor)))))

(s/fdef norm1
  :args (s/cat :tensor ::tensor)
  :ret ::m/number)

(defn norm
  "The square-root of the sum of the squared values of the elements."
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (m/sqrt (reduce + (map m/sq (flatten tensor))))))

(s/fdef norm
  :args (s/cat :tensor ::tensor)
  :ret ::m/number)

(def ^{:doc "See [[norm]]"} norm2 norm)

(defn norm-p
  "The 1/`p` power of the sum of the element values to the power `p`. To be a
  norm, `p` must be <= 1.0."
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
  "Returns as length one in [[norm1]]."
  [tensor]
  (emap #(m/div % (norm1 tensor))
    tensor))

(s/fdef normalize1
  :args (s/cat :tensor ::tensor)
  :ret ::tensor)

(defn normalize
  "Returns as length one in [[norm2]]."
  [tensor]
  (emap #(m/div % (norm tensor))
    tensor))

(s/fdef normalize
  :args (s/cat :tensor ::tensor)
  :ret ::tensor)

(def ^{:doc "See [[normalize2]]"} normalize2 normalize)

(defn normalize-p
  "Returns as length one in [[norm-p]]. To be a norm, `p` must be <= 1.0."
  [tensor p]
  (emap #(m/div % (norm-p tensor p)) tensor))

(s/fdef normalize-p
  :args (s/cat :tensor ::tensor
          :p (s/and ::m/finite+ #(>= % 1.0)))
  :ret ::tensor)

(defn inner-product
  "The inner product is the generalization of the [[dot-product]]. It is the
  tensor sum of the tensor products of the corresponding entries of two tensors.
  The count of both tensors must be the same."
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
  "Returns true if every element compared across two similarly-shaped tensors
  are within `accu` of each other."
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
  "Returns a tensor with later duplicate top-level rows (or elements) removed."
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
