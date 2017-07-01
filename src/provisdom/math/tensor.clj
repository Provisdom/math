(ns provisdom.math.tensor
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]))

(set! *warn-on-reflection* true)

(declare transpose dimensionality tensor?)

(s/def ::number ::m/number)
(s/def ::tensor1D (s/with-gen (s/coll-of ::number :kind vector? :into [])
                              #(gen/vector (s/gen ::number) 0 2)))
(s/def ::tensor2D (s/with-gen #(and (tensor? %) (= 2 (dimensionality %)))
                              #(gen/bind (gen/large-integer* {:min 1 :max 2})
                                         (fn [i] (gen/vector (gen/vector (s/gen ::number) i) 1 2)))))
(s/def ::tensor3D (s/with-gen #(and (tensor? %) (= 3 (dimensionality %)))
                              #(gen/bind (gen/tuple (gen/large-integer* {:min 1 :max 2})
                                                    (gen/large-integer* {:min 1 :max 2}))
                                         (fn [[i j]] (gen/vector (gen/vector (gen/vector (s/gen ::number) i) j) 1 2)))))
(s/def ::tensor4D (s/with-gen #(and (tensor? %) (= 4 (dimensionality %)))
                              #(gen/bind
                                 (gen/tuple (gen/large-integer* {:min 1 :max 2})
                                            (gen/large-integer* {:min 1 :max 2})
                                            (gen/large-integer* {:min 1 :max 2}))
                                 (fn [[i j k]]
                                   (gen/vector (gen/vector (gen/vector (gen/vector (s/gen ::number) i) j) k) 1 2)))))
(s/def ::tensor5D+ (s/with-gen #(and (tensor? %) (>= (dimensionality %) 5))
                               #(gen/bind
                                  (gen/tuple (gen/large-integer* {:min 1 :max 2})
                                             (gen/large-integer* {:min 1 :max 2})
                                             (gen/large-integer* {:min 1 :max 2})
                                             (gen/large-integer* {:min 1 :max 2}))
                                  (fn [[i j k l]]
                                    (gen/vector
                                      (gen/vector
                                        (gen/vector (gen/vector (gen/vector (s/gen ::number) i) j) k) l) 1 2)))))
(s/def ::tensor (s/or :number ::number
                      :t1 ::tensor1D
                      :t2 ::tensor2D
                      :t3+ (s/or :t3 ::tensor3D             ;used 2nd 's/or' to reduce these in tests
                                 :t4 ::tensor4D
                                 :t5+ ::tensor5D+)))
(s/def ::accu ::m/non-)
(s/def ::shape (s/with-gen (s/coll-of ::m/int+) #(gen/vector (gen/large-integer* {:min 1 :max 2}) 0 6)))
(s/def ::index ::m/int-non-)
(s/def ::indices (s/with-gen (s/coll-of ::index) #(gen/vector (s/gen ::index) 0 2)))

;;;TENSOR TYPES
(defn tensor?
  "Returns true if a tensor (contains numbers only, and each dimension has rows of equal lengths)."
  [x]
  (or (number? x)
      (and (vector? x) (every? #(number? %) x))
      (and (vector? x)
           (every? #(and (vector? %) (= (count %) (count (first x)))) x)
           (every? tensor? x))))

(s/fdef tensor?
        :args (s/cat :x any?)
        :ret boolean?)

;;;TENSOR CONSTRUCTOR
(defn to-tensor
  "Tries to convert to tensor, otherwise returns nil."
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
      (mapv #(recursive-compute-tensor shape f (conj sh %)) (range (get shape dim))))))

(defn compute-tensor
  "`f` takes a vector of `indices` and returns a number."
  [shape f] (recursive-compute-tensor shape f []))

(s/fdef compute-tensor
        :args (s/cat :shape ::shape :f (s/fspec :args (s/cat :indices ::indices) :ret ::number))
        :ret ::tensor)

(defn repeat-tensor
  "Constructs a new tensor of zeros (doubles) with the given `shape`, or
  constructs a new tensor with a new shape that is the concat of `shape` and the shape of the `seed-tensor`
  by placing copies of `seed-tensor` into `shape`."
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
        :args (s/cat :seed-tensor ::tensor :shape ::shape)
        :ret ::tensor)

;;;TENSOR INFO
(defn dimensionality
  "Returns the dimensionality of an `tensor`.
  The dimensionality is equal to the number of dimensions in the `tensor`'s shape."
  [tensor]
  (if (number? tensor)
    0
    (inc (dimensionality (first tensor)))))

(s/fdef dimensionality
        :args (s/cat :tensor ::tensor)
        :ret ::m/int-non-)

(defn shape
  "Returns the shape of the `tensor`."
  [tensor]
  (loop [dim 0, sh [], remain tensor]
    (if (sequential? remain)
      (let [r (first remain)]
        (recur (inc dim) (conj sh (count remain)) r))
      sh)))

(s/fdef shape
        :args (s/cat :tensor ::tensor)
        :ret ::shape)

(defn every-kv?
  "Returns true if (pred indices number) is logically true for every element in `tensor`, else false."
  [pred tensor] (every? true? (flatten (compute-tensor (shape tensor) #(pred % (get-in tensor %))))))

(s/fdef every-kv?
        :args (s/cat :pred (s/fspec :args (s/cat :indices ::indices :number ::number) :ret boolean?))
        :ret boolean?)

;;;TENSOR MANIPULATION
(defn- convertible-shape?
  "Tests whether a tensor with `shape` can be expanded into `expanded-shape`."
  [shape expanded-shape] (every? zero? (map - (take-last (count shape) expanded-shape) shape)))

(defn- recursive-emap
  "Recursively maps for [[emap]]."
  [shape f sh tensors]
  (let [c (count shape)
        dim (count sh)]
    (if (= dim c)
      (apply f (map #(get-in % sh) tensors))
      (mapv #(recursive-emap shape f (conj sh %) tensors) (range (get shape dim))))))

(defn emap
  "Element-wise mapping over all elements of one or more tensors.
  Returns nil if tensors can't be expanded as necessary."
  ([f tensor] (recursive-emap (shape tensor) f [] [tensor]))
  ([f tensor & more]
   (let [tensors (concat [tensor] more)
         shapes (map shape tensors)
         [largest-shape large-count] (reduce (fn [[sh c] new-sh] (let [new-c (count new-sh)]
                                                                   (if (> new-c c) [new-sh new-c] [sh c])))
                                             [(first shapes) (count (first shapes))]
                                             (rest shapes))
         new-tensors (when (every? #(convertible-shape? % largest-shape) shapes)
                       (map #(repeat-tensor (vec (take (- large-count (count %2)) largest-shape)) %1) tensors shapes))]
     (recursive-emap largest-shape f [] new-tensors))))

(s/fdef emap
        :args (s/cat :f (s/fspec :args (s/cat :number ::number)
                                 :ret ::number)
                     :tensor ::tensor
                     :more (s/* ::tensor))
        :ret (s/nilable ::tensor))

(defn- recursive-emap-kv
  "Recursively maps for [[emap-kv]]."
  [shape f sh tensors]
  (let [c (count shape)
        dim (count sh)]
    (if (= dim c)
      (apply f sh (map #(get-in % sh) tensors))
      (mapv #(recursive-emap-kv shape f (conj sh %) tensors) (range (get shape dim))))))

(defn emap-kv
  "Element-wise mapping over all elements of one or more tensors.
  `f` takes the shape and a number, and returns a number."
  ([f tensor] (recursive-emap-kv (shape tensor) f [] [tensor]))
  ([f tensor & more]
   (let [tensors (concat [tensor] more)
         shapes (map shape tensors)
         [largest-shape large-count] (reduce (fn [[sh c] new-sh] (let [new-c (count new-sh)]
                                                                   (if (> new-c c) [new-sh new-c] [sh c])))
                                             [(first shapes) (count (first shapes))]
                                             (rest shapes))
         new-tensors (when (every? #(convertible-shape? % largest-shape) shapes)
                       (map #(repeat-tensor (vec (take (- large-count (count %2)) largest-shape)) %1) tensors shapes))]
     (recursive-emap-kv largest-shape f [] new-tensors))))

(s/fdef emap-kv
        :args (s/cat :f (s/fspec :args (s/cat :shape ::shape :number ::number)
                                 :ret ::number)
                     :tensor ::tensor
                     :more (s/* ::tensor))
        :ret (s/nilable ::tensor))

;;;TENSOR MATH
(defn add
  "Performs element-wise addition for one or more tensors."
  ([] 0.0)
  ([tensor] (emap + tensor))
  ([tensor & more] (apply emap + tensor more)))

(s/fdef add
        :args (s/or :zero (s/cat)
                    :one+ (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor))))
        :ret (s/nilable ::tensor))

(defn subtract
  "Performs element-wise subtraction for one or more tensors."
  ([tensor] (emap - tensor))
  ([tensor & more] (apply emap - tensor more)))

(s/fdef subtract
        :args (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor)))
        :ret (s/nilable ::tensor))

(defn multiply
  "Performs element-wise multiplication for one or more tensors."
  ([] 1.0)
  ([tensor] (emap * tensor))
  ([tensor & more] (apply emap * tensor more)))

(s/fdef multiply
        :args (s/or :zero (s/cat)
                    :one+ (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor))))
        :ret (s/nilable ::tensor))

(defn divide
  "Performs element-wise division for one or more tensors."
  ([tensor] (emap m/div tensor))
  ([tensor & more] (apply emap m/div tensor more)))

(s/fdef divide
        :args (s/cat :tensor ::tensor :more (s/? (s/keys* :tensors ::tensor)))
        :ret (s/nilable ::tensor))

(defn ecount
  "Returns the total count of elements."
  [tensor] (if (number? tensor) 1 (count (flatten tensor))))

(s/fdef ecount
        :args (s/cat :tensor ::tensor)
        :ret ::m/int-non-)

(defn norm1
  "The sum of the absolute values of the elements."
  [tensor] (apply + (map m/abs (flatten tensor))))

(s/fdef norm1
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(defn norm
  "The square-root of the sum of the squared values of the elements."
  [tensor] (m/sqrt (apply + (map m/sq (flatten tensor)))))

(s/fdef norm
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(def ^{:doc "See [[norm]]"} norm2 norm)

(defn norm-p
  "The 1/`p` power of the sum of the element values to the power `p`.
  To be a norm, `p` must be <= 1.0."
  [tensor p] (m/pow (apply + (map #(m/pow (m/abs %) p) (flatten tensor))) (/ p)))

(s/fdef norm-p
        :args (s/cat :tensor ::tensor :p (s/and ::m/finite+ #(>= % 1.0)))
        :ret ::number)

(defn normalize1
  "Returns as length one in [[norm1]]."
  [tensor]
  (let [n1 (norm1 tensor)
        ser (vec (emap #(m/div % n1) tensor))
        diff (m/one- (apply + (flatten ser)))]
    ;;check for slight rounding errors...
    (if (zero? diff)
      ser
      (assoc ser 0 (+ diff (first ser))))))

(s/fdef normalize1
        :args (s/cat :tensor ::tensor)
        :ret ::tensor)

(defn normalize
  "Returns as length one in [[norm2]]."
  [tensor] (emap #(m/div % (norm tensor)) tensor))

(s/fdef normalize
        :args (s/cat :tensor ::tensor)
        :ret ::tensor)

(def ^{:doc "See [[normalize2]]"} normalize2 normalize)

(defn normalize-p
  "Returns as length one in [[norm-p]].
  To be a norm, `p` must be <= 1.0."
  [tensor p] (emap #(m/div % (norm-p tensor p)) tensor))

(s/fdef normalize-p
        :args (s/cat :tensor ::tensor :p (s/and ::m/finite+ #(>= % 1.0)))
        :ret ::tensor)

;;;TENSOR NUMERICAL STABILITY
(defn roughly?
  "Returns true if every element compared across two tensors are within `accu` of each other."
  [tensor1 tensor2 accu] (every? true? (flatten (emap #(m/roughly? %1 %2 accu) tensor1 tensor2))))

(s/fdef roughly?
        :args (s/cat :tensor1 ::tensor :tensor2 ::tensor :accu ::accu)
        :ret boolean?)

(defn roughly-distinct
  "Returns a tensor with later duplicate top-level rows (or elements) removed"
  [tensor accu]
  (if (number? tensor)
    tensor
    (loop [[h & t] tensor, seen []]
      (cond (not h) seen
            (some #(roughly? h % accu) seen) (recur t seen)
            :else (recur t (conj seen h))))))

(s/fdef roughly-distinct
        :args (s/cat :tensor ::tensor :accu ::accu)
        :ret ::tensor)