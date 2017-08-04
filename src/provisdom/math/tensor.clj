(ns provisdom.math.tensor
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.random2 :as random]
            [provisdom.utility-belt.core :as co]))

(set! *warn-on-reflection* true)

(declare transpose dimensionality tensor?)

(def mdl 6)                                                 ;max-dim-length for generators

(s/def ::number ::m/number)
(s/def ::numbers (s/with-gen (s/coll-of ::number)
                             #(s/gen (s/or :v (s/coll-of ::number :min-count 0 :max-count mdl :kind vector? :into [])
                                           :l (s/coll-of ::number :min-count 0 :max-count mdl :kind list? :into '())))))
(s/def ::tensor1D (s/with-gen (s/coll-of ::number :kind vector? :into [])
                              #(gen/vector (s/gen ::number) 0 mdl)))
(s/def ::tensor2D (s/with-gen #(and (tensor? %) (= 2 (dimensionality %)))
                              #(gen/bind (gen/large-integer* {:min 0 :max mdl})
                                         (fn [i] (gen/vector (gen/vector (s/gen ::number) i) 1 mdl)))))
(s/def ::tensor3D (s/with-gen #(and (tensor? %) (= 3 (dimensionality %)))
                              #(gen/bind (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                                                    (gen/large-integer* {:min 1 :max mdl}))
                                         (fn [[i j]] (gen/vector
                                                       (gen/vector (gen/vector (s/gen ::number) i) j) 1 mdl)))))
(s/def ::tensor4D (s/with-gen #(and (tensor? %) (= 4 (dimensionality %)))
                              #(gen/bind
                                 (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                                            (gen/large-integer* {:min 1 :max mdl})
                                            (gen/large-integer* {:min 1 :max mdl}))
                                 (fn [[i j k]]
                                   (gen/vector (gen/vector (gen/vector (gen/vector (s/gen ::number) i) j) k) 1 mdl)))))
(s/def ::tensor5D+ (s/with-gen #(and (tensor? %) (>= (dimensionality %) 5))
                               #(gen/bind
                                  (gen/tuple (gen/large-integer* {:min 0 :max mdl})
                                             (gen/large-integer* {:min 1 :max mdl})
                                             (gen/large-integer* {:min 1 :max mdl})
                                             (gen/large-integer* {:min 1 :max mdl}))
                                  (fn [[i j k l]]
                                    (gen/vector
                                      (gen/vector
                                        (gen/vector (gen/vector (gen/vector (s/gen ::number) i) j) k) l) 1 mdl)))))
(s/def ::tensor (s/or :number ::number
                      :t1 ::tensor1D
                      :t2 ::tensor2D
                      :t3+ (s/or :t3 ::tensor3D             ;used 2nd 's/or' to reduce these in tests
                                 :t4 ::tensor4D
                                 :t5+ ::tensor5D+)))
(s/def ::accu (s/with-gen ::m/non- #(gen/double* {:min m/tiny-dbl :max 1e-3 :NaN? false})))
(s/def ::shape (s/with-gen (s/coll-of ::m/int-non-) #(gen/vector (gen/large-integer* {:min 0 :max mdl}) 0 mdl)))
(s/def ::index (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))
(s/def ::indices (s/with-gen (s/coll-of ::index) #(gen/vector (s/gen ::index) 0 mdl)))

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
      (mapv #(recursive-compute-tensor shape f (conj sh %)) (range (get shape dim))))))

(defn compute-tensor
  "`f` takes a vector of `indices` and returns a number."
  [shape f] (recursive-compute-tensor shape f []))

(s/fdef compute-tensor
        :args (s/cat :shape ::shape :f (s/fspec :args (s/cat :indices ::indices) :ret ::number))
        :ret ::tensor)

(defn repeat-tensor
  "Constructs a new tensor of zeros (doubles) with the given `shape`, or constructs a new tensor with a new shape
  that is the concat of `shape` and the shape of the `seed-tensor` by placing copies of `seed-tensor` into `shape`."
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
    (let [tot (apply * shape)]
      (if (zero? tot)
        (repeat-tensor shape)
        (let [cn (count numbers)
              rem (- tot cn)
              tensor (vec (if (neg? rem) (take tot numbers) (concat numbers (repeat rem 0.0))))]
          (reduce (fn [tot sh] (vec (map vec (partition sh tot)))) tensor (reverse (rest shape))))))))

(s/fdef fill-tensor
        :args (s/cat :shape ::shape :numbers ::numbers)
        :ret ::tensor)

(defn rnd-tensor!
  "Creates a new tensor with a given `shape` with random doubles."
  [shape] (fill-tensor shape (take (apply * shape) (random/rand-double-lazy!))))

(s/fdef rnd-tensor!
        :args (s/cat :shape ::shape)
        :ret ::tensor)

;;;TENSOR INFO
(defn ecount
  "Returns the total count of elements."
  [tensor] (if (number? tensor) 1 (count (flatten tensor))))

(s/fdef ecount
        :args (s/cat :tensor ::tensor)
        :ret ::m/int-non-)

(defn dimensionality
  "Returns the dimensionality of an `tensor`.
  The dimensionality is equal to the number of dimensions in the `tensor`'s shape."
  [tensor]
  (if (vector? tensor)
    (let [f (first tensor)]
      (if f (inc (dimensionality f)) 1))
    0))

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
  [pred tensor]
  (if (number? tensor)
    (pred [] tensor)
    (every? true? (flatten (recursive-compute-tensor (shape tensor) #(pred % (get-in tensor %)) [])))))

(comment                                                    ;instrumentation won't pass (fspec issues)
  (s/fdef every-kv?
          :args (s/cat :pred (s/with-gen (s/fspec :args (s/cat :indices ::indices :number ::number)
                                                  :ret boolean?)
                                         #(gen/return (constantly true)))
                       :tensor ::tensor)
          :ret boolean?))

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
     (when new-tensors (recursive-emap largest-shape f [] new-tensors)))))

(comment                                                    ;slow
  (s/fdef emap
          :args (s/and (s/cat :f (s/with-gen (s/fspec :args (s/cat :number (s/+ ::number))
                                                      :ret ::number)
                                             #(gen/return (constantly 1.0)))
                              :tensor ::tensor
                              :more (s/* ::tensor))
                       #(some (fn [a] (or (= (inc (count (:more %))) a) (= a :rest))) (co/arities (:f %))))
          :ret (s/nilable ::tensor)))

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

(comment                                                    ;;slow
  (s/fdef emap-kv
          :args (and (s/cat :f (s/with-gen (s/fspec :args (s/cat :shape ::shape :number (s/+ ::number))
                                                    :ret ::number)
                                           #(gen/return (constantly 1.0)))
                            :tensor ::tensor
                            :more (s/* ::tensor))
                     #(some (fn [a] (or (= (+ 2 (count (:more %))) a) (= a :rest))) (co/arities (:f %))))
          :ret (s/nilable ::tensor)))

;;;TENSOR MATH
(defn ===
  "Tensor equality that works with NaN."
  ([tensor] true)
  ([tensor1 tensor2]
   (let [bs (emap (fn [i j] (m/=== i j)) tensor1 tensor2)]
     (if (and bs (every? true? (flatten bs)))
       true
       false)))
  ([tensor1 tensor2 & more] (and (=== tensor1 tensor2) (apply === tensor2 more))))

(s/fdef ===
        :args (s/or :one (s/cat :tensor ::tensor)
                    :two+ (s/cat :tensor1 ::tensor :tensor2 ::tensor :more (s/* ::tensor)))
        :ret boolean?)

(defn add
  "Performs element-wise addition for one or more tensors."
  ([] 0.0)
  ([tensor] tensor)
  ([tensor1 tensor2] (emap (fn [i j] (+ (double i) j)) tensor1 tensor2))
  ([tensor1 tensor2 & more] (when-let [tensor3 (add tensor1 tensor2)] (apply emap + tensor3 more))))

(s/fdef add
        :args (s/or :zero (s/cat)
                    :one (s/cat :tensor ::tensor)
                    :two+ (s/cat :tensor1 ::tensor :tensor2 ::tensor :more (s/* ::tensor)))
        :ret (s/nilable ::tensor))

(defn subtract
  "Performs element-wise subtraction for one or more tensors."
  ([] 0.0)
  ([tensor] (emap - tensor))
  ([tensor1 tensor2] (emap (fn [i j] (- (double i) j)) tensor1 tensor2))
  ([tensor1 tensor2 & more] (when-let [tensor3 (subtract tensor1 tensor2)] (apply emap - tensor3 more))))

(s/fdef subtract
        :args (s/or :zero (s/cat)
                    :one (s/cat :tensor ::tensor)
                    :two+ (s/cat :tensor1 ::tensor :tensor2 ::tensor :more (s/* ::tensor)))
        :ret (s/nilable ::tensor))

(defn multiply
  "Performs element-wise multiplication for one or more tensors."
  ([] 1.0)
  ([tensor] tensor)
  ([tensor1 tensor2] (emap (fn [i j] (* (double i) j)) tensor1 tensor2))
  ([tensor1 tensor2 & more] (when-let [tensor3 (multiply tensor1 tensor2)] (apply emap * tensor3 more))))

(s/fdef multiply
        :args (s/or :zero (s/cat)
                    :one (s/cat :tensor ::tensor)
                    :two+ (s/cat :tensor1 ::tensor :tensor2 ::tensor :more (s/* ::tensor)))
        :ret (s/nilable ::tensor))

(defn divide
  "Performs element-wise division for one or more tensors."
  ([] 1.0)
  ([tensor] (emap m/div tensor))
  ([tensor & more] (reduce (fn [tot e] (when tot (emap m/div tot e))) tensor more)))

(s/fdef divide
        :args (s/or :zero (s/cat)
                    :one+ (s/cat :tensor ::tensor :more (s/* ::tensor)))
        :ret (s/nilable ::tensor))

(defn average
  "The average of the values of the elements."
  [tensor]
  (if (number? tensor)
    tensor
    (let [v (flatten tensor)] (m/div (apply + v) (count v) m/nan))))

(s/fdef average
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(defn norm1
  "The sum of the absolute values of the elements."
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (apply + (map m/abs (flatten tensor)))))

(s/fdef norm1
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(defn norm
  "The square-root of the sum of the squared values of the elements."
  [tensor]
  (if (number? tensor)
    (m/abs tensor)
    (m/sqrt (apply + (map m/sq (flatten tensor))))))

(s/fdef norm
        :args (s/cat :tensor ::tensor)
        :ret ::number)

(def ^{:doc "See [[norm]]"} norm2 norm)

(defn norm-p
  "The 1/`p` power of the sum of the element values to the power `p`.
  To be a norm, `p` must be <= 1.0."
  [tensor p]
  (if (number? tensor)
    (m/abs tensor)
    (m/pow (apply + (map #(m/pow (m/abs %) p) (flatten tensor))) (/ p))))

(s/fdef norm-p
        :args (s/cat :tensor ::tensor :p (s/and ::m/finite+ #(>= % 1.0)))
        :ret ::number)

(defn normalize1
  "Returns as length one in [[norm1]]."
  [tensor] (emap #(m/div % (norm1 tensor)) tensor))

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
  "Returns true if every element compared across two similarly-shaped tensors are within `accu` of each other."
  [tensor1 tensor2 accu]
  (and (= (shape tensor1) (shape tensor2))
       (if (number? tensor1)
         (m/roughly? tensor1 tensor2 accu)
         (every? true? (map #(m/roughly? %1 %2 accu) (flatten tensor1) (flatten tensor2))))))

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