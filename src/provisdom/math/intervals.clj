(ns provisdom.math.intervals
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]))

(def mdl 6)

(s/def ::size
  (s/with-gen ::m/int-non- #(gen/large-integer* {:min 0 :max mdl})))

(s/def ::interval
  (s/and (s/tuple ::m/number ::m/number)
         (fn [[x1 x2]]
           (or (and (m/nan? x1) (m/nan? x2))
               (>= x2 x1)))))

(s/def ::num-interval
  (s/and (s/tuple ::m/num ::m/num)
         (fn [[x1 x2]] (>= x2 x1))))

(s/def ::finite-interval
  (s/and (s/tuple ::m/finite ::m/finite)
         (fn [[x1 x2]] (>= x2 x1))))

(s/def ::int+-interval
  (s/and (s/tuple ::m/int+ ::m/int+)
         (fn [[x1 x2]] (>= x2 x1))))

(s/def ::long-interval
  (s/and (s/tuple ::m/long ::m/long)
         (fn [[x1 x2]] (>= x2 x1))))

(defn long-interval-gen
  ""
  [min max]
  (gen/tuple (gen/large-integer* {:min min :max max})
             (gen/large-integer* {:min min :max max})))

(s/def ::lower ::m/number)
(s/def ::upper ::m/number)
(s/def ::open-lower? boolean?)
(s/def ::open-upper? boolean?)

(s/def ::bounds
  (s/and (s/keys :req [::lower ::upper ::open-lower? ::open-upper?])
         (fn [{::keys [lower upper]}]
           (or (and (m/nan? lower) (m/nan? upper))
               (>= upper lower)))))

(s/def ::vector-bounds (s/coll-of ::bounds :kind vector? :into []))
(s/def ::by-upper? boolean?)

;;INTERVALS
(defn in-interval?
  "Tests whether `number` is inside of the interval."
  [[lower upper] number]
  (and (>= number lower) (<= number upper)))

(s/fdef in-interval?
        :args (s/cat :interval ::interval :number ::m/number)
        :ret boolean?)

(defn in-interval-roughly?
  "Tests whether `number` is roughly inside of the interval."
  [[lower upper] number accu]
  (and (>= number (- lower accu)) (<= number (+ upper accu))))

(s/fdef in-interval-roughly?
        :args (s/cat :interval ::interval
                     :number ::m/number
                     :accu ::m/accu)
        :ret boolean?)

;;;BOUNDS TEST
(defn in-bounds?
  "Tests whether `number` is inside of the bounds."
  [{::keys [lower upper open-lower? open-upper?]} number]
  (and (if open-lower?
         (> number lower)
         (>= number lower))
       (if open-upper?
         (< number upper)
         (<= number upper))))

(s/fdef in-bounds?
        :args (s/cat :bounds ::bounds :number ::m/number)
        :ret boolean?)

;;;BOUNDS CONSTRUCTORS
(defn bounds
  "Default bounds are Inf- to Inf+. Bounds are closed by default."
  ([] (bounds m/inf- m/inf+))
  ([[lower upper]] (bounds lower upper false false))
  ([lower upper] (bounds lower upper false false))
  ([[lower upper] open-lower? open-upper?]
   (bounds lower upper open-lower? open-upper?))
  ([lower upper open-lower? open-upper?]
   {::lower       lower
    ::upper       upper
    ::open-lower? open-lower?
    ::open-upper? open-upper?}))

(s/fdef bounds
        :args (s/or :zero (s/cat)
                    :one (s/cat :interval ::interval)
                    :two (s/and (s/cat :lower ::lower :upper ::upper)
                                (fn [{:keys [lower upper]}]
                                  (or (and (m/nan? lower) (m/nan? upper))
                                      (>= upper lower))))
                    :three (s/cat :interval ::interval
                                  :open-lower? ::open-lower?
                                  :open-upper? ::open-upper?)
                    :four (s/and (s/cat :lower ::lower
                                        :upper ::upper
                                        :open-lower? ::open-lower?
                                        :open-upper? ::open-upper?)
                                 (fn [{:keys [lower upper]}]
                                   (or (and (m/nan? lower) (m/nan? upper))
                                       (>= upper lower)))))
        :ret ::bounds)

(def bounds-finite (bounds m/inf- m/inf+ true true))
(def bounds+ (bounds 0.0 m/inf+ true false))
(def bounds-non- (bounds 0.0 m/inf+))
(def bounds-prob (bounds 0.0 1.0))
(def bounds-open-prob (bounds 0.0 1.0 true true))
(def bounds-long-non- (bounds 0 m/max-long))

(defn vector-bounds
  "Returns a vector of bounds."
  ([size] (vector-bounds size (bounds)))
  ([size bounds] (into [] (repeat size bounds))))

(s/fdef vector-bounds
        :args (s/cat :size ::size :bounds (s/? ::bounds))
        :ret ::vector-bounds)

(defn positive-definite-matrix-bounds
  "Returns a vector of bounds flattened for a symmetric positive matrix."
  [size]
  (let [m (if (zero? size)
            [[]]
            (mapv (fn [row]
                    (mapv (fn [column]
                            (if (== row column) bounds+ (bounds)))
                          (range size)))
                  (range size)))]
    (vec (for [r (range size)
               c (range r size)]
           (get-in m [r c])))))

(s/fdef positive-definite-matrix-bounds
        :args (s/cat :size ::size)
        :ret ::vector-bounds)

(defn get-interval
  "Returns Interval from bounds."
  [bounds]
  [(::lower bounds)
   (::upper bounds)])

(s/fdef get-interval
        :args (s/cat :bounds ::bounds)
        :ret ::interval)

;;;BOUNDS MANIPULATION
(defn- min-bound
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (< bound bound2) [bound open-bound?]
                  (< bound2 bound) [bound2 open-bound2?]
                  :else [bound (and open-bound? open-bound2?)]))
          [m/inf+ false]
          bound-coll))

(defn- max-bound
  [bound-coll]
  (reduce (fn [[bound open-bound?] [bound2 open-bound2?]]
            (cond (> bound bound2) [bound open-bound?]
                  (> bound2 bound) [bound2 open-bound2?]
                  :else [bound (and open-bound? open-bound2?)]))
          [m/inf- false]
          bound-coll))

(defn sort-bounds
  "Returns a vector of bounds sorted by lower bound first (by default) or upper
  bound first."
  ([vector-bounds] (sort-bounds vector-bounds {}))
  ([vector-bounds {::keys [by-upper?] :or {by-upper? false}}]
   (let [f (if by-upper?
             (fn [{::keys [lower upper open-lower? open-upper?]}]
               (vector (- upper) (not open-upper?) (- lower) open-lower?))
             (fn [{::keys [lower upper open-lower? open-upper?]}]
               (vector lower (not open-lower?) upper open-upper?)))]
     (vec (sort-by f vector-bounds)))))

(s/fdef sort-bounds
        :args (s/cat :vector-bounds ::vector-bounds
                     :opts (s/? (s/keys :opt [::by-upper?])))
        :ret ::vector-bounds)

(defn intersection
  "Returns the bounds intersection or nil."
  [vector-bounds]
  (let [[lower open-lower?] (max-bound (map #(vector (::lower %) (::open-lower? %))
                                            vector-bounds))
        [upper open-upper?] (min-bound (map #(vector (::upper %) (::open-upper? %))
                                            vector-bounds))]
    (when-not (or (> lower upper)
                  (and (= upper lower)
                       open-lower?
                       (not open-upper?)))
      (bounds lower upper open-lower? open-upper?))))

(s/fdef intersection
        :args (s/cat :vector-bounds ::vector-bounds)
        :ret (s/nilable ::bounds))

(defn union
  "Returns a vector of non-overlapping bounds."
  [vector-bounds]
  (if (empty? vector-bounds)
    []
    (loop [sep []
           [a b & c] (sort-bounds vector-bounds)]
      (if b
        (let [g (intersection [a b])]
          (if g
            (recur sep (cons g c))
            (recur (conj sep a) (cons b c))))
        (conj sep a)))))

(s/fdef union
        :args (s/cat :vector-bounds ::vector-bounds)
        :ret ::vector-bounds)

(defn encompassing-bounds
  "Returns smallest bounds that encompass the bounds in `vector-bounds`."
  [vector-bounds]
  (let [[lower open-lower?] (min-bound
                              (map (fn [bounds]
                                     (vector (::lower bounds) (::open-lower? bounds)))
                                   vector-bounds))
        [upper open-upper?] (max-bound
                              (map (fn [bounds]
                                     (vector (::upper bounds) (::open-upper? bounds)))
                                   vector-bounds))]
    (bounds lower upper open-lower? open-upper?)))

(s/fdef encompassing-bounds
        :args (s/cat :vector-bounds (s/and ::vector-bounds
                                           (fn [vb]
                                             (pos? (count vb)))))
        :ret ::bounds)
  

