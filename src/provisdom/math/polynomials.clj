(ns provisdom.math.polynomials
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]
    [provisdom.math.vector :as vector]
    [provisdom.math.combinatorics :as combinatorics]
    [provisdom.math.derivatives :as derivatives]))

;;;DECLARATIONS
(declare polynomial-fn)

(s/def ::second-kind? boolean?)

(s/def ::degree
  (s/with-gen ::m/long-non-
              #(s/gen (s/int-in 0 3))))

(s/def ::start-degree ::degree)
(s/def ::end-degree ::degree)
(s/def ::basis-count ::degree)
(s/def ::chebyshev-kind (s/int-in 0 3))

(s/def ::number->number
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::m/number))

(s/def ::number->v
  (s/fspec :args (s/cat :number ::m/number)
           :ret ::vector/vector))

(s/def ::number2->v
  (s/fspec :args (s/cat :number1 ::m/number
                        :number2 ::m/number)
           :ret ::vector/vector))

(s/def ::v->v
  (s/fspec :args (s/cat :v ::vector/vector)
           :ret ::vector/vector))

;;;CONSTANTS
(def ^:const ^:private chebyshev-polynomial-of-the-first-kind-fns
  [(fn [_] 1.0)
   #(* 1.0 %)
   #(dec (* 2.0 (m/sq %)))
   #(+ (* 4 (m/cube %)) (* -3 %))
   #(+ (* 8 (m/pow % 4)) (* -8 (m/sq %)) 1)
   #(+ (* 16 (m/pow % 5)) (* -20 (m/cube %)) (* 5 %))
   #(+ (* 32 (m/pow % 6)) (* -48 (m/pow % 4)) (* 18 (m/sq %)) -1)
   #(+ (* 64 (m/pow % 7)) (* -112 (m/pow % 5)) (* 56 (m/cube %)) (* -7 %))
   #(+ (* 128 (m/pow % 8))
       (* -256 (m/pow % 6))
       (* 160 (m/pow % 4))
       (* -32 (m/sq %)) 1)
   #(+ (* 256 (m/pow % 9))
       (* -576 (m/pow % 7))
       (* 432 (m/pow % 5))
       (* -120 (m/cube %)) (* 9 %))
   #(+ (* 512 (m/pow % 10))
       (* -1280 (m/pow % 8))
       (* 1120 (m/pow % 6))
       (* -400 (m/pow % 4))
       (* 50 (m/sq %)) -1)
   #(+ (* 1024 (m/pow % 11))
       (* -2816 (m/pow % 9))
       (* 2816 (m/pow % 7))
       (* -1232 (m/pow % 5))
       (* 220 (m/cube %))
       (* -11 %))])

(def ^:const ^:private chebyshev-polynomial-of-the-second-kind-fns
  [(fn [_] 1.0)
   #(* 2.0 %)
   #(dec (* 4.0 (m/sq %)))
   #(+ (* 8 (m/cube %)) (* -4 %))
   #(+ (* 16 (m/pow % 4)) (* -12 (m/sq %)) 1)
   #(+ (* 32 (m/pow % 5)) (* -32 (m/cube %)) (* 6 %))
   #(+ (* 64 (m/pow % 6)) (* -80 (m/pow % 4)) (* 24 (m/sq %)) -1)
   #(+ (* 128 (m/pow % 7)) (* -192 (m/pow % 5)) (* 80 (m/cube %)) (* -8 %))
   #(+ (* 256 (m/pow % 8))
       (* -448 (m/pow % 6))
       (* 240 (m/pow % 4))
       (* -40 (m/sq %))
       1)
   #(+ (* 512 (m/pow % 9))
       (* -1024 (m/pow % 7))
       (* 672 (m/pow % 5))
       (* -160 (m/cube %))
       (* 10 %))])

;;;CHEBYSHEV POLYNOMIALS
(defn chebyshev-polynomial-fn
  "Returns a chebyshev polynomial function of `degree`. Can optionally use first
  kind (default) or set `second-kind?` to true."
  ([degree] (chebyshev-polynomial-fn degree {}))
  ([degree {::keys [second-kind?] :or {second-kind? false}}]
   (let [degree (long degree)
         fns (if second-kind?
               chebyshev-polynomial-of-the-second-kind-fns
               chebyshev-polynomial-of-the-first-kind-fns)
         m (if second-kind? 9 11)]
     (if (<= degree m)
       (fns degree)
       #(second (last (take (inc (- degree m))
                            (iterate (fn [[old new]]
                                       [new
                                        (- (* 2 new %) old)])
                                     [((fns (dec m)) %) ((fns m) %)]))))))))

(s/fdef chebyshev-polynomial-fn
  :args (s/cat :degree ::degree
               :opts (s/? (s/keys :opt [::second-kind?])))
  :ret ::number->number)

;;http://en.wikipedia.org/wiki/Chebyshev_polynomials 
;;-- also solved for the derivative of second-kind at x = +-1
(defn chebyshev-derivative-fn
  "Returns a chebyshev-derivative function. Can optionally use first kind
  (default) or set `second-kind?` to true. Will use numerical derivative when
  necessary."
  ([degree derivative] (chebyshev-derivative-fn degree derivative {}))
  ([degree derivative {::keys [second-kind?] :or {second-kind? false}}]
   (cond (zero? degree)
         (constantly 0.0)

         (m/one? derivative)
         (if second-kind?
           (let [y (inc degree)]
             #(if (m/one? (m/abs %))
                (* (/ 3)
                   (m/pow (m/sgn %) y)
                   (- (m/cube y) y))
                (/ (- (* y ((chebyshev-polynomial-fn y) %))
                      (* % ((chebyshev-polynomial-fn
                              degree {::second-kind? true}) %)))
                   (dec (m/sq %)))))
           #(* degree ((chebyshev-polynomial-fn
                         (dec degree) {::second-kind? true}) %)))

         (and (= 2 derivative)
              (not second-kind?))
         #(if (m/one? (m/abs %))
            (* (/ 3)
               (m/pow (m/sgn %) degree)
               (- (m/pow degree 4) (m/sq degree)))
            (let [first-kind ((chebyshev-polynomial-fn degree) %)
                  second-kind ((chebyshev-polynomial-fn
                                 degree {::second-kind? true}) %)]
              (* degree
                 (- (* (inc degree) first-kind) second-kind)
                 (/ (dec (m/sq %))))))

         :else
         (derivatives/derivative-fn
           (chebyshev-polynomial-fn degree {::second-kind? second-kind?})
           {::derivatives/derivative derivative}))))

(s/fdef chebyshev-derivative-fn
  :args (s/cat :degree ::degree
               :derivative ::derivatives/derivative
               :opts (s/? (s/keys :opt [::second-kind?])))
  :ret ::number->number)

(defn chebyshev-polynomial-factors-to-regular-polynomial-factors
  "Returns polynomial factors a (i.e., a0 + a1 * x + a2 * x^2 +...) from
  chebyshev factors (i.e., b0 + b1 * x + b2 * (2x^2 - 1) + ...). Can optionally
  use first kind (default) or set `second-kind?` to true."
  ([chebyshev-factors]
   (chebyshev-polynomial-factors-to-regular-polynomial-factors
     chebyshev-factors {}))
  ([chebyshev-factors {::keys [second-kind?] :or {second-kind? false}}]
   (let [n (count chebyshev-factors)]
     (map (fn [i]
            ((derivatives/derivative-fn
               #(vector/dot-product
                  (vec chebyshev-factors)
                  (vec ((polynomial-fn (dec n)
                                       {::chebyshev-kind (if second-kind? 2 1)})
                        %)))
               {::derivatives/derivative i})
             0.0))
          (range n)))))

(s/fdef chebyshev-polynomial-factors-to-regular-polynomial-factors
  :args (s/cat :chebyshev-factors ::m/numbers
               :opts (s/? (s/keys :opt [::second-kind?])))
  :ret (s/coll-of ::m/number))

;;;POLYNOMIAL SERIES
(defn- polynomial-functions
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular
  polynomial."
  [chebyshev-kind]
  (condp = chebyshev-kind
    0 (fn [x]
        (fn [degree]
          (m/pow x degree)))
    1 (fn [x]
        (fn [degree]
          ((chebyshev-polynomial-fn degree) x)))
    2 (fn [x]
        (fn [degree]
          ((chebyshev-polynomial-fn degree {::second-kind? true}) x)))))

(s/fdef polynomial-functions
  :args (s/cat :chebyshev-kind ::chebyshev-kind)
  :ret (s/fspec :args (s/cat :number ::m/number)
                :ret (s/fspec :args (s/cat :degree ::degree)
                              :ret ::m/number)))

(defn polynomial-fn
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular
  polynomial. Returns a function that takes a number and returns a vector."
  ([end-degree] (polynomial-fn end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (fn [x] (mapv (fn [degree]
                   (((polynomial-functions chebyshev-kind) x) degree))
                 (range start-degree (inc end-degree))))))

(s/fdef polynomial-fn
  :args (s/and (s/cat :end-degree ::end-degree
                      :opts (s/? (s/keys :opt [::start-degree
                                               ::chebyshev-kind])))
               (fn [{:keys [end-degree opts]}]
                 (let [{::keys [start-degree]} opts]
                   (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::number->v)

(defn polynomial-fns
  "Cheybshev-kind can be 0 (default), 1, or 2, where 0 means a regular
  polynomial. Returns a collection of functions that each take a number and
  return a number."
  ([end-degree] (polynomial-fns end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (map (fn [degree]
          (fn [x]
            (((polynomial-functions chebyshev-kind) x) degree)))
        (range start-degree (inc end-degree)))))

(s/fdef polynomial-fns
  :args (s/and (s/cat :end-degree ::end-degree
                      :opts (s/? (s/keys :opt [::start-degree
                                               ::chebyshev-kind])))
               (fn [{:keys [end-degree opts]}]
                 (let [{::keys [start-degree]} opts]
                   (or (not start-degree) (<= start-degree end-degree)))))
  :ret (s/coll-of ::number->number))

(defn polynomial-2D-count
  "Returns the number of elements in 2D between `start-degree` and
  `end-degree`."
  ([end-degree] (polynomial-2D-count end-degree {}))
  ([end-degree {::keys [start-degree]
                :or    {start-degree 0}}]
   (let [d (inc end-degree)
         f (fn [degree]
             (* 0.5 (+ degree (m/sq degree))))]
     (long (- (f d) (f start-degree))))))

(s/fdef polynomial-2D-count
  :args (s/and (s/cat :end-degree ::end-degree
                      :opts (s/? (s/keys :opt [::start-degree])))
               (fn [{:keys [end-degree opts]}]
                 (let [{::keys [start-degree]} opts]
                   (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::m/long-non-)

(defn- polynomial-2D-degrees
  ^double [count]
  (- (m/sqrt (+ 0.25 (* 2.0 count))) 1.5))

(defn polynomial-2D-fn-by-degree
  "`cheybshev-kind` can be 0 (default), 1, or 2, where 0 means a regular
  polynomial. Order retains x to the highest powers first, e.g.,
  [1 x y x^2 xy y^2 x^3 (x^2 × y) (y^2 × x) y^3]. Returns a function that takes
  two numbers (an x and a y) and returns a vector."
  ([end-degree] (polynomial-2D-fn-by-degree end-degree {}))
  ([end-degree {::keys [start-degree chebyshev-kind]
                :or    {start-degree 0, chebyshev-kind 0}}]
   (let [p (polynomial-functions chebyshev-kind)]
     (fn [x y]
       (let [fx (p x)
             fy (p y)]
         (loop [arr []
                i start-degree]
           (if (> i end-degree)
             arr
             (recur (reduce (fn [tot e]
                              (conj tot (* (fx e) (fy (- i e)))))
                            arr
                            (range (inc i)))
                    (inc i)))))))))

(s/fdef polynomial-2D-fn-by-degree
  :args (s/and (s/cat :end-degree ::end-degree
                      :opts (s/? (s/keys :opt [::start-degree
                                               ::chebyshev-kind])))
               (fn [{:keys [end-degree opts]}]
                 (let [{::keys [start-degree]} opts]
                   (or (not start-degree) (<= start-degree end-degree)))))
  :ret ::number2->v)

(defn polynomial-2D-fn-by-basis-count
  "`cheybshev-kind` can be 0 (default), 1, or 2, where 0 means a regular
  polynomial. Order retains x to the highest powers first, e.g.,
  [1 x y x^2 xy y^2 x^3 (x^2 × y) (y^2 × x) y^3]. Returns a function that takes
  two numbers (an x and a y) and returns a vector."
  ([basis-count] (polynomial-2D-fn-by-basis-count basis-count {}))
  ([basis-count {::keys [start-degree chebyshev-kind]
                 :or    {start-degree 0, chebyshev-kind 0}}]
   (let [end-degree (m/ceil' (polynomial-2D-degrees basis-count))]
     (fn [x y]
       (if (> start-degree end-degree)
         []
         (vec (take
                basis-count
                ((polynomial-2D-fn-by-degree
                   end-degree
                   {::start-degree   start-degree
                    ::chebyshev-kind chebyshev-kind})
                 x
                 y))))))))

(s/fdef polynomial-2D-fn-by-basis-count
  :args (s/cat :basis-count ::basis-count
               :opts (s/? (s/keys :opt [::start-degree ::chebyshev-kind])))
  :ret ::number2->v)

(defn polynomial-ND-fn
  "Returns a function that takes a vector [x y z ...] and returns a vector.
  Terms are sorted by order and then by dimension, e.g.,
  [1 x y z x^2 xy xz y^2 yz z^2 x^3 (x^2 × y) (x^2 × z) (x × y^2)
  (x × y × z) (x × z^2) y^3 (y^2 × z) (y × z^2) z^3]."
  ([end-degree] (polynomial-ND-fn end-degree {}))
  ([end-degree {::keys [chebyshev-kind] :or {chebyshev-kind 0}}]
   (let [p (polynomial-functions chebyshev-kind)]
     (fn [v]
       (let [fv (mapv p v)]
         (mapv (fn [degrees]
                 (reduce-kv (fn [tot index degree]
                              (* tot ((get fv index m/nan) degree)))
                            1.0
                            (vec degrees)))
               (sort-by
                 (fn [degrees]
                   (reduce-kv
                     (fn [[tot1 tot2] index degree]
                       [(+ tot1 degree)
                        (+ tot2 (* degree (inc (- (m/pow 0.01 (+ 2 index))))))])
                     [0.0 0.0]
                     (vec degrees)))
                 (apply combinatorics/cartesian-product
                        (repeat (count v) (range (inc end-degree)))))))))))

(s/fdef polynomial-ND-fn
  :args (s/cat :end-degree ::end-degree
               :opts (s/? (s/keys :opt [::chebyshev-kind])))
  :ret ::v->v)

(defn polynomial-ND-fn-without-cross-terms
  "Returns a function that takes a vector [x y z ...] and returns a vector.
  Terms are sorted by order and then by dimension, e.g.,
  [1 x y z x^2 y^2 z^2 x^3 y^3 z^3]."
  ([end-degree] (polynomial-ND-fn-without-cross-terms end-degree {}))
  ([end-degree {::keys [chebyshev-kind] :or {chebyshev-kind 0}}]
   (fn [v]
     (if (zero? end-degree)
       [1.0]
       (vec (cons 1.0 (apply
                        interleave
                        (map (polynomial-fn
                               end-degree {::start-degree   1
                                           ::chebyshev-kind chebyshev-kind})
                             v))))))))

(s/fdef polynomial-ND-fn-without-cross-terms
  :args (s/cat :end-degree ::end-degree
               :opts (s/? (s/keys :opt [::chebyshev-kind])))
  :ret ::v->v)