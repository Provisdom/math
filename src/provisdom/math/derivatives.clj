(ns provisdom.math.derivatives
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as st]
    [orchestra.spec.test :as ost]
    [provisdom.math.core :as m]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]
    [provisdom.math.matrix :as mx]))

;;;NUMERICAL DERIVATIVES
;;; references: Wiki http://en.wikipedia.org/wiki/Finite_difference
;;;    and http://en.wikipedia.org/wiki/Finite_difference_coefficients

(s/def ::number->number
  (s/with-gen
    (s/fspec :args (s/cat :number ::m/number)
             :ret ::m/number)
    #(gen/one-of (map gen/return
                      (list m/sq m/cube m/cos)))))

(s/def ::v->tensor
  (s/with-gen
    (s/fspec :args (s/cat :v ::vector/vector)
             :ret ::tensor/tensor)
    #(gen/one-of (map gen/return
                      (list vector/kahan-sum identity)))))

(s/def ::v->number
  (s/with-gen
    (s/fspec :args (s/cat :v ::vector/vector)
             :ret ::m/number)
    #(gen/one-of (map gen/return
                      (list vector/kahan-sum tensor/average tensor/norm1)))))

(s/def ::v->v
  (s/with-gen
    (s/fspec :args (s/cat :v ::vector/vector)
             :ret ::vector/vector)
    #(gen/one-of (map gen/return
                      (list (fn [v]
                              (mapv m/sq v))
                            (fn [v]
                              (mapv m/cube v)))))))

(s/def ::v->m
  (s/fspec :args (s/cat :v ::vector/vector)
           :ret ::mx/matrix))

(s/def ::v->symmetric-m
  (s/fspec :args (s/cat :v ::vector/vector)
           :ret ::mx/symmetric-matrix))

(s/def ::fxy
  (s/with-gen
    (s/fspec :args (s/cat :x ::m/number :y ::m/number)
             :ret ::m/number)
    #(gen/one-of (map gen/return
                      (list + - (fn [x y] (+ x (* 2 y))))))))

(s/def ::h
  (s/with-gen
    ::m/finite+
    #(gen/double* {:infinite? false
                   :NaN?      false
                   :min       m/tiny-dbl
                   :max       0.1})))

(s/def ::dx ::h)
(s/def ::multiplier ::h)
(s/def ::type #{:central :forward :backward})
(s/def ::accuracy (s/and (s/int-in 1 9) (partial not= 7)))

(s/def ::coefficients
  (s/coll-of ::vector/vector-2D :kind clojure.core/vector? :into []))

(s/def ::derivative (s/int-in 0 9))

(comment "The zero coefficient is left out below, but can be found because 
    the sum of coefficients equals zero")
(def ^:const ^:private central-coefficient1
  [[[1 (/ 2)]] [[2 (/ -12)] [1 (/ 2 3)]] [[3 (/ 60)] [2 (/ -3 20)] [1 (/ 3 4)]]
   [[4 (/ -280)] [3 (/ 4 105)] [2 (/ -5)] [1 (/ 4 5)]]])

(def ^:const ^:private central-coefficient2
  [[[1 1]] [[2 (/ -12)] [1 (/ 4 3)]] [[3 (/ 90)] [2 (/ -3 20)] [1 (/ 3 2)]]
   [[4 (/ -560)] [3 (/ 8 315)] [2 (/ -5)] [1 (/ 8 5)]]])

(def ^:const ^:private central-coefficient3
  [[[2 (/ 2)] [1 -1]] [[3 (/ -8)] [2 1] [1 (/ -13 8)]]
   [[4 (/ 7 240)] [3 (/ -3 10)] [2 (/ 169 120)] [1 (/ -61 30)]]])

(def ^:const ^:private central-coefficient4
  [[[2 1] [1 -4]] [[3 (/ -6)] [-2 2] [1 (/ -13 2)]]
   [[4 (/ 7 240)] [3 (/ -2 5)] [2 (/ 169 60)] [1 (/ -122 15)]]])

(def ^:const ^:private forward-coefficient1
  [[[1 1]]
   [[2 (/ -2)] [1 2]]
   [[3 (/ 3)] [2 (/ -3 2)] [1 3]]
   [[4 (/ -4)] [3 (/ 4 3)] [2 -3] [1 4]]
   [[5 (/ 5)] [4 (/ -5 4)] [3 (/ 10 3)] [2 -5] [1 5]]
   [[6 (/ -6)] [5 (/ 6 5)] [4 (/ -15 4)] [3 (/ 20 3)] [2 -7.5] [1 6]]])

(def ^:const ^:private forward-coefficient2
  [[[2 1] [1 -2]]
   [[3 -1] [2 4] [1 -5]]
   [[4 (/ 11 12)] [3 (/ -14 3)] [2 (/ 19 2)] [1 (/ -26 3)]]
   [[5 (/ -5 6)] [4 (/ 61 12)] [3 -13] [2 (/ 107 6)] [1 (/ -77 6)]]
   [[6 (/ 137 180)] [5 (/ -27 5)] [4 (/ 33 2)] [3 (/ -254 9)] [2 (/ 117 4)]
    [1 (/ -87 5)]]
   [[7 (/ -7 10)] [6 (/ 1019 180)] [5 (/ -201 10)] [4 41] [3 (/ -949 18)]
    [2 (/ 879 20)] [1 (/ -223 10)]]])

(def ^:const ^:private forward-coefficient3
  [[[3 1] [2 -3] [1 3]]
   [[4 (/ -3 2)] [3 7] [2 -12] [1 9]]
   [[5 (/ 7 4)] [4 (/ -41 4)] [3 (/ 49 2)] [2 (/ -59 2)] [1 (/ 71 4)]]
   [[6 (/ -15 8)] [5 13] [4 (/ -307 8)] [3 62] [2 (/ -461 8)] [1 29]]
   [[7 (/ 29 15)] [6 (/ -1849 120)] [5 (/ 268 5)] [4 (/ -2545 24)]
    [3 (/ 389 3)] [2 (/ -3929 40)] [1 (/ 638 15)]]
   [[8 (/ -469 240)] [7 (/ 527 30)] [6 (/ -561 8)] [5 (/ 4891 30)]
    [4 (/ -1457 6)] [3 (/ 2391 10)] [2 (/ -18353 120)] [1 (/ 349 6)]]])

(def ^:const ^:private forward-coefficient4
  [[[4 1] [3 -4] [2 6] [1 -4]]
   [[5 -2] [4 11] [3 -24] [2 26] [1 -14]]
   [[6 (/ 17 6)] [5 -19] [4 (/ 107 2)] [3 (/ -242 3)] [2 (/ 137 2)] [1 -31]]
   [[7 (/ -7 2)] [6 (/ 82 3)] [5 (/ -185 2)] [4 176] [3 (/ -1219 6)] [2 142]
    [1 (/ -111 2)]]
   [[8 (/ 967 240)] [7 (/ -536 15)] [6 (/ 2803 20)] [5 (/ -4772 15)]
    [4 (/ 10993 24)] [3 (/ -2144 5)] [2 (/ 15289 60)] [1 (/ -1316 15)]]])

(defn- convert-central-coefficients
  [m no-zero?]
  (let [r (map #(let [[e1 e2] %]
                  [(- e1)
                   (if no-zero? (- e2) e2)])
               m)
        extra (when-not no-zero?
                [[0
                  (* -2.0 (apply + (map second m)))]])]
    (vec (concat m extra r))))

(s/fdef convert-central-coefficients
        :args (s/cat :m ::mx/matrix :no-zero? boolean?)
        :ret ::mx/matrix)

(defn- get-central-coefficients
  [deriv accuracy]
  (let [a (/ accuracy 2)
        m (condp = deriv
            1 central-coefficient1
            2 central-coefficient2
            3 central-coefficient3
            4 central-coefficient4)]
    (convert-central-coefficients (nth m (dec a)) (odd? deriv))))

(s/fdef get-central-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 5)
                            :accuracy #{2 4 6 8})
                     (fn [{:keys [deriv accuracy]}]
                       (or (<= accuracy 6) (<= deriv 2))))
        :ret ::coefficients)

(defn- get-forward-coefficients
  [deriv accuracy]
  (let [m (condp = deriv
            1 forward-coefficient1
            2 forward-coefficient2
            3 forward-coefficient3
            4 forward-coefficient4)
        coefficient (nth m (dec accuracy))]
    (conj coefficient [0 (- (apply + (map second coefficient)))])))

(s/fdef get-forward-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 7)
                            :accuracy (s/int-in 1 7))
                     (fn [{:keys [deriv accuracy]}]
                       (or (<= accuracy 5) (<= deriv 3))))
        :ret ::coefficients)

(defn- get-backward-coefficients
  "backward is like forward, except for odd derivatives the sign switches."
  [deriv accuracy]
  (let [coefficient (get-forward-coefficients deriv accuracy)]
    (mapv #(let [[e1 e2] %]
             [(- e1) (if (odd? deriv) (- e2) e2)])
          coefficient)))

(s/fdef get-backward-coefficients
        :args (s/and (s/cat :deriv (s/int-in 1 7)
                            :accuracy (s/int-in 1 7))
                     (fn [{:keys [deriv accuracy]}]
                       (or (<= accuracy 5) (<= deriv 3))))
        :ret ::coefficients)

(defn derivative-fn
  "Returns a numerical derivative function. Function `number->number` takes and
  returns a number. Note that [[derivative-fn]] will not be accurate when inputs
  or outputs are so large when divided by `::h` that they lose precision.
  Options:
    `::derivative` - can be 0 or 1 (default) to 8
    `::h` - (default is m/*sgl-close* for 1st deriv, 10x less for others) is
      the denominator, which is equal to (dx ^ `::derivative`), where 'dx' is
      the small change (smaller `::h` isn't usually better, changes to `::h` can
      be important)
    `::type` - can be `:central` (default), `:forward`, or `:backward`
    `::accuracy` - can be 2, 4, 6, or 8 for central (no 8 for 3rd or 4th deriv),
      and 1-6 for forward or backward (no 6 for 4th deriv). (default
      `::accuracy` is 2 for `::derivative` <= 2, else 6. `::accuracy` is ignored
      for `::derivative` > 4 and default accuracies are used."
  ([number->number] (derivative-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy]
                    :or    {derivative 1
                            type       :central}}]
   (let [derivative (int derivative)
         accuracy (when accuracy (int accuracy))
         h (when h (double h))]
     (cond (zero? derivative) number->number
           (> derivative 4) (let [exc (- derivative 4)
                                  x (if h
                                      (/ h (/ m/sgl-close 10))
                                      1.0)]
                              (derivative-fn
                                (derivative-fn
                                  number->number
                                  {::derivative exc
                                   ::h          (* x (m/pow 10 (/ (+ 3 exc) -2)))
                                   ::type       type})
                                {::derivative 4
                                 ::h          (* x (m/pow 10 (/ (+ 11 (- exc)) -2)))
                                 ::type       type}))
           :else (let [h (cond h h
                               (m/one? derivative) m/sgl-close
                               :else (/ m/sgl-close 10))
                       accuracy (cond accuracy accuracy
                                      (<= derivative 2) 2
                                      (and (== derivative 4) (not= type :central)) 5
                                      :else 6)
                       coefficient-fn (condp = type
                                        :central get-central-coefficients
                                        :forward get-forward-coefficients
                                        :backward get-backward-coefficients)
                       multiplier (/ h)
                       dx (m/pow h (/ derivative))
                       coefficient (map #(let [[e1 e2] %] [(* dx e1) e2])
                                        (coefficient-fn derivative accuracy))]
                   (fn [v]
                     (* multiplier
                        (apply + (map #(let [[e1 e2] %]
                                         (* (number->number (+ v e1)) e2))
                                      coefficient)))))))))

(s/fdef derivative-fn
        :args (s/cat :number->number ::number->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::derivative ::h ::type ::accuracy])
                                  (fn [v]
                                    (let [d (get v ::derivative 1)
                                          t (get v ::type :central)
                                          a (get v ::accuracy (cond (<= d 2) 2
                                                                    (and (== d 4) (not= t :central)) 5
                                                                    :else 6))]
                                      (if (= t :central)
                                        (and (even? a)
                                             (or (and (<= d 2) (<= a 8)) (<= a 6)))
                                        (and (<= a 6)
                                             (or (<= d 3) (<= a 5)))))))))
        :ret ::number->number)

(defn gradient-fn
  "Returns a numerical gradient function. Function `v->number` takes a vector
  and returns a number. The output function takes and returns a vector.
  Options:
    `::h` (default m/*sgl-close*) is the denominator, which is equal to 'dx',
      where 'dx' is the small change (smaller `::h` isn't usually better,
      changes to `::h` can be important)
    `::type` can be `:central` (default), `:forward`, or `:backward`
    `::accuracy` can be 2 (default), 4, 6, or 8 for `:central`, and 1-6 for
      `:forward` or `:backward`."
  ([v->number] (gradient-fn v->number {}))
  ([v->number {::keys [h type accuracy]
               :or    {h        m/sgl-close
                       type     :central
                       accuracy 2}}]
   (let [coefficient-fn (condp = type
                          :central get-central-coefficients
                          :forward get-forward-coefficients
                          :backward get-backward-coefficients)
         multiplier (/ h)
         dx (double h)
         coefficient (map #(let [[e1 e2] %]
                             [(* dx e1) e2])
                          (coefficient-fn 1 accuracy))]
     (fn [v]
       (vec
         (map-indexed
           (fn [i e]
             (* multiplier
                (apply + (map #(let [[e1 e2] %]
                                 (* (v->number (assoc v i (+ e e1))) e2))
                              coefficient))))
           v))))))

(s/fdef gradient-fn
        :args (s/cat :v->number ::v->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v] (let [t (get v ::type :central)
                                                a (get v ::accuracy 2)]
                                            (if (= t :central)
                                              (and (even? a) (<= a 8))
                                              (<= a 6)))))))
        :ret ::v->v)

(defn jacobian-fn
  "Returns a numerical jacobian function. Function `v->v` takes a vector and
  returns a vector. The output function takes a vector and returns a matrix,
  where each row is the gradient of f's output.
  Options:
    `::h` -- (default m/*sgl-close*) is the denominator, which is equal to 'dx',
      where 'dx' is the small change (smaller `::h` isn't usually better,
      changes to `::h` can be important)
    `::type` -- can be `:central` (default), `:forward`, or `:backward`
    `::accuracy` -- can be 2 (default), 4, 6, or 8 for `:central`, and 1-6 for
    `:forward` or `:backward`."
  ([v->v] (jacobian-fn v->v {}))
  ([v->v {::keys [h type accuracy]
          :or    {h        m/sgl-close
                  type     :central
                  accuracy 2}}]
   (let [coefficient-fn (condp = type
                          :central get-central-coefficients
                          :forward get-forward-coefficients
                          :backward get-backward-coefficients)
         multiplier (/ h)
         dx (double h)
         coefficient (map #(let [[e1 e2] %]
                             [(* dx e1) e2])
                          (coefficient-fn 1 accuracy))]
     (fn [v]
       (if (empty? v)
         [[]]
         (mx/transpose
           (let [m (vec
                     (map-indexed
                       (fn [i e]
                         (apply tensor/add
                                (mapv #(let [[e1 e2] %]
                                         (tensor/multiply e2
                                                          multiplier
                                                          (v->v (assoc v i (+ e e1)))))
                                      coefficient)))
                       v))]
             m)))))))

(s/fdef jacobian-fn
        :args (s/cat :v->v ::v->v
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v]
                                    (let [t (get v ::type :central)
                                          a (get v ::accuracy 2)]
                                      (if (= t :central)
                                        (and (even? a) (<= a 8))
                                        (<= a 6)))))))
        :ret ::v->m)

(defn- joint-central-derivative
  [v->number v row column dx multiplier]
  (if (or (>= row (count v)) (>= column (count v)))
    m/nan
    (let [i+ (assoc v row (+ (get v row) dx))
          i- (assoc v row (- (get v row) dx))
          e++ (assoc i+ column (+ (get v row) dx))
          e+- (assoc i+ column (- (get v row) dx))
          e-+ (assoc i- column (+ (get v row) dx))
          e-- (assoc i- column (- (get v row) dx))]
      (* 0.25
         multiplier
         (- (+ (v->number e++) (v->number e--))
            (v->number e+-)
            (v->number e-+))))))

(s/fdef joint-central-derivative
        :args (s/cat :v->number ::v->number
                     :v ::vector/vector
                     :row ::mx/row
                     :column ::mx/column
                     :dx ::dx
                     :multiplier ::multiplier)
        :ret ::m/number)

(defn hessian-fn
  "Returns a numerical Hessian function. Function `v->number` takes a vector and
  returns a number. The output function takes a vector and returns a symmetric
  matrix. Options:
    `::h` -- (default m/*sgl-close* / 10) is the denominator, which is equal to
    (dx * dx), where 'dx' is the small change (smaller `::h` isn't usually
    better, changes to `::h` can be important)
    `::type` can be `:joint-central` (default), `:central`, `:forward`, or
      `:backward`
    `::accuracy` can be 2 (default, only choice for `:joint-central`); 2, 4, 6,
    and 1-6 for `:forward` or `:backward`."
  ([v->number] (hessian-fn v->number {}))
  ([v->number {::keys [h type accuracy]
               :or    {h        (* m/sgl-close 0.1)
                       type     :joint-central
                       accuracy 2}}]
   (if-not (= type :joint-central)
     (fn [v]
       (mx/symmetric-matrix-by-averaging
         ((jacobian-fn (gradient-fn v->number {::h        (m/sqrt h)
                                               ::type     type
                                               ::accuracy accuracy}))
           v)))
     (let [multiplier (/ h)
           dx (m/sqrt h)
           coefficient (map (fn [central-coefficient]
                              (let [[e1 e2] central-coefficient]
                                [(* dx e1) e2]))
                            (get-central-coefficients 2 2))]
       (fn [v]
         (if (empty? v)
           [[]]
           (let [cv (count v)]
             (mx/symmetric-matrix-by-averaging
               (mx/compute-matrix
                 cv
                 cv
                 (fn [row column]
                   (cond
                     (or (>= row cv) (>= column cv)) m/nan

                     (== row column)
                     (* multiplier
                        (apply + (map (fn [coeff]
                                        (let [[e1 e2] coeff]
                                          (* (v->number (assoc v row (+ (get v row) e1))) e2)))
                                      coefficient)))

                     :else (joint-central-derivative v->number v row column dx multiplier))))))))))))

(s/fdef hessian-fn
        :args (s/cat :v->number ::v->number
                     :opts (s/? (s/and
                                  (s/keys :opt [::h ::type ::accuracy])
                                  (fn [v] (let [t (get v ::type :joint-central)
                                                a (get v ::accuracy 2)]
                                            (cond (= t :joint-central) (== a 2)
                                                  (= t :central) (and (even? a) (<= a 8))
                                                  :else (<= a 6)))))))
        :ret ::v->symmetric-m)

(defn partial-derivative-x-of-fxy
  ([fxy] (partial-derivative-x-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h m/sgl-close}}]
   (fn [x y]
     ((derivative-fn
        (fn [x-local]
          (fxy x-local y))
        {::h h})
       x))))

(s/fdef partial-derivative-x-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn partial-derivative-y-of-fxy
  ([fxy] (partial-derivative-y-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h m/sgl-close}}]
   (fn [x y]
     ((derivative-fn
        (fn [y-local]
          (fxy x y-local))
        {::h h})
       y))))

(s/fdef partial-derivative-y-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-xx-of-fxy
  ([fxy] (second-partial-derivative-xx-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h (* m/sgl-close 0.1)}}]
   (fn [x y]
     ((derivative-fn
        (fn [x-local]
          (fxy x-local y))
        {::derivative 2 ::h h})
       x))))

(s/fdef second-partial-derivative-xx-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-yy-of-fxy
  ([fxy] (second-partial-derivative-yy-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h (* m/sgl-close 0.1)}}]
   (fn [x y]
     ((derivative-fn
        (fn [y-local]
          (fxy x y-local))
        {::derivative 2 ::h h})
       y))))

(s/fdef second-partial-derivative-yy-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)

(defn second-partial-derivative-xy-of-fxy
  ([fxy] (second-partial-derivative-xy-of-fxy fxy {}))
  ([fxy {::keys [h] :or {h (* m/sgl-close 0.1)}}]
   (fn [x y]
     (joint-central-derivative
       (fn [v]
         (if (= 2 (count v))
           (let [[x-local y-local] v]
             (fxy x-local y-local))
           m/nan))
       [x y]
       0
       1
       (m/sqrt h)
       (/ h)))))

(s/fdef second-partial-derivative-xy-of-fxy
        :args (s/cat :fxy ::fxy
                     :opts (s/? (s/keys :opt [::h])))
        :ret ::fxy)