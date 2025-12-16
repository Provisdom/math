(ns provisdom.math.derivatives
  "Numerical differentiation using finite difference methods.

  Provides high-accuracy numerical derivatives for scalar and vector functions:
  - Derivatives of scalar functions (1st through 8th order)
  - Gradients of multivariable scalar functions
  - Jacobians of vector-valued functions
  - Hessians (second partial derivatives)
  - Partial derivatives for bivariate functions
  - Higher-order mixed partial derivatives

  Accuracy improvements:
  - Richardson extrapolation for improved accuracy
  - Adaptive step size selection with convergence testing
  - Error estimates with derivative values

  Vector calculus operations:
  - Directional derivatives
  - Laplacian (sum of second partials)
  - Divergence and curl (3D) of vector fields

  Sparse computation:
  - Sparse Jacobian/Hessian for specified entries only

  Choosing a derivative function:

  `derivative-fn` - Basic finite differences. Use when you need a quick derivative
  and know the function is well-behaved. Fast and simple, but accuracy depends on
  choosing a good step size h.

  `richardson-derivative-fn` - Richardson extrapolation. Use when you need higher
  accuracy without manually tuning h. Computes derivatives at multiple step sizes
  and extrapolates to eliminate truncation error. Typically 2-4 orders of magnitude
  more accurate than basic. Best for smooth functions where roundoff isn't dominant.

  `adaptive-derivative-fn` - Automatic step size selection. Use when you want the
  algorithm to find the right step size automatically. Adapts to the function and
  converges to specified tolerance. Best for black-box functions or production code
  where robustness matters more than speed.

  `derivative-with-error-fn` - Returns value with error estimate. Use when you need
  to know how accurate your derivative is. Useful for sensitivity analysis,
  optimization convergence checks, or propagating uncertainty.

  Supports central, forward, and backward difference schemes with configurable
  accuracy levels. Uses pre-computed finite difference coefficients for optimal
  precision."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [provisdom.math.core :as m]
    [provisdom.math.matrix :as mx]
    [provisdom.math.series :as series]
    [provisdom.math.tensor :as tensor]
    [provisdom.math.vector :as vector]))

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

;;; New specs for improved derivatives
(s/def ::richardson-levels (s/int-in 1 7))
(s/def ::rel-tol ::m/finite+)
(s/def ::abs-tol ::m/finite+)
(s/def ::max-iterations (s/int-in 1 21))
(s/def ::direction ::vector/vector)
(s/def ::derivative-orders
  (s/coll-of ::m/int-non- :kind vector? :min-count 1))
(s/def ::value ::m/number)
(s/def ::error-bound ::m/finite-non-)
(s/def ::derivative-result (s/keys :req [::value ::error-bound]))
(s/def ::sparsity-pattern
  (s/coll-of (s/tuple ::m/int-non- ::m/int-non-) :kind set?))

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
  "Creates a numerical derivative function using finite differences.
  
  Takes a function `number->number` and returns its derivative function.
  Uses finite difference methods with pre-computed coefficients for high accuracy.
  
  Options:
    ::derivative - Order (0-8, default 1). Order 0 returns original function.
    ::h - Step size (default m/sgl-close for 1st deriv, smaller for higher orders).
           Smaller isn't always better due to floating-point precision limits.
    ::type - Difference scheme: :central (default), :forward, or :backward
    ::accuracy - Points used: 2,4,6,8 for central; 1-6 for forward/backward
  
  Returns function that computes nth derivative at given point.
  
  Examples:
    (def f (derivative-fn #(* % % %)))  ; derivative of x³  
    (f 2.0)  ;=> ~12.0 (3x² at x=2)
    
    (def f'' (derivative-fn sin {::derivative 2}))  ; second derivative of sin
    (f'' 0.0)  ;=> ~0.0 (-sin(0))"
  ([number->number] (derivative-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy]
                    :or    {derivative 1
                            type       :central}}]
   (let [derivative (int derivative)
         accuracy (when accuracy (int accuracy))
         h (when h (double h))]
     (cond (zero? derivative) number->number

           (> derivative 4)
           (let [exc (- derivative 4)
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

           :else
           (let [h (cond h h
                         (m/one? derivative) m/sgl-close
                         :else (/ m/sgl-close 10))
                 accuracy (cond accuracy accuracy
                                (<= derivative 2) 2
                                (and (== derivative 4) (not= type :central)) 5
                                :else 6)
                 coefficient-fn (case type
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
                               a (get v ::accuracy
                                   (cond (<= d 2) 2
                                         (and (== d 4) (not= t :central)) 5
                                         :else 6))]
                           (if (= t :central)
                             (and (even? a)
                               (or (and (<= d 2) (<= a 8)) (<= a 6)))
                             (and (<= a 6)
                               (or (<= d 3) (<= a 5)))))))))
  :ret ::number->number)

(defn gradient-fn
  "Creates a numerical gradient function for multivariable functions.
  
  Takes a function `v->number` (vector → scalar) and returns its gradient function
  (vector → vector). The gradient points in the direction of steepest ascent.
  
  Options:
    ::h - Step size (default: m/sgl-close). Balance between accuracy and numerical stability.
    ::type - Difference scheme: :central (default), :forward, or :backward  
    ::accuracy - Number of points: 2 (default), 4, 6, 8 for central; 1-6 for others
  
  Returns a function that computes ∇f at any point.
  
  Examples:
    (def grad-f (gradient-fn (fn [[x y]] (+ (* x x) (* y y)))))
    (grad-f [2.0 3.0])  ;=> [4.0 6.0] (gradient of x²+y² at (2,3))
    
    (def grad-g (gradient-fn (fn [[x y]] (* x y))))  
    (grad-g [1.0 2.0])  ;=> [2.0 1.0] (gradient of xy at (1,2))"
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
  "Creates a numerical Jacobian function for vector-valued functions.
  
  Takes a function `v->v` (vector → vector) and returns its Jacobian function
  (vector → matrix). The Jacobian matrix contains all first-order partial derivatives.
  
  For function f: ℝⁿ → ℝᵐ, the Jacobian J is an m×n matrix where J[i,j] = ∂fᵢ/∂xⱼ.
  Each row represents the gradient of one output component.
  
  Options:
    ::h - Step size (default: m/sgl-close)
    ::type - Difference scheme: :central (default), :forward, or :backward
    ::accuracy - Number of points: 2 (default), 4, 6, 8 for central; 1-6 for others
  
  Examples:
    (def jac-f (jacobian-fn (fn [[x y]] [(* x y) (+ x y)])))
    (jac-f [2.0 3.0])  ;=> [[3.0 2.0]    ; ∂(xy)/∂x, ∂(xy)/∂y
                       ;     [1.0 1.0]]   ; ∂(x+y)/∂x, ∂(x+y)/∂y"
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
  "Creates a numerical Hessian function for scalar multivariable functions.
  
  Takes a function `v->number` (vector → scalar) and returns its Hessian function
  (vector → symmetric matrix). The Hessian contains all second-order partial derivatives.
  
  For function f: ℝⁿ → ℝ, the Hessian H is an n×n matrix where H[i,j] = ∂²f/(∂xᵢ∂xⱼ).
  Used for optimization, analyzing critical points, and Newton's method.
  
  Options:
    ::h - Step size (default: m/sgl-close * 0.1, since this involves second derivatives)
    ::type - Method: :joint-central (default, most accurate), :central, :forward, :backward
    ::accuracy - 2 (default, only option for :joint-central); 2,4,6,8 for central; 1-6 for others
  
  Examples:
    (def hess-f (hessian-fn (fn [[x y]] (+ (* x x y) (* y y y)))))  ; f(x,y) = x²y + y³
    (hess-f [1.0 2.0])  ;=> [[4.0 2.0]    ; ∂²f/∂x², ∂²f/∂x∂y
                        ;     [2.0 12.0]]  ; ∂²f/∂y∂x, ∂²f/∂y²"
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
                       (apply +
                         (map
                           (fn [coeff]
                             (let [[e1 e2] coeff]
                               (* (v->number (assoc v row (+ (get v row) e1)))
                                 e2)))
                           coefficient)))

                     :else
                     (joint-central-derivative
                       v->number v row column dx multiplier))))))))))))

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

;;;IMPROVED DERIVATIVE FUNCTIONS

(defn richardson-derivative-fn
  "Creates derivative function with Richardson extrapolation for improved accuracy.

  Computes derivatives at h, h/2, h/4, ... and uses Richardson extrapolation
  to eliminate leading error terms. For central differences with order p error,
  extrapolation eliminates O(h^p), O(h^(p+2)), etc.

  Options:
    ::derivative - Order of derivative (default 1)
    ::h - Initial step size (default m/sgl-close)
    ::type - :central (default), :forward, :backward
    ::accuracy - Base accuracy level (default 2)
    ::richardson-levels - Number of refinement levels (default 3, max 6)

  Returns function computing extrapolated derivative at any point.

  Examples:
    (def f' (richardson-derivative-fn sin))
    (f' 0.0)  ;=> ~1.0 (cos(0))

    (def f'' (richardson-derivative-fn sin {::derivative 2}))
    (f'' 0.0)  ;=> ~0.0 (-sin(0))"
  ([number->number] (richardson-derivative-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy richardson-levels]
                    :or    {derivative        1
                            h                 m/sgl-close
                            type              :central
                            accuracy          2
                            richardson-levels 3}}]
   (let [base-order (if (= type :central) 2 1)]
     (fn [x]
       (let [approxs (map (fn [level]
                            (let [h-level (/ h (m/pow 2 level))]
                              ((derivative-fn number->number
                                 {::derivative derivative
                                  ::h          h-level
                                  ::type       type
                                  ::accuracy   accuracy})
                               x)))
                       (range richardson-levels))
             refined (series/richardson-extrapolate approxs {::series/order base-order})]
         (last (take richardson-levels refined)))))))

(s/fdef richardson-derivative-fn
  :args (s/cat :number->number ::number->number
          :opts (s/? (s/keys :opt [::derivative ::h ::type ::accuracy
                                   ::richardson-levels])))
  :ret ::number->number)

(defn adaptive-derivative-fn
  "Creates derivative function with automatic step size selection.

  Iteratively refines step size until estimates converge within tolerance.
  Uses Richardson extrapolation internally to estimate errors and determine
  optimal stopping point.

  Options:
    ::derivative - Order of derivative (default 1)
    ::h - Initial step size (default determined by derivative order)
    ::type - :central (default), :forward, :backward
    ::accuracy - Base accuracy level (default 2)
    ::rel-tol - Relative tolerance (default m/dbl-close)
    ::abs-tol - Absolute tolerance (default m/quad-close)
    ::max-iterations - Maximum refinement iterations (default 10)

  Returns function computing adaptively refined derivative.

  Examples:
    (def f' (adaptive-derivative-fn sin))
    (f' m/PI)  ;=> ~-1.0 (cos(PI))"
  ([number->number] (adaptive-derivative-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy rel-tol abs-tol max-iterations]
                    :or    {derivative     1
                            type           :central
                            accuracy       2
                            rel-tol        m/dbl-close
                            abs-tol        m/quad-close
                            max-iterations 10}}]
   (let [h0 (or h (if (m/one? derivative) m/sgl-close (/ m/sgl-close 10)))]
     (fn [x]
       (loop [h-cur h0
              prev-estimate nil
              iter 0]
         (let [estimate ((derivative-fn number->number
                           {::derivative derivative
                            ::h          h-cur
                            ::type       type
                            ::accuracy   accuracy})
                         x)]
           (if (and prev-estimate
                    (or (>= iter max-iterations)
                        (<= (m/abs (- estimate prev-estimate))
                            (+ abs-tol (* rel-tol (m/abs estimate))))))
             estimate
             (recur (/ h-cur 2.0) estimate (inc iter)))))))))

(s/fdef adaptive-derivative-fn
  :args (s/cat :number->number ::number->number
          :opts (s/? (s/keys :opt [::derivative ::h ::type ::accuracy
                                   ::rel-tol ::abs-tol ::max-iterations])))
  :ret ::number->number)

(defn derivative-with-error-fn
  "Creates derivative function that returns value with error estimate.

  Uses Richardson extrapolation to compute both the derivative and an
  estimate of the truncation error.

  Options:
    ::derivative - Order of derivative (default 1)
    ::h - Initial step size
    ::type - Difference scheme (default :central)
    ::accuracy - Accuracy level (default 2)
    ::richardson-levels - Refinement levels (default 4)

  Returns function computing {::value derivative ::error-bound error-estimate}

  Examples:
    (def df (derivative-with-error-fn sin))
    (df m/PI)  ;=> {::value -1.0 ::error-bound ~1e-15}"
  ([number->number] (derivative-with-error-fn number->number {}))
  ([number->number {::keys [derivative h type accuracy richardson-levels]
                    :or    {derivative        1
                            h                 m/sgl-close
                            type              :central
                            accuracy          2
                            richardson-levels 4}}]
   (let [base-order (if (= type :central) 2 1)]
     (fn [x]
       (let [approxs (mapv (fn [level]
                             (let [h-level (/ h (m/pow 2 level))]
                               ((derivative-fn number->number
                                  {::derivative derivative
                                   ::h          h-level
                                   ::type       type
                                   ::accuracy   accuracy})
                                x)))
                       (range richardson-levels))
             refined (vec (take richardson-levels
                            (series/richardson-extrapolate approxs {::series/order base-order})))
             n (count refined)
             value (if (pos? n) (peek refined) (peek approxs))
             prev-value (if (> n 1) (nth refined (- n 2)) (first approxs))
             error-bound (m/abs (- value prev-value))]
         {::value       value
          ::error-bound (if (m/nan? error-bound) 0.0 error-bound)})))))

(s/fdef derivative-with-error-fn
  :args (s/cat :number->number ::number->number
          :opts (s/? (s/keys :opt [::derivative ::h ::type ::accuracy
                                   ::richardson-levels])))
  :ret (s/fspec :args (s/cat :number ::m/number)
         :ret ::derivative-result))

;;;VECTOR CALCULUS FUNCTIONS

(defn directional-derivative-fn
  "Creates function computing the directional derivative.

  For function f: R^n -> R and direction vector d, computes:
    D_d f(x) = grad(f)(x) . d/|d|

  The direction is automatically normalized to unit length.

  Options:
    ::h - Step size for gradient computation (default m/sgl-close)
    ::type - Difference scheme (default :central)
    ::accuracy - Accuracy level (default 2)

  Returns function that computes directional derivative at any point.

  Examples:
    (def df (directional-derivative-fn (fn [[x y]] (+ (* x x) (* y y))) [1 0]))
    (df [3.0 4.0])  ;=> 6.0 (derivative in x-direction at (3,4))"
  ([v->number direction] (directional-derivative-fn v->number direction {}))
  ([v->number direction {::keys [h type accuracy]
                         :or    {h        m/sgl-close
                                 type     :central
                                 accuracy 2}}]
   (let [dir-norm (tensor/norm direction)]
     (if (m/roughly? dir-norm 0.0 m/sgl-close)
       (constantly m/nan)
       (let [unit-dir (tensor/normalize direction)
             grad-fn' (gradient-fn v->number {::h h ::type type ::accuracy accuracy})]
         (fn [v]
           (vector/dot-product (grad-fn' v) unit-dir)))))))

(s/fdef directional-derivative-fn
  :args (s/cat :v->number ::v->number
          :direction ::direction
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret ::v->number)

(defn laplacian-fn
  "Creates function computing the Laplacian (sum of second partials).

  For function f: R^n -> R, computes:
    nabla^2 f = sum_i d^2f/dx_i^2 = trace(Hessian(f))

  Options:
    ::h - Step size (default m/sgl-close * 0.1)
    ::type - Difference scheme (default :joint-central)
    ::accuracy - Accuracy level (default 2)

  Returns function computing Laplacian at any point.

  Examples:
    (def lap (laplacian-fn (fn [[x y]] (+ (* x x) (* y y)))))
    (lap [1.0 2.0])  ;=> 4.0 (2 + 2)"
  ([v->number] (laplacian-fn v->number {}))
  ([v->number {::keys [h type accuracy]
               :or    {h        (* m/sgl-close 0.1)
                       type     :joint-central
                       accuracy 2}}]
   (let [hess-fn' (hessian-fn v->number {::h h ::type type ::accuracy accuracy})]
     (fn [v]
       (mx/trace (hess-fn' v))))))

(s/fdef laplacian-fn
  :args (s/cat :v->number ::v->number
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret ::v->number)

(defn divergence-fn
  "Creates function computing the divergence of a vector field.

  For vector field F: R^n -> R^n, computes:
    div(F) = sum_i dF_i/dx_i (sum of diagonal of Jacobian)

  Options:
    ::h - Step size (default m/sgl-close)
    ::type - Difference scheme (default :central)
    ::accuracy - Accuracy level (default 2)

  Returns function computing divergence at any point.

  Examples:
    (def divF (divergence-fn (fn [[x y]] [(* x y) (+ x (* y y))])))
    (divF [2.0 3.0])  ;=> 9.0 (y + 2y = 3 + 6 = 9)"
  ([v->v] (divergence-fn v->v {}))
  ([v->v {::keys [h type accuracy]
          :or    {h        m/sgl-close
                  type     :central
                  accuracy 2}}]
   (fn [v]
     (if (empty? v)
       0.0
       (reduce +
         (map-indexed
           (fn [i vi]
             ((derivative-fn
                (fn [xi]
                  (nth (v->v (assoc v i xi)) i))
                {::h h ::type type ::accuracy accuracy})
              vi))
           v))))))

(s/fdef divergence-fn
  :args (s/cat :v->v ::v->v
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret ::v->number)

(defn curl-fn
  "Creates function computing the curl of a 3D vector field.

  For vector field F = [Fx, Fy, Fz]: R^3 -> R^3, computes:
    curl(F) = [dFz/dy - dFy/dz, dFx/dz - dFz/dx, dFy/dx - dFx/dy]

  Only works for 3D vector fields. Returns NaN vector for non-3D input.

  Options:
    ::h - Step size (default m/sgl-close)
    ::type - Difference scheme (default :central)
    ::accuracy - Accuracy level (default 2)

  Returns function computing curl at any point.

  Examples:
    (def curlF (curl-fn (fn [[x y z]] [(* y z) (* x z) (* x y)])))
    (curlF [1.0 2.0 3.0])  ;=> [-1.0 0.0 1.0]"
  ([v->v] (curl-fn v->v {}))
  ([v->v {::keys [h type accuracy]
          :or    {h        m/sgl-close
                  type     :central
                  accuracy 2}}]
   (fn [[x y z :as v]]
     (if (not= 3 (count v))
       [m/nan m/nan m/nan]
       (let [deriv-opts {::h h ::type type ::accuracy accuracy}
             dFz-dy ((derivative-fn (fn [yi] (nth (v->v [x yi z]) 2)) deriv-opts) y)
             dFy-dz ((derivative-fn (fn [zi] (nth (v->v [x y zi]) 1)) deriv-opts) z)
             dFx-dz ((derivative-fn (fn [zi] (nth (v->v [x y zi]) 0)) deriv-opts) z)
             dFz-dx ((derivative-fn (fn [xi] (nth (v->v [xi y z]) 2)) deriv-opts) x)
             dFy-dx ((derivative-fn (fn [xi] (nth (v->v [xi y z]) 1)) deriv-opts) x)
             dFx-dy ((derivative-fn (fn [yi] (nth (v->v [x yi z]) 0)) deriv-opts) y)]
         [(- dFz-dy dFy-dz)
          (- dFx-dz dFz-dx)
          (- dFy-dx dFx-dy)])))))

(s/fdef curl-fn
  :args (s/cat :v->v ::v->v
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret (s/fspec :args (s/cat :v ::vector/vector)
         :ret ::vector/vector))

;;;HIGHER-ORDER MIXED PARTIALS

(defn mixed-partial-fn
  "Creates function computing arbitrary mixed partial derivatives.

  For function f: R^n -> R and derivative orders [o1, o2, ..., on], computes:
    d^(o1+o2+...+on) f / (dx1^o1 dx2^o2 ... dxn^on)

  Options:
    ::h - Step size (default adjusted for total derivative order)
    ::type - Difference scheme (default :central)
    ::accuracy - Accuracy level (default 2)

  Returns function computing mixed partial at any point.

  Examples:
    ;; d^3f/(dx^2 dy) for f(x,y) = x^2*y^3
    (def d3f (mixed-partial-fn (fn [[x y]] (* x x y y y)) [2 1]))
    (d3f [1.0 2.0])  ;=> 24.0 (2 * 3 * y^2 = 2 * 3 * 4 = 24)"
  ([v->number derivative-orders]
   (mixed-partial-fn v->number derivative-orders {}))
  ([v->number derivative-orders {::keys [h type accuracy]
                                 :or    {type     :central
                                         accuracy 2}}]
   (let [total-order (apply + derivative-orders)
         ;; For higher derivatives, use larger h to avoid roundoff errors
         ;; Optimal h ~ eps^(1/(n+1)), so for n=3: h ~ 1e-4
         h0 (or h (m/pow m/dbl-close (/ 1.0 (inc total-order))))]
     (fn [v]
       (if (not= (count v) (count derivative-orders))
         m/nan
         (loop [current-fn v->number
                remaining-orders (vec (map-indexed vector derivative-orders))
                h-current h0]
           (if (empty? remaining-orders)
             (current-fn v)
             (let [[idx order] (first remaining-orders)]
               (if (zero? order)
                 (recur current-fn (rest remaining-orders) h-current)
                 (let [next-fn (fn [v']
                                 ((derivative-fn
                                    (fn [xi]
                                      (current-fn (assoc v' idx xi)))
                                    {::derivative order
                                     ::h          h-current
                                     ::type       type
                                     ::accuracy   accuracy})
                                  (nth v' idx)))]
                   (recur next-fn (rest remaining-orders) (* h-current 0.5))))))))))))

(s/fdef mixed-partial-fn
  :args (s/cat :v->number ::v->number
          :derivative-orders ::derivative-orders
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret ::v->number)

;;;SPARSE JACOBIANS AND HESSIANS

(defn sparse-jacobian-fn
  "Creates Jacobian function that only computes specified entries.

  For large sparse systems, this is much more efficient than computing
  the full Jacobian and extracting entries.

  Args:
    v->v - Vector-valued function
    sparsity-pattern - Set of [row col] pairs indicating entries to compute
    opts - Same options as jacobian-fn

  Returns function computing sparse Jacobian as vector of [row col value].

  Examples:
    ;; Only compute diagonal entries
    (def sJ (sparse-jacobian-fn f #{[0 0] [1 1] [2 2]}))
    (sJ [1.0 2.0 3.0])  ;=> [[0 0 val1] [1 1 val2] [2 2 val3]]"
  ([v->v sparsity-pattern] (sparse-jacobian-fn v->v sparsity-pattern {}))
  ([v->v sparsity-pattern {::keys [h type accuracy]
                           :or    {h        m/sgl-close
                                   type     :central
                                   accuracy 2}}]
   (fn [v]
     (vec
       (for [[row col] (sort sparsity-pattern)]
         (let [deriv ((derivative-fn
                        (fn [xi]
                          (nth (v->v (assoc v col xi)) row))
                        {::h h ::type type ::accuracy accuracy})
                      (nth v col))]
           [row col deriv]))))))

(s/fdef sparse-jacobian-fn
  :args (s/cat :v->v ::v->v
          :sparsity-pattern ::sparsity-pattern
          :opts (s/? (s/keys :opt [::h ::type ::accuracy])))
  :ret (s/fspec :args (s/cat :v ::vector/vector)
         :ret (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::m/number))))

(defn sparse-hessian-fn
  "Creates Hessian function that only computes specified entries.

  For symmetric Hessians, specifying [i j] automatically includes [j i].

  Args:
    v->number - Scalar-valued function
    sparsity-pattern - Set of [row col] pairs indicating entries to compute
    opts - Same options as hessian-fn

  Returns function computing sparse Hessian as vector of [row col value].

  Examples:
    ;; Only compute diagonal of Hessian
    (def sH (sparse-hessian-fn f #{[0 0] [1 1]}))
    (sH [1.0 2.0])  ;=> [[0 0 d2f/dx^2] [1 1 d2f/dy^2]]"
  ([v->number sparsity-pattern] (sparse-hessian-fn v->number sparsity-pattern {}))
  ([v->number sparsity-pattern {::keys [h accuracy]
                                :or    {h        (* m/sgl-close 0.1)
                                        accuracy 2}}]
   (let [multiplier (/ h)
         dx (m/sqrt h)]
     (fn [v]
       (let [expanded-pattern (into #{}
                                (mapcat (fn [[r c]] [[r c] [c r]]))
                                sparsity-pattern)]
         (vec
           (for [[row col] (sort expanded-pattern)
                 :when (<= row col)]
             (let [deriv (if (== row col)
                           ;; Diagonal: d^2f/dx_i^2 using central differences
                           ((derivative-fn
                              (fn [xi]
                                (v->number (assoc v row xi)))
                              {::derivative 2 ::h h ::type :central ::accuracy accuracy})
                            (nth v row))
                           ;; Off-diagonal: d^2f/dx_i dx_j using joint-central
                           (joint-central-derivative v->number v row col dx multiplier))]
               [row col deriv]))))))))

(s/fdef sparse-hessian-fn
  :args (s/cat :v->number ::v->number
          :sparsity-pattern ::sparsity-pattern
          :opts (s/? (s/keys :opt [::h ::accuracy])))
  :ret (s/fspec :args (s/cat :v ::vector/vector)
         :ret (s/coll-of (s/tuple ::m/int-non- ::m/int-non- ::m/number))))
