(ns provisdom.math.apache
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as st]
            [orchestra.spec.test :as ost]
            [provisdom.math.core :as m]
            [provisdom.math.arrays :as ar]
            [provisdom.math.matrix :as mx]
            [provisdom.math.bounds :as bo])
  (:import [java.util ArrayList]
           [org.apache.commons.math3.exception TooManyEvaluationsException TooManyIterationsException]
           [org.apache.commons.math3.analysis UnivariateFunction
                                              MultivariateFunction MultivariateVectorFunction
                                              MultivariateMatrixFunction]
           [org.apache.commons.math3.analysis.differentiation
            UnivariateDifferentiableFunction FiniteDifferencesDifferentiator]
           [org.apache.commons.math3.analysis.solvers
            BaseUnivariateSolver BisectionSolver BracketingNthOrderBrentSolver
            BrentSolver IllinoisSolver MullerSolver MullerSolver2 PegasusSolver
            RegulaFalsiSolver RiddersSolver SecantSolver NewtonRaphsonSolver]
           [org.apache.commons.math3.special Gamma Beta Erf]
           [org.apache.commons.math3.random RandomDataGenerator MersenneTwister
                                            ISAACRandom SobolSequenceGenerator]
           [org.apache.commons.math3.distribution IntegerDistribution
                                                  HypergeometricDistribution PascalDistribution PoissonDistribution
                                                  GeometricDistribution, UniformIntegerDistribution ZipfDistribution
                                                  KolmogorovSmirnovDistribution MixtureMultivariateNormalDistribution
                                                  MultivariateNormalDistribution RealDistribution BetaDistribution
                                                  BinomialDistribution CauchyDistribution ChiSquaredDistribution
                                                  ExponentialDistribution FDistribution GammaDistribution
                                                  LevyDistribution LogNormalDistribution NormalDistribution,
                                                  TDistribution, TriangularDistribution, UniformRealDistribution,
                                                  WeibullDistribution ParetoDistribution, GumbelDistribution,
                                                  LaplaceDistribution, LogisticDistribution, NakagamiDistribution]
           [org.apache.commons.math3.optim OptimizationData InitialGuess
                                           SimpleBounds MaxEval MaxIter PointValuePair PointVectorValuePair
                                           SimpleValueChecker SimplePointChecker SimpleVectorValueChecker]
           [org.apache.commons.math3.optim.univariate BrentOptimizer
                                                      SearchInterval UnivariateObjectiveFunction
                                                      UnivariatePointValuePair]
           [org.apache.commons.math3.optim.linear SimplexSolver
                                                  LinearObjectiveFunction LinearConstraintSet LinearConstraint
                                                  Relationship NonNegativeConstraint]
           [org.apache.commons.math3.optim.nonlinear.scalar GoalType
                                                            ObjectiveFunction MultivariateOptimizer ObjectiveFunctionGradient]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv
            SimplexOptimizer MultiDirectionalSimplex NelderMeadSimplex
            PowellOptimizer BOBYQAOptimizer CMAESOptimizer CMAESOptimizer$Sigma
            CMAESOptimizer$PopulationSize]
           [org.apache.commons.math3.optim.nonlinear.scalar.gradient
            NonLinearConjugateGradientOptimizer
            NonLinearConjugateGradientOptimizer$BracketingStep
            NonLinearConjugateGradientOptimizer$Formula]
           [org.apache.commons.math3.fitting.leastsquares
            LevenbergMarquardtOptimizer GaussNewtonOptimizer
            LeastSquaresFactory LeastSquaresProblem$Evaluation]
           [org.apache.commons.math3.analysis.interpolation
            BicubicInterpolator BicubicInterpolatingFunction
            PiecewiseBicubicSplineInterpolator
            PiecewiseBicubicSplineInterpolatingFunction
            TricubicInterpolator TricubicInterpolatingFunction
            DividedDifferenceInterpolator HermiteInterpolator
            LinearInterpolator LoessInterpolator MicrosphereInterpolator
            NevilleInterpolator SplineInterpolator AkimaSplineInterpolator
            UnivariatePeriodicInterpolator UnivariateInterpolator]
           [org.apache.commons.math3.analysis.polynomials
            PolynomialFunctionNewtonForm PolynomialSplineFunction
            PolynomialFunctionLagrangeForm]))

;;; name all internal ns differently somehow
;;;TODO:
;;;CREATE FUNCTIONS FROM THESE APACHE PACKAGES
;;;org.apache.commons.math3.genetics (GeneticAlgorithm)
;;;org.apache.commons.math3.ml.clustering (Clustering algorithms)
;;;org.apache.commons.math3.ode + subcategories (ODE)

;;;(set! *warn-on-reflection* true)

;;;RANDOM NUMBER GENERATORS
;;several WELL algos are alternatives in Apache too
;;for testing, can use 'nextT' etc.. for random sampling of:
;;Zipf, Weibull, Uniform, T, Poisson, Pascal, Hypergeometric, Gaussian, Gamma, 
;;F, Exponential, Chi-Square, Cauchy, Binomial, Beta
(defn ^MersenneTwister mersenne-twister$ [] (MersenneTwister.))

(defn ^MersenneTwister mersenne-twister [^long seed] (MersenneTwister. seed))

;;Halton is an alternative in Apache too
(defn ^SobolSequenceGenerator sobol-quasi-random-sequence
  [^long dimensions] (SobolSequenceGenerator. dimensions))

(defn next-quasi-random-vector! [^SobolSequenceGenerator ssg] (.nextVector ssg))

(defn ^ISAACRandom isaac-random-cryptographic [^long seed] (ISAACRandom. seed))

(defn next-double! ^double [^RandomDataGenerator rdg] (.nextDouble rdg))

;;;APACHE FUNCTIONS
(defn- ^UnivariateFunction univariate-function
  "f is a function of a single variable (double) returning a double"
  [f] (reify UnivariateFunction (value [_ x] (f x))))

(defn- ^MultivariateFunction multivariate-function
  "f is a function accepting a vector (doubles) and returning a double"
  [f] (reify MultivariateFunction (value [_ x] (f x))))

(defn- ^MultivariateVectorFunction multivariate-vector-function
  "f is a function accepting a vector (doubles) and returning a collection of doubles"
  [f] (reify MultivariateVectorFunction (value [_ x] (double-array (f x)))))

(defn- ^MultivariateMatrixFunction multivariate-matrix-function
  "f is a function accepting a vector (doubles) and returning a double-layered collection of doubles"
  [f] (reify MultivariateMatrixFunction (value [_ x] (ar/jagged-2D-array :double (f x)))))

(defn- ^UnivariateDifferentiableFunction univariate-differentiable-function
  "deriv is a function accepting a single variable (double) returning a double"
  ([deriv ^long points ^double step-size]
   (.differentiate (FiniteDifferencesDifferentiator. points step-size) (univariate-function deriv)))
  ([deriv points step-size var-low-bound var-high-bound]
   (.differentiate
     (FiniteDifferencesDifferentiator. points step-size var-low-bound var-high-bound)
     (univariate-function deriv))))

;;;INTERPOLATION
(defn interpolation-1D
  "Implements an algorithm for interpolation of real univariate functions. 
If a 'period' length is entered, then the data is assumed to be periodic.
Solver types:
   :polynomial Polynomial Divided Difference Algorithm. 
   For reference, see Introduction to Numerical Analysis, ISBN 038795452X, 
      chapter 2.  Default.
   :cubic Computes a natural (also known as 'free', 'unclamped') cubic spline 
      interpolation for the data set.
   :akima Also a cubic spline interpolation.
   :linear Linear function
   :neville Neville's Algorithm. For reference, see Introduction to Numerical 
      Analysis, ISBN 038795452X, chapter 2.
   :loess The Local Regression Algorithm (also Loess, Lowess).  
   For reference, see William S. Cleveland - Robust Locally Weighted 
      Regression and Smoothing Scatterplots.
Return a function that accepts a point x and returns the interpolated value."
  [xvals fvals
   & {:keys [solver period bandwidth-for-loess]
      :or   {solver :dd, bandwidth-for-loess 0.3, period 0.0}}]
  (let [s (condp = solver
            :polynomial (DividedDifferenceInterpolator.),
            :neville (NevilleInterpolator.),
            :loess (LoessInterpolator. bandwidth-for-loess 2),
            :cubic (SplineInterpolator.),
            :linear (LinearInterpolator.),
            :akima (AkimaSplineInterpolator.),
            (throw (ex-info (format "Invalid solver type specified %s" solver)
                            {:fn (var interpolation-1D)}))),
        ^UnivariateFunction f (.interpolate
                                (if (pos? period)
                                  (UnivariatePeriodicInterpolator. s period) s)
                                (double-array xvals) (double-array fvals))]
    #(.value f ^double %)))

(defn interpolation-1D-using-derivatives
  "data should be a collection of seqs, where each seq 
   contains [x value & first derivative, the second derivative, and so ...]
Uses Hermite Interpolator.
Returns a function that accepts a point x and returns the interpolated value."
  [data]
  (let [hi (HermiteInterpolator.),
        _ (doseq [d data] (.addSamplePoint hi ^double (first d)
                                           (ar/jagged-2D-array :d (partition 1 (rest d)))))]
    #(first (.value hi ^double %))))

(defn interpolation-ND-microsphere$
  "Interpolator that implements the algorithm described in William Dudziak's 
   MS thesis.
Results are randomized.
xvals - the arguments for the interpolation points. 
   xvals[i][0] is the first component of interpolation point i, 
xvals[i][1] is the second component, and so on until xvals[i][d-1], 
   the last component of that interpolation point (where d is thus the
    dimension of the space).
fvals - the values for the interpolation points.
Returns a function that accepts a vector point x and returns the interpolated value."
  [xvals fvals]
  (let [^MultivariateFunction f (.interpolate (MicrosphereInterpolator.)
                                              (ar/jagged-2D-array :d xvals)
                                              (double-array fvals))]
    #(.value f (double-array %))))

(defn interpolation-2D
  "Generates a bicubic interpolation function. 
Prior to generating the interpolating function, the input can be smoothed 
   using polynomial fitting.
xvals - All the x-coordinates of the interpolation points, in ascending order.
yvals - All the y-coordinates of the interpolation points, in ascending order.
fvals - The values of the interpolation points on all the grid knots: 
   fvals[i][j] = f(xvals[i], yvals[j]).
The non-smoothed version requires at least 5 data points.
Options:
Returns a map of functions that each accept an 'x' and a 'y' value:
:valid-fn? (Is the x,y point within the interpolation range?)
:val-fn (Value at point x,y)"
  [xvals yvals fvals smooth?]
  (if smooth?
    (let [^BicubicInterpolatingFunction f
          (.interpolate (BicubicInterpolator.) (double-array xvals)
                        (double-array yvals)
                        (ar/jagged-2D-array :d fvals))]
      {:valid-fn? #(.isValidPoint f % %2), :val-fn #(.value f % %2)})
    (let [^PiecewiseBicubicSplineInterpolatingFunction f
          (.interpolate
            (PiecewiseBicubicSplineInterpolator.)
            (double-array xvals)
            (double-array yvals)
            (ar/jagged-2D-array :d fvals))]
      {:valid-fn? #(.isValidPoint f % %2), :val-fn #(.value f % %2)})))

(defn interpolation-3D
  "xvals - All the x-coordinates of the interpolation points, in ascending 
           order.
yvals - All the y-coordinates of the interpolation points, in ascending order.
zvals - All the z-coordinates of the interpolation points, in ascending order.
fvals - the values of the interpolation points on all the grid knots: 
             fvals[i][j][k] = f(xvals[i], yvals[j], zvals[k]).
Returns a value function that accepts an 'x', 'y', and 'z' value"
  [xvals yvals zvals fvals]
  (let [^TricubicInterpolatingFunction f
        (.interpolate
          (TricubicInterpolator.) (double-array xvals)
          (double-array yvals) (double-array zvals)
          (ar/jagged-3D-array :d fvals))] #(.value f % %2 %3)))

;;;OPTIMIZATION HELPERS
(defn- value-point [^PointValuePair pv]
  {:value (.getValue pv), :point (vec (.getPoint pv))})

(defn- errors-vector-point [^LeastSquaresProblem$Evaluation e]
  {::point  (mx/coerce :persistent-vector (.getPoint e))
   ::errors (mx/coerce :persistent-vector (.getResiduals e))})

(defn- uni-value-point [^UnivariatePointValuePair pv]
  {:value (.getValue pv), :point (.getPoint pv)})

(defn- goal-fn [g]
  (condp = g :min (GoalType/valueOf "MINIMIZE"),
             :max (GoalType/valueOf "MAXIMIZE"),
             (throw (ex-info (format "Invalid goal type specified %s" g) {:fn (var goal-fn)}))))

(defn- checker-fn [check-by-objective? rel abs]
  (if check-by-objective? (SimpleValueChecker. rel abs)
                          (SimplePointChecker. rel abs)))

(defn- vector-checker-fn [check-by-objective? rel abs]
  (if check-by-objective? (SimpleVectorValueChecker. rel abs)
                          (SimplePointChecker. rel abs)))

;;;ROOT SOLVERS
(s/def ::exception (partial instance? Exception))
(s/def ::root-f (s/fspec :args (s/cat :a ::m/number) :ret ::m/number))
(s/def ::guess ::m/finite)
(s/def ::bounds ::bo/finite-interval)
(s/def ::root-f-with-guess-and-bounds
  (s/with-gen
    (s/and (s/keys :req [::root-f ::guess] :opt [::bounds]) (fn [m] (let [[l u] (::bounds m)
                                                                          g (::guess m)]
                                                                      (if l (and (<= g u) (>= g l)) true))))
    #(gen/one-of (map gen/return
                      (list {::root-f identity ::guess 3.0 ::bounds [-5.0 5.0]}
                            {::root-f (fn [v] (- (m/cube v) (* 3 v))) ::guess 3.0 ::bounds [-50.0 50.0]}
                            {::root-f (fn [v] (- (m/exp v) (* 5 v))) ::guess 3.0 ::bounds [-50.0 50.0]})))))
(s/def ::max-iter (s/with-gen ::m/int+ #(s/gen (s/int-in 100 1000))))
(s/def ::rel-accu ::m/finite+)
(s/def ::abs-accu
  (s/with-gen ::m/finite+
              #(s/gen (s/double-in :infinite? false :NaN? false :min m/*dbl-close* :max 1.0))))
(s/def ::root-solver #{:bisection :bracketing-brent :brent :illinois :muller :muller2
                       :pegasus :regula :ridders :secant :newton-raphson})

(defn root-solver
  "solver options:
:bisection, :bracketing-brent, :brent, :illinois, :muller, :muller2, 
:newton-raphson, :pegasus, :regula, :ridders, :secant.
'root-f' should return NaN when out of range."
  ([args] (root-solver args {}))
  ([{::keys [root-f guess bounds]}
    {::keys [max-iter root-solver rel-accu abs-accu]
     :or    {max-iter 1000, root-solver :brent, rel-accu 1e-14, abs-accu 1e-6}}]
   (let [ex-d {:fn (var root-solver)}
         [lower upper] bounds]
     (if (= root-solver :newton-raphson)
       (try (.solve (NewtonRaphsonSolver. abs-accu) max-iter
                    (univariate-differentiable-function root-f 2 0.25) lower upper)
            (catch Exception e (ex-info (.getMessage e) ex-d)))
       (let [^BaseUnivariateSolver s
             (case root-solver
               :bisection (BisectionSolver. rel-accu abs-accu)
               :bracketing-brent (BracketingNthOrderBrentSolver. rel-accu abs-accu 5)
               :brent (BrentSolver. rel-accu abs-accu)
               :illinois (IllinoisSolver. rel-accu abs-accu)
               :muller (MullerSolver. rel-accu abs-accu)
               :muller2 (MullerSolver2. rel-accu abs-accu)
               :pegasus (PegasusSolver. rel-accu abs-accu)
               :regula (RegulaFalsiSolver. rel-accu abs-accu)
               :ridders (RiddersSolver. rel-accu abs-accu)
               :secant (SecantSolver. rel-accu abs-accu)
               nil)
             uni-fn (univariate-function root-f)]
         (when s (try (.solve s max-iter uni-fn lower upper guess)
                      (catch Exception e (ex-info (.getMessage e) ex-d)))))))))

(s/fdef root-solver
        :args (s/cat :root-f-with-guess-and-bounds ::root-f-with-guess-and-bounds
                     :opts (s/? (s/keys :opt [::max-iter ::root-solver ::rel-accu ::abs-accu])))
        :ret (s/nilable (s/or :finite ::m/finite :exception ::exception)))

;;;UNIVARIATE OPTIMIZE
(defn optimize-univariate
  "Brent Optimizer.  Search over an interval."
  [f [lo init hi]
   & {:keys [max-eval goal rel abs]
      :or   {max-eval 1000, goal :min, rel 1e-14, abs 1e-6}}]
  (let [data [(SearchInterval. lo hi init)
              (MaxEval. max-eval)
              (UnivariateObjectiveFunction. (univariate-function f))
              (goal-fn goal)],
        s (BrentOptimizer. rel abs)]
    (try
      (let [pv (.optimize s (into-array OptimizationData data))]
        (uni-value-point pv))
      (catch TooManyEvaluationsException e
        (throw (ex-info (format "Max evals (%d) exceeded" max-eval)
                        {:fn (var optimize-univariate)}))))))

;;;LINEAR PROGRAMMING
(defn- linear-constraint [[coeff relation ^double value]]
  (let [r (condp = relation :eq (Relationship/valueOf "EQ"),
                            :leq (Relationship/valueOf "LEQ"),
                            :geq (Relationship/valueOf "GEQ"),
                            (throw (ex-info (format "Invalid relation type specified %s" relation)
                                            {:fn (var linear-constraint)})))]
    (LinearConstraint. (double-array coeff) ^Relationship r value)))

(defn- add-linear-constraint [^ArrayList array-list constraint]
  (doto array-list (.add (linear-constraint constraint))))

(defn linear-programming
  [obj-coeff linear-constraints
   & {:keys [obj-constant goal non-neg-vars?]
      :or   {obj-constant 0.0, goal :min, non-neg-vars? false}}]
  (let [lcal (ArrayList.),
        _ (doseq [lc linear-constraints] (add-linear-constraint lcal lc)),
        data [(LinearObjectiveFunction.
                (double-array obj-coeff) ^double obj-constant)
              (LinearConstraintSet. lcal)
              (goal-fn goal)
              (NonNegativeConstraint. non-neg-vars?)]
        pv (.optimize (SimplexSolver.) (into-array OptimizationData data))]
    (value-point pv)))

;;;NONLINEAR LEAST SQUARES and SYSTEMS
(defn nonlinear-least-squares
  "constraints-fn takes an array and returns a vector.
jacobian-fn is the jacobian matrix function that takes an array and returns a double-layered vector.
solver can be :lm Levenberg-Marquardt (default) or :gauss-newton.
Each constraints-fn should return m/inf+ or m/inf- when out of range.
Jacobian-fn should return m/nan when out of range.
Returns map of ::point and ::errors."
  ([args] (nonlinear-least-squares args {}))
  ([{::keys [constraints-fn n-cons jacobian-fn guesses]}
    {::keys [target max-eval max-iter nls-solver check-by-objective? rel-accu abs-accu weights]
     :or    {max-eval 1000, max-iter 1000, nls-solver :lm, rel-accu 1e-14, abs-accu 1e-6}}]
   (let [ex-d {:fn (var nonlinear-least-squares)}
         c (multivariate-vector-function constraints-fn)
         j (multivariate-matrix-function jacobian-fn)
         s (condp = nls-solver
             :lm (LevenbergMarquardtOptimizer.)
             :gauss-newton (GaussNewtonOptimizer.)
             nil)
         checker (LeastSquaresFactory/evaluationChecker (vector-checker-fn check-by-objective? rel-accu abs-accu))
         observed (if (and (some? target) (= (count target) n-cons))
                    (mx/coerce :apache-commons target)
                    (mx/compute-vector-apache n-cons 0.0))
         start (mx/coerce :apache-commons guesses)
         weights (if (and (some? weights) (= (count weights) n-cons))
                   (mx/diagonal-matrix-apache weights)
                   (mx/diagonal-matrix-apache n-cons 1.0))]
     (try
       (when s (let [multivariate-jacobian-fn (LeastSquaresFactory/model c j)
                     problem (LeastSquaresFactory/create
                               multivariate-jacobian-fn observed start weights checker max-eval max-iter)
                     e (.optimize s problem)]
                 (errors-vector-point e)))
       (catch TooManyEvaluationsException _
         (ex-info (format "Max evals (%d) exceeded." max-eval) ex-d))
       (catch Exception e (ex-info (.getMessage e) ex-d))))))

(s/def ::constraints-fn fn?)
(s/def ::n-cons ::m/long+)
(s/def ::jacobian-fn fn?)
(s/def ::guesses (s/coll-of ::guess))
(s/def ::target (s/nilable (s/coll-of ::m/finite)))
(s/def ::max-eval (s/with-gen ::m/int+ #(s/gen (s/int-in 100 1000))))
(s/def ::nls-solver #{:lm :gauss-newton})
(s/def ::check-by-objective? (s/nilable boolean?))
(s/def ::weights (s/nilable (s/coll-of ::m/finite+)))
(s/def ::errors (s/coll-of ::m/number :kind vector?))
(s/def ::point (s/coll-of ::m/number :kind vector?))
(s/def ::errors-vector-point (s/keys :req [::errors ::point]))
(s/def ::constraints-with-jacobian
  (s/with-gen
    (s/keys :req [::constraints-fn ::n-cons ::jacobian-fn ::guesses])
    #(gen/one-of
       (map gen/return
            (list {::constraints-fn (fn [[a1 a2]] [(+ a1 (m/sq a2)) (- (m/cube a1) a2)])
                   ::n-cons         2
                   ::jacobian-fn    (fn [[a1 a2]] [[1.0 (* 3 (m/sq a1))] [(* 2 a2) -1.0]])
                   ::guesses        [2.0 -2.0]}
                  {::constraints-fn (fn [[a1 a2]] [(+ a1 (m/exp a2)) (if (m/non+? a1) m/inf- (- (m/log a1) a2))])
                   ::n-cons         2
                   ::jacobian-fn    (fn [[a1 a2]] [[1.0 (if (not (zero? a1)) (- (/ a1)) m/inf-)] [(m/exp a2) -1.0]])
                   ::guesses        [1.9 -1.9]})))))

(s/fdef nonlinear-least-squares
        :args (s/cat :constraints-with-jacobian ::constraints-with-jacobian
                     :opts (s/? (s/keys :opt [::target ::max-eval ::max-iter ::nls-solver
                                              ::check-by-objective? ::rel-accu ::abs-accu ::weights])))
        :ret (s/nilable (s/or :ret ::errors-vector-point :exception ::exception)))

;;;OPTIMIZE WITH GRADIENT
(defn optimize-with-gradient
  [f grad start
   & {:keys [update-type check-by-objective? max-eval goal initial-step rel abs]
      :or   {update-type  :fletcher-reeves, max-eval 1000, goal :min,
             initial-step 1.0, rel 1e-14, abs 1e-6}}]
  (let [u (condp = update-type
            :polak-ribiere
            NonLinearConjugateGradientOptimizer$Formula/POLAK_RIBIERE,
            :fletcher-reeves
            NonLinearConjugateGradientOptimizer$Formula/FLETCHER_REEVES,
            (throw (ex-info (format "Invalid update type %s" update-type)
                            {:fn (var optimize-with-gradient)}))),
        checker (checker-fn check-by-objective? rel abs),
        data [(NonLinearConjugateGradientOptimizer$BracketingStep.
                initial-step),
              (MaxEval. max-eval),
              (ObjectiveFunction. (multivariate-function f)),
              (ObjectiveFunctionGradient. (multivariate-vector-function grad)),
              (goal-fn goal),
              (InitialGuess. (double-array start))]]
    (try
      (let [pv (.optimize (NonLinearConjugateGradientOptimizer. u checker)
                          (into-array OptimizationData data))] (value-point pv))
      (catch TooManyEvaluationsException e
        (throw (ex-info (format "Max evals (%d) exceeded." max-eval)
                        {:fn (var optimize-with-gradient) :solver? true :external? true}))))))

;;;OPTIMIZE NO-DERIVATIVE NO-CONSTRAINTS
(defn optimize-without-constraints
  "This is for when you can't or don't want to calculate the derivative of the 
   function"
  [f start
   & {:keys [max-eval goal solver rel abs]
      :or   {max-eval 1000, goal :min, solver :multi, rel 1e-14, abs 1e-6}}]
  (let [ndim (count start), initial (double-array start),
        data [(MaxEval. max-eval)
              (ObjectiveFunction. (multivariate-function f))
              (goal-fn goal)
              (InitialGuess. initial)],
        data (condp = solver
               :powell data
               :nelder-mead (conj data (doto (NelderMeadSimplex. ndim)
                                         (.build initial)))
               :multi-directional-simplex (conj
                                            data
                                            (doto (MultiDirectionalSimplex.
                                                    ndim)
                                              (.build initial)))
               (throw (ex-info (format "Invalid optimizer specified %s" solver)
                               {:fn (var optimize-without-constraints)})))
        s (if (= solver :powell) (PowellOptimizer. rel abs)
                                 (SimplexOptimizer. rel abs))]
    (try
      (let [pv (.optimize s (into-array OptimizationData data))]
        (value-point pv))
      (catch TooManyEvaluationsException e
        (throw (ex-info (format "Max evals (%d) exceeded." max-eval)
                        {:fn (var optimize-without-constraints) :solver? true :external? true}))))))

;;;OPTIMIZE NO-DERIVATIVE WITH SIMPLE BOUNDS ONLY
(defn optimize-cma-evolution
  "An implementation of the active Covariance Matrix Adaptation Evolution 
   Strategy (CMA-ES) for non-linear, non-convex, non-smooth, global function 
   minimization. 
The CMA-Evolution Strategy (CMA-ES) is a reliable stochastic optimization 
   method which should be applied if derivative-based methods, 
   e.g. quasi-Newton BFGS or conjugate gradient, fail due to a rugged search 
   landscape (e.g. noise, local optima, outlier, etc.) of the objective 
   function. 
Like a quasi-Newton method, the CMA-ES learns and applies a variable metric 
   on the underlying search space. 
Unlike a quasi-Newton method, the CMA-ES neither estimates nor uses gradients, 
   making it considerably more reliable in terms of finding a good, 
   or even close to optimal, solution.
In general, on smooth objective functions the CMA-ES is roughly ten times 
   slower than BFGS (counting objective function evaluations, no gradients 
   provided). 
For up to N=10 variables also the derivative-free simplex direct search 
   method (Nelder and Mead) can be faster, but it is far less reliable 
   than CMA-ES.
The CMA-ES is particularly well suited for non-separable and/or badly 
   conditioned problems. 
To observe the advantage of CMA compared to a conventional evolution 
   strategy, it will usually take about 30 N function evaluations. 
On difficult problems the complete optimization (a single run) is expected 
   to take roughly between 30 N and 300 N2 function evaluations.

Population size. The number of offspring is the primary strategy parameter. 
In the absence of better clues, a good default could be an integer close 
   to 4 + 3 ln(n), where n is the number of optimized parameters. 
Increasing the population size improves global search properties at the 
   expense of speed (which in general decreases at most linearly with 
   increasing population size)."
  [f lower-bounds upper-bounds start rnd-long &
   {:keys [sigma-array population max-iter stop-fitness active-cma? diag-only
           check-feasible-count generate-stats? check-by-objective? goal rel
           abs]
    :or   {stop-fitness         0.0, active-cma? true, diag-only 0,
           check-feasible-count 0, generate-stats? false, goal :min,
           rel                  1e-14, abs 1e-6}}]
  (let [ndim (count start),
        p (if population population (+ 4 (m/floor (* 3 (m/log ndim))))),
        iter (if max-iter max-iter
                          (m/floor (* 1000 (m/sq (+ 5 ndim)) (m/pow p -0.5)))),
        sigma (if sigma-array (double-array sigma-array)
                              (double-array ndim 0.3))
        data [(MaxEval. iter)
              (ObjectiveFunction. (multivariate-function f))
              (goal-fn goal)
              (InitialGuess. (double-array start))
              (CMAESOptimizer$Sigma. sigma)
              (CMAESOptimizer$PopulationSize. p)
              (SimpleBounds. (double-array lower-bounds)
                             (double-array upper-bounds))],
        checker (checker-fn check-by-objective? rel abs),
        s (CMAESOptimizer. iter stop-fitness active-cma? diag-only
                           check-feasible-count (mersenne-twister rnd-long)
                           generate-stats? checker)]
    (try
      ;:value calc'd b/c not consistent with :point values
      (let [pv (.optimize s (into-array OptimizationData data)),
            pt (:point (value-point pv))] {:point pt, :value (f pt)})
      (catch TooManyEvaluationsException e
        (throw (ex-info (format "Max evals (%d) exceeded." iter)
                        {:fn (var optimize-cma-evolution) :solver? true :external? true})))
      (catch TooManyIterationsException e
        (throw (ex-info (format "Max iterations (%d) exceeded." iter),
                        {:fn (var optimize-cma-evolution) :solver? true :external? true}))))))

(defn optimize-bobyqa
  "Powell's BOBYQA algorithm  (Bound Optimization BY Quadratic Approximation).  
Faster but less robust than cma-evolution strategy.
BOBYQA is particularly well suited for high dimensional problems where 
   derivatives are not available. 
In most cases it outperforms the PowellOptimizer significantly. 
BOBYQA could also be considered as a replacement of any derivative-based 
   optimizer when the derivatives are approximated by finite differences."
  [f lower-bounds upper-bounds start
   & {:keys [interpolation-points max-eval goal]
      :or   {max-eval 1000, goal :min}}]
  (let [data [(MaxEval. max-eval)
              (ObjectiveFunction. (multivariate-function f))
              (goal-fn goal)
              (InitialGuess. (double-array start))
              (SimpleBounds. (double-array lower-bounds)
                             (double-array upper-bounds))],
        p (or interpolation-points (m/ceil (* 1.5 (inc (count start))))),
        s (BOBYQAOptimizer. p)]
    (try
      (let [pv (.optimize s (into-array OptimizationData data))]
        (value-point pv))
      (catch TooManyEvaluationsException e
        (throw (ex-info (format "Max evals (%d) exceeded." max-eval)
                        {:fn (var optimize-bobyqa) :solver? true :external? true}))))))

;SPECIAL FUNCTIONS
(defn log-gamma ^double [^double x] (Gamma/logGamma x))

(defn regularized-gamma-p
  (^double [^double a ^double x] (Gamma/regularizedGammaP a x))
  (^double [^double a ^double x ^double abs ^long max-iter]
   (Gamma/regularizedGammaP a x abs max-iter)))

(defn regularized-gamma-q
  (^double [^double a ^double x] (Gamma/regularizedGammaQ a x))
  (^double [^double a ^double x ^double abs ^long max-iter]
   (Gamma/regularizedGammaQ a x abs max-iter)))

(defn digamma ^double [^double x] (Gamma/digamma x))

(defn trigamma ^double [^double x] (Gamma/trigamma x))

(defn log-beta ^double [^double p ^double q] (Beta/logBeta p q))

(defn regularized-beta
  (^double [^double x ^double a ^double b] (Beta/regularizedBeta x a b))
  ([x a b abs max-iter] (Beta/regularizedBeta x a b abs max-iter))
  (^double [^double x ^double a ^double b ^long max-iter]
   (Beta/regularizedBeta x a b max-iter)))

(defn erf
  (^double [^double x] (Erf/erf x))
  (^double [^double x1 ^double x2] (Erf/erf x1 x2)))

(defn erfc ^double [^double x] (Erf/erfc x))

(defn erfc-inv ^double [^double x] (Erf/erfcInv x))

(defn erf-inv ^double [^double x] (Erf/erfInv x))

;;;DISTRIBUTIONS
(defn beta-dist-pdf ^double [^double alpha ^double beta ^double value]
  (.density (BetaDistribution. alpha beta) value))

(defn beta-dist-cdf ^double [^double alpha ^double beta ^double value]
  (.cumulativeProbability (BetaDistribution. alpha beta) value))

(defn beta-dist-icdf ^double [^double alpha ^double beta ^double probability]
  (.inverseCumulativeProbability (BetaDistribution. alpha beta) probability))

(defn beta-dist-mean ^double [^double alpha ^double beta]
  (.getNumericalMean (BetaDistribution. alpha beta)))

(defn beta-dist-variance ^double [^double alpha ^double beta]
  (.getNumericalVariance (BetaDistribution. alpha beta)))

(defn cauchy-dist-pdf ^double [^double median ^double scale ^double value]
  (.density (CauchyDistribution. median scale) value))

(defn cauchy-dist-cdf ^double [^double median ^double scale ^double value]
  (.cumulativeProbability (CauchyDistribution. median scale) value))

(defn cauchy-dist-icdf
  ^double [^double median ^double scale ^double probability]
  (.inverseCumulativeProbability
    (CauchyDistribution. median scale) probability))

(defn cauchy-dist-mean ^double [^double median ^double scale]
  (.getNumericalMean (CauchyDistribution. median scale)))

(defn cauchy-dist-variance ^double [^double median ^double scale]
  (.getNumericalVariance (CauchyDistribution. median scale)))

(defn chi-sq-dist-pdf ^double [^double degrees ^double value]
  (.density (ChiSquaredDistribution. degrees) value))

(defn chi-sq-dist-cdf ^double [^double degrees ^double value]
  (.cumulativeProbability (ChiSquaredDistribution. degrees) value))

(defn chi-sq-dist-icdf ^double [^double degrees ^double probability]
  (.inverseCumulativeProbability (ChiSquaredDistribution. degrees) probability))

(defn chi-sq-dist-mean ^double [^double degrees]
  (.getNumericalMean (ChiSquaredDistribution. degrees)))

(defn chi-sq-dist-variance ^double [^double degrees]
  (.getNumericalVariance (ChiSquaredDistribution. degrees)))

(defn exponential-dist-pdf ^double [^double mean ^double value]
  (.density (ExponentialDistribution. mean) value))

(defn exponential-dist-cdf ^double [^double mean ^double value]
  (.cumulativeProbability (ExponentialDistribution. mean) value))

(defn exponential-dist-icdf ^double [^double mean ^double probability]
  (.inverseCumulativeProbability (ExponentialDistribution. mean) probability))

(defn exponential-dist-mean ^double [^double mean]
  (.getNumericalMean (ExponentialDistribution. mean)))

(defn exponential-dist-variance ^double [^double mean]
  (.getNumericalVariance (ExponentialDistribution. mean)))

(defn f-dist-pdf ^double [^double num-deg ^double den-deg ^double value]
  (.density (FDistribution. num-deg den-deg) value))

(defn f-dist-cdf ^double [^double num-deg ^double den-deg ^double value]
  (.cumulativeProbability (FDistribution. num-deg den-deg) value))

(defn f-dist-icdf ^double [^double num-deg ^double den-deg ^double probability]
  (.inverseCumulativeProbability (FDistribution. num-deg den-deg) probability))

(defn f-dist-mean ^double [^double num-deg ^double den-deg]
  (.getNumericalMean (FDistribution. num-deg den-deg)))

(defn f-dist-variance ^double [^double num-deg ^double den-deg]
  (.getNumericalVariance (FDistribution. num-deg den-deg)))

(defn gamma-dist-pdf ^double [^double shape ^double scale ^double value]
  (.density (GammaDistribution. shape scale) value))

(defn gamma-dist-cdf ^double [^double shape ^double scale ^double value]
  (.cumulativeProbability (GammaDistribution. shape scale) value))

(defn gamma-dist-icdf ^double [^double shape ^double scale ^double probability]
  (.inverseCumulativeProbability (GammaDistribution. shape scale) probability))

(defn gamma-dist-mean ^double [^double shape ^double scale]
  (.getNumericalMean (GammaDistribution. shape scale)))

(defn gamma-dist-variance ^double [^double shape ^double scale]
  (.getNumericalVariance (GammaDistribution. shape scale)))

(defn gumbel-dist-pdf ^double [^double mu ^double beta]
  (.density (GumbelDistribution. mu beta)))

(defn gumbel-dist-cdf ^double [^double mu ^double beta ^double value]
  (.cumulativeProbability (GumbelDistribution. mu beta) value))

(defn gumbel-dist-icdf ^double [^double mu ^double beta ^double probability]
  (.inverseCumulativeProbability (GumbelDistribution. mu beta) probability))

(defn gumbel-dist-mean ^double [^double mu ^double beta]
  (.getNumericalMean (GumbelDistribution. mu beta)))

(defn gumbel-dist-variance ^double [^double mu ^double beta]
  (.getNumericalVariance (GumbelDistribution. mu beta)))

(defn laplace-dist-pdf ^double [^double mu ^double beta ^double value]
  (.density (LaplaceDistribution. mu beta) value))

(defn laplace-dist-cdf ^double [^double mu ^double beta ^double value]
  (.cumulativeProbability (LaplaceDistribution. mu beta) value))

(defn laplace-dist-icdf
  ^double [^double mu ^double beta ^double probability]
  (.inverseCumulativeProbability (LaplaceDistribution. mu beta) probability))

(defn laplace-dist-mean ^double [^double mu ^double beta]
  (.getNumericalMean (LaplaceDistribution. mu beta)))

(defn laplace-dist-variance ^double [^double mu ^double beta]
  (.getNumericalVariance (LaplaceDistribution. mu beta)))

(defn levy-dist-pdf ^double [^double location ^double scale ^double value]
  (.density (LevyDistribution. (mersenne-twister 0) location scale) value))

(defn levy-dist-cdf ^double [^double location ^double scale ^double value]
  (.cumulativeProbability (LevyDistribution. (mersenne-twister 0) location
                                             scale) value))

(defn levy-dist-icdf
  ^double [^double location ^double scale ^double probability]
  (.inverseCumulativeProbability (LevyDistribution. location scale)
                                 probability))

(defn levy-dist-mean ^double [^double location ^double scale]
  (.getNumericalMean (LevyDistribution. location scale)))

(defn levy-dist-variance ^double [^double location ^double scale]
  (.getNumericalVariance (LevyDistribution. location scale)))

(defn logistic-dist-pdf ^double [^double mu ^double s ^double value]
  (.density (LogisticDistribution. mu s) value))

(defn logistic-dist-cdf ^double [^double mu ^double s ^double value]
  (.cumulativeProbability (LogisticDistribution. mu s) value))

(defn logistic-dist-icdf
  ^double [^double mu ^double s ^double probability]
  (.inverseCumulativeProbability (LogisticDistribution. mu s) probability))

(defn logistic-dist-mean ^double [^double mu ^double s]
  (.getNumericalMean (LogisticDistribution. mu s)))

(defn logistic-dist-variance ^double [^double mu ^double s]
  (.getNumericalVariance (LogisticDistribution. mu s)))

(defn lognormal-dist-pdf ^double [^double scale ^double shape ^double value]
  (.density (LogNormalDistribution. scale shape) value))

(defn lognormal-dist-cdf ^double [^double scale ^double shape ^double value]
  (.cumulativeProbability (LogNormalDistribution. scale shape) value))

(defn lognormal-dist-icdf
  ^double [^double scale ^double shape ^double probability]
  (.inverseCumulativeProbability
    (LogNormalDistribution. scale shape) probability))

(defn lognormal-dist-mean ^double [^double scale ^double shape]
  (.getNumericalMean (LogNormalDistribution. scale shape)))

(defn lognormal-dist-variance ^double [^double scale ^double shape]
  (.getNumericalVariance (LogNormalDistribution. scale shape)))

(defn nakagami-dist-pdf ^double [^double mu ^double omega ^double value]
  (.density (NakagamiDistribution. mu omega) value))

(defn nakagami-dist-cdf ^double [^double mu ^double omega ^double value]
  (.cumulativeProbability (NakagamiDistribution. mu omega) value))

(defn nakagami-dist-icdf
  ^double [^double mu ^double omega ^double probability]
  (.inverseCumulativeProbability (NakagamiDistribution. mu omega)
                                 probability))

(defn nakagami-dist-mean ^double [^double mu ^double omega]
  (.getNumericalMean (NakagamiDistribution. mu omega)))

(defn nakagami-dist-variance ^double [^double mu ^double omega]
  (.getNumericalVariance (NakagamiDistribution. mu omega)))

(defn normal-dist-pdf ^double [^double mean ^double sd ^double value]
  (.density (NormalDistribution. mean sd) value))

(defn normal-dist-cdf ^double [^double mean ^double sd ^double value]
  (.cumulativeProbability (NormalDistribution. mean sd) value))

(defn normal-dist-icdf ^double [^double mean ^double sd ^double probability]
  (.inverseCumulativeProbability (NormalDistribution. mean sd) probability))

(defn normal-dist-mean ^double [^double mean ^double sd]
  (.getNumericalMean (NormalDistribution. mean sd)))

(defn normal-dist-variance ^double [^double mean ^double sd]
  (.getNumericalVariance (NormalDistribution. mean sd)))

(defn pareto-dist-pdf ^double [^double scale ^double shape ^double value]
  (.density (ParetoDistribution. scale shape) value))

(defn pareto-dist-cdf ^double [^double scale ^double shape ^double value]
  (.cumulativeProbability (ParetoDistribution. scale shape) value))

(defn pareto-dist-icdf
  ^double [^double scale ^double shape ^double probability]
  (.inverseCumulativeProbability (ParetoDistribution. scale shape)
                                 probability))

(defn pareto-dist-mean ^double [^double scale ^double shape]
  (.getNumericalMean (ParetoDistribution. scale shape)))

(defn pareto-dist-variance ^double [^double scale ^double shape]
  (.getNumericalVariance (ParetoDistribution. scale shape)))

(defn t-dist-pdf ^double [^double degrees ^double value]
  (.density (TDistribution. degrees) value))

(defn t-dist-cdf ^double [^double degrees ^double value]
  (.cumulativeProbability (TDistribution. degrees) value))

(defn t-dist-icdf ^double [^double degrees ^double probability]
  (.inverseCumulativeProbability (TDistribution. degrees) probability))

(defn t-dist-mean ^double [^double degrees]
  (.getNumericalMean (TDistribution. degrees)))

(defn t-dist-variance ^double [^double degrees]
  (.getNumericalVariance (TDistribution. degrees)))

(defn triangular-dist-pdf ^double [^double a ^double b ^double c ^double value]
  (.density (TriangularDistribution. a b c) value))

(defn triangular-dist-cdf ^double [^double a ^double b ^double c ^double value]
  (.cumulativeProbability (TriangularDistribution. a b c) value))

(defn triangular-dist-icdf
  ^double [^double a ^double b ^double c ^double probability]
  (.inverseCumulativeProbability (TriangularDistribution. a b c) probability))

(defn triangular-dist-mean ^double [^double a ^double b ^double c]
  (.getNumericalMean (TriangularDistribution. a b c)))

(defn triangular-dist-variance ^double [^double a ^double b ^double c]
  (.getNumericalVariance (TriangularDistribution. a b c)))

(defn uniform-real-dist-pdf ^double [^double lower ^double upper ^double value]
  (.density (UniformRealDistribution. lower upper) value))

(defn uniform-real-dist-cdf ^double [^double lower ^double upper ^double value]
  (.cumulativeProbability (UniformRealDistribution. lower upper) value))

(defn uniform-real-dist-icdf
  ^double [^double lower ^double upper ^double probability]
  (.inverseCumulativeProbability
    (UniformRealDistribution. lower upper) probability))

(defn uniform-real-dist-mean ^double [^double lower ^double upper]
  (.getNumericalMean (UniformRealDistribution. lower upper)))

(defn uniform-real-dist-variance ^double [^double lower ^double upper]
  (.getNumericalVariance (UniformRealDistribution. lower upper)))

(defn weibull-dist-pdf ^double [^double alpha ^double beta ^double value]
  (.density (WeibullDistribution. alpha beta) value))

(defn weibull-dist-cdf ^double [^double alpha ^double beta ^double value]
  (.cumulativeProbability (WeibullDistribution. alpha beta) value))

(defn weibull-dist-icdf
  ^double [^double alpha ^double beta ^double probability]
  (.inverseCumulativeProbability
    (WeibullDistribution. alpha beta) probability))

(defn weibull-dist-mean ^double [^double alpha ^double beta]
  (.getNumericalMean (WeibullDistribution. alpha beta)))

(defn weibull-dist-variance ^double [^double alpha ^double beta]
  (.getNumericalVariance (WeibullDistribution. alpha beta)))

;;;SPECIAL
(defn inv-chi-sq-dist-icdf
  ^double [^double degrees ^double scaling ^double probability]
  (* scaling degrees
     (m/div (.inverseCumulativeProbability
              (ChiSquaredDistribution. degrees) (m/one- probability)))))

(defn students-t-dist-icdf
  ^double [^double location ^double squared-scale ^double degrees
           ^double probability]
  (+ location (* (m/sqrt squared-scale)
                 (.inverseCumulativeProbability
                   (TDistribution. degrees) probability))))

;;;INTEGER DISTRIBUTIONS 
(defn binomial-dist-pdf ^double [^long trials ^double p ^long value]
  (.probability (BinomialDistribution. trials p) value))

(defn binomial-dist-cdf ^double [^long trials ^double p ^long value]
  (.cumulativeProbability (BinomialDistribution. trials p) value))

(defn binomial-dist-icdf ^long [^long trials ^double p ^double probability]
  (.inverseCumulativeProbability (BinomialDistribution. trials p) probability))

(defn binomial-dist-mean ^double [^long trials ^double p]
  (.getNumericalMean (BinomialDistribution. trials p)))

(defn binomial-dist-variance ^double [^long trials ^double p]
  (.getNumericalVariance (BinomialDistribution. trials p)))

(defn geometric-dist-pdf ^double [^double p ^long value]
  (.probability (GeometricDistribution. p) value))

(defn geometric-dist-cdf ^double [^double p ^long value]
  (.cumulativeProbability (GeometricDistribution. p) value))

(defn geometric-dist-icdf ^long [^double p ^double probability]
  (.inverseCumulativeProbability (GeometricDistribution. p) probability))

(defn geometric-dist-mean ^double [^double p]
  (.getNumericalMean (GeometricDistribution. p)))

(defn geometric-dist-variance ^double [^double p]
  (.getNumericalVariance (GeometricDistribution. p)))

(defn hypergeometric-dist-pdf
  ^double [^long population-size ^long number-of-successes ^long sample-size
           ^long value]
  (.probability (HypergeometricDistribution.
                  population-size number-of-successes sample-size) value))

(defn hypergeometric-dist-cdf
  ^double [^long population-size ^long number-of-successes ^long sample-size
           ^long value]
  (.cumulativeProbability
    (HypergeometricDistribution.
      population-size number-of-successes sample-size) value))

(defn hypergeometric-dist-icdf
  ^long [^long population-size ^long number-of-successes ^long sample-size
         ^double probability]
  (.inverseCumulativeProbability
    (HypergeometricDistribution.
      population-size number-of-successes sample-size) probability))

(defn hypergeometric-dist-mean
  ^double [^long population-size ^long number-of-successes ^long sample-size]
  (.getNumericalMean
    (HypergeometricDistribution.
      population-size number-of-successes sample-size)))

(defn hypergeometric-dist-variance
  ^double [^long population-size ^long number-of-successes ^long sample-size]
  (.getNumericalVariance
    (HypergeometricDistribution.
      population-size number-of-successes sample-size)))

(defn pascal-dist-pdf ^double [^long r, ^double p ^long value]
  (.probability (PascalDistribution. r p) value))

(defn pascal-dist-cdf ^double [^long r, ^double p ^long value]
  (.cumulativeProbability (PascalDistribution. r p) value))

(defn pascal-dist-icdf ^long [^long r, ^double p ^double probability]
  (.inverseCumulativeProbability (PascalDistribution. r p) probability))

(defn pascal-dist-mean ^double [^long r, ^double p]
  (.getNumericalMean (PascalDistribution. r p)))

(defn pascal-dist-variance ^double [^long r, ^double p]
  (.getNumericalVariance (PascalDistribution. r p)))

(defn poisson-dist-pdf ^double [^double p ^long value]
  (.probability (PoissonDistribution. p) value))

(defn poisson-dist-cdf ^double [^double p ^long value]
  (.cumulativeProbability (PoissonDistribution. p) value))

(defn poisson-dist-icdf ^long [^double p ^double probability]
  (.inverseCumulativeProbability (PoissonDistribution. p) probability))

(defn poisson-dist-mean ^double [^double p]
  (.getNumericalMean (PoissonDistribution. p)))

(defn poisson-dist-variance ^double [^double p]
  (.getNumericalVariance (PoissonDistribution. p)))

(defn uniform-integer-dist-pdf ^double [^long lower ^long upper ^long value]
  (.probability (UniformIntegerDistribution. lower upper) value))

(defn uniform-integer-dist-cdf ^double [^long lower ^long upper ^long value]
  (.cumulativeProbability (UniformIntegerDistribution. lower upper) value))

(defn uniform-integer-dist-icdf
  ^long [^long lower ^long upper ^double probability]
  (.inverseCumulativeProbability
    (UniformIntegerDistribution. lower upper) probability))

(defn uniform-integer-dist-mean
  ^double [^long lower ^long upper]
  (.getNumericalMean (UniformIntegerDistribution. lower upper)))

(defn uniform-integer-dist-variance
  ^double [^long lower ^long upper]
  (.getNumericalVariance (UniformIntegerDistribution. lower upper)))

(defn zipf-dist-pdf
  ^double [^long number-of-elements ^double exponent ^long value]
  (.probability (ZipfDistribution. number-of-elements exponent) value))

(defn zipf-dist-cdf
  ^double [^long number-of-elements ^double exponent ^long value]
  (.cumulativeProbability
    (ZipfDistribution. number-of-elements exponent) value))

(defn zipf-dist-icdf
  ^long [^long number-of-elements ^double exponent ^double probability]
  (.inverseCumulativeProbability
    (ZipfDistribution. number-of-elements exponent) probability))

(defn zipf-dist-mean ^double [^long number-of-elements ^double exponent]
  (.getNumericalMean (ZipfDistribution. number-of-elements exponent)))

(defn zipf-dist-variance ^double [^long number-of-elements ^double exponent]
  (.getNumericalVariance (ZipfDistribution. number-of-elements exponent)))

;;;OTHER DISTRIBUTIONS
(defn kolmogorov-smirnov-dist-cdf ^double [^long n ^double value]
  (.cdf (KolmogorovSmirnovDistribution. n) value))

(defn mixture-multivariate-normal-dist-pdf
  ^double [weights means covariances values]
  (.density (MixtureMultivariateNormalDistribution.
              (double-array weights) (ar/jagged-2D-array :d means)
              (ar/jagged-3D-array :d covariances)) (double-array values)))

(defn multivariate-normal-dist-pdf ^double [means covariances values]
  (.density (MultivariateNormalDistribution.
              (double-array means) (ar/jagged-2D-array :d covariances))
            (double-array values)))

