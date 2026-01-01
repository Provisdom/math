# provisdom.math

A comprehensive Clojure mathematics library providing numerical computing primitives with robust handling of edge cases, IEEE 754 compliance, and spec-driven development.

## Features

### Combinatorics (`provisdom.math.combinatorics`)
Combinatorial computations:
- Factorials: factorial, subfactorial (derangements), double factorial
- Pochhammer symbols: rising and falling factorials
- Binomial coefficients: choose-k-from-n, multinomial coefficient
- Special numbers: Stirling (1st/2nd kind), Bell, Catalan
- Binomial probability calculations
- Integer partitions (partitioning numbers, not sets)
- Combinations and permutations (lazy sequences)
- K-permutations (partial permutations)
- Direct access: nth-combination, nth-permutation for random sampling
- Counting functions for combinatorial enumeration
- Cartesian product and selections with replacement
- Log-space calculations to avoid overflow

### Core (`provisdom.math.core`)
Foundational mathematical functions and constants:
- Mathematical constants (PI, E, sqrt-two, log-two-pi, etc.)
- Enhanced numeric type predicates that handle NaN/infinity correctly
- Robust arithmetic with division-by-zero handling
- High-precision rounding and comparison functions
- Angle conversion utilities (radians/degrees)
- Functions with apostrophe variants (e.g., `floor'`) that return longs when possible

### Derivatives (`provisdom.math.derivatives`)
Numerical differentiation:
- Scalar derivatives (1st through 8th order)
- Gradients, Jacobians, and Hessians
- Partial derivatives for bivariate functions (∂f/∂x, ∂f/∂y, ∂²f/∂x², ∂²f/∂y², ∂²f/∂x∂y)
- Central, forward, and backward difference schemes
- Configurable accuracy levels
- Richardson extrapolation for improved accuracy
- Adaptive step size selection with convergence testing
- Error estimates with derivative values
- Vector calculus: directional derivatives, Laplacian, divergence, curl (3D)
- Higher-order mixed partial derivatives
- Sparse Jacobian and Hessian computation for efficiency

### Format (`provisdom.math.format`)
Number formatting and parsing utilities:
- Intelligent formatting that auto-selects fixed-point vs scientific notation
- Shorthand notation: K (thousands), M (millions), B (billions), T (trillions)
- Bidirectional parsing/unparsing (`unparse-shorthand`, `parse-shorthand`)
- Scientific notation parsing support (e.g., "1.23E+4")
- Money formatting with $ prefix
- Percentage formatting (`format-percent`)
- Engineering notation (`format-as-engineering`) with exponents divisible by 3
- Extended formatting options (`format-number-extended`):
  - Thousand separators
  - Custom rounding modes (half-up, half-even, floor, ceiling)
  - Localization (custom decimal/grouping symbols)
- Customizable shorthand letters (`unparse-shorthand-custom`)
- Special value handling (NaN, Infinity)
- Safe parsing (no code execution risk)

### Integrals (`provisdom.math.integrals`)
Numerical integration with multiple quadrature methods:
- **Gauss-Kronrod**: `integration` - Default adaptive quadrature for 1D integrals
- **Clenshaw-Curtis**: `clenshaw-curtis-integration` - Chebyshev-based, includes endpoints
- **Tanh-sinh**: `tanh-sinh-integration` - Double-exponential for endpoint singularities
- **Monte Carlo**: `monte-carlo-integration`, `quasi-monte-carlo-integration` (Halton) - High-dimensional integrals
- **Sparse grids**: `sparse-grid-integration` - Smolyak algorithm for moderate dimensions (5-15)
- **Oscillatory**: `oscillatory-integration` - Filon-type for f(x)·sin(ωx) integrands
- Multi-dimensional integration: `rectangular-integration`, `non-rectangular-2D-integration`
- `integration-with-error` returns value with error estimate
- Singularity handling via `::singularities` option
- Automatic infinite interval transformation
- Parallel processing support via `::parallel?` option

### Intervals (`provisdom.math.intervals`)
Interval arithmetic and bounds manipulation:
- **Intervals**: Simple `[lower, upper]` vectors with inclusive endpoints
- **Bounds**: Maps with `::lower`, `::upper`, `::open-lower?`, `::open-upper?` keys for flexible endpoint handling
- Interval operations: `in-interval?`, `bound-by-interval`, `interval-width`, `interval-midpoint`
- Bounds operations: `intersection`, `union`, `encompassing-bounds`, `overlaps?`, `contains-bounds?`
- Bounds utilities: `bound-by-bounds`, `bounds-width`, `bounds-midpoint`
- Predefined bounds: `bounds-prob` [0,1], `bounds-open-prob` (0,1), `bounds+` (0,∞], `bounds-finite` (-∞,+∞)
- Specialized bounds for optimization constraints and positive-definite matrices

### Linear Algebra (`provisdom.math.linear-algebra`)
Matrix decompositions and linear system solving:
- LU decomposition with partial pivoting
- Cholesky decomposition for positive-definite matrices
- QR decomposition using Householder reflections
- Eigendecomposition for symmetric matrices
- Singular Value Decomposition (SVD)
- Linear system solving (exact and least squares)
- Matrix inverse and Moore-Penrose pseudoinverse
- Determinant, condition number, and matrix rank
- Minors, cofactors, and adjugate matrices
- Matrix power (including negative powers via inverse)
- Matrix exponential (e^M) using Padé approximation
- Induced matrix norms (1-norm, infinity-norm, spectral norm)
- Positive definite/semi-definite utilities
- Correlation/covariance matrix conversions

### Matrix (`provisdom.math.matrix`)
Comprehensive matrix operations:
- Matrix creation (identity, diagonal, Toeplitz, random)
- Specialized types (symmetric, triangular, sparse, positive-definite)
  - Note: Sparse matrix support is limited to conversion utilities (`sparse->matrix`, `matrix->sparse`). Full sparse algebra is not implemented; convert to dense for operations.
- Matrix multiplication, transpose, Kronecker product
- Slicing, filtering, and partitioning
- Row/column manipulation (insert, remove, update)
- Serialization/deserialization of triangular matrices
- Matrix trace, outer product, and element-wise operations
- Matrix rounding utilities

### Polynomials (`provisdom.math.polynomials`)
Polynomial evaluation, orthogonal polynomials, and interpolation:
- Polynomial evaluation using Horner's method (`horner-eval`)
- Chebyshev series evaluation using Clenshaw algorithm (`clenshaw-eval`)
- Polynomial arithmetic: add, subtract, multiply, scale, divide
- Chebyshev polynomials (first and second kind) with derivatives
- Bidirectional Chebyshev ↔ standard polynomial coefficient conversion
- Chebyshev nodes and extrema for optimal interpolation
- Legendre polynomials for Gaussian quadrature
- Hermite polynomials (physicist's and probabilist's conventions)
- Laguerre polynomials for exponential-weighted problems
- Lagrange and Newton interpolation (functions and coefficients)
- 1D, 2D, and N-dimensional polynomial basis functions

### Random (`provisdom.math.random`)
Splittable random number generation for reproducible parallel computation:
- Immutable RNG with `rng`, `rnd`, `rnd-long`, `rnd-normal`, `rnd-uuid`
- Bound RNG via `bind-seed` macro for scoped randomness
- Lazy infinite sequences of random values
- Clock-seeded generators for non-reproducible randomness

### Series (`provisdom.math.series`)
Infinite series summation and acceleration:
- Adaptive convergence testing with diagnostics
- Power series evaluation, multiplication, composition, and inversion
- Continued fractions (standard, generalized, multiplicative)
- Series acceleration: Aitken, Wynn epsilon, Euler transform, Richardson
- Padé approximants for rational function approximation
- Multiple summation algorithms: Kahan, Neumaier, pairwise
- Radius of convergence estimation

### Special Functions (`provisdom.math.special-functions`)
Advanced mathematical functions:
- Gamma functions (gamma, log-gamma, digamma, trigamma)
- Incomplete gamma functions (regularized P and Q)
- Beta functions (beta, log-beta, regularized incomplete beta)
- Error functions (erf, erfc, inverse erf)
- Sigmoid functions (logistic, logit, probit)
- Log-sum-exp for numerical stability

### Tensor (`provisdom.math.tensor`)
N-dimensional array operations:
- Tensor creation, validation, and manipulation
- Element-wise operations with broadcasting (`emap`, `emap-kv`)
- Tensor arithmetic (add, subtract, multiply, divide)
- Norms (L1, L2, Lp) and normalization
- Inner products and numerical stability utilities

### Vector (`provisdom.math.vector`)
Specialized 1D tensor operations:
- Dot products, cross products, projections
- Kahan summation for improved numerical accuracy
- Softmax for converting vectors to probability distributions
- Probability vector validation and operations
- Vector filtering and manipulation

## Usage

```clojure
(require '[provisdom.math.core :as m])
(require '[provisdom.math.random :as random])
(require '[provisdom.math.matrix :as mx])
(require '[provisdom.math.linear-algebra :as la])
(require '[provisdom.math.special-functions :as special-fns])

;; Core math
(m/sqrt 2)           ;=> 1.4142135623730951
(m/roughly? 1.0 1.0000001 1e-6)  ;=> true
(m/div 1 0)          ;=> ##Inf (no exception)

;; Random generation with reproducibility
(random/bind-seed 42
  (random/rnd!))     ;=> deterministic random double

;; Matrix operations
(mx/mx* [[1 2] [3 4]] [[5 6] [7 8]])  ;=> [[19 22] [43 50]]
(mx/transpose [[1 2] [3 4]])          ;=> [[1 3] [2 4]]

;; Linear algebra
(la/determinant [[1 2] [3 4]])        ;=> -2.0
(la/inverse [[4 7] [2 6]])            ;=> [[0.6 -0.7] [-0.2 0.4]]
(la/solve [[2 1] [1 3]] [4 5])        ;=> [1.4 1.2]
(la/matrix-power [[1 2] [3 4]] 3)     ;=> [[37 54] [81 118]]

;; Special functions
(special-fns/gamma 5)      ;=> 24.0 (4!)
(special-fns/erf 1.0)      ;=> 0.8427007929497149
(special-fns/logistic 0)   ;=> 0.5
```

## Design Philosophy

- **Doubles by default**: Functions return doubles unless marked with `'` (apostrophe), which attempts to return longs when possible
- **NaN/Infinity aware**: Type predicates and comparisons handle IEEE 754 special values correctly
- **Spec-driven**: All functions have `clojure.spec.alpha` specs with generators for property-based testing
- **Anomalies over exceptions**: Prefer returning anomaly values rather than throwing exceptions
- **Immutable RNG**: Random number generators are immutable and splittable for parallel computation

## License

Copyright © 2018-2026 Provisdom Corp.

Distributed under the GNU Lesser General Public License version 3.0.
