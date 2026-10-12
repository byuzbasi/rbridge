# Fit a bridge coefficient path

Fit a bridge-penalized least-squares path using local quadratic
approximation.

## Usage

``` r
bridge(
  X,
  y,
  q = 1,
  lambda.min = ifelse(n > p, 0.001, 0.05),
  nlambda = 100,
  lambda,
  eta = 1e-07,
  converge = 10^10
)
```

## Arguments

- X:

  Numeric design matrix with `n` observation rows and `p` predictor
  columns. Supply finite values and predictors with nonzero variation;
  omit an intercept column. A data frame is converted using
  `model.matrix(~0+., X)`; restrictions and future predictions must use
  the resulting predictor order.

- y:

  Numeric response vector of length `n`, containing finite values.

- q:

  Bridge-penalty exponent, a finite number in `(0, 2]`. Values 1 and 2
  specify the L1 and L2 penalty forms. Estimation uses local quadratic
  approximation (LQA).

- lambda.min:

  Positive absolute endpoint of the automatically generated penalty
  grid. The default is `0.001` when `n > p`, and `0.05` otherwise. This
  argument is a penalty value, not a ratio. In CV results, the field
  also named `lambda.min` instead denotes the selected penalty.

- nlambda:

  Number of automatically generated penalty values, an integer of at
  least 2. The default is `100`; ignored when `lambda` is supplied.

- lambda:

  Optional nonempty numeric vector of finite, nonnegative penalty
  values. Its order is retained. When supplied, it determines the fitted
  path and overrides `nlambda` and the grid argument `lambda.min`.

- eta:

  Positive numerical tolerance, with default `1e-7`. Used for
  coefficient elimination and stopping on squared coefficient changes.

- converge:

  Nonnegative initial convergence-distance value, retained for
  compatibility. The default is `1e10`. Iteration stops when the updated
  squared coefficient change is at most `eta`, or at the historical cap.

## Value

A list of class `bridge` with components:

- betas:

  Numeric coefficient matrix with `p + 1` rows and `length(lambda)`
  columns, one column per fitted penalty. The first row is
  `(Intercept)`; the remaining rows are original-scale slopes in
  predictor order. Column names identify the fitted penalties.

- lambda:

  Numeric vector of penalties, in fitted column order.

## Details

Predictors are centered and divided by their centered root-mean-square
(RMS) deviations, using the observation count `n` as the divisor. The
response is centered. The intercept is reconstructed after fitting, and
returned coefficients are expressed on the original predictor scale. The
generated penalties are logarithmically spaced between an internally
computed starting value and the absolute endpoint `lambda.min`.

Let \\Z\\ be the \\n\\ by \\p\\ centered, RMS-standardized design,
\\y_c\\ the centered response vector of length \\n\\, and \\b\\ the
\\p\\-dimensional standardized slope vector. For exponent \\q \in
(0,2\]\\ and penalty weight \\\lambda \ge 0\\, Equation (1) defines the
penalized least-squares target: \$\$\min\_{b}
\left\\\\y_c-Zb\\\_2^2+\lambda\sum\_{j=1}^{p}\|b_j\|^q\right\\. \qquad
(1)\$\$ In Equation (1), \\\\\cdot\\\_2^2\\ is the squared Euclidean
norm, so its first term is the unnormalized residual sum of squares. The
index \\j\\ runs over the predictors; the second term is the bridge
penalty, with \\\lambda\\ controlling its weight relative to residual
error.

The fitting routine applies the article's LQA updates, coefficient
elimination and stopping rules. The criterion is nonconvex when \\q \<
1\\, so the iterative result is not a guarantee of a global minimum. The
getting-started vignette explains the penalty geometry and the fixed-LQA
restriction correction.

## References

Yuzbasi, B., Arashi, M. and Akdeniz, F. (2021). Penalized regression via
the restricted bridge estimator. Soft Computing, 25, 8401–8416.
[doi:10.1007/s00500-021-05763-9](https://doi.org/10.1007/s00500-021-05763-9)
.

## See also

[`cv.bridge`](https://byuzbasi.github.io/rbridge/reference/cv.bridge.md),
[`coef.bridge`](https://byuzbasi.github.io/rbridge/reference/coef.bridge.md),
[`predict.bridge`](https://byuzbasi.github.io/rbridge/reference/predict.bridge.md)

## Author

Bahadir Yuzbasi  
Maintainer: Bahadir Yuzbasi <b.yzb@hotmail.com>

## Examples

``` r
# Reproducible synthetic demonstration: 100 observations, eight predictors.
set.seed(2019)
beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)
n <- 100
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- as.numeric(X %*% beta + rnorm(n))

# Five penalties keep this help example fast; the default is 100.
# L1 and L2 penalty forms on the same observations.
model1 <- bridge(X, y, q = 1, nlambda = 5)
model2 <- bridge(X, y, q = 2, nlambda = 5)
dim(model1$betas)
#> [1] 9 5
# Extract one fitted column and predict five observations.
selected <- model1$lambda[2]
coef(model1, s = selected)
#>             26.8035044402758
#> (Intercept)    -3.121540e-02
#> V1              2.848999e+00
#> V2              1.433841e+00
#> V3              1.282147e-04
#> V4             -1.150407e-05
#> V5              1.879705e+00
#> V6             -1.064341e-04
#> V7              8.497907e-08
#> V8             -1.609602e-05
predict(model1, newx = X[1:5, , drop = FALSE], s = selected)
#> [1] -1.5002196  0.2180873 -1.5417858  2.4731030 -9.8607011
```
