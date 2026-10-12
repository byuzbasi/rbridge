# Fit a restricted bridge coefficient path

Fit a bridge-penalized least-squares path subject to user-supplied
linear restrictions on standardized slopes, using local quadratic
approximation.

## Usage

``` r
rbridge(
  X,
  y,
  q = 1,
  R,
  r,
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

- R:

  Finite numeric matrix with `m` linearly independent restriction rows
  and `p` predictor columns. Each row specifies a linear combination of
  the standardized predictor coefficients, in the order of `X`. The
  intercept is excluded.

- r:

  Finite numeric target vector of length `m`, or an `m` by 1 matrix. The
  targets have the units of the response when `R` is dimensionless. See
  Details for the coefficient scale and a residual check.

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

A list of class `rbridge` with components:

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
Restrictions act on standardized slopes. To check a fitted
original-scale slope vector, multiply it elementwise by the centered RMS
deviations of the predictors before applying `R`. The resulting
restriction residual is the fitted linear combination minus `r`; it
should be examined with a numerical tolerance, particularly after
coefficient thresholding. For example, a zero restriction on one slope
has the same meaning on either scale, whereas a nonzero sum or contrast
depends on the scales.

Let \\Z\\ be the \\n\\ by \\p\\ centered, RMS-standardized design,
\\y_c\\ the centered response vector of length \\n\\, and \\b\\ the
\\p\\-dimensional standardized slope vector. For exponent \\q \in
(0,2\]\\ and penalty weight \\\lambda \ge 0\\, Equation (1) defines the
penalized least-squares target: \$\$\min\_{b:\\Rb=r}
\left\\\\y_c-Zb\\\_2^2+\lambda\sum\_{j=1}^{p}\|b_j\|^q\right\\. \qquad
(1)\$\$ In Equation (1), \\\\\cdot\\\_2^2\\ is the squared Euclidean
norm, so its first term is the unnormalized residual sum of squares. The
index \\j\\ runs over the predictors; the second term is the bridge
penalty, with \\\lambda\\ controlling its weight relative to residual
error. Here \\R\\ is an \\m\\ by \\p\\ matrix with independent rows and
\\r\\ is the \\m\\-dimensional target vector. The condition \\Rb=r\\
restricts the standardized slopes and excludes the intercept. The
fitting routine applies the article's LQA updates, coefficient
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

[`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md),
[`coef.rbridge`](https://byuzbasi.github.io/rbridge/reference/coef.rbridge.md),
[`predict.rbridge`](https://byuzbasi.github.io/rbridge/reference/predict.rbridge.md)

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
# A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
# This target is illustrative; it is not asserted to equal the generating sum.
R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
r1.vec <- matrix(6.5, nrow = 1, ncol = 1)
# A second standardized-slope contrast: -b[1] + b[2] + b[5] = 0.5.
R2.mat <- matrix(c(-1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
r2.vec <- matrix(0.5, nrow = 1, ncol = 1)
# Impose both independent restrictions together.
R3.mat <- rbind(R1.mat, R2.mat)
r3.vec <- matrix(c(6.5, 0.5), ncol = 1)
# Alternatively, set the five inactive generating slopes to zero.
p.active <- which(beta != 0)
R4.mat <- diag(p)[-p.active, , drop = FALSE]
r4.vec <- matrix(rep(0, p - length(p.active)), ncol = 1)

# Five penalties keep this help example fast; the default is 100.
model1 <- rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5)
model2 <- rbridge(X, y, q = 1, R = R2.mat, r = r2.vec, nlambda = 5)
model3 <- rbridge(X, y, q = 1, R = R3.mat, r = r3.vec, nlambda = 5)
model4 <- rbridge(X, y, q = 1, R = R4.mat, r = r4.vec, nlambda = 5)
# Rows include the intercept; columns correspond to fitted penalties.
dim(model1$betas)
#> [1] 9 5
# Extract the second fitted penalty exactly; no interpolation is performed.
selected <- model1$lambda[2]
coefficients <- coef(model1, s = selected)
coefficients
#>             26.8035044402758
#> (Intercept)     1.288482e-02
#> V1              2.983157e+00
#> V2              1.579113e+00
#> V3              5.156258e-05
#> V4             -2.378501e-05
#> V5              1.993807e+00
#> V6             -3.666309e-04
#> V7              3.309060e-05
#> V8              0.000000e+00
predict(model1, newx = X[1:5, , drop = FALSE], s = selected)
#> [1]  -1.6230925   0.3471289  -1.5657036   2.6335804 -10.4459031
# Convert returned slopes to standardized slopes before checking the target.
x_centered <- sweep(X, 2, colMeans(X), "-")
x_rms <- sqrt(colMeans(x_centered^2))
standardized_slopes <- coefficients[-1, 1] * x_rms
as.numeric(R1.mat %*% standardized_slopes - r1.vec)
#> [1] 0
```
