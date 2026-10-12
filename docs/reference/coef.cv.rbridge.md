# Extract coefficients from a fitted cv.rbridge object

Extract original-scale intercept and slope coefficients from a fitted
cross-validated cv.rbridge object.

## Usage

``` r
# S3 method for class 'cv.rbridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  Fitted object returned by
  [`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md),
  with `p` predictors in its training design.

- s:

  Penalty selection, using numeric fitted values or a character name.
  Numeric values must match `object$lambda` exactly; no interpolation is
  used. The character selectors are:

  `"lambda.min"`

  :   Select the penalty with minimum CV error.

  `"lambda.1se"`

  :   Select using the historical CV error bars.

  The default is `lambda.1se`.

- ...:

  Additional arguments accepted for compatibility.

## Value

Numeric matrix with `p + 1` rows and one column per selected penalty,
even when only one penalty is selected. The first row is `(Intercept)`;
subsequent rows are original-scale slopes in the training predictor
order.

## Details

The returned matrix retains its dimensions for a single selected
penalty. The defaults differ between
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html): coefficient
extraction uses `lambda.1se`, whereas response prediction uses
`lambda.min`. Specify the same `s` in both calls when comparing
coefficients and their predictions. Returned slopes are on the original
predictor scale; the linear restrictions concern standardized slopes.
See [`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md)
for converting scales before checking restriction residuals.

## See also

[`predict.cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/predict.cv.rbridge.md),
[`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md)

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

# Five penalties keep this help example fast; the default is 100.
model1 <- cv.rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5,
                     nfolds = 3, num_threads = 1)
# A single selected penalty still gives a coefficient matrix.
coefficients <- coef(model1, s = "lambda.min")
coefficients
#>             802.149726533951
#> (Intercept)       0.01283347
#> V1                2.98320054
#> V2                1.57870923
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.99412306
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000
dim(coefficients)
#> [1] 9 1
# The default uses the historical error-bar selector.
coef(model1)
#>             802.149726533951
#> (Intercept)       0.01283347
#> V1                2.98320054
#> V2                1.57870923
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.99412306
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000
```
