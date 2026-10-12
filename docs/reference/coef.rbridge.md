# Extract coefficients from a fitted rbridge object

Extract original-scale intercept and slope coefficients from a fitted
rbridge object.

## Usage

``` r
# S3 method for class 'rbridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  Fitted object returned by
  [`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md),
  with `p` predictors in its training design.

- s:

  Numeric fitted penalty value(s). Values must match `object$lambda`
  exactly; no interpolation is used. Omitting this argument returns all
  fitted penalties.

  The character selector `lambda.min` requests the smallest fitted
  penalty. Use a CV object for the selector `lambda.1se`.

- ...:

  Additional arguments accepted for compatibility.

## Value

Numeric matrix with `p + 1` rows and one column per selected penalty,
even when only one penalty is selected. The first row is `(Intercept)`;
subsequent rows are original-scale slopes in the training predictor
order.

## Details

The returned matrix retains its dimensions for a single selected
penalty. Returned slopes are on the original predictor scale; the linear
restrictions concern standardized slopes. See
[`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md) for
converting scales before checking restriction residuals.

## See also

[`predict.rbridge`](https://byuzbasi.github.io/rbridge/reference/predict.rbridge.md),
[`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md)

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
model1 <- rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5)
# A single selected penalty still gives a coefficient matrix.
coefficients <- coef(model1, s = model1$lambda[2])
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
dim(coefficients)
#> [1] 9 1
# Omitting s extracts the full fitted coefficient path.
dim(coef(model1))
#> [1] 9 5
```
