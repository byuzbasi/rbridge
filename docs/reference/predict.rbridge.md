# Predict from a fitted rbridge object

Compute response predictions, extract coefficients, or identify nonzero
predictors from a fitted rbridge object.

## Usage

``` r
# S3 method for class 'rbridge'
predict(
  object,
  newx,
  s = c("lambda.min", "lambda.1se"),
  type = c("response", "nonzero", "coefficients"),
  ...
)
```

## Arguments

- object:

  Fitted object returned by
  [`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md),
  with `p` predictors in its training design.

- newx:

  Numeric matrix with one row per new observation and the same `p`
  columns, in the same order, as the training design. A numeric vector
  of length `p` represents one observation. Values are supplied on the
  original predictor scale. Required only for `type = "response"`.

- s:

  Numeric fitted penalty value(s). Values must match `object$lambda`
  exactly; no interpolation is used. Omitting this argument returns all
  fitted penalties.

  The character selector `lambda.min` requests the smallest fitted
  penalty. Use a CV object for the selector `lambda.1se`.

- type:

  Output to return: `"response"` for fitted response values,
  `"coefficients"` for intercept and slopes, or `"nonzero"` for
  predictor indices over the entire fitted path.

- ...:

  Additional arguments accepted for compatibility.

## Value

For `type = "response"`, a numeric vector for one selected penalty or a
matrix with one row per new observation and one column per selected
penalty. For `type = "coefficients"`, a numeric matrix with `p + 1`
rows, including the intercept, even for one penalty. For
`type = "nonzero"`, a pathwise collection of 1-based predictor indices
excluding the intercept. Its historical container can simplify depending
on the support pattern and number of predictors.

## Details

Prediction uses the fitted intercept and original-scale slopes. Columns
of `newx` are matched by position, not reordered by their names. For
`type = "nonzero"`, all fitted penalties are inspected and `s` does not
restrict the returned indices. Returned slopes are on the original
predictor scale; the linear restrictions concern standardized slopes.
See [`rbridge`](https://byuzbasi.github.io/rbridge/reference/rbridge.md)
for converting scales before checking restriction residuals.

## See also

[`coef.rbridge`](https://byuzbasi.github.io/rbridge/reference/coef.rbridge.md),
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
# Predict five observations at one explicitly selected penalty.
predict(model1, newx = X[1:5, , drop = FALSE], s = model1$lambda[2])
#> [1]  -1.6230925   0.3471289  -1.5657036   2.6335804 -10.4459031
# Extract coefficients using the complete type name.
predict(model1, s = model1$lambda[2], type = "coefficients")
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
# Predictor indices for every fitted penalty; the intercept is excluded.
predict(model1, type = "nonzero")
#> $`802.149726533951`
#> [1] 1 2 5
#> 
#> $`26.8035044402758`
#> [1] 1 2 3 4 5 6 7
#> 
#> $`0.895628118436414`
#> [1] 1 2 3 4 5 6 7 8
#> 
#> $`0.0299270466039737`
#> [1] 1 2 3 4 5 6 7 8
#> 
#> $`0.001`
#> [1] 1 2 3 4 5 6 7 8
#> 
```
