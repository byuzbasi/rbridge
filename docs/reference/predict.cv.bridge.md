# Predict from a fitted cv.bridge object

Compute response predictions, extract coefficients, or identify nonzero
predictors from a fitted cv.bridge object.

## Usage

``` r
# S3 method for class 'cv.bridge'
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
  [`cv.bridge`](https://byuzbasi.github.io/rbridge/reference/cv.bridge.md),
  with `p` predictors in its training design.

- newx:

  Numeric matrix with one row per new observation and the same `p`
  columns, in the same order, as the training design. A numeric vector
  of length `p` represents one observation. Values are supplied on the
  original predictor scale. Required only for `type = "response"`.

- s:

  Penalty selection, using numeric fitted values or a character name.
  Numeric values must match `object$lambda` exactly; no interpolation is
  used. The character selectors are:

  `"lambda.min"`

  :   Select the penalty with minimum CV error.

  `"lambda.1se"`

  :   Select using the historical CV error bars.

  The default is `lambda.min`.

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
restrict the returned indices. The defaults differ between
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html): coefficient
extraction uses `lambda.1se`, whereas response prediction uses
`lambda.min`. Specify the same `s` in both calls when comparing
coefficients and their predictions.

## See also

[`coef.cv.bridge`](https://byuzbasi.github.io/rbridge/reference/coef.cv.bridge.md),
[`cv.bridge`](https://byuzbasi.github.io/rbridge/reference/cv.bridge.md)

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
model1 <- cv.bridge(X, y, q = 1, nlambda = 5,
                     nfolds = 3, num_threads = 1)
# Predict five observations at one explicitly selected penalty.
predict(model1, newx = X[1:5, , drop = FALSE], s = "lambda.min")
#> [1]  -1.4546979   0.3068586  -1.5652478   2.6580196 -10.4100022
# Extract coefficients using the complete type name.
predict(model1, s = "lambda.min", type = "coefficients")
#>                   0.001
#> (Intercept)  0.02436200
#> V1           2.97264933
#> V2           1.57302384
#> V3           0.07468856
#> V4          -0.08748886
#> V5           1.98528242
#> V6          -0.09864804
#> V7           0.07149838
#> V8          -0.02593340
# Predictor indices for every fitted penalty; the intercept is excluded.
predict(model1, type = "nonzero")
#> $`802.149726533951`
#> [1] 1 5
#> 
#> $`26.8035044402758`
#> [1] 1 2 3 4 5 6 7 8
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
