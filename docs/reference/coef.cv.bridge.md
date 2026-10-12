# Extract coefficients from a fitted cv.bridge object

Extract original-scale intercept and slope coefficients from a fitted
cross-validated cv.bridge object.

## Usage

``` r
# S3 method for class 'cv.bridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  Fitted object returned by
  [`cv.bridge`](https://byuzbasi.github.io/rbridge/reference/cv.bridge.md),
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
coefficients and their predictions.

## See also

[`predict.cv.bridge`](https://byuzbasi.github.io/rbridge/reference/predict.cv.bridge.md),
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
# A single selected penalty still gives a coefficient matrix.
coefficients <- coef(model1, s = "lambda.min")
coefficients
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
dim(coefficients)
#> [1] 9 1
# The default uses the historical error-bar selector.
coef(model1)
#>             0.895628118436414
#> (Intercept)        0.02127596
#> V1                 2.96822233
#> V2                 1.56711565
#> V3                 0.06806783
#> V4                -0.08208236
#> V5                 1.98231627
#> V6                -0.09139546
#> V7                 0.06236402
#> V8                -0.01928428
```
