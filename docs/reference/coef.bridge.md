# Extract coefficients from a fitted bridge object

Extract original-scale intercept and slope coefficients from a fitted
bridge object.

## Usage

``` r
# S3 method for class 'bridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  Fitted object returned by
  [`bridge`](https://byuzbasi.github.io/rbridge/reference/bridge.md),
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
penalty.

## See also

[`predict.bridge`](https://byuzbasi.github.io/rbridge/reference/predict.bridge.md),
[`bridge`](https://byuzbasi.github.io/rbridge/reference/bridge.md)

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
model1 <- bridge(X, y, q = 1, nlambda = 5)
# A single selected penalty still gives a coefficient matrix.
coefficients <- coef(model1, s = model1$lambda[2])
coefficients
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
dim(coefficients)
#> [1] 9 1
# Omitting s extracts the full fitted coefficient path.
dim(coef(model1))
#> [1] 9 5
```
