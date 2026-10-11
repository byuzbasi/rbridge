# Make predictions from a 'cv.bridge' object

Makes predictions from a cross-validated 'cv.bridge' model

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

  A 'cv.bridge' object.

- newx:

  Matrix of new values for x at which predictions are to be made.

- s:

  Exact fitted lambda value(s), or a named selector. CV objects use
  `lambda.min` or `lambda.1se`; prediction defaults to the former and
  coefficient extraction to the latter. Path objects return all fitted
  columns when `s` is omitted. For a path, `lambda.min` means the
  smallest fitted lambda; `lambda.1se` requires CV.

- type:

  One of "response", "nonzero" or "coefficients". The "response" is for
  predicted values, the "nonzero" is for exacting non-zero coefficients
  and the "coefficients" is for the estimated coefficients.

- ...:

  Additional arguments for compatibility.

## Value

Response predictions (a vector for one lambda, a matrix for several), a
coefficient matrix, or nonzero indices over the entire path when
`type = "nonzero"`.

## See also

[`coef.cv.bridge`](coef.cv.bridge.md)

## Author

Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz  
Maintainer: Bahadir Yuzbasi <b.yzb@hotmail.com>

## Examples

``` r
set.seed(2019) 
beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)

n = 100
X = matrix(rnorm(n*p),n,p)
y = X%*%beta + rnorm(n) 

model1 <- cv.bridge(X, y, q = 1, nlambda = 5, nfolds = 3, num_threads = 1)
coef(model1,s='lambda.min')
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
predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -1.4546979   0.3068586  -1.5652478   2.6580196 -10.4100022
predict(model1, s="lambda.min",type="coefficient")
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
```
