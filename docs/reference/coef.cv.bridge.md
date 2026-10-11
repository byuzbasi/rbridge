# Extract coefficients from a 'cv.bridge' object

Extract coefficients from a 'cv.bridge' object.

## Usage

``` r
# S3 method for class 'cv.bridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  A 'cv.bridge' object.

- s:

  Exact fitted lambda value(s), or a named selector. CV objects use
  `lambda.min` or `lambda.1se`; prediction defaults to the former and
  coefficient extraction to the latter. Path objects return all fitted
  columns when `s` is omitted. For a path, `lambda.min` means the
  smallest fitted lambda; `lambda.1se` requires CV.

- ...:

  Additional arguments for compatibility.

## Value

A vector of coefficients

## See also

[`predict.cv.rbridge`](predict.cv.rbridge.md)

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
```
