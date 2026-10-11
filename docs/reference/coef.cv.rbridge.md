# Extract coefficients from a 'cv.rbridge' object

Extract coefficients from a 'cv.rbridge' object.

## Usage

``` r
# S3 method for class 'cv.rbridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  A 'cv.rbridge' object.

- s:

  Exact fitted lambda value(s), or a named selector. CV objects use
  `lambda.min` or `lambda.1se`; prediction defaults to the former and
  coefficient extraction to the latter. Path objects return all fitted
  columns when `s` is omitted. For a path, `lambda.min` means the
  smallest fitted lambda; `lambda.1se` requires CV.

- ...:

  Additional arguments for compatibility.

## Value

A matrix of coefficients including the intercept.

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

### Restricted Matrix and vector
c1 <- c(1,1,0,0,1,0,0,0)
R1.mat <- matrix(c1,nrow = 1, ncol = p)
r1.vec <- as.matrix(c(6.5),1,1)

n = 100
X = matrix(rnorm(n*p),n,p)
y = X%*%beta + rnorm(n) 

######## Model 1 based on first restrictions
model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec, nlambda = 5, nfolds = 3, num_threads = 1)
coef(model1,s='lambda.min')
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
