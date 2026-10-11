# Extract coefficients from a 'rbridge' object

Makes predictions from a cross-validated 'rbridge' model

## Usage

``` r
# S3 method for class 'rbridge'
coef(object, s = c("lambda.1se", "lambda.min"), ...)
```

## Arguments

- object:

  A 'rbridge' object.

- s:

  Exact fitted lambda value(s), or a named selector. CV objects use
  `lambda.min` or `lambda.1se`; prediction defaults to the former and
  coefficient extraction to the latter. Path objects return all fitted
  columns when `s` is omitted. For a path, `lambda.min` means the
  smallest fitted lambda; `lambda.1se` requires CV.

- ...:

  Additional arguments for compatibility.

## Value

Response predictions (a vector for one lambda, a matrix for several), a
coefficient matrix, or nonzero indices over the entire path when
`type = "nonzero"`.

## See also

[`predict.rbridge`](predict.rbridge.md)

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
model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec, nlambda = 5)
coef(model1,s='lambda.min')
#>                   0.001
#> (Intercept)  0.02754702
#> V1           2.98095693
#> V2           1.58225006
#> V3           0.07489720
#> V4          -0.08861955
#> V5           1.99283183
#> V6          -0.10001440
#> V7           0.07373247
#> V8          -0.02407676
```
