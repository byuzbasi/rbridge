# Fit a Restricted Bridge Estimation

Fit a restricted bridge-penalized least-squares path using local
quadratic approximation. Computes coefficients over the fitted penalty
grid using the historical thresholding and stopping rules.

## Usage

``` r
rbridge(
  X,
  y,
  q = 1,
  R,
  r,
  lambda.min = ifelse(n > p, 0.001, 0.05),
  nlambda = 100,
  lambda,
  eta = 1e-07,
  converge = 10^10
)
```

## Arguments

- X:

  Design matrix with n observation rows and p predictor columns.

- y:

  Numeric response vector with one value per observation.

- q:

  Bridge-penalty exponent in (0, 2\]. Values one and two give the L1 and
  L2 penalty forms; fitting uses local quadratic approximation.

- R:

  Numeric matrix with m restriction rows and p predictor columns. Each
  row defines one linear combination of standardized slopes; use
  linearly independent rows.

- r:

  Target vector or one-column matrix with m values. Writing beta for the
  p-dimensional standardized predictor-coefficient vector, the
  restriction residual is zero when all supplied linear restrictions
  hold: \$\$r - R\beta = 0.\$\$ This expression specifies the target
  coefficient subspace; the intercept is excluded. Standardized
  predictor coefficients have response units.

- lambda.min:

  The smallest value for lambda if `n>p` is `0.001` and `0.05`
  otherwise.

- nlambda:

  The number of lambda values - default is `100`

- lambda:

  User-supplied nonnegative penalty sequence; generated internally when
  omitted.

- eta:

  Positive threshold used for coefficient elimination and the squared
  coefficient-change stopping rule. The default is `1e-7`; it is
  unchanged from the historical solver.

- converge:

  Initial convergence-distance value, retained for compatibility. The
  default is `10^10`. The solver updates the squared Euclidean
  coefficient change and stops when it is at most `eta`, or after the
  historical iteration cap.

## Value

An object of class rbridge, a list with entries

- betas:

  Coefficients computed over the path of lambda

- lambda:

  The lambda values which is given at the function

## Details

Predictors are centered and divided by their root-mean-square centered
deviations. The restriction is imposed on the coefficients of these
standardized predictors, not on the original-scale coefficients returned
in `betas`. Here `R` has one column per predictor and one row per
restriction; `r` contains the corresponding target values. The
restriction rows must be linearly independent. The reconstructed
intercept is not included in the restrictions. Check restriction
residuals after accounting for predictor scales, especially when
thresholding occurs.

In order to couple the bridge estimator with the restriction
`R beta = r`, we solve the following optimization problem Here RSS
denotes the residual sum of squares for the centered response and
standardized design. The operator min denotes minimization over the
slope vector beta; the q-indexed expression denotes the bridge penalty
constraint. The following compact statement combines that penalty with
the supplied linear restrictions, and is implemented through local
quadratic approximation: \$\$\min RSS w.r.t \|\|\beta\|\|\_q and R\beta
= r. \$\$ The exponent q controls the penalty shape; n is the
observation count and p is the predictor count. See Section 3.1 of the
cited article for the computation. The expression describes the
estimator, not a guarantee of global convergence for every supplied
dataset.

## References

Yuzbasi, B., Arashi, M. and Akdeniz, F. (2021). Penalized regression via
the restricted bridge estimator. Soft Computing, 25, 8401–8416.
[doi:10.1007/s00500-021-05763-9](https://doi.org/10.1007/s00500-021-05763-9)
.

## See also

[`cv.rbridge`](cv.rbridge.md)

## Author

Bahadir Yuzbasi, Mohammad Arashi and Fikri Akdeniz  
Maintainer: Bahadir Yuzbasi <b.yzb@hotmail.com>

## Examples

``` r
set.seed(2019) 
beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)
p.active <- which(beta != 0)

### Restricted Matrix and vector
### Res 1
c1 <- c(1,1,0,0,1,0,0,0)
R1.mat <- matrix(c1,nrow = 1, ncol = p)
r1.vec <- as.matrix(c(6.5),1,1)
### Res 2
c2 <- c(-1,1,0,0,1,0,0,0)
R2.mat <- matrix(c2,nrow = 1, ncol = p)
r2.vec <- matrix(c(0.5),nrow = 1, ncol = 1)
### Res 3
R3.mat <- t(matrix(c(c1,c2),nrow = p, ncol = 2))
r3.vec <- matrix(c(6.5,0.5),nrow = 2, ncol = 1)
### Res 4
R4.mat = diag(1,p,p)[-p.active,]
r4.vec <- matrix(rep(0,p-length(p.active)),nrow = p-length(p.active), ncol = 1)

n = 100
X = matrix(rnorm(n*p),n,p)
y = X%*%beta + rnorm(n) 

######## Model 1 based on first restrictions
model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec, nlambda = 5)
print(model1)
#> $betas
#>             802.149726533951 26.8035044402758 0.895628118436414
#> (Intercept)       0.01283347     1.288482e-02        0.02613052
#> V1                2.98320054     2.983157e+00        2.98088443
#> V2                1.57870923     1.579113e+00        1.58117833
#> V3                0.00000000     5.156258e-05        0.06838693
#> V4                0.00000000    -2.378501e-05       -0.08380496
#> V5                1.99412306     1.993807e+00        1.99382037
#> V6                0.00000000    -3.666309e-04       -0.09347868
#> V7                0.00000000     3.309060e-05        0.06577367
#> V8                0.00000000     0.000000e+00       -0.01646600
#>             0.0299270466039737       0.001
#> (Intercept)         0.02750118  0.02754702
#> V1                  2.98095453  2.98095693
#> V2                  1.58221529  1.58225006
#> V3                  0.07468659  0.07489720
#> V4                 -0.08846393 -0.08861955
#> V5                  1.99286393  1.99283183
#> V6                 -0.09980300 -0.10001440
#> V7                  0.07347469  0.07373247
#> V8                 -0.02382978 -0.02407676
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> attr(,"class")
#> [1] "rbridge"

######## Model 2 based on second restrictions
model2 <- rbridge(X, y, q = 1, R2.mat, r2.vec, nlambda = 5)
print(model2)
#> $betas
#>             802.149726533951 26.8035044402758 0.895628118436414
#> (Intercept)     -0.604299044    -5.331852e-02       -0.01932371
#> V1               0.000000000     3.040784e+00        3.21128506
#> V2               0.001544417     1.290959e+00        1.37968739
#> V3               0.000000000     1.937483e-07        0.03804874
#> V4               0.000000000    -6.240756e-06       -0.09166177
#> V5               0.440031823     1.738332e+00        1.79685972
#> V6               0.000000000    -1.703000e-05       -0.08794552
#> V7               0.000000000     0.000000e+00        0.02778167
#> V8               0.000000000    -2.705950e-04       -0.05853606
#>             0.0299270466039737       0.001
#> (Intercept)        -0.01667782 -0.01658913
#> V1                  3.21755258  3.21776209
#> V2                  1.38386987  1.38400982
#> V3                  0.04418786  0.04439369
#> V4                 -0.09697099 -0.09714834
#> V5                  1.79821129  1.79825635
#> V6                 -0.09492979 -0.09516339
#> V7                  0.03631099  0.03659738
#> V8                 -0.06531316 -0.06554013
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> attr(,"class")
#> [1] "rbridge"

######## Model 3 based on third restrictions
model3 <- rbridge(X, y, q = 1, R3.mat, r3.vec, nlambda = 5)
print(model3)
#> $betas
#>             802.149726533951 26.8035044402758 0.895628118436414
#> (Intercept)       0.00145598     1.504089e-03       0.004867497
#> V1                3.32994823     3.329948e+00       3.329948235
#> V2                1.43778887     1.438289e+00       1.438659271
#> V3                0.00000000     0.000000e+00       0.035278492
#> V4                0.00000000    -7.452260e-05      -0.104069970
#> V5                1.84076374     1.840330e+00       1.840007722
#> V6                0.00000000    -4.777447e-04      -0.100546781
#> V7                0.00000000     1.075132e-07       0.043748531
#> V8                0.00000000    -9.811793e-07      -0.046910375
#>             0.0299270466039737        0.001
#> (Intercept)        0.006238754  0.006284733
#> V1                 3.329948235  3.329948235
#> V2                 1.439728643  1.439764461
#> V3                 0.041560364  0.041771054
#> V4                -0.108724504 -0.108879996
#> V5                 1.839078877  1.839047765
#> V6                -0.106866625 -0.107078002
#> V7                 0.051461989  0.051720176
#> V8                -0.054307284 -0.054554702
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> attr(,"class")
#> [1] "rbridge"

######## Model 4 based on fourth restrictions
model4 <- rbridge(X, y, q = 1, R4.mat, r4.vec, nlambda = 5)
print(model4)
#> $betas
#>             802.149726533951 26.8035044402758 0.895628118436414
#> (Intercept)    -6.412489e-01      -0.03123568       0.005358115
#> V1              1.232515e-03       2.84900284       2.960304124
#> V2              0.000000e+00       1.43381164       1.554345409
#> V3              0.000000e+00       0.00000000       0.000000000
#> V4              0.000000e+00       0.00000000       0.000000000
#> V5              3.132217e-05       1.87970617       1.974393656
#> V6              0.000000e+00       0.00000000       0.000000000
#> V7              0.000000e+00       0.00000000       0.000000000
#> V8              0.000000e+00       0.00000000       0.000000000
#>             0.0299270466039737       0.001
#> (Intercept)        0.006580821 0.006621677
#> V1                 2.964023247 2.964147519
#> V2                 1.558372653 1.558507223
#> V3                 0.000000000 0.000000000
#> V4                 0.000000000 0.000000000
#> V5                 1.977557539 1.977663258
#> V6                 0.000000000 0.000000000
#> V7                 0.000000000 0.000000000
#> V8                 0.000000000 0.000000000
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> attr(,"class")
#> [1] "rbridge"
```
