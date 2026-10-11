# Cross-validation for rbridge

Performs cross-validation for restricted bridge models and returns the
fitted path and selected lambda values. Use plot() to display the
result.

## Usage

``` r
cv.rbridge(
  X,
  y,
  q,
  R,
  r,
  lambda,
  nfolds = 10,
  lambda.min = ifelse(n > p, 0.001, 0.05),
  nlambda = 100,
  eta = 1e-07,
  converge = 10^10,
  num_threads = 10
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

- lambda:

  User-supplied penalty sequence; generated internally when omitted.

- nfolds:

  number of folds - default is 10.

- lambda.min:

  The smallest value for lambda if `n>p` is `0.001` and `0.05`
  otherwise.

- nlambda:

  The number of lambda values - default is `100`

- eta:

  Positive threshold used for coefficient elimination and the squared
  coefficient-change stopping rule. The default is `1e-7`; it is
  unchanged from the historical solver.

- converge:

  Initial convergence-distance value, retained for compatibility. The
  default is `10^10`. The solver updates the squared Euclidean
  coefficient change and stops when it is at most `eta`, or after the
  historical iteration cap.

- num_threads:

  Requested fold-level workers, capped at the number of folds. Builds
  without OpenMP run sequentially. Inspect support with
  [`rbridge_info()`](rbridge_info.md).

## Value

An object of class cv.rbridge, a list with entries

- cve:

  the prediction error aggregated over the validation folds.

- cvse:

  the historical CV error-bar calculation, retained for compatibility.

- cvup:

  upper curve = `cve+cvse`.

- cvlo:

  lower curve = `cve-cvse`.

- lambda:

  the values of `lambda` used in the fits

- nz:

  number of non-zero coefficients at each `lambda`.

- betas:

  estimated coefficient at each `lambda`.

- lambda.min:

  value of lambda that gives minimum `cve`

- lambda.1se:

  largest value of `lambda` such that error is within 1 standard error
  of the minimum

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

Uses global centering and scaling before allocating contiguous folds.
The historical CV error and error-bar calculations are retained.

## See also

[`rbridge`](rbridge.md)

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
model1 <- cv.rbridge(X, y, q = 1, R1.mat, r1.vec, nlambda = 5, nfolds = 3, num_threads = 1)
print(model1)
#> $cve
#> [1] 1.015159 1.015216 1.031148 1.030640 1.030639
#> 
#> $cvse
#> [1] 0.06811741 0.06812103 0.06926750 0.06924655 0.06924690
#> 
#> $cvup
#> [1] 1.083276 1.083337 1.100415 1.099886 1.099886
#> 
#> $cvlo
#> [1] 0.9470415 0.9470949 0.9618803 0.9613933 0.9613920
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> $nz
#>   802.149726533951   26.8035044402758  0.895628118436414 0.0299270466039737 
#>                  3                  7                  8                  8 
#>              0.001 
#>                  8 
#> 
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
#> $lambda.min
#> [1] 802.1497
#> 
#> $lambda.1se
#> [1] 802.1497
#> 
#> attr(,"class")
#> [1] "cv.rbridge"
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
coef(model1,s='lambda.1se')
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
predict(model1,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -1.6233986   0.3470059  -1.5648146   2.6330664 -10.4462475
predict(model1, s="lambda.min",type="coefficient")
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
predict(model1, s="lambda.1se",type="coefficient")
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

######## Model 2 based on second restrictions
model2 <- cv.rbridge(X, y, q = 1, R2.mat, r2.vec, nlambda = 5, nfolds = 3, num_threads = 1)
print(model2)
#> $cve
#> [1] 16.665345  1.297568  1.184489  1.184166  1.184170
#> 
#> $cvse
#> [1] 1.12472632 0.08713585 0.07940701 0.07939220 0.07939270
#> 
#> $cvup
#> [1] 17.790071  1.384703  1.263896  1.263558  1.263562
#> 
#> $cvlo
#> [1] 15.540619  1.210432  1.105082  1.104773  1.104777
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> $nz
#>   802.149726533951   26.8035044402758  0.895628118436414 0.0299270466039737 
#>                  2                  7                  8                  8 
#>              0.001 
#>                  8 
#> 
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
#> $lambda.min
#> [1] 0.02992705
#> 
#> $lambda.1se
#> [1] 0.8956281
#> 
#> attr(,"class")
#> [1] "cv.rbridge"
coef(model2,s='lambda.min')
#>             0.0299270466039737
#> (Intercept)        -0.01667782
#> V1                  3.21755258
#> V2                  1.38386987
#> V3                  0.04418786
#> V4                 -0.09697099
#> V5                  1.79821129
#> V6                 -0.09492979
#> V7                  0.03631099
#> V8                 -0.06531316
coef(model2,s='lambda.1se')
#>             0.895628118436414
#> (Intercept)       -0.01932371
#> V1                 3.21128506
#> V2                 1.37968739
#> V3                 0.03804874
#> V4                -0.09166177
#> V5                 1.79685972
#> V6                -0.08794552
#> V7                 0.02778167
#> V8                -0.05853606
predict(model2,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -0.89432445  -0.02218518  -2.36960745   2.88846616 -10.12064662
predict(model2, s="lambda.min",type="coefficient")
#>             0.0299270466039737
#> (Intercept)        -0.01667782
#> V1                  3.21755258
#> V2                  1.38386987
#> V3                  0.04418786
#> V4                 -0.09697099
#> V5                  1.79821129
#> V6                 -0.09492979
#> V7                  0.03631099
#> V8                 -0.06531316
predict(model2, s="lambda.1se",type="coefficient")
#>             0.895628118436414
#> (Intercept)       -0.01932371
#> V1                 3.21128506
#> V2                 1.37968739
#> V3                 0.03804874
#> V4                -0.09166177
#> V5                 1.79685972
#> V6                -0.08794552
#> V7                 0.02778167
#> V8                -0.05853606

######## Model 3 based on third restrictions
model3 <- cv.rbridge(X, y, q = 1, R3.mat, r3.vec, nlambda = 5, nfolds = 3, num_threads = 1)
print(model3)
#> $cve
#> [1] 1.118618 1.118638 1.150158 1.153818 1.153956
#> 
#> $cvse
#> [1] 0.07507514 0.07507670 0.07719971 0.07744604 0.07745528
#> 
#> $cvup
#> [1] 1.193693 1.193715 1.227358 1.231264 1.231411
#> 
#> $cvlo
#> [1] 1.043542 1.043561 1.072959 1.076372 1.076500
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> $nz
#>   802.149726533951   26.8035044402758  0.895628118436414 0.0299270466039737 
#>                  3                  7                  8                  8 
#>              0.001 
#>                  8 
#> 
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
#> $lambda.min
#> [1] 802.1497
#> 
#> $lambda.1se
#> [1] 802.1497
#> 
#> attr(,"class")
#> [1] "cv.rbridge"
coef(model3,s='lambda.min')
#>             802.149726533951
#> (Intercept)       0.00145598
#> V1                3.32994823
#> V2                1.43778887
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.84076374
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000
coef(model3,s='lambda.1se')
#>             802.149726533951
#> (Intercept)       0.00145598
#> V1                3.32994823
#> V2                1.43778887
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.84076374
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000
predict(model3,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -1.066938205  -0.003399281  -2.386553460   2.948182193 -10.356157685
predict(model3, s="lambda.min",type="coefficient")
#>             802.149726533951
#> (Intercept)       0.00145598
#> V1                3.32994823
#> V2                1.43778887
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.84076374
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000
predict(model3, s="lambda.1se",type="coefficient")
#>             802.149726533951
#> (Intercept)       0.00145598
#> V1                3.32994823
#> V2                1.43778887
#> V3                0.00000000
#> V4                0.00000000
#> V5                1.84076374
#> V6                0.00000000
#> V7                0.00000000
#> V8                0.00000000

######## Model 4 based on fourth restrictions
model4 <- cv.rbridge(X, y, q = 1, R4.mat, r4.vec, nlambda = 5, nfolds = 3, num_threads = 1)
print(model4)
#> $cve
#> [1] 18.929730  1.220778  1.043423  1.041512  1.041453
#> 
#> $cvse
#> [1] 1.27629963 0.08199606 0.06996136 0.06983744 0.06983360
#> 
#> $cvup
#> [1] 20.206030  1.302774  1.113384  1.111350  1.111287
#> 
#> $cvlo
#> [1] 17.6534305  1.1387821  0.9734613  0.9716749  0.9716193
#> 
#> $lambda
#>              [,1]
#> [1,] 802.14972653
#> [2,]  26.80350444
#> [3,]   0.89562812
#> [4,]   0.02992705
#> [5,]   0.00100000
#> 
#> $nz
#>   802.149726533951   26.8035044402758  0.895628118436414 0.0299270466039737 
#>                  2                  3                  3                  3 
#>              0.001 
#>                  3 
#> 
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
#> $lambda.min
#> [1] 0.001
#> 
#> $lambda.1se
#> [1] 0.8956281
#> 
#> attr(,"class")
#> [1] "cv.rbridge"
coef(model4,s='lambda.min')
#>                   0.001
#> (Intercept) 0.006621677
#> V1          2.964147519
#> V2          1.558507223
#> V3          0.000000000
#> V4          0.000000000
#> V5          1.977663258
#> V6          0.000000000
#> V7          0.000000000
#> V8          0.000000000
coef(model4,s='lambda.1se')
#>             0.895628118436414
#> (Intercept)       0.005358115
#> V1                2.960304124
#> V2                1.554345409
#> V3                0.000000000
#> V4                0.000000000
#> V5                1.974393656
#> V6                0.000000000
#> V7                0.000000000
#> V8                0.000000000
predict(model4,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -1.6059306   0.3290159  -1.5622392   2.6104065 -10.3631199
predict(model4, s="lambda.min",type="coefficient")
#>                   0.001
#> (Intercept) 0.006621677
#> V1          2.964147519
#> V2          1.558507223
#> V3          0.000000000
#> V4          0.000000000
#> V5          1.977663258
#> V6          0.000000000
#> V7          0.000000000
#> V8          0.000000000
predict(model4, s="lambda.1se",type="coefficient")
#>             0.895628118436414
#> (Intercept)       0.005358115
#> V1                2.960304124
#> V2                1.554345409
#> V3                0.000000000
#> V4                0.000000000
#> V5                1.974393656
#> V6                0.000000000
#> V7                0.000000000
#> V8                0.000000000

```
