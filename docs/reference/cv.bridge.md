# Cross-validation for bridge

Compute historical contiguous-fold cross-validation and select fitted
penalties. Call [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
on the returned object to display the error curve.

## Usage

``` r
cv.bridge(
  X,
  y,
  q,
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

An object of class cv.bridge, a list with entries

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

Uses global centering and scaling before allocating contiguous folds.
The historical CV error and error-bar calculations are retained.

## See also

[`bridge`](bridge.md)

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

######## Model 1 
model1 <- cv.bridge(X, y, q = 1, nlambda = 5, nfolds = 3, num_threads = 1)
print(model1)
#> $cve
#> [1] 18.929731  1.221062  1.075844  1.072250  1.072142
#> 
#> $cvse
#> [1] 1.27629971 0.08201570 0.07216068 0.07193574 0.07192906
#> 
#> $cvup
#> [1] 20.206031  1.303077  1.148005  1.144186  1.144071
#> 
#> $cvlo
#> [1] 17.653432  1.139046  1.003683  1.000314  1.000213
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
#>                  2                  8                  8                  8 
#>              0.001 
#>                  8 
#> 
#> $betas
#>             802.149726533951 26.8035044402758 0.895628118436414
#> (Intercept)    -6.412489e-01    -3.121540e-02        0.02127596
#> V1              1.232328e-03     2.848999e+00        2.96822233
#> V2              0.000000e+00     1.433841e+00        1.56711565
#> V3              0.000000e+00     1.282147e-04        0.06806783
#> V4              0.000000e+00    -1.150407e-05       -0.08208236
#> V5              3.129117e-05     1.879705e+00        1.98231627
#> V6              0.000000e+00    -1.064341e-04       -0.09139546
#> V7              0.000000e+00     8.497907e-08        0.06236402
#> V8              0.000000e+00    -1.609602e-05       -0.01928428
#>             0.0299270466039737       0.001
#> (Intercept)         0.02426219  0.02436200
#> V1                  2.97250618  2.97264933
#> V2                  1.57283276  1.57302384
#> V3                  0.07447441  0.07468856
#> V4                 -0.08731409 -0.08748886
#> V5                  1.98518663  1.98528242
#> V6                 -0.09841349 -0.09864804
#> V7                  0.07120272  0.07149838
#> V8                 -0.02571779 -0.02593340
#> 
#> $lambda.min
#> [1] 0.001
#> 
#> $lambda.1se
#> [1] 0.8956281
#> 
#> attr(,"class")
#> [1] "cv.bridge"
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

######## Model 2 
model2 <- cv.bridge(X, y, q = 2, nlambda = 5, nfolds = 3, num_threads = 1)
print(model2)
#> $cve
#> [1] 18.862723 14.574809  1.300875  1.073597  1.072156
#> 
#> $cvse
#> [1] 1.27182455 0.98539539 0.08747346 0.07202327 0.07192999
#> 
#> $cvup
#> [1] 20.134547 15.560204  1.388349  1.145620  1.144086
#> 
#> $cvlo
#> [1] 17.590898 13.589413  1.213402  1.001574  1.000226
#> 
#> $lambda
#>              [,1]
#> [1,] 4.010749e+04
#> [2,] 5.039867e+02
#> [3,] 6.333047e+00
#> [4,] 7.958044e-02
#> [5,] 1.000000e-03
#> 
#> $nz
#>   X40107.4863266975   X503.986708518281   X6.33304715967736 X0.0795804445807973 
#>                   8                   8                   8                   8 
#>              X0.001 
#>                   8 
#> 
#> $betas
#>             40107.4863266975 503.986708518281 6.33304715967736
#> (Intercept)    -0.6395890057      -0.52600174      -0.01372693
#> V1              0.0088526374       0.56837767       2.81984333
#> V2              0.0045644036       0.29364959       1.48896890
#> V3              0.0003809337       0.02213786       0.07192869
#> V4              0.0008297924       0.04282305      -0.05936483
#> V5              0.0061107545       0.38751914       1.88142530
#> V6              0.0005318734       0.02634813      -0.07546794
#> V7             -0.0013956004      -0.06838175       0.04452534
#> V8             -0.0016262036      -0.08460553      -0.04921121
#>             0.0795804445807973       0.001
#> (Intercept)         0.02385767  0.02435906
#> V1                  2.97062924  2.97262881
#> V2                  1.57191349  1.57301640
#> V3                  0.07465778  0.07469548
#> V4                 -0.08709909 -0.08748992
#> V5                  1.98390242  1.98526833
#> V6                 -0.09833115 -0.09865206
#> V7                  0.07113750  0.07150393
#> V8                 -0.02626757 -0.02594497
#> 
#> $lambda.min
#> [1] 0.001
#> 
#> $lambda.1se
#> [1] 0.07958044
#> 
#> attr(,"class")
#> [1] "cv.bridge"
coef(model2,s='lambda.min')
#>                   0.001
#> (Intercept)  0.02435906
#> V1           2.97262881
#> V2           1.57301640
#> V3           0.07469548
#> V4          -0.08748992
#> V5           1.98526833
#> V6          -0.09865206
#> V7           0.07150393
#> V8          -0.02594497
predict(model2,newx=X[1:5,], s="lambda.min", type="response")
#> [1]  -1.4546709   0.3068607  -1.5652471   2.6579988 -10.4099477
predict(model2, s="lambda.min",type="coefficient")
#>                   0.001
#> (Intercept)  0.02435906
#> V1           2.97262881
#> V2           1.57301640
#> V3           0.07469548
#> V4          -0.08748992
#> V5           1.98526833
#> V6          -0.09865206
#> V7           0.07150393
#> V8          -0.02594497
```
