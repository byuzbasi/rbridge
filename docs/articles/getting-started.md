# Getting started with rbridge

`rbridge` estimates a bridge regression coefficient path, optionally
subject to linear restrictions. The methods are described by [Yuzbasi,
Arashi and Akdeniz (2021)](https://doi.org/10.1007/s00500-021-05763-9).
The examples below are small synthetic demonstrations; they are not
reported results from that article.

## Prepare the inputs

Use a finite numeric matrix `X` with one observation per row and one
predictor per column, together with a matching response vector `y`.
Constant predictors are rejected. Any treatment of missing values
belongs in your documented data preparation before fitting.

``` r

set.seed(2019)
X <- matrix(rnorm(40 * 4), 40, 4)
colnames(X) <- paste0("x", 1:4)
y <- as.numeric(1 + X %*% c(1, 0.5, -0.2, 0) + rnorm(40))
lambda <- c(5, 2, 1)
```

## Fit and inspect a path

The penalty exponent `q` lies in `(0, 2]`. Values one and two give the
L1 and L2 penalty forms. All supported exponents use the historical
local quadratic approximation and thresholding algorithm. In particular,
the package does not certify a global solution for a nonconvex penalty.

``` r

path <- bridge(X, y, q = 2, lambda = lambda)
knitr::kable(coef(path), digits = 3, caption = "Demonstration coefficient path")
```

|             |      5 |      2 |      1 |
|:------------|-------:|-------:|-------:|
| (Intercept) |  0.831 |  0.835 |  0.837 |
| x1          |  0.869 |  0.935 |  0.960 |
| x2          |  0.468 |  0.505 |  0.519 |
| x3          |  0.167 |  0.159 |  0.155 |
| x4          | -0.032 | -0.043 | -0.047 |

Demonstration coefficient path

``` r

predict(path, newx = X[1:3, , drop = FALSE], s = 2)
#> [1]  0.4599273  1.1796233 -0.3368181
```

`coef(path)` returns the complete path. A numeric selector chooses an
exact fitted penalty; it does not interpolate between penalties.
Returned slopes and the reconstructed intercept are on the original
predictor scale.

## State a restriction on the correct scale

Restrictions apply to **standardized predictor slopes**, after centering
each predictor and dividing by its root-mean-square centered deviation.
The intercept is excluded. Here the fourth standardized slope is
restricted to zero.

``` r

R <- matrix(c(0, 0, 0, 1), nrow = 1)
r <- matrix(0, nrow = 1)
restricted <- rbridge(X, y, q = 2, R = R, r = r, lambda = lambda)
scale <- sqrt(colMeans(sweep(X, 2, colMeans(X), "-")^2))
standardized_slopes <- restricted$betas[-1, , drop = FALSE] * scale
max(abs(sweep(R %*% standardized_slopes, 1, as.numeric(r), "-")))
#> [1] 0
```

This residual checks the supplied restriction on its fitting scale.
Applying the same matrix directly to returned slopes generally describes
a different restriction when predictor scales differ. Thresholding can
also affect the residual, so this check is not a convergence
certificate. Restriction rows must be linearly independent; unresolved
linear systems fail explicitly.

## Select a penalty with cross-validation

Use 100 logarithmically spaced penalties over the same 1-to-5 range as
the short path above. The table displays five representative rows; the
plot displays the complete CV path.

``` r

lambda_cv <- exp(seq(log(5), log(1), length.out = 100))
cv <- cv.rbridge(X, y, q = 2, R = R, r = r, lambda = lambda_cv,
                 nfolds = 4, num_threads = 1)
rows <- unique(round(seq(1, length(cv$lambda), length.out = 5)))
knitr::kable(data.frame(lambda = cv$lambda, error = cv$cve,
                       historical_error_bar = cv$cvse)[rows, ], digits = 3,
             row.names = FALSE,
             caption = "Five representative rows from the 100-penalty CV path")
```

| lambda | error | historical_error_bar |
|-------:|------:|---------------------:|
|  5.000 | 0.889 |                0.107 |
|  3.330 | 0.873 |                0.105 |
|  2.254 | 0.866 |                0.104 |
|  1.501 | 0.863 |                0.104 |
|  1.000 | 0.862 |                0.104 |

Five representative rows from the 100-penalty CV path

``` r

coef(cv, s = "lambda.min")
#>                     1
#> (Intercept) 0.8342982
#> x1          0.9548408
#> x2          0.5148605
#> x3          0.1510886
#> x4          0.0000000
predict(cv, newx = X[1:3, , drop = FALSE])
#> [1]  0.4900825  1.2203342 -0.4896711
```

CV retains the original contiguous folds, global preprocessing and
historical error-bar calculation. Prediction defaults to `lambda.min`;
coefficient extraction defaults to `lambda.1se`. Record your chosen
selector when reporting results. The method returns its fitted object;
call [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to view
it.

``` r

par(mar = c(4.2, 4.2, 1.2, 1), col.axis = "#152b3d", col.lab = "#152b3d",
    bg = "white")
plot(cv)
```

![Synthetic demonstration: historical CV errors across 100
logarithmically spaced penalties from 1 to
5.](getting-started_files/figure-html/cv-plot-1.png)

Synthetic demonstration: historical CV errors across 100 logarithmically
spaced penalties from 1 to 5.

## Check the build and save provenance

``` r

rbridge_info()
#> $version
#> [1] "1.0.3"
#> 
#> $openmp
#> [1] TRUE
#> 
#> $openmp_version
#> [1] 202011
```

OpenMP support is a property of the installed build. Unsupported
toolchains run CV sequentially. Supported builds use at most the
requested number of workers or the number of folds, whichever is
smaller. Set BLAS threads to one before starting R when using fold-level
parallelism.

Record the package version,
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html), seed,
penalty grid, input definitions and preprocessing with each analysis.
The solver retains its historical stopping rule and iteration cap and
does not report a convergence certificate. The [reproducibility
guide](reproducibility.md) details the validation scope and the
materials needed for a complete reproduction of the published study.
