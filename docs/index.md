![rbridge: Restricted Bridge Estimation](reference/figures/banner.svg)

`rbridge` fits bridge-penalized linear models with optional linear
coefficient restrictions. The R interface provides coefficient paths,
cross-validation, predictions and coefficient extraction through
compiled RcppArmadillo solvers.

The methodology is described in [Yuzbasi, Arashi and Akdeniz (2021),
*Penalized regression via the restricted bridge
estimator*](https://doi.org/10.1007/s00500-021-05763-9). The banner is
decorative artwork.

## Install from source

The package is [currently archived on
CRAN](https://cran.r-project.org/package=rbridge). This source tree
contains the unreleased 1.0.3 maintenance update. It requires R, a
working C++ toolchain, and the Rcpp, RcppArmadillo and Matrix packages.

Run this command in the package source directory:

``` sh
R CMD INSTALL .
```

Alternatively, install the built source archive from R:

``` r

install.packages("rbridge_1.0.3.tar.gz", repos = NULL, type = "source")
```

After the 1.0.3 update has been published to the source repository, it
can also be installed with
`remotes::install_github("byuzbasi/rbridge")`. Until then, use the
prepared local source tree or archive for this candidate.

## A small working example

This is a demonstration with synthetic data, not a reproduction of the
paper’s reported numerical results. It uses the historical example seed
and 100 logarithmically spaced penalties between 5 and 1.

``` r

library(rbridge)
set.seed(2019)
X <- matrix(rnorm(40 * 4), 40, 4)
y <- as.numeric(1 + X %*% c(1, 0.5, -0.2, 0) + rnorm(40))

# Restrict the fourth standardized predictor coefficient to zero.
R <- matrix(c(0, 0, 0, 1), nrow = 1)
r <- matrix(0, nrow = 1)
lambda_cv <- exp(seq(log(5), log(1), length.out = 100))
fit <- cv.rbridge(
  X, y, q = 2, R = R, r = r,
  lambda = lambda_cv, nfolds = 4, num_threads = 1
)
coef(fit, s = "lambda.min")
predict(fit, newx = X[1:3, , drop = FALSE])
plot(fit)
```

## Functions

| Function | Purpose |
|----|----|
| [`bridge()`](reference/bridge.md) | Unrestricted bridge coefficient path |
| [`rbridge()`](reference/rbridge.md) | Bridge coefficient path with linear restrictions |
| [`cv.bridge()`](reference/cv.bridge.md) | Cross-validation for an unrestricted model |
| [`cv.rbridge()`](reference/cv.rbridge.md) | Cross-validation for a restricted model |
| [`coef()`](https://rdrr.io/r/stats/coef.html) / [`predict()`](https://rdrr.io/r/stats/predict.html) | Extract coefficients or predict from fitted models |
| [`plot()`](https://rdrr.io/r/graphics/plot.default.html) | Display cross-validation errors and selected penalties |
| [`rbridge_info()`](reference/rbridge_info.md) | Inspect compiled OpenMP support and package version |

The supported penalty exponent is positive and at most two. The exponent
controls the bridge penalty; one and two give the L1 and L2 penalty
forms. The solver uses local quadratic approximation and the historical
elimination and convergence thresholds.

## Coefficient scales and selectors

**Restrictions apply to standardized predictor coefficients.**
Predictors are centered and divided by their root-mean-square centered
deviations before fitting. Returned coefficients include a reconstructed
intercept and are on the original predictor scale. Consequently,
applying the same restriction matrix directly to returned slopes
generally tests a different restriction. The intercept is not part of
the restriction matrix.

For CV objects, prediction defaults to `lambda.min`; coefficient
extraction defaults to `lambda.1se`. For path objects, omitting `s`
returns the entire path. A numeric `s` selects exact values already
present in the fitted lambda grid; there is no interpolation. A path
object’s `s = "lambda.min"` selects the smallest fitted penalty, while
`lambda.1se` requires a CV object. Nonzero indices are reported over the
whole fitted path, preserving the historical behavior.

The historical contiguous fold allocation and CV-error aggregation are
retained. Use `num_threads = 1` for a sequential demonstration;
multithreading depends on the build’s OpenMP support. Inspect it with
[`rbridge_info()`](reference/rbridge_info.md). Supported builds use at
most one worker per fold; unsupported builds run sequentially. Set BLAS
threads to one before starting R when requesting parallel CV.

The maintenance update caches fixed and active-set cross-products,
reuses CV data slices and solves the restricted coefficient and
constraint systems together. These are implementation changes to the
historical estimator. Speed ratios depend on design size, penalty,
toolchain and hardware; no universal speedup is claimed. Singular or
numerically unresolved systems fail explicitly without substituting an
approximate fit.

## Measured performance

The completed benchmark compares the optimized engine with the first
repaired 1.0.3 candidate on four synthetic designs, three penalty
exponents and all four fitting interfaces. Each engine uses two warmups
and 15 timed fits per case. The speed ratio divides the baseline median
time by the optimized median time.

| Method | Median speed ratio across 12 cases | Measured range |
|----|---:|---:|
| [`bridge()`](reference/bridge.md) | 3.14x | 1.52-14.62x |
| [`rbridge()`](reference/rbridge.md) | 2.59x | 1.18-3.93x |
| [`cv.bridge()`](reference/cv.bridge.md) | 2.99x | 1.31-13.00x |
| [`cv.rbridge()`](reference/cv.rbridge.md) | 2.57x | 1.41-3.88x |

![Measured speed ratios for each synthetic design, exponent and
method](reference/figures/benchmark-speedups.svg)

All 96 timing shards and 48 numerical comparisons pass validation.
Selected penalties, variable support and variable counts match exactly;
the maximum relative coefficient/CV difference is 5.51e-14. These
timings use one CV worker and one BLAS thread on an Apple M3 Pro with R
4.6.0. The fixed engine order and single machine limit generalization.
This measures serial optimization against the repaired candidate, rather
than a direct speed comparison with CRAN 1.0.2 or a reproduction of the
article’s numerical study.

See the [benchmark report](inst/doc/benchmarks/REPORT.txt), [case
comparisons](inst/doc/benchmarks/comparison.tsv), [individual
timings](inst/doc/benchmarks/timings.tsv) and [recorded protocol and
environment](inst/doc/benchmarks/provenance.json).

## Guides and citation

- [Reproducibility and validation scope](inst/doc/reproducibility.md)
- [Getting started guide](vignettes/getting-started.Rmd)
- [Version notes](NEWS.md)
- Contribution guidelines: `CONTRIBUTING.md` in the source repository
- [Source repository](https://github.com/byuzbasi/rbridge)
- [Report a reproducible
  issue](https://github.com/byuzbasi/rbridge/issues)

``` r

citation("rbridge")
```

Yuzbasi, B., Arashi, M. and Akdeniz, F. (2021). Penalized regression via
the restricted bridge estimator. *Soft Computing*, **25**, 8401-8416.
[doi:10.1007/s00500-021-05763-9](https://doi.org/10.1007/s00500-021-05763-9).

The package is licensed under GPL-3. Utility-function attribution
inherited from the original source is retained.

The documentation site can be built locally with the optional `pkgdown`
tool:

``` r

pkgdown::build_site()
```

The site includes the working guide, curated function reference and
validation scope. Building it does not publish a website.
