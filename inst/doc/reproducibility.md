# Reproducibility and validation

The 1.0.3 maintenance candidate starts from the CRAN 1.0.2 source archive,
published on 2020-02-29. It repairs compatibility and interface errors while
retaining the historical local quadratic approximation solver, initialization,
lambda grid, contiguous CV folds, elimination threshold and iteration cap.

The article is [Yuzbasi, Arashi and Akdeniz (2021)](https://doi.org/10.1007/s00500-021-05763-9).
The published algorithm is described in Section 3.1. Package examples demonstrate
the interfaces; they do not claim to reproduce the article's tables or figures.

## Input dictionary

| Input | Meaning |
| --- | --- |
| `X` | Numeric design matrix: rows are observations, columns are predictors |
| `y` | Numeric response vector with one entry per observation |
| `q` | Positive bridge-penalty exponent, at most two |
| `lambda` | Nonnegative fitted penalty grid |
| `R` | Restriction matrix with one column per standardized predictor |
| `r` | Target vector or one-column matrix with one entry per restriction |
| `eta` | Historical elimination and squared coefficient-change threshold |
| `converge` | Initial convergence-distance value |
| `nfolds` | Number of contiguous validation folds |
| `num_threads` | Requested fold-level thread count when supported by the build |

Missing or nonfinite data, constant predictors and incompatible dimensions are
rejected. The package does not impute missing values or silently remove constant
predictors. Supply linearly independent restriction rows. The historical solver
does not return iteration counts or a convergence certificate; validate a fitted
path before substantive interpretation.
Zero penalties are accepted only when the resulting linear systems can be
resolved. Singular or numerically unresolved systems fail explicitly; no
automatic pseudoinverse or approximate estimate is substituted.

## Checking restriction scales

The restriction defines linear combinations of standardized predictor slopes.
The fitted intercept is excluded. To inspect the restriction residual, use the
same column deviations as the solver. In this example `path` is a fitted
restricted model; `X`, `R` and `r` are its supplied inputs.

```r
center <- colMeans(X)
scale <- sqrt(colMeans(sweep(X, 2, center, "-")^2))
standardized_slopes <- path$betas[-1, , drop = FALSE] * scale
restriction_residual <- sweep(R %*% standardized_slopes, 1, as.numeric(r), "-")
max(abs(restriction_residual))
```

The calculated residual measures departure from each supplied standardized-scale
restriction over the fitted path. Thresholding can affect residuals. This check
does not certify convergence or global optimality.

## Local validation

The maintenance tests exercise an independent ridge reference for `q = 2`,
coefficient and prediction methods, restriction scales, small CV fits and invalid
inputs. Use a single BLAS/OpenMP thread for reproducible small checks.
The second-stage tests include 36 frozen synthetic cases from the first
maintenance candidate, with `q = 0.5, 1, 2`, ordinary, wide and correlated designs,
and all four fitting interfaces. They compare coefficient paths, CV errors,
selected penalties and support at the historical threshold. Independent
constrained ridge and uneven-fold CV references provide additional checks.
One- and two-thread requests are compared when supported by the installed build.
These tests are regression checks, not a full simulation study or a proof of
global optimality. Their frozen reference records its source-archive checksum,
seed and R version without requiring the historical package at test time.

From the directory containing a built source archive:

```sh
OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 VECLIB_MAXIMUM_THREADS=1 \
  R CMD check --as-cran rbridge_1.0.3.tar.gz
```

Record `sessionInfo()` and package versions with any study outputs. Examples use
seed 2019. The repository's published version and software environment should be
recorded separately from any data-analysis configuration.

## Performance and parallel builds

Fixed cross-products are cached, active-set products are reused until the set
changes, and the restricted solver shares a linear-system factorization across
coefficient and restriction right-hand sides. CV reuses its training slices and
calculates predictions for the path in one matrix multiplication. Roundoff can
differ from the historical order of operations, so compatibility is checked
numerically rather than asserted to be bit-for-bit on every platform.

`rbridge_info()` reports the actual compiled OpenMP capability. The build uses
R's portable OpenMP flags and falls back to sequential CV when those flags are
empty. No additional OpenMP runtime is installed by the package. Worker errors
are collected and reported after the workers finish. Set BLAS threads to one
before starting R to avoid nested parallelism.

The development workspace supplies a separate timing driver with frozen seeds,
isolated engine processes, warmups, timing shards, resume checks and a SHA-256
manifest. This driver is development tooling and is not a runtime dependency of
the package. Full timings require their numerical gates to pass before any speed
claim. Small smoke timings establish executability only. Record the actual
hardware and engine hashes and report the tested designs with any benchmark.

## Completed timing evidence

The completed timing protocol contains four synthetic designs: 80 observations
and 16 predictors; 300 and 60; 500 and 180; and a wide design with 60 observations
and 120 predictors. The bridge-penalty exponent `q` takes values 0.5, 1 and 2.
Each of the four interfaces uses the same seed 2019, 24 penalties and, where
applicable, five contiguous CV folds. Each engine has two warmups and 15 timed
fits per case, in an isolated R process. Startup and loading are excluded.

The reference is the first repaired 1.0.3 candidate, identified by its engine
hash. Both engines share the same unreleased version number. CV and BLAS use
one thread, so these measurements assess serial engine optimization.
All 96 shards and 48 comparisons pass the SHA-256, dimension and numerical
gates. Coefficient/CV relative differences are at most 5.51e-14 and prediction
differences at most 3.31e-15. Selected penalties, support at 1e-7 and reported
variable counts are identical. A relative error divides the largest absolute
difference by the greater of one and the largest reference magnitude.

| Interface | Median of case speed ratios | Range |
| --- | ---: | ---: |
| `bridge()` | 3.14x | 1.52-14.62x |
| `rbridge()` | 2.59x | 1.18-3.93x |
| `cv.bridge()` | 2.99x | 1.31-13.00x |
| `cv.rbridge()` | 2.57x | 1.41-3.88x |

Each ratio divides the median baseline time by the median optimized time.
Results apply to the recorded Apple M3 Pro, R 4.6.0 environment. They use one
data seed and a fixed baseline-then-candidate order; engine order was not
randomized across sessions. They do not measure accuracy, certify convergence,
or compare directly with the original CRAN 1.0.2 engine.
The timer's precision notice is retained in the provenance; solver warnings
were treated as errors.

Inspect the [report](benchmarks/REPORT.txt),
[case comparisons](benchmarks/comparison.tsv),
[raw timing repetitions](benchmarks/timings.tsv) and
[protocol and environment](benchmarks/provenance.json).
The GitHub preparation includes the frozen historical driver and reference
source for this recorded comparison. Its engine hashes depend on installed
build contents; rebuilding on another platform establishes a new signature.
The current development driver uses byte ordering for file hashes, making
new signatures independent of the operating system's collation setting.
The completed historical run remains verifiable with its frozen driver and
`LC_ALL=en_US.UTF-8`; changing that driver invalidates its recorded signature.

## Reproducing the published study

The package source provides estimators and demonstrations. The original complete
simulation and empirical reproduction drivers are not included in this source
archive. A full reproduction requires the original datasets, configuration,
seeds and scripts; neither the README demonstration nor a successful package
check establishes agreement with the published results.
