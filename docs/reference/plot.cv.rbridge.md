# Plot the restricted bridge cross-validation curve

Display validation mean squared error over the fitted penalty grid, with
historical error bars, counts of nonzero slopes and both selected
penalties.

## Usage

``` r
# S3 method for class 'cv.rbridge'
plot(x, sign.lambda = 1, ...)
```

## Arguments

- x:

  Fitted CV object returned by
  [`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md).

- sign.lambda:

  Horizontal-axis direction. The default `1` plots `log(lambda)`; `-1`
  reverses it using `-log(lambda)`. Use positive fitted penalties for
  the logarithmic axis.

- ...:

  Graphical arguments passed to the initial base-R
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) call, such as
  `main`, `xlab`, `ylab` or `ylim`.

## Value

Draws the plot and returns `NULL` invisibly.

## Details

Points show `x$cve`; bars span `x$cvlo` to `x$cvup`. The bars use the
historical `cvse` calculation described in
[`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md),
rather than conventional foldwise standard errors. The top axis shows
full-data nonzero-slope counts, excluding the intercept. Vertical dotted
lines mark `lambda.min` and `lambda.1se`; the lines coincide when both
selectors choose the same fitted penalty. The curve is a diagnostic for
the supplied grid and folds; its shape depends on the data and need not
be U-shaped.

## See also

[`cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/cv.rbridge.md),
[`coef.cv.rbridge`](https://byuzbasi.github.io/rbridge/reference/coef.cv.rbridge.md)

## Author

Bahadir Yuzbasi  
Maintainer: Bahadir Yuzbasi <b.yzb@hotmail.com>

## Examples

``` r
# Reproducible synthetic demonstration: 100 observations, eight predictors.
set.seed(2019)
beta <- c(3, 1.5, 0, 0, 2, 0, 0, 0)
p <- length(beta)
beta <- matrix(beta, nrow = p, ncol = 1)
n <- 100
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- as.numeric(X %*% beta + rnorm(n))
# A user-specified sum of standardized slopes: b[1] + b[2] + b[5] = 6.5.
# This target is illustrative; it is not asserted to equal the generating sum.
R1.mat <- matrix(c(1, 1, 0, 0, 1, 0, 0, 0), nrow = 1)
r1.vec <- matrix(6.5, nrow = 1, ncol = 1)

# Five penalties keep this help example fast; the default is 100.
model1 <- cv.rbridge(X, y, q = 1, R = R1.mat, r = r1.vec, nlambda = 5,
                     nfolds = 3, num_threads = 1)
plot(model1, main = "Cross-validation over the fitted penalty grid")

# Reverse the log-penalty axis without refitting the model.
plot(model1, sign.lambda = -1)
```
