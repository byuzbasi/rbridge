# Small, frozen-seed package audit; no production study is performed.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: validate-package.R LIBRARY OUTPUT_DIRECTORY")
lib <- normalizePath(args[1L], mustWork = TRUE)
out <- args[2L]
if (file.exists(out)) stop("Refusing to overwrite an existing validation directory")
dir.create(out, recursive = TRUE)
.libPaths(c(lib, .libPaths()))
library(rbridge)

set.seed(2019)
n <- 30L
p <- 4L
X <- sweep(matrix(rnorm(n * p), n, p), 2L, c(1, 10, 2, 0.5), "*")
y <- as.numeric(1 + X %*% c(1, 0.1, 0.5, 0) + rnorm(n, sd = 0.1))
lambda <- c(2, 1)
R <- matrix(c(1, 1, 0, 0), nrow = 1L)
r <- matrix(2, nrow = 1L)
checks <- data.frame(check = character(), status = character(), detail = character())
record <- function(name, expression) {
  value <- tryCatch(force(expression), error = function(e) e)
  if (inherits(value, "error")) {
    status <- "FAIL"
    detail <- conditionMessage(value)
  } else {
    status <- if (!is.null(value$status)) value$status else
      if (isTRUE(value$ok)) "PASS" else "FAIL"
    detail <- value$detail
  }
  checks[nrow(checks) + 1L, ] <<- list(name, status, detail)
  cat(name, status, detail, sep = " | ")
  cat("\n")
}

fit <- bridge(X, y, q = 2, lambda = lambda)
restricted <- rbridge(X, y, q = 2, R = R, r = r, lambda = lambda)
center <- colMeans(X)
scale <- sqrt(colMeans(sweep(X, 2L, center, "-")^2))
XX <- sweep(sweep(X, 2L, center, "-"), 2L, scale, "/")
yy <- y - mean(y)
reference <- vapply(lambda, function(lam) {
  as.numeric(solve(crossprod(XX) + diag(lam, p), crossprod(XX, yy)))
}, numeric(p))
record("q2_ridge_reference", {
  error <- max(abs(fit$betas[-1L, , drop = FALSE] * scale - reference))
  list(ok = error < 1e-9, detail = sprintf("max_abs_error=%.12g", error))
})
record("restriction_in_standardized_scale", {
  residual <- max(abs(R %*% (restricted$betas[-1L, , drop = FALSE] * scale) - 2))
  list(ok = residual < 1e-9, detail = sprintf("max_abs_residual=%.12g", residual))
})
record("restriction_in_original_scale", {
  residual <- max(abs(R %*% restricted$betas[-1L, , drop = FALSE] - 2))
  list(status = "EXPECTED_SCALE_DIFFERENCE",
       detail = sprintf("original-scale residual=%.12g; historical restrictions use standardized slopes", residual))
})
record("path_coefficients", {
  value <- coef(fit)
  list(ok = length(value) > 0L && all(is.finite(value)),
       detail = paste("dimensions", paste(dim(value), collapse = "x")))
})
record("path_default_prediction", {
  value <- predict(fit, newx = X[1:2, , drop = FALSE])
  list(ok = length(value) > 0L && all(is.finite(value)),
       detail = paste("length", length(value)))
})

cv <- cv.bridge(X, y, q = 2, lambda = lambda, nfolds = 3, num_threads = 1)
cv_restricted <- cv.rbridge(X, y, q = 2, R = R, r = r, lambda = lambda,
                           nfolds = 3, num_threads = 1)
record("cv_default_prediction", {
  value <- predict(cv, newx = X[1:2, , drop = FALSE])
  list(ok = length(value) == 2L && all(is.finite(value)),
       detail = paste("length", length(value)))
})
record("cv_explicit_prediction", {
  value <- predict(cv, newx = X[1:2, , drop = FALSE], type = "response")
  manual <- as.numeric(cbind(1, X[1:2, , drop = FALSE]) %*% coef(cv, s = "lambda.min"))
  error <- max(abs(value - manual))
  list(ok = error < 1e-12, detail = sprintf("max_abs_error=%.12g", error))
})
record("cv_output_dimensions", {
  valid <- length(cv$cve) == length(lambda) &&
    all(is.finite(cv$cve)) && identical(dim(cv$betas), c(p + 1L, length(lambda))) &&
    all(is.finite(cv_restricted$cve))
  list(ok = valid, detail = paste("lambda_count", length(cv$cve)))
})

additional <- lapply(c(0.5, 1), function(exponent) {
  list(q = exponent,
       bridge = bridge(X, y, q = exponent, lambda = lambda),
       rbridge = rbridge(X, y, q = exponent, R = R, r = r, lambda = lambda))
})
# A small timing smoke test: three fits per version, not a production benchmark.
set.seed(2019)
timing_X <- matrix(rnorm(80L * 16L), 80L, 16L)
timing_y <- as.numeric(timing_X %*% c(1, 0.5, -0.2, rep(0, 13)) + rnorm(80))
timing_R <- diag(16)[15:16, , drop = FALSE]
timing_lambda <- exp(seq(log(10), log(0.1), length.out = 12L))
timing_fit <- function() rbridge(timing_X, timing_y, q = 1, R = timing_R,
                                r = matrix(0, 2L, 1L), lambda = timing_lambda)
invisible(timing_fit())
timings <- vapply(seq_len(3L), function(i) system.time(timing_fit())[["elapsed"]], numeric(1))
write.table(data.frame(repeat_id = seq_along(timings), elapsed_seconds = timings),
            file.path(out, "timing-smoke.tsv"), sep = "\t", row.names = FALSE)
write.table(checks, file.path(out, "checks.tsv"), sep = "\t", row.names = FALSE,
            quote = TRUE)
saveRDS(list(seed = 2019L, X = X, y = y, lambda = lambda, R = R, r = r,
             bridge = fit, rbridge = restricted, cv.bridge = cv,
             cv.rbridge = cv_restricted, additional = additional), file.path(out, "snapshot.rds"))
writeLines(capture.output(sessionInfo()), file.path(out, "sessionInfo.txt"))
cat("Audit complete; failures are recorded for review, not silently corrected.\n")
