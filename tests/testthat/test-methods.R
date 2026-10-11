test_that("q=2 agrees with an independent ridge reference", {
  d <- small_fixture()
  fit <- bridge(d$X, d$y, q = 2, lambda = d$lambda)
  center <- colMeans(d$X)
  scale <- sqrt(colMeans(sweep(d$X, 2, center, "-")^2))
  XX <- sweep(sweep(d$X, 2, center, "-"), 2, scale, "/")
  expected <- vapply(d$lambda, function(lam) {
    as.numeric(solve(crossprod(XX) + diag(lam, 4), crossprod(XX, d$y - mean(d$y))))
  }, numeric(4))
  expect_equal(unname(fit$betas[-1, ] * scale), unname(expected), tolerance = 1e-10)
})

test_that("path methods return usable coefficients and predictions", {
  d <- small_fixture()
  fit <- bridge(d$X, d$y, q = 2, lambda = d$lambda)
  expect_equal(coef(fit), fit$betas)
  expect_equal(predict(fit, newx = d$X[1:2, , drop = FALSE]),
               cbind(1, d$X[1:2, , drop = FALSE]) %*% fit$betas)
  expect_equal(coef(fit, s = 1), fit$betas[, 2, drop = FALSE])
  expect_equal(predict(fit, newx = d$X[1, ], s = 1),
               as.numeric(c(1, d$X[1, ]) %*% fit$betas[, 2]))
  expect_error(coef(fit, s = "lambda.1se"), "cross-validated")
  expect_error(coef(fit, s = 0.75), "match")
  expect_error(predict(fit, newx = matrix(1, 2, 3)), "columns")
})

test_that("CV defaults select the documented fitted coefficient column", {
  d <- small_fixture()
  cv <- cv.bridge(d$X, d$y, q = 2, lambda = d$lambda, nfolds = 2, num_threads = 1)
  expect_equal(coef(cv), cv$betas[, match(cv$lambda.1se, cv$lambda), drop = FALSE])
  expected <- as.numeric(cbind(1, d$X[1:2, , drop = FALSE]) %*%
                          cv$betas[, match(cv$lambda.min, cv$lambda)])
  expect_equal(predict(cv, newx = d$X[1:2, , drop = FALSE]), expected)
  expect_equal(predict(cv, type = "coefficient", s = "lambda.min"),
               coef(cv, s = "lambda.min"))
})

test_that("restricted paths retain historical standardized-scale constraints", {
  d <- small_fixture()
  X <- sweep(d$X, 2, c(1, 10, 2, 0.5), "*")
  R <- matrix(c(1, 1, 0, 0), 1)
  fit <- rbridge(X, d$y, q = 2, R = R, r = 2, lambda = d$lambda)
  scale <- sqrt(colMeans(sweep(X, 2, colMeans(X), "-")^2))
  expect_equal(as.numeric(R %*% (fit$betas[-1, ] * scale)), c(2, 2), tolerance = 1e-10)
  expect_equal(coef(fit), fit$betas)
  cv <- cv.rbridge(X, d$y, q = 2, R = R, r = 2, lambda = d$lambda,
                  nfolds = 2, num_threads = 1)
  expect_length(predict(cv, newx = X[1:2, , drop = FALSE]), 2)
})

test_that("invalid inputs fail before entering the numerical solver", {
  d <- small_fixture()
  expect_error(bridge(d$X, d$y, q = 0), "q must")
  expect_error(bridge(cbind(d$X, 1), d$y), "constant column")
  expect_error(bridge(d$X, d$y, lambda = -1), "nonnegative")
  expect_error(bridge(d$X, replace(d$y, 1, Inf)), "finite")
  expect_error(cv.bridge(d$X, d$y, q = 2, nfolds = 21), "nfolds")
  expect_error(cv.bridge(d$X, d$y, q = 2, num_threads = 0), "num_threads")
  expect_error(rbridge(d$X, d$y, R = matrix(1, 1, 3), r = 0), "per predictor")
})
