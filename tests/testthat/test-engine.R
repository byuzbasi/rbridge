test_that("optimized paths and CV agree with frozen maintenance results", {
  reference <- readRDS(test_path("fixtures", "maintenance-reference.rds"))
  for (id in names(reference$cases)) {
    case <- reference$cases[[id]]
    actual <- do.call(get(case$method), case$args)
    expect_identical(class(actual), class(case$expected), info = id)
    expect_identical(names(actual), names(case$expected), info = id)
    for (field in intersect(c("betas", "lambda", "cve", "cvse", "cvup", "cvlo"),
                            names(actual))) {
      expect_equal(actual[[field]], case$expected[[field]], tolerance = 1e-8,
                   info = paste(id, field))
    }
    expect_identical(abs(actual$betas) > 1e-7,
                     abs(case$expected$betas) > 1e-7, info = id)
    if (startsWith(case$method, "cv.")) {
      expect_identical(actual$nz, case$expected$nz, info = id)
      expect_identical(actual$lambda.min, case$expected$lambda.min, info = id)
      expect_identical(actual$lambda.1se, case$expected$lambda.1se, info = id)
    }
  }
})

test_that("q=2 restricted paths satisfy an independent constrained ridge reference", {
  d <- small_fixture()
  R <- rbind(c(1, 1, 0, 0), c(0, 0, 0, 1))
  r <- matrix(c(0.5, 0), 2, 1)
  center <- colMeans(d$X)
  scale <- sqrt(colMeans(sweep(d$X, 2, center, "-")^2))
  XX <- sweep(sweep(d$X, 2, center, "-"), 2, scale, "/")
  expected <- vapply(d$lambda, function(lam) {
    A <- crossprod(XX) + diag(lam, 4)
    b <- solve(A, crossprod(XX, d$y - mean(d$y)))
    G <- solve(A, t(R))
    as.numeric(b - G %*% solve(R %*% G, R %*% b - r))
  }, numeric(4))
  fit <- rbridge(d$X, d$y, q = 2, R = R, r = r, lambda = d$lambda)
  expect_equal(unname(fit$betas[-1, ] * scale), expected, tolerance = 1e-10)
  expect_equal(unname(R %*% (fit$betas[-1, ] * scale)),
               matrix(rep(r, 2), 2), tolerance = 1e-10)
})

test_that("uneven CV folds retain their original q=2 error aggregation", {
  d <- small_fixture()
  XX <- sweep(d$X, 2, colMeans(d$X), "-")
  XX <- sweep(XX, 2, sqrt(colMeans(XX^2)), "/")
  yy <- d$y - mean(d$y)
  n <- nrow(XX)
  boundaries <- as.integer(seq(0, n, length.out = 4))
  errors <- matrix(0, length(d$lambda), 3)
  for (fold in 1:3) {
    test <- seq.int(boundaries[fold] + 1L, boundaries[fold + 1L])
    train <- setdiff(seq_len(n), test)
    for (i in seq_along(d$lambda)) {
      b <- solve(crossprod(XX[train, ]) + diag(d$lambda[i], ncol(XX)),
                 crossprod(XX[train, ], yy[train]))
      intercept <- mean(yy[train]) - as.numeric(colMeans(XX[train, ]) %*% b)
      predicted <- intercept + as.numeric(XX[test, ] %*% b)
      errors[i, fold] <- sum((yy[test] / sqrt(n) - predicted / sqrt(n))^2)
    }
  }
  fit <- cv.bridge(d$X, d$y, q = 2, lambda = d$lambda, nfolds = 3, num_threads = 1)
  expect_equal(unname(fit$cve), rowSums(errors), tolerance = 1e-10)
})

test_that("thread requests preserve fitted paths and selected penalties", {
  d <- small_fixture()
  for (method in c("cv.bridge", "cv.rbridge")) {
    args <- c(d, list(q = 1, nfolds = 3))
    if (method == "cv.rbridge") {
      args <- c(args, list(R = matrix(c(0, 0, 0, 1), 1), r = 0))
    }
    sequential <- do.call(get(method), c(args, list(num_threads = 1)))
    parallel <- do.call(get(method), c(args, list(num_threads = 2)))
    expect_equal(parallel, sequential, tolerance = 1e-12)
  }
  info <- rbridge_info()
  expect_type(info$openmp, "logical")
  expect_length(info$openmp, 1)
  expect_equal(info$version, as.character(packageVersion("rbridge")))
})

test_that("unidentified restriction systems fail safely including worker failures", {
  d <- small_fixture()
  R <- rbind(c(1, 0, 0, 0), c(1, 0, 0, 0))
  expect_error(rbridge(d$X, d$y, q = 2, R = R, r = c(0, 0), lambda = 1),
               "singular or numerically unresolved")
  expect_error(cv.rbridge(d$X, d$y, q = 2, R = R, r = c(0, 0), lambda = 1,
                          nfolds = 3, num_threads = 2), "CV fold 1")
})

test_that("single predictors and the supplied initial distance remain supported", {
  d <- small_fixture()
  for (q in c(0.5, 1, 2)) {
    fit <- bridge(d$X[, 1, drop = FALSE], d$y, q = q, lambda = c(5, 1))
    expect_true(all(is.finite(fit$betas)))
    expect_equal(dim(fit$betas), c(2L, 2L))
  }
  fit <- bridge(d$X, d$y, q = 2, lambda = d$lambda, converge = 0)
  expect_true(all(is.finite(fit$betas)))
  expect_equal(fit$lambda, d$lambda)
})
