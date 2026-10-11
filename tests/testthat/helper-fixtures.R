small_fixture <- function() {
  set.seed(2019)
  X <- matrix(rnorm(80), 20, 4)
  y <- as.numeric(1 + X %*% c(1, 0.5, -0.2, 0) + rnorm(20, sd = 0.1))
  list(X = X, y = y, lambda = c(2, 1))
}
