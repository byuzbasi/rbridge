test_that("historical examples fit all four restriction specifications", {
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
  model1 <- rbridge(X, y, q = 1, R1.mat, r1.vec)
  expect_true(all(is.finite(model1$betas)))
  expect_equal(dim(model1$betas), c(p + 1L, 100L))
  model2 <- rbridge(X, y, q = 1, R2.mat, r2.vec)
  expect_true(all(is.finite(model2$betas)))
  expect_equal(dim(model2$betas), c(p + 1L, 100L))
  model3 <- rbridge(X, y, q = 1, R3.mat, r3.vec)
  expect_true(all(is.finite(model3$betas)))
  expect_equal(dim(model3$betas), c(p + 1L, 100L))
  model4 <- rbridge(X, y, q = 1, R4.mat, r4.vec)
  expect_true(all(is.finite(model4$betas)))
  expect_equal(dim(model4$betas), c(p + 1L, 100L))
})
